"""Append a column to a worksheet, at the XML level, and extend its Excel table.

WHY NOT openxlsx: loadWorkbook + saveWorkbook rewrites the whole workbook and MANGLES shared
strings carrying xml:space="preserve" -- it turned the TEMPLATE's gating_template `dims` cell
"CD3, FSC-A" into 'xml:space="preserve">CD3, FSC-A'. A blank column is not worth rewriting a
workbook that also holds data validation dropdowns and three other sheets.

Everything except the one sheet XML and the one table XML is copied through byte-for-byte. The new
header is written as an INLINE string so xl/sharedStrings.xml is never opened, let alone rewritten.
"""
import re, shutil, sys, zipfile

def col_letter(n):
    s = ""
    while n:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s

def col_index(letters):
    n = 0
    for ch in letters:
        n = n * 26 + (ord(ch) - 64)
    return n

def esc(t):
    return t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")

def add_column(path, sheet_name, header, values=None):
    """values: {sheet_row_number: numeric_value} for data cells, or None for header only."""
    zin = zipfile.ZipFile(path)
    names = zin.namelist()
    wb = zin.read("xl/workbook.xml").decode("utf-8")
    order = re.findall(r'<sheet[^>]*name="([^"]+)"', wb)
    idx = order.index(sheet_name) + 1
    spath = f"xl/worksheets/sheet{idx}.xml"
    x = zin.read(spath).decode("utf-8")

    # ANCHOR ON THE HEADER ROW, not on the sheet dimension and not on the widest row.
    # Two sheets here have <dimension ref="A1"> -- no range at all -- and fall through to the
    # cells; a single styled or stray cell down at AA then made the "last column" AA, and the new
    # header landed at AB with ...25/...26/...27 phantom columns in front of it. Row 1 IS the
    # header row, so the column after its last cell is the only correct place for a new header.
    hdr = re.search(r'<row[^>]*r="1"[^>]*>.*?</row>', x, flags=re.S)
    if not hdr:
        raise SystemExit(f"{path}: {sheet_name} has no row 1 to append a header to")
    hcells = re.findall(r'<c r="([A-Z]+)1"', hdr.group(0))
    if not hcells:
        raise SystemExit(f"{path}: {sheet_name} row 1 has no cells")
    last_col = max(hcells, key=col_index)
    dim = re.search(r'<dimension ref="([A-Z]+)\d+:([A-Z]+)(\d+)"', x)
    last_row = int(dim.group(3)) if dim else max(
        int(r) for _, r in re.findall(r'<c r="([A-Z]+)(\d+)"', x))
    new_col = col_letter(col_index(last_col) + 1)

    def put(row_xml, row_no, cell_xml):
        # cells must stay in column order, and the new one is the last, so append before </row>
        row_xml = row_xml.replace("</row>", cell_xml + "</row>")
        return re.sub(r'(<row[^>]*?)spans="1:\d+"', rf'\1spans="1:{col_index(new_col)}"', row_xml)

    out, pos = [], 0
    for m in re.finditer(r'<row[^>]*r="(\d+)"[^>]*>.*?</row>', x, flags=re.S):
        row_no = int(m.group(1))
        cell = None
        if row_no == 1:
            cell = f'<c r="{new_col}1" t="inlineStr"><is><t>{esc(header)}</t></is></c>'
        elif values and row_no in values:
            cell = f'<c r="{new_col}{row_no}"><v>{values[row_no]}</v></c>'
        if cell is None:
            continue
        out.append(x[pos:m.start()]); out.append(put(m.group(0), row_no, cell)); pos = m.end()
    out.append(x[pos:])
    x = "".join(out)
    if dim:
        x = x.replace(f'<dimension ref="{dim.group(1)}1:{dim.group(2)}{last_row}"',
                      f'<dimension ref="{dim.group(1)}1:{new_col}{last_row}"', 1)

    patched = {spath: x.encode("utf-8")}

    # the Excel table, if this sheet has one, so the column lands INSIDE it
    rels_path = f"xl/worksheets/_rels/sheet{idx}.xml.rels"
    if rels_path in names:
        rels = zin.read(rels_path).decode("utf-8")
        for t in [t for t in re.findall(r'Target="([^"]+)"', rels) if "tables/" in t]:
            tpath = "xl/" + t.replace("../", "")
            tx = zin.read(tpath).decode("utf-8")
            tm = re.search(r'ref="([A-Z]+\d+):([A-Z]+)(\d+)"', tx)
            if not tm:
                continue
            cnt = int(re.search(r'tableColumns count="(\d+)"', tx).group(1))
            ids = [int(i) for i in re.findall(r'<tableColumn id="(\d+)"', tx)]
            tx = tx.replace(f':{tm.group(2)}{tm.group(3)}"', f':{new_col}{tm.group(3)}"')
            tx = tx.replace(f'tableColumns count="{cnt}"', f'tableColumns count="{cnt+1}"')
            tx = tx.replace("</tableColumns>",
                            f'<tableColumn id="{max(ids)+1}" name="{esc(header)}"/></tableColumns>')
            patched[tpath] = tx.encode("utf-8")

    tmp = path + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            zout.writestr(item, patched.get(item.filename, zin.read(item.filename)))
    zin.close()
    shutil.move(tmp, path)
    return new_col, sorted(patched)
