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


# ONE CELL, WHOLE. The `[^>]*?` must stay NON-GREEDY: a greedy one runs past a self-closing
# cell's own "/>" (a "/" is not a ">") and then matches the `>.*?</c>` branch instead, swallowing
# every cell up to the next </c>. Those swallowed cells keep their old r= and the column insert
# silently leaves them pointing one column to the left of where their values now sit.
_CELL_RX = r'<c r="([A-Z]+)%d"[^>]*?(?:/>|>.*?</c>)'


def _shared_strings(zin):
    """Ordered <si> text, so a header written as t="s" can be matched by name."""
    if "xl/sharedStrings.xml" not in zin.namelist():
        return []
    sx = zin.read("xl/sharedStrings.xml").decode("utf-8")
    return ["".join(re.findall(r"<t[^>]*>(.*?)</t>", si, flags=re.S))
            for si in re.findall(r"<si>(.*?)</si>", sx, flags=re.S)]


def _cell_text(cell_xml, sst):
    if 't="s"' in cell_xml:
        v = re.search(r"<v>(\d+)</v>", cell_xml)
        return sst[int(v.group(1))] if v and int(v.group(1)) < len(sst) else ""
    return "".join(re.findall(r"<t[^>]*>(.*?)</t>", cell_xml, flags=re.S))


def insert_column(path, sheet_name, header, after_header, default=None):
    """Insert a column directly AFTER the one whose header is `after_header`.

    add_column() appends, which is all a new trailing field needs. A column that has to sit next
    to a related one cannot append: every cell from the insertion point rightwards has to be
    renumbered, and so do the sheet's <cols> widths and the Excel table's column list. Excel
    repairs a workbook where any of those three disagree, which is the whole reason this is one
    function and not three edits.

    REFUSES a sheet carrying anything else that counts COLUMNS -- the same contract
    insert_workbook_rows.R states for rows. Layout_Plate is plain cells plus one table and passes;
    a sheet with merges, validation dropdowns or formulas does not, and is left untouched rather
    than silently repointed at the wrong column.

    `default` is written as an inline string into the rows where the after-column has a value, so
    a blank plate row stays blank. Styles are copied from the after-column, cell by cell, so the
    new column looks like the one it sits beside.
    """
    zin = zipfile.ZipFile(path)
    names = zin.namelist()
    wb = zin.read("xl/workbook.xml").decode("utf-8")
    order = re.findall(r'<sheet[^>]*name="([^"]+)"', wb)
    idx = order.index(sheet_name) + 1
    spath = f"xl/worksheets/sheet{idx}.xml"
    x = zin.read(spath).decode("utf-8")

    for tag in ("mergeCell", "dataValidation", "conditionalFormatting", "hyperlink",
                "autoFilter", "sheetProtection"):
        if re.search(r"<%s[ />]" % tag, x):
            raise SystemExit(f"{path}: {sheet_name} contains <{tag}>, which counts columns. "
                             "Shift it by hand.")
    if re.search(r"<f[ >]", x):
        raise SystemExit(f"{path}: {sheet_name} contains formulas; their references would not "
                         "be shifted.")
    # A definedName may name a column range on this sheet. Print_Titles as $1:$1 is rows only.
    for dn in re.findall(r"<definedName[^>]*>([^<]*)</definedName>", wb):
        if sheet_name in dn and re.search(r"\$[A-Z]+\$?\d*:", dn):
            raise SystemExit(f"{path}: definedName '{dn}' spans columns of {sheet_name}. "
                             "Shift it by hand.")

    sst = _shared_strings(zin)
    hdr = re.search(r'<row[^>]*r="1"[^>]*>.*?</row>', x, flags=re.S)
    if not hdr:
        raise SystemExit(f"{path}: {sheet_name} has no row 1 to insert a header into")
    after_col = None
    for m in re.finditer(_CELL_RX % 1, hdr.group(0), flags=re.S):
        if _cell_text(m.group(0), sst).strip() == after_header:
            after_col = m.group(1)
    if after_col is None:
        raise SystemExit(f"{path}: {sheet_name} row 1 has no header '{after_header}'")
    ins = col_index(after_col) + 1
    new_col = col_letter(ins)
    last_col_i = max(col_index(c) for c in re.findall(r'<c r="([A-Z]+)\d+"', x))

    def shift_row(row_xml, row_no):
        cells = list(re.finditer(_CELL_RX % row_no, row_xml, flags=re.S))
        if not cells:
            return row_xml
        src = None            # the after-column's cell in THIS row, for its style and its value
        for m in cells:
            if m.group(1) == after_col:
                src = m.group(0)
        out, pos = [], 0
        placed = False
        for m in cells:
            ci = col_index(m.group(1))
            out.append(row_xml[pos:m.start()])
            if ci >= ins and not placed:
                out.append(_new_cell(row_no, src))
                placed = True
            txt = m.group(0)
            if ci >= ins:
                txt = txt.replace('r="%s%d"' % (m.group(1), row_no),
                                  'r="%s%d"' % (col_letter(ci + 1), row_no), 1)
            out.append(txt)
            pos = m.end()
        tail = row_xml[pos:]
        if not placed:        # the row ends before the insertion point
            tail = _new_cell(row_no, src) + tail
        out.append(tail)
        row_xml = "".join(out)
        return re.sub(r'(<row[^>]*?)spans="1:\d+"', rf'\1spans="1:{last_col_i + 1}"', row_xml)

    def _new_cell(row_no, src):
        style = ""
        if src:
            s = re.search(r'\ss="(\d+)"', src)
            if s:
                style = ' s="%s"' % s.group(1)
        if row_no == 1:
            return f'<c r="{new_col}1"{style} t="inlineStr"><is><t>{esc(header)}</t></is></c>'
        # Only where the after-column actually holds a value: a blank plate row stays blank.
        has_value = bool(src) and ("<v>" in src or "<is>" in src)
        if default is None or not has_value:
            return f'<c r="{new_col}{row_no}"{style}/>'
        return (f'<c r="{new_col}{row_no}"{style} t="inlineStr">'
                f'<is><t>{esc(default)}</t></is></c>')

    out, pos = [], 0
    for m in re.finditer(r'<row[^>]*?r="(\d+)"[^>]*?>.*?</row>', x, flags=re.S):
        out.append(x[pos:m.start()])
        out.append(shift_row(m.group(0), int(m.group(1))))
        pos = m.end()
    out.append(x[pos:])
    x = "".join(out)

    # column widths: shift everything at or past the insertion point, then clone the neighbour's
    def fix_cols(block):
        kept = []
        for c in re.findall(r"<col [^/]*/>", block):
            lo = int(re.search(r'min="(\d+)"', c).group(1))
            hi = int(re.search(r'max="(\d+)"', c).group(1))
            if lo == col_index(after_col):
                kept.append(c)
                kept.append(c.replace(f'min="{lo}"', f'min="{ins}"').replace(f'max="{hi}"',
                                                                            f'max="{ins}"'))
            elif lo >= ins:
                kept.append(c.replace(f'min="{lo}"', f'min="{lo+1}"').replace(f'max="{hi}"',
                                                                             f'max="{hi+1}"'))
            else:
                kept.append(c)
        return "<cols>" + "".join(kept) + "</cols>"
    if "<cols>" in x:
        x = re.sub(r"<cols>.*?</cols>", lambda m: fix_cols(m.group(0)), x, count=1, flags=re.S)

    dim = re.search(r'<dimension ref="([A-Z]+\d+):([A-Z]+)(\d+)"', x)
    if dim:
        x = x.replace(f'ref="{dim.group(1)}:{dim.group(2)}{dim.group(3)}"',
                      f'ref="{dim.group(1)}:{col_letter(last_col_i + 1)}{dim.group(3)}"', 1)

    patched = {spath: x.encode("utf-8")}

    rels_path = f"xl/worksheets/_rels/sheet{idx}.xml.rels"
    if rels_path in names:
        rels = zin.read(rels_path).decode("utf-8")
        for t in [t for t in re.findall(r'Target="([^"]+)"', rels) if "tables/" in t]:
            tpath = "xl/" + t.replace("../", "")
            tx = zin.read(tpath).decode("utf-8")
            tm = re.search(r'ref="([A-Z]+\d+):([A-Z]+)(\d+)"', tx)
            if not tm:
                continue
            wider = col_letter(col_index(tm.group(2)) + 1)
            tx = tx.replace(f':{tm.group(2)}{tm.group(3)}"', f':{wider}{tm.group(3)}"')
            cnt = int(re.search(r'tableColumns count="(\d+)"', tx).group(1))
            ids = [int(i) for i in re.findall(r'<tableColumn id="(\d+)"', tx)]
            tcs = re.findall(r"<tableColumn [^/]*/>", tx)
            # INSERT AT THE SAME INDEX AS THE CELLS. tableColumn order IS the table's column
            # order; appending here would label the new cells with the last column's name.
            at = next((i for i, c in enumerate(tcs)
                       if re.search(r'name="([^"]*)"', c).group(1) == after_header), None)
            if at is None:
                raise SystemExit(f"{tpath}: no tableColumn named '{after_header}'")
            tcs.insert(at + 1, f'<tableColumn id="{max(ids)+1}" name="{esc(header)}"/>')
            tx = re.sub(r"<tableColumns count=\"\d+\">.*?</tableColumns>",
                        f'<tableColumns count="{cnt+1}">' + "".join(tcs) + "</tableColumns>",
                        tx, count=1, flags=re.S)
            patched[tpath] = tx.encode("utf-8")

    tmp = path + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            # The SharePoint sync client injects [trash]/NNNN.dat into any xlsx in a synced
            # library. They are never content, and carrying them through makes the package
            # malformed -- the same drop insert_workbook_rows.R makes.
            if item.filename.startswith("[trash]/"):
                continue
            zout.writestr(item, patched.get(item.filename, zin.read(item.filename)))
    zin.close()
    shutil.move(tmp, path)
    return new_col, sorted(patched)


if __name__ == "__main__":
    # add_layout_col.py <file.xlsx> <SheetName> <header> [after_header] [default]
    #   with after_header: INSERT directly after that column. Without: APPEND at the end.
    a = sys.argv[1:]
    if len(a) < 3:
        raise SystemExit(__doc__ + "\nusage: add_layout_col.py <file.xlsx> <Sheet> <header> "
                                   "[after_header] [default]")
    if len(a) >= 4 and a[3]:
        col, parts = insert_column(a[0], a[1], a[2], a[3], a[4] if len(a) > 4 else None)
        print(f"inserted {a[2]} at column {col} of {a[1]} in {a[0]}")
    else:
        col, parts = add_column(a[0], a[1], a[2])
        print(f"appended {a[2]} at column {col} of {a[1]} in {a[0]}")
    for p in parts:
        print("   patched", p)
