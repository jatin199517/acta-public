"""Rename, insert and delete COLUMNS of an .xlsx worksheet, at the XML level.

Companion to add_layout_col.py (which only APPENDS) and insert_workbook_rows.R (rows, and it
refuses a sheet carrying a table). 3.6.8 renames two Layout_Plate columns, inserts two in the
middle and deletes one, and none of those is an append.

WHY NOT openxlsx: loadWorkbook + saveWorkbook rewrites the whole workbook and mangles shared
strings carrying xml:space="preserve". Everything except the one sheet XML and its table XML is
copied through byte-for-byte here.

ONLY SAFE ON A SHEET WHOSE ONLY STRUCTURE IS ITS TABLE. Moving columns re-letters every cell, so a
data validation, conditional format, merge range, hyperlink or autoFilter anchored to a column
letter would silently point at the wrong column afterwards. This script REFUSES such a sheet
rather than corrupting it. Measured on TEMPLATE_Ab_Titration_Instructions.xlsx before writing it:
Layout_Plate has none of those -- 0 dataValidation, 0 conditionalFormatting, 0 mergeCell,
0 hyperlink, no autoFilter -- and its one defined name is Print_Titles on row 1, which is
unaffected by column changes.

Headers are written as INLINE strings, so xl/sharedStrings.xml is never opened or rewritten.

Usage:
  python3 layout_columns.py <file.xlsx> <SheetName> <op> [<op> ...]
    op := rename:OLD=NEW | insert-after:ANCHOR=NEW | insert-first:NEW | delete:NAME
  Ops are applied in the order given, each against the result of the last.
"""
import re, shutil, sys, zipfile

BLOCKERS = ("dataValidation", "conditionalFormatting", "mergeCell", "hyperlink", "autoFilter")


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
    # NO xml:space="preserve". The attribute is valid OOXML and readxl handles it, but openxlsx
    # returns it as part of the VALUE -- a cell reading `#CD3` comes back as
    # `xml:space="preserve">#CD3`. ACTA reads with readxl and is blind to it; users open these
    # workbooks with whatever they have. Nothing shipped needs it, so a value that WOULD need it
    # is refused here rather than written and caught two gates later.
    if t != t.strip():
        raise SystemExit("REFUSED: %r has leading/trailing whitespace, which would need "
                         'xml:space="preserve" -- trim it or decide deliberately.' % t)
    return t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")


CELL_RE = re.compile(r'<c\b[^>]*?r="([A-Z]+)(\d+)"[^>]*?(?:/>|>.*?</c>)', re.S)
ROW_RE = re.compile(r'<row\b[^>]*?r="(\d+)"[^>]*?(?:/>|>.*?</row>)', re.S)


def sheet_target(zin, sheet_name):
    wb = zin.read("xl/workbook.xml").decode("utf8")
    rid = dict(re.findall(r'<sheet name="([^"]+)"[^>]*?r:id="(rId\d+)"', wb)).get(sheet_name)
    if not rid:
        raise SystemExit("no sheet named %r" % sheet_name)
    rels = zin.read("xl/_rels/workbook.xml.rels").decode("utf8")
    tgt = dict(re.findall(r'Id="(rId\d+)"[^>]*?Target="([^"]+)"', rels))[rid]
    return "xl/" + tgt.lstrip("/")


def header_text(cell_xml, shared):
    """The displayed text of a header cell, inline or shared."""
    if 't="inlineStr"' in cell_xml:
        m = re.search(r"<t[^>]*>(.*?)</t>", cell_xml, re.S)
        return m.group(1) if m else ""
    if 't="s"' in cell_xml:
        m = re.search(r"<v>(\d+)</v>", cell_xml)
        return shared[int(m.group(1))] if m else ""
    m = re.search(r"<t[^>]*>(.*?)</t>", cell_xml, re.S)
    return m.group(1) if m else ""


def main():
    path, sheet_name, ops = sys.argv[1], sys.argv[2], sys.argv[3:]
    zin = zipfile.ZipFile(path)
    names = zin.namelist()
    starget = sheet_target(zin, sheet_name)
    sx = zin.read(starget).decode("utf8")

    for b in BLOCKERS:
        if "<%s" % b in sx:
            raise SystemExit("REFUSED: %s carries <%s>; moving columns would silently re-point it."
                             % (sheet_name, b))

    shared = []
    if "xl/sharedStrings.xml" in names:
        ss = zin.read("xl/sharedStrings.xml").decode("utf8")
        for si in ss.split("<si>")[1:]:
            body = si.split("</si>")[0]
            shared.append("".join(re.findall(r"<t[^>]*>(.*?)</t>", body, re.S)))

    # table that covers this sheet, if any
    trel = starget.replace("worksheets/", "worksheets/_rels/") + ".rels"
    tpath = None
    if trel in names:
        r = zin.read(trel).decode("utf8")
        m = re.search(r'Target="([^"]*tables/[^"]+)"', r)
        if m:
            tpath = "xl/" + m.group(1).replace("../", "")

    # ---- current header order, from row 1 -------------------------------------------------
    rows = {}
    for rm in ROW_RE.finditer(sx):
        rows[int(rm.group(1))] = rm.group(0)
    if 1 not in rows:
        raise SystemExit("sheet has no row 1")
    hdr_cells = {}
    for cm in CELL_RE.finditer(rows[1]):
        hdr_cells[cm.group(1)] = cm.group(0)
    order = [header_text(hdr_cells[L], shared)
             for L in sorted(hdr_cells, key=col_index)]
    letters = sorted(hdr_cells, key=col_index)
    pos = {name: letters[i] for i, name in enumerate(order)}

    # ---- apply ops ------------------------------------------------------------------------
    new_order = list(order)
    inserted = []
    for op in ops:
        kind, _, rest = op.partition(":")
        if kind == "rename":
            old, _, new = rest.partition("=")
            if old not in new_order:
                raise SystemExit("rename: no column named %r (have: %s)" % (old, new_order))
            new_order[new_order.index(old)] = new
            if old in pos:
                pos[new] = pos.pop(old)
        elif kind == "insert-after":
            anchor, _, new = rest.partition("=")
            if anchor not in new_order:
                raise SystemExit("insert-after: no column named %r" % anchor)
            new_order.insert(new_order.index(anchor) + 1, new)
            inserted.append(new)
        elif kind == "insert-first":
            new_order.insert(0, rest)
            inserted.append(rest)
        elif kind == "delete":
            if rest not in new_order:
                raise SystemExit("delete: no column named %r" % rest)
            new_order.remove(rest)
        else:
            raise SystemExit("unknown op %r" % op)

    # ---- rewrite every row in the new column order ----------------------------------------
    out_rows = []
    for rn in sorted(rows):
        rxml = rows[rn]
        cells = {}
        for cm in CELL_RE.finditer(rxml):
            cells[cm.group(1)] = cm.group(0)
        attrs = re.match(r"<row\b([^>]*?)(?:/)?>", rxml).group(1)
        attrs = re.sub(r'\s+spans="[^"]*"', "", attrs)
        parts = []
        for i, name in enumerate(new_order, 1):
            L = col_letter(i)
            if rn == 1:
                parts.append('<c r="%s%d" t="inlineStr"><is><t>%s</t></is></c>'
                             % (L, rn, esc(name)))
                continue
            if name in inserted:
                continue                       # new column: no data cells, reads back as NA
            src = pos.get(name)
            if src is None or src not in cells:
                continue
            c = cells[src]
            parts.append(re.sub(r'r="[A-Z]+%d"' % rn, 'r="%s%d"' % (L, rn), c, count=1))
        if not parts:
            out_rows.append('<row%s/>' % attrs)
        else:
            out_rows.append('<row%s spans="1:%d">%s</row>' % (attrs, len(new_order), "".join(parts)))

    last = col_letter(len(new_order))
    maxrow = max(rows)
    body = "".join(out_rows)
    sx_new = re.sub(r"<sheetData>.*?</sheetData>", lambda m: "<sheetData>%s</sheetData>" % body,
                    sx, count=1, flags=re.S)
    sx_new = re.sub(r'<dimension ref="[^"]*"/>', '<dimension ref="A1:%s%d"/>' % (last, maxrow),
                    sx_new, count=1)

    # ---- table ----------------------------------------------------------------------------
    tx_new = None
    if tpath:
        tx = zin.read(tpath).decode("utf8")
        tx_new = re.sub(r'ref="[A-Z]+\d+:[A-Z]+\d+"', 'ref="A1:%s%d"' % (last, maxrow), tx)
        cols = "".join('<tableColumn id="%d" name="%s"/>' % (i, esc(n))
                       for i, n in enumerate(new_order, 1))
        tx_new = re.sub(r"<tableColumns[^>]*>.*?</tableColumns>",
                        '<tableColumns count="%d">%s</tableColumns>' % (len(new_order), cols),
                        tx_new, count=1, flags=re.S)
        tx_new = re.sub(r'(<autoFilter ref=)"[^"]*"', r'\1"A1:%s%d"' % (last, maxrow), tx_new)

    tmp = path + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            if item.filename.startswith("[trash]/"):
                continue
            if item.filename == starget:
                zout.writestr(item, sx_new)
            elif tpath and item.filename == tpath:
                zout.writestr(item, tx_new)
            else:
                zout.writestr(item, zin.read(item.filename))
    zin.close()
    shutil.move(tmp, path)
    print("  %s: %d -> %d columns" % (sheet_name, len(order), len(new_order)))
    print("  now:", ", ".join(new_order))


if __name__ == "__main__":
    main()
