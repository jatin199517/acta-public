"""Add a worksheet to an .xlsx, at the XML level, from a TSV.

WHY NOT openxlsx: loadWorkbook + saveWorkbook rewrites the whole workbook and mangles shared
strings carrying xml:space="preserve". Adding a sheet should not rewrite the sheets already
there, and here nothing but xl/workbook.xml, its rels and [Content_Types].xml is touched --
every existing part is copied through byte-for-byte.

Cells are written as INLINE strings, so xl/sharedStrings.xml is never opened. Everything is
written as text: a concentration is metadata to be published verbatim, not a number to be
reformatted by whatever locale opens the file.

IN THE REPO ON PURPOSE, like its siblings: anything that produces a tracked artefact has to be
tracked itself, or the only way to regenerate a shipped workbook disappears with the scratch
directory it was written in.

Usage:
  python3 add_sheet.py <file.xlsx> <SheetName> <data.tsv>
    data.tsv  tab-separated, first line is the header row
  Refuses if a sheet of that name already exists.
"""
import re, shutil, sys, zipfile

CT = "http://schemas.openxmlformats.org/package/2006/content-types"


def col_letter(n):
    s = ""
    while n:
        n, r = divmod(n - 1, 26)
        s = chr(65 + r) + s
    return s


def esc(t):
    # NO xml:space="preserve". The attribute is valid OOXML and readxl handles it, but openxlsx
    # returns it as part of the VALUE -- a cell reading `#CD3` comes back as
    # `xml:space="preserve">#CD3`. ACTA reads with readxl and is blind to it; users open these
    # workbooks with whatever they have. Nothing shipped needs it, so a value that WOULD need it
    # is refused here rather than written and caught two gates later.
    if t != t.strip():
        raise SystemExit("REFUSED: %r has leading/trailing whitespace, which would need "
                         'xml:space="preserve" -- trim it or decide deliberately.' % t)
    return (t.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
             .replace('"', "&quot;"))


def main():
    path, sheet_name, tsv = sys.argv[1], sys.argv[2], sys.argv[3]
    rows = [l.rstrip("\n").split("\t") for l in open(tsv, encoding="utf8") if l.strip("\n") != ""]
    if not rows:
        raise SystemExit("empty TSV")
    ncol = max(len(r) for r in rows)

    zin = zipfile.ZipFile(path)
    names = zin.namelist()
    wb = zin.read("xl/workbook.xml").decode("utf8")
    if re.search(r'<sheet name="%s"' % re.escape(sheet_name), wb):
        raise SystemExit("REFUSED: %r already has a sheet named %r" % (path, sheet_name))

    used = [int(m) for m in re.findall(r'Id="rId(\d+)"',
            zin.read("xl/_rels/workbook.xml.rels").decode("utf8"))]
    new_rid = "rId%d" % (max(used) + 1 if used else 1)
    existing = [int(m) for m in re.findall(r"worksheets/sheet(\d+)\.xml", " ".join(names))]
    new_n = (max(existing) + 1) if existing else 1
    new_target = "worksheets/sheet%d.xml" % new_n
    sheet_ids = [int(m) for m in re.findall(r'sheetId="(\d+)"', wb)]
    new_sid = (max(sheet_ids) + 1) if sheet_ids else 1

    body = []
    for ri, r in enumerate(rows, 1):
        cells = []
        for ci, v in enumerate(r, 1):
            if v == "":
                continue
            cells.append('<c r="%s%d" t="inlineStr"><is><t>%s</t></is></c>'
                         % (col_letter(ci), ri, esc(v)))
        body.append('<row r="%d" spans="1:%d">%s</row>' % (ri, ncol, "".join(cells)))
    sheet_xml = (
        '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>\n'
        '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">'
        '<dimension ref="A1:%s%d"/><sheetViews><sheetView workbookViewId="0"/></sheetViews>'
        '<sheetFormatPr defaultRowHeight="15"/><sheetData>%s</sheetData></worksheet>'
        % (col_letter(ncol), len(rows), "".join(body)))

    wb_new = re.sub(r"</sheets>",
                    '<sheet name="%s" sheetId="%d" r:id="%s"/></sheets>'
                    % (esc(sheet_name), new_sid, new_rid), wb, count=1)
    rels = zin.read("xl/_rels/workbook.xml.rels").decode("utf8")
    rels_new = re.sub(
        r"</Relationships>",
        '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/'
        'relationships/worksheet" Target="%s"/></Relationships>' % (new_rid, new_target),
        rels, count=1)
    ct = zin.read("[Content_Types].xml").decode("utf8")
    ct_new = re.sub(
        r"</Types>",
        '<Override PartName="/xl/%s" ContentType="application/vnd.openxmlformats-officedocument.'
        'spreadsheetml.worksheet+xml"/></Types>' % new_target, ct, count=1)

    tmp = path + ".tmp"
    with zipfile.ZipFile(tmp, "w", zipfile.ZIP_DEFLATED) as zout:
        for item in zin.infolist():
            if item.filename.startswith("[trash]/"):
                continue
            if item.filename == "xl/workbook.xml":
                zout.writestr(item, wb_new)
            elif item.filename == "xl/_rels/workbook.xml.rels":
                zout.writestr(item, rels_new)
            elif item.filename == "[Content_Types].xml":
                zout.writestr(item, ct_new)
            else:
                zout.writestr(item, zin.read(item.filename))
        zout.writestr("xl/" + new_target, sheet_xml)
    zin.close()
    shutil.move(tmp, path)
    print("  added sheet %r (%d rows x %d cols) to %s"
          % (sheet_name, len(rows), ncol, path.split("/")[-1]))


if __name__ == "__main__":
    main()
