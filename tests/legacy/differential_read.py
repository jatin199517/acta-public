#!/usr/bin/env python3
"""Read every shipped workbook with a SECOND implementation and report where it disagrees with R.

    python3 differential_read.py <dump.json> [more.json ...]

WHY THIS EXISTS. The row-F defect was a string silently reinterpreted at a library boundary, and
test_pdata_restore.R now probes nineteen such shapes through GatingSet(). But that probe starts
from a frame R already holds -- it cannot see a value that was ALREADY reinterpreted on the way
in, when readxl guessed the type of a workbook column.

A single reader cannot detect its own guessing: it reports what it decided, confidently. Two
readers can. openpyxl exposes the cell's STORED type and its raw value, so a disagreement with
readxl's text rendering is exactly the signal wanted -- a cell where the type system, not the
operator, chose what the value says.

The R side writes <dump.json> (see differential_read.R); this reads the same workbooks with
openpyxl and diffs. Run as:  Rscript differential_read.R  -- which calls this.

NOT a style check. It reports only cells where the two readers produce different TEXT for the
same cell, which is the thing that can reach a published artefact.
"""
import json
import re
import sys
from pathlib import Path

try:
    from openpyxl import load_workbook
except ImportError:
    print("openpyxl is not installed -- a reader that cannot read must not report clean.")
    print("  python3 -m pip install openpyxl")
    sys.exit(2)


def as_text(v):
    """Render a cell the way a person reading the sheet would, so the comparison is about
    MEANING and not about float formatting. None and empty both become None, matching the R
    side's norm()."""
    if v is None:
        return None
    if isinstance(v, bool):
        return "TRUE" if v else "FALSE"
    if isinstance(v, float):
        # 3.0 -> "3", not "3.0": readxl renders a whole number without the trailing zero, and a
        # disagreement about that is formatting, not reinterpretation.
        if v == int(v) and abs(v) < 1e15:
            return str(int(v))
        return repr(v)
    if isinstance(v, int):
        return str(v)
    s = str(v).strip()
    return s if s and s != "NA" else None


def read_sheet(path, sheet):
    # read_only=False DELIBERATELY. In read-only mode openpyxl trusts the sheet's stored
    # <dimension> element, and these workbooks are written at the XML level by this project's own
    # tools, which do not maintain it. MEASURED on OQ_Test1!gating_template: read_only=True
    # reports dims=A1:A1, max_column=1 and yields ONE column of SIXTEEN -- silently, with no
    # error. That is the same fault this whole file exists to detect, inside the detector.
    wb = load_workbook(path, data_only=True, read_only=False)
    if sheet not in wb.sheetnames:
        wb.close()
        return None
    ws = wb[sheet]
    rows = [[as_text(c.value) for c in r] for r in ws.iter_rows()]
    wb.close()
    if not rows:
        return None
    hdr = [h for h in rows[0]]
    return hdr, rows[1:]


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        sys.exit(2)
    total = diffs = sheets = 0
    for dump_path in sys.argv[1:]:
        dump = json.loads(Path(dump_path).read_text())
        for entry in dump:
            path, sheet = entry["file"], entry["sheet"]
            r_hdr, r_rows = entry["header"], entry["rows"]
            got = read_sheet(path, sheet)
            if got is None:
                print(f"  [FAIL] {Path(path).name}!{sheet}: openpyxl could not read it")
                diffs += 1
                continue
            p_hdr, p_rows = got
            sheets += 1
            # ALIGN BY HEADER NAME, NOT BY POSITION.
            #
            # readxl normalises the grid and openpyxl does not: readxl drops fully-empty columns
            # and renames a blank header to "...N", so the Nth column of one is not the Nth of the
            # other. Comparing positionally reported 4322 "disagreements" on the shipped
            # workbooks, every one of them a value sitting one column over from where it was
            # expected -- a harness artefact that would have buried a real finding completely.
            #
            # A header readxl generated ("...2") names no operator-authored column, so it is
            # skipped rather than reported: there is nothing on that side to be reinterpreted.
            def index_of(hdrs, name):
                try:
                    return hdrs.index(name)
                except ValueError:
                    return None

            # readxl repairs a BLANK header to "...N" and a DUPLICATE one to "name...N".
            # Neither names an operator-authored column, so neither is comparable.
            generated = lambda h: h is None or re.search(r"\.\.\.\d+$", str(h)) is not None
            shared = [h for h in r_hdr
                      if not generated(h) and index_of(p_hdr, h) is not None]
            # A header present in R and absent from openpyxl IS reported: the two readers
            # disagreeing about what a column is called is the same class of fault as
            # disagreeing about a value, and "Channel ($PnN)" slipped a comparison exactly once.
            for h in r_hdr:
                if generated(h):
                    continue
                if index_of(p_hdr, h) is None:
                    print(f"  [FAIL] {Path(path).name}!{sheet}: column {h!r} seen by readxl, "
                          f"not by openpyxl")
                    diffs += 1
            n = min(len(r_rows), len(p_rows))
            for i in range(n):
                for h in shared:
                    rj, pj = index_of(r_hdr, h), index_of(p_hdr, h)
                    a = r_rows[i][rj] if rj < len(r_rows[i]) else None
                    b = p_rows[i][pj] if pj < len(p_rows[i]) else None
                    total += 1
                    if a != b:
                        diffs += 1
                        print(f"  [FAIL] {Path(path).name}!{sheet} row {i+2} {h!r}: "
                              f"R={a!r} openpyxl={b!r}")
    print(f"\n{total} cell(s) across {sheets} sheet(s) read by both; {diffs} disagreement(s)")
    sys.exit(1 if diffs else 0)


if __name__ == "__main__":
    main()
