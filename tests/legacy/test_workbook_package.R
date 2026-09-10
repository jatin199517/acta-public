## Every .xlsx this version ships must be a well-formed OPC package, and actaOpcScrub() must keep
## the ones we write that way.
##
## WHY THIS GATE EXISTS. Six shipped workbooks -- the TEMPLATE in both versions and OQ_Test1/OQ_Test2
## -- were malformed for an unknown number of releases and nothing noticed, because every reader we
## use is tolerant. readxl reads them, the OQ suite passed on them, and the pre-flight passed on
## them. EXCEL is the strict one: it offers to "recover" the file instead of opening it, which is
## what a colleague hit. So the only thing that can catch this is a check on the package itself, and
## it has to be a check nobody has to remember to run.
##
## Three distinct defects were found, and all three are pinned here:
##
##   1. DANGLING RELATIONSHIPS. Every sheet's .rels declared a `drawing` -> ../drawings/drawingN.xml
##      and a `vmlDrawing` -> ../drawings/vmlDrawingN.vml that were NOT IN THE ARCHIVE. openxlsx
##      4.2.8.1 writes both unconditionally -- reproduced below from a workbook built from scratch --
##      so this is not historical: it is written fresh into the titration export on every run, which
##      is why actaOpcScrub() runs after the save and not just over the files in the repo.
##
##   2. OVERRIDES FOR PARTS THAT ARE NOT THERE. [Content_Types].xml declares a content type for each
##      of those absent drawings. Removing the relationships alone leaves this behind, which is the
##      same fault stated in a different file -- it is why the first version of the scrub still left
##      a fresh workbook with two findings.
##
##   3. PARTS WITH NO CONTENT TYPE. Five `[trash]/000N.dat` entries holding Windows printer DEVMODE
##      blobs, with neither an Override nor a `.dat` Default, under a part name beginning with `[`
##      which OPC reserves. Referenced by nothing.
##      The note here used to say these "came with the original workbook ... a one-off repair, not a
##      recurring one". THAT WAS WRONG, and being wrong hid a live fault for four versions: as of
##      2026-09-08 no shipped workbook contains a [trash] part, yet every generated titration export
##      in 2_94..2_97 had exactly five. openxlsx stages the printerSettings blobs it cannot map
##      under [trash]/ and zips them into whatever it writes next, so it recurs on every run -- and
##      the scrub's repair was being written to the wrong path (see actaOpcScrub) so it never took.
##
## Rscript <this> [version_dir]   -- defaults to the version folder this file lives in.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }

## The conformance check itself now lives in ACTA_Functions.R as actaOpcFindings(), so that the
## scrub verifies its own repair against the SAME definition of "valid" this gate applies.
opcFindings <- h$actaOpcFindings
chk("the shared validator is available from ACTA_Functions.R", is.function(opcFindings))

cat("=== every shipped workbook is a well-formed package ===\n")
books <- sort(c(Sys.glob(file.path(vd, "Template", "*.xlsx")),
                Sys.glob(file.path(actaTestCaseRoot(), "OQ_Test*", "*Instructions*.xlsx")),
                Sys.glob(file.path(vd, "*Instructions*.xlsx"))))
chk("there are workbooks to check at all", length(books) >= 4L)
## THE SUBJECT IS THE STAGED BLOB, NOT THE WORKING FILE. What ships is the committed content, and in
## this repository the two differ within seconds of any write: the OneDrive sync client re-injects
## customXml/* and [trash]/*.dat into every Office file in the library. Checking the worktree meant
## this gate failed on bytes nobody will ever receive, while saying nothing about the bytes that do
## publish. `git show :./path` is immutable once staged, so it is the honest subject. Falls back to
## the working file when the workbook is not staged (a bare copy, or a file not yet added).
## ANCHORED AT THE FILE'S OWN DIRECTORY, not at vd. `:./x` is resolved relative to whatever `git -C`
## points at, so the previous version had to strip a vd prefix off the path -- which silently stopped
## working the moment a workbook lived outside vd. In the package layout the Template is under
## inst/pipeline/ but the case workbooks are under inst/extdata/oq_small/, so the prefix did not
## match, `rel` stayed ABSOLUTE, and `:./​<abs>` is not a path git can resolve: every case workbook
## reported "not staged" and fell back to the working file, which is the one subject this gate exists
## to avoid. dirname/basename needs no prefix arithmetic and holds for any layout.
.stagedCopy <- function(p) {
  tf  <- tempfile(fileext = ".xlsx")
  st  <- suppressWarnings(system2("git", c("-C", shQuote(dirname(p)), "show",
                                           shQuote(paste0(":./", basename(p)))),
                                  stdout = tf, stderr = FALSE))
  if (!identical(as.integer(st), 0L) || !file.exists(tf) || file.size(tf) == 0) { unlink(tf); return(NA_character_) }
  tf
}
for (b in books) {
  s <- .stagedCopy(b)
  subj <- if (is.na(s)) b else s
  f <- opcFindings(subj)
  chk(sprintf("%s%s", basename(b), if (is.na(s)) " (worktree -- not staged)" else " (staged blob)"),
      length(f) == 0L)
  for (x in f) cat("         ", x, "\n")
  if (!is.na(s)) unlink(s)
}

cat("=== actaOpcScrub() repairs what openxlsx writes ===\n")
## Built from scratch, so a pass here is about the CURRENT openxlsx and not about a file's history.
## If openxlsx is ever fixed upstream, `before` becomes 0 and this check says so rather than
## silently passing -- the scrub would then be belt-and-braces, which is worth knowing.
tmp <- file.path(tempdir(), sprintf("opcgate_%s", basename(tempfile(""))))
dir.create(tmp, showWarnings = FALSE)
p <- file.path(tmp, "fresh.xlsx")
suppressMessages({
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "One")
  openxlsx::writeData(wb, "One", data.frame(n = 1:3, s = c("x_1", "CD8+", "a b"),
                                            stringsAsFactors = FALSE))
  openxlsx::addWorksheet(wb, "Two")
  openxlsx::writeData(wb, "Two", data.frame(z = "kept", stringsAsFactors = FALSE))
  openxlsx::saveWorkbook(wb, p, overwrite = TRUE)
})
before <- opcFindings(p)
chk(sprintf("openxlsx %s still writes the defect (%d finding(s)) -- the scrub is load-bearing",
            as.character(utils::packageVersion("openxlsx")), length(before)),
    length(before) > 0L)
## Both halves matter, and the Override half cost a failure: with the relationships gone the package
## still declared a content type for a drawing that is not in the archive.
chk("they are dangling drawing/vmlDrawing rels and Overrides for the same missing parts",
    all(grepl("dangling .* -> \\.\\./drawings/(vml)?[Dd]rawing|Override names a missing part: /xl/drawings/",
              before)) &&
      any(grepl("^Override names a missing part", before)) &&
      any(grepl("dangling", before)))

chk("the scrub reports that it changed something", isTRUE(h$actaOpcScrub(p)))
chk("the package is then clean", length(opcFindings(p)) == 0L)

## The scrub rewrites the archive, so the cells have to be re-read, not assumed.
d1 <- suppressMessages(readxl::read_excel(p, sheet = "One"))
d2 <- suppressMessages(readxl::read_excel(p, sheet = "Two"))
chk("no cell was lost or altered",
    identical(d1$n, c(1, 2, 3)) && identical(as.character(d1$s), c("x_1", "CD8+", "a b")) &&
      identical(as.character(d2$z), "kept"))
chk("both sheets survive, in order",
    identical(readxl::excel_sheets(p), c("One", "Two")))
## printerSettings LOOKS like the same defect and is not: openxlsx declares the relationship, the
## `<pageSetup r:id>` that uses it, the .bin part and a `.bin` Default. That chain is complete, so
## the scrub must leave every link of it alone -- removing any one would break a valid workbook.
sh <- paste(readLines(unz(p, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
rl <- paste(readLines(unz(p, "xl/worksheets/_rels/sheet1.xml.rels"), warn = FALSE), collapse = "")
chk("the printerSettings chain is untouched",
    grepl("<pageSetup[^>]*paperSize[^>]*r:id=", sh) && grepl("printerSettings", rl) &&
      "xl/printerSettings/printerSettings1.bin" %in% utils::unzip(p, list = TRUE)$Name)
chk("a second call is a no-op", isFALSE(h$actaOpcScrub(p)))

## THE REFERENCE GUARD. In every workbook seen so far the sheet body does NOT use the invented
## relationship -- so this branch would never run on real input, and untested code that never runs
## is the kind that is wrong when it finally does. A `<drawing r:id>` is injected here on purpose:
## dropping the relationship while leaving the reference would trade the fault for its mirror image.
q <- file.path(tmp, "referenced.xlsx")
stage <- file.path(tmp, "stage"); dir.create(stage, showWarnings = FALSE)
suppressMessages({
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "One")
  openxlsx::writeData(wb, "One", data.frame(n = 1:2))
  openxlsx::saveWorkbook(wb, q, overwrite = TRUE)
})
parts <- utils::unzip(q, exdir = stage)
sf <- file.path(stage, "xl", "worksheets", "sheet1.xml")
b  <- paste(readLines(sf, warn = FALSE), collapse = "")
writeLines(sub("</worksheet>", '<drawing r:id="rId1"/></worksheet>', b, fixed = TRUE), sf,
           useBytes = TRUE)
owd <- setwd(stage)
utils::zip("../referenced.xlsx", files = utils::unzip(q, list = TRUE)$Name, flags = "-Xq9")
setwd(owd)
b0 <- paste(readLines(unz(q, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
chk("the fixture really does reference the invented relationship", grepl("<drawing r:id=", b0))
h$actaOpcScrub(q)
b1 <- paste(readLines(unz(q, "xl/worksheets/sheet1.xml"), warn = FALSE), collapse = "")
chk("a referencing <drawing> element is removed with its relationship", !grepl("<drawing", b1))
chk("and the package is clean afterwards", length(opcFindings(q)) == 0L)
chk("the sheet's data is still readable",
    identical(suppressMessages(readxl::read_excel(q, sheet = "One"))$n, c(1, 2)))
## ON A COPY. This used to call actaOpcScrub() on the TRACKED workbook itself -- a release gate
## writing to a shipped repository artifact, which a reviewer hit on 2026-09-08 and had to restore
## with `git checkout`. The assertion only needs an undamaged package, not that particular file.
## From the STAGED blob, not the working file. The premise is "a package with nothing wrong with
## it", and the working copy does not satisfy that here: the sync client re-injects [trash] parts
## within seconds, so the scrub correctly finds work to do and the assertion inverted.
.src3 <- books[grepl("OQ_Test3", books)][1]
.staged3 <- .stagedCopy(.src3)
if (is.na(.staged3)) {
  cat("  [skip] OQ_Test3 is not staged, so no known-undamaged package is available here\n")
} else {
  chk("a workbook that was never damaged is left alone", isFALSE(h$actaOpcScrub(.staged3)))
  unlink(.staged3)
}
chk("a path that does not exist is not an error",
    isFALSE(h$actaOpcScrub(file.path(tmp, "nope.xlsx"))))
unlink(tmp, recursive = TRUE)

cat("=== actaFindInstructions(): which files count as THE workbook ===\n")
## Found while repairing the workbooks above: the discovery pattern was written twice and the copies
## drifted -- the OQ runner anchored the extension, the script did not -- so a backup left beside a
## workbook counted as a second one and the run died with "found 2" naming a file that is not a
## workbook. Excluding `~$` matters more than the anchoring did: Excel writes that lock file WHILE
## THE WORKBOOK IS OPEN, so filling the sheet in, leaving it open and pressing Run would have failed
## on a hidden file the analyst never created.
fd <- file.path(tempdir(), sprintf("findinstr_%s", basename(tempfile(""))))
dir.create(fd, showWarnings = FALSE)
real <- "EXP1_Ab_Titration_Instructions_3_0.xlsx"
for (f in c(real,
            paste0(real, ".orig"),                 # a backup, as left by the repair
            paste0("~$", real),                    # Excel's lock file, present while it is OPEN
            paste0("._", real),                    # macOS AppleDouble over OneDrive/SMB
            "EXP1_Ab_Titration_Instructions_3_0.xlsx.bak",
            "EXP1_Ab_Titration_Instructions_3_0.xls",
            "EXP1_Ab_Titration_Layout_2_86.xlsx",  # the pre-2_87 name: must NOT count
            "notes.txt"))
  invisible(file.create(file.path(fd, f)))
got <- h$actaFindInstructions(fd)
chk("exactly the one real workbook is found", identical(got, real))
chk("full.names returns a usable path",
    identical(h$actaFindInstructions(fd, full.names = TRUE), file.path(fd, real)))
chk("an empty folder returns character(0), not NA or \"\"",
    identical(h$actaFindInstructions(file.path(fd, "nope")), character(0)))
## Two REAL workbooks must still both be reported -- the callers turn that into their own error, and
## silently picking one would run the wrong layout.
invisible(file.create(file.path(fd, "EXP2_Ab_Titration_Instructions_3_0.xlsx")))
chk("two real workbooks are both reported, for the caller to reject",
    length(h$actaFindInstructions(fd)) == 2L)
unlink(fd, recursive = TRUE)

cat("=== what ACTA WRITES is a well-formed package, at its final path ===\n")
## The gate above only ever looked at workbooks we SHIP. The titration export is the one we write on
## every run, and it was malformed in every version from 2_94 to 2_97 while this file passed --
## because nothing here opened it. Any export left by a previous OQ run is checked; a fresh
## writeTitrationExport() through a RELATIVE path (which is what ACTA_Script passes) is checked too,
## since that relative path is exactly what defeated the scrub.
## TWO CAUSES, TOLD APART. An export on disk here can be malformed for a reason that is ACTA's
## (openxlsx's dangling drawings, which the scrub exists to remove) or for one that is not: if this
## checkout lives in a OneDrive/SharePoint synced library, the sync client writes its own
## document-library metadata into every Office file -- 9 customXml parts, docProps/custom.xml and
## five [trash]/000N.dat blobs that carry NO content type. Traced 2026-09-08 with a control copy in
## /tmp; see the note beside actaOpcScrub() in ACTA_Functions.R. Blaming the pipeline for that would
## make this gate cry wolf on the very machine ACTA is developed on, so [trash]-only findings are
## reported as the environment's doing and anything else still fails.
SYNC_ONLY <- "^part with no content type: \\[trash\\]/[0-9]+\\.dat$"
gen <- Sys.glob(file.path(actaTestCaseRoot(), "OQ_Test*", "Outputs", "*TitrationExport*.xlsx"))
if (!length(gen)) cat("  (no previous OQ export on disk -- run the OQ cases to cover this)\n")
for (g in gen) {
  f <- opcFindings(g)
  mine <- grep(SYNC_ONLY, f, invert = TRUE, value = TRUE)
  chk(sprintf("generated export %s", basename(g)), length(mine) == 0L)
  for (x in mine) cat("         ", x, "\n")
  if (length(f) && !length(mine))
    cat(sprintf("          note: %d [trash] part(s) injected by the file-sync client, not by ACTA\n",
                length(f)))
}

relp <- file.path(tempdir(), sprintf("opcrel_%s", basename(tempfile(""))))
dir.create(relp, showWarnings = FALSE)
oldwd <- setwd(relp)
res <- try({
  suppressMessages(h$writeTitrationExport(data.frame(Reagent = c("CD4", "CD8"), SI = c(9.1, 4.2),
                                                     stringsAsFactors = FALSE),
                                          "rel_export.xlsx"))
  opcFindings("rel_export.xlsx")
}, silent = TRUE)
setwd(oldwd)
chk("an export written through a RELATIVE path is well formed",
    !inherits(res, "try-error") && length(res) == 0L)
if (!inherits(res, "try-error")) for (x in res) cat("         ", x, "\n")
unlink(relp, recursive = TRUE)

cat(if (ok) "\nWORKBOOK PACKAGE: all checks passed\n" else "\nWORKBOOK PACKAGE: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
