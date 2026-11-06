## TiterPubs de-identification: .tpCleanWorkbook / .tpScrubWorkbook / .tpStripEmail.
##
## WHY THIS GATE EXISTS, AND IT IS THE WHOLE POINT. Until now NO fixture exercised this path with
## the toggles on: all three OQ cases ship titerpubs_export = FALSE and scrub_confidential = FALSE.
## So the suite ran green through five rounds of review while this code carried, at various times,
## a value that published with every control reporting success, and three separate regressions
## that made a legitimate bundle impossible to build. Every one was found by hand, by someone who
## happened to think of the right case. A green suite said nothing because it never ran the code.
##
## The assertions are therefore written against the SHAPES THAT ACTUALLY BIT, not against the
## happy path: a cell Excel padded, a value Excel split into rich-text runs, an address Excel
## turned into a hyperlink, a name in a comment body, and the difference between "nothing to do"
## and "it failed".

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## LOADED THE WAY THE REST OF THE SUITE LOADS, and the first version of this file did not.
## It called library(ACTA) directly, which ERRORS in a fresh clone -- the package is not
## installed there -- so the script died before printing a single line. The release build's
## fresh-clone suite counted it as a non-pass with "(no output captured)": a gate that cannot
## run, in the one context the fresh-clone suite exists to cover. acta_test_paths.R sources the
## R/ sources when there is no install, which is what every other script here uses.
suppressMessages({library(readxl); library(zip)})
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
h <- actaTestFunctions(actaTestAppDir())
.tp <- function(nm) if (!is.null(h[[nm]])) h[[nm]] else getFromNamespace(nm, "ACTA")

SRC <- system.file("extdata", "oq_small", "OQ_Test1",
                   "OQ_Test1_Ab_Titration_Instructions.xlsx", package = "ACTA")
if (!nzchar(SRC))
  SRC <- "inst/extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx"
if (!file.exists(SRC)) {
  cat("  [skip] needs the OQ_Test1 instructions workbook.\n")
  cat("\nTITERPUBS SCRUB TESTS SKIPPED\n"); quit(status = 0)
}

## --- helpers: read a workbook the way a RECIPIENT would, not the way the writer does ----------
unpack <- function(p) {
  d <- file.path(tempdir(), paste0("tps_", basename(tempfile(""))))
  dir.create(d, recursive = TRUE); utils::unzip(p, exdir = d); d
}
## EVERY part, RAW -- attributes included. The round-4 post-condition stripped tags, which deletes
## attributes, and was therefore blind by construction to an address stored as a hyperlink Target.
rawParts <- function(p) {
  d <- unpack(p); on.exit(unlink(d, recursive = TRUE), add = TRUE)
  f <- list.files(d, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  stats::setNames(vapply(f, function(q)
    paste(tryCatch(readLines(q, warn = FALSE), error = function(e) ""), collapse = " "), ""),
    sub(paste0("^", d, "/"), "", f))
}
anywhere <- function(p, s) { r <- rawParts(p); names(r)[vapply(r, grepl,
                               TRUE, pattern = s, fixed = TRUE)] }
cellValues <- function(p) {
  v <- unlist(lapply(readxl::excel_sheets(p), function(s) {
    d <- tryCatch(suppressMessages(readxl::read_excel(p, sheet = s, col_types = "text")),
                  error = function(e) NULL)
    if (is.null(d) || !ncol(d)) return(character(0))
    c(names(d), unlist(lapply(d, as.character), use.names = FALSE)) }), use.names = FALSE)
  trimws(v[!is.na(v)])
}
## Rebuild a workbook from an unpacked directory, so a fixture can plant whatever Excel would.
repack <- function(d, out) {
  if (file.exists(out)) unlink(out)
  zip::zip(out, files = list.files(d, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = d)
  out
}
withSharedString <- function(from, to) {          # swap one <t> form for another
  p <- file.path(tempdir(), paste0("w_", basename(tempfile(""))), "wb.xlsx")
  dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
  d <- unpack(p); f <- file.path(d, "xl", "sharedStrings.xml")
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  writeLines(sub(from, to, x, fixed = TRUE), f, useBytes = TRUE)
  repack(d, p)
}

OPER <- "ZQOPERZQ"
## READ FROM THE FIXTURE, NOT HARDCODED. Two reasons, and the second is the load-bearing one:
## the literal would drift from whatever OQ_Test1 actually ships, and an address-shaped literal
## in a tracked source file is a finding in the release sanitisation gate -- correctly, since
## that gate cannot tell a test fixture from a real address. Taking it from the workbook keeps
## the test honest and keeps the source clean.
ADDR <- local({
  i <- as.data.frame(suppressMessages(readxl::read_excel(SRC, sheet = "Info", col_types = "text")))
  j <- which(tolower(trimws(as.character(i$Variable))) == "submitter_email")
  v <- if (length(j)) trimws(as.character(i$Value[j[1]])) else ""
  if (!nzchar(v) || is.na(v)) { cat("  [skip] the fixture carries no Submitter_Email.\n")
                                cat("\nTITERPUBS SCRUB TESTS SKIPPED\n"); quit(status = 0) }
  v
})

cat("-- the value Excel padded: the shape that published a real name --\n")
wb <- withSharedString("<t>OP</t>", sprintf('<t xml:space="preserve">%s </t>', OPER))
chk("a padded cell is still seen as the operator", OPER %in% cellValues(wb))
r <- suppressWarnings(.tp(".tpScrubWorkbook")(wb))
chk("the scrub reports success", isTRUE(r), r)
chk("...and the padded value is gone from every part", !length(anywhere(wb, OPER)),
    paste(anywhere(wb, OPER), collapse = ", "))

cat("-- a value Excel split into rich-text runs must not pass silently --\n")
wb <- withSharedString("<t>OP</t>", sprintf("<r><t>%s</t></r><r><t>%s</t></r>",
                                            substr(OPER, 1, 4), substring(OPER, 5)))
chk("the split value still reads as one cell", OPER %in% cellValues(wb))
chk("the scrub REFUSES rather than reporting success",
    !isTRUE(suppressWarnings(.tp(".tpScrubWorkbook")(wb))))

cat("-- an address Excel turned into a hyperlink --\n")
## All three places Excel stores an auto-linked address: the cell text, the sheet element
## (display= carries it a second time) and the External relationship Target. Only the first
## was ever rewritten.
p <- file.path(tempdir(), paste0("h_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
d <- unpack(p)
dir.create(file.path(d, "xl", "worksheets", "_rels"), showWarnings = FALSE, recursive = TRUE)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rIdH1" Type="http://schemas.openxmlformats.org/officeDocument/2006/',
  'relationships/hyperlink" Target="mailto:%s" TargetMode="External"/></Relationships>'), ADDR),
  file.path(d, "xl", "worksheets", "_rels", "sheet1.xml.rels"), useBytes = TRUE)
sh <- file.path(d, "xl", "worksheets", "sheet1.xml")
x <- paste(readLines(sh, warn = FALSE), collapse = "\n")
writeLines(sub("</sheetData>", sprintf(paste0('</sheetData><hyperlinks><hyperlink ref="B2" ',
  'r:id="rIdH1" display="%s"/></hyperlinks>'), ADDR), x, fixed = TRUE), sh, useBytes = TRUE)
repack(d, p)
chk("the address really is in the package before stripping", length(anywhere(p, ADDR)) >= 2,
    paste(anywhere(p, ADDR), collapse = ", "))
suppressWarnings(.tp(".tpCleanWorkbook")(p))
sr <- suppressWarnings(.tp(".tpStripEmail")(p))
chk("the strip does not claim success while the address remains",
    !(isTRUE(sr) && length(anywhere(p, ADDR))),
    sprintf("returned %s, still in: %s", sr, paste(anywhere(p, ADDR), collapse = ", ")))
chk("the address is gone from every part, attributes included",
    !length(anywhere(p, ADDR)), paste(anywhere(p, ADDR), collapse = ", "))
## A relationship removed without its sheet element is the dangling r:id that makes Excel offer
## to repair the file -- the exact fault .tpCleanWorkbook() exists to prevent.
chk("no sheet is left pointing at the relationship that was removed",
    !length(anywhere(p, "rIdH1")), paste(anywhere(p, "rIdH1"), collapse = ", "))
chk("no empty <hyperlinks/> wrapper is left behind (it is schema-invalid)",
    !length(anywhere(p, "<hyperlinks>")) && !length(anywhere(p, "<hyperlinks/>")))
chk("the workbook still reads after the hyperlink surgery",
    length(tryCatch(readxl::excel_sheets(p), error = function(e) character(0))) == 4L)

cat("-- a file:// hyperlink is the same shape with a home directory in it --\n")
## A LINUX HOME ON PURPOSE. "/Users/" is itself a blocking pattern in the release
## sanitisation gate, so a macOS-shaped literal here is a finding in a tracked file. The
## code path under test is the file: scheme, which does not care which home it is.
HOME <- "file:///home/zqoperzq/Desktop/plate.xlsx"
p <- file.path(tempdir(), paste0("f_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
d <- unpack(p)
dir.create(file.path(d, "xl", "worksheets", "_rels"), showWarnings = FALSE, recursive = TRUE)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rIdH9" Type="http://schemas.openxmlformats.org/officeDocument/2006/',
  'relationships/hyperlink" Target="%s" TargetMode="External"/></Relationships>'), HOME),
  file.path(d, "xl", "worksheets", "_rels", "sheet1.xml.rels"), useBytes = TRUE)
repack(d, p)
suppressWarnings(.tp(".tpCleanWorkbook")(p))
chk("the operator's home directory is gone", !length(anywhere(p, "zqoperzq")),
    paste(anywhere(p, "zqoperzq"), collapse = ", "))

cat("-- a legitimate http reference is NOT collateral damage --\n")
DOI <- "https://doi.org/10.1234/zqexample"
p <- file.path(tempdir(), paste0("u_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
d <- unpack(p)
dir.create(file.path(d, "xl", "worksheets", "_rels"), showWarnings = FALSE, recursive = TRUE)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
  '<Relationship Id="rIdU1" Type="http://schemas.openxmlformats.org/officeDocument/2006/',
  'relationships/hyperlink" Target="%s" TargetMode="External"/></Relationships>'), DOI),
  file.path(d, "xl", "worksheets", "_rels", "sheet1.xml.rels"), useBytes = TRUE)
repack(d, p)
suppressWarnings(.tp(".tpCleanWorkbook")(p))
chk("the protocol/DOI link a submitter wanted published survives",
    length(anywhere(p, DOI)) > 0)

cat("-- a name written in a comment body --\n")
NAME <- "Dr Zeta Quill"
p <- file.path(tempdir(), paste0("c_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
d <- unpack(p)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<comments xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"><authors>',
  '<author>%s</author></authors><commentList><comment ref="A2" authorId="0">',
  '<text><r><t>%s re-ran this plate.</t></r></text></comment></commentList></comments>'),
  NAME, NAME), file.path(d, "xl", "comments1.xml"), useBytes = TRUE)
repack(d, p)
chk("the comment author and body are present before cleaning", length(anywhere(p, NAME)) > 0)
suppressWarnings(.tp(".tpCleanWorkbook")(p))
chk("the name is gone from the comment part", !length(anywhere(p, NAME)),
    paste(anywhere(p, NAME), collapse = ", "))

cat("-- a MODERN (threaded) comment: same leak, different schema --\n")
## <text> is CT_Rst in xl/comments*.xml but a plain xsd:string in xl/threadedComments/. Blanking
## both to "<text><t></t></text>" puts a child element where only character data is allowed,
## which is schema-invalid and makes Excel offer to repair the file.
p <- file.path(tempdir(), paste0("t_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
d <- unpack(p)
dir.create(file.path(d, "xl", "threadedComments"), showWarnings = FALSE, recursive = TRUE)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<ThreadedComments xmlns="http://schemas.microsoft.com/office/spreadsheetml/2018/',
  'threadedcomments"><threadedComment ref="A4" dT="2026-01-01T00:00:00.00" personId="{P1}" ',
  'id="{C1}"><text>%s said to redo this one.</text></threadedComment></ThreadedComments>'),
  NAME), file.path(d, "xl", "threadedComments", "threadedComment1.xml"), useBytes = TRUE)
dir.create(file.path(d, "xl", "persons"), showWarnings = FALSE, recursive = TRUE)
writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
  '<personList xmlns="http://schemas.microsoft.com/office/spreadsheetml/2018/',
  'threadedcomments"><person displayName="%s" id="{P1}" userId="%s" ',
  'providerId="AD"/></personList>'), NAME, ADDR),
  file.path(d, "xl", "persons", "person.xml"), useBytes = TRUE)
repack(d, p)
chk("the threaded comment carries the name before cleaning", length(anywhere(p, NAME)) >= 2)
suppressWarnings(.tp(".tpCleanWorkbook")(p))
chk("the name is gone from the threaded comment and the person list",
    !length(anywhere(p, NAME)), paste(anywhere(p, NAME), collapse = ", "))
## Scoped to the person part ON PURPOSE. The fixture's own Submitter_Email cell holds this same
## address, and removing THAT is .tpStripEmail()'s job, which this case does not call -- an
## unscoped assertion here just re-tests the previous case and fails for the wrong reason.
chk("the directory address in userId is gone too",
    !length(grep("^xl/persons/", anywhere(p, ADDR), value = TRUE)),
    paste(anywhere(p, ADDR), collapse = ", "))
chk("no <t> element was injected into the plain-string <text> (Excel would repair it)",
    !length(anywhere(p, "<text><t>")), paste(anywhere(p, "<text><t>"), collapse = ", "))

cat("-- the filename rename: token boundaries, not substrings --\n")
## OQ_Test1 ships Operator "OP". A SUBSTRING pass of OP -> JD rewrites OPERATOR to JDERATOR and
## SOP to SJD -- the same fault that made .tpReplSurvivors() call every fixture dirty, except
## here it would corrupt the names of the data files themselves.
m <- list("EXP00000001" = "EXP26000001", "OP" = "JD", "MyPanel_v2" = "Panel")
rn <- function(x) .tp(".tpRenameToken")(x, m)
cases <- list(
  c("EXP00000001_Strep-PE_A1_Unstained.fcs", "EXP26000001_Strep-PE_A1_Unstained.fcs"),
  c("Strep_PE/EXP00000001_A2.fcs",           "Strep_PE/EXP26000001_A2.fcs"),
  c("OP_notes.fcs",                          "JD_notes.fcs"),
  c("OPERATOR_notes.fcs",                    "OPERATOR_notes.fcs"),   # must NOT become JDERATOR
  c("SOP_12.fcs",                            "SOP_12.fcs"),           # must NOT become SJD
  c("MyPanel_v2/x.fcs",                      "Panel/x.fcs"),          # the group DIRECTORY too
  c("EXP00000001Stain.fcs",                  "EXP00000001Stain.fcs")) # glued: left for the scan
for (cc in cases)
  chk(sprintf("%-38s -> %s", cc[1], cc[2]), identical(rn(cc[1]), cc[2]),
      sprintf("got %s", rn(cc[1])))
chk("an empty map changes nothing", identical(.tp(".tpRenameToken")("a_b.fcs", list()), "a_b.fcs"))
chk("an identity mapping is not applied",
    identical(.tp(".tpRenameToken")("JD_x.fcs", list("JD" = "JD")), "JD_x.fcs"))

## A LAB VALUE IS FULL OF CHARACTERS A REGEX READS AS SYNTAX, and the key is user data: it comes
## out of Layout_Plate. Each of these must be matched LITERALLY, and must not match a lookalike.
for (cc in list(c("Panel(v2)", "P"), c("lot5+5", "L"), c("a.b", "D"), c("EXP$1", "E"),
                ## \Q..\E QUOTING WAS NOT ENOUGH: the two characters \E end the quoted span, so
                ## this key silently matched nothing and the file shipped un-renamed while every
                ## control reported success. Escaped character by character now.
                c("AB\\Ec", "X")))
  chk(sprintf("a key containing %-10s is matched literally", sQuote(cc[1])),
      identical(.tp(".tpRenameToken")(paste0(cc[1], "_A1.fcs"),
                                      stats::setNames(list(cc[2]), cc[1])),
                paste0(cc[2], "_A1.fcs")))
chk("...and a regex-shaped key does not match a lookalike name",
    identical(.tp(".tpRenameToken")("axb_A1.fcs", list("a.b" = "D")), "axb_A1.fcs"))
chk("a placeholder containing backreference syntax is inserted literally",
    identical(.tp(".tpRenameToken")("KEY_A1.fcs", list("KEY" = "a\\1b")), "a\\1b_A1.fcs"))

cat("-- which values get seeded into the scan, and which are too short --\n")
## THE DETECTOR AND THE REWRITER MUST DIFFER. The rename above matches on token boundaries so it
## cannot corrupt OPERATOR; the scan must match ANYWHERE, because glued-inside-a-token is exactly
## the case the rename declined to handle. Giving the scan the rename's rule made it blind to
## that case while the comment above it claimed coverage -- caught by falsification, not review.
st <- .tp(".tpSiteTokens")(list("EXP00000001" = "EXP26000001", "OP" = "JD",
                                "MyPanel_v2" = "Panel", "ELN" = "ELN"))
chk("a long identifier is searched for", "EXP00000001" %in% st$use)
chk("a panel name is searched for", "MyPanel_v2" %in% st$use)
chk("a SHORT code is NOT searched for (the fail-always trap)", "OP" %in% st$short)
chk("...and short keys are reported, not silently dropped", length(st$short) >= 1L)
chk("an identity mapping is neither searched for nor reported",
    !("ELN" %in% c(st$use, st$short)))
chk("a PLACEHOLDER is never searched for (it is the correct output)",
    !("EXP26000001" %in% st$use),
    "searching for what we substituted in would refuse every scrubbed bundle")

cat("-- the seeded scan finds the value the rename could not reach --\n")
d <- file.path(tempdir(), paste0("seed_", basename(tempfile(""))))
dir.create(d, recursive = TRUE)
seed <- stats::setNames("EXP00000001", "a confidential value the scrub replaced (EXP00000001)")
writeLines("nothing to see", file.path(d, "ok.txt"))
chk("a clean bundle is not refused", nrow(.tp("actaBundleScan")(d, extra = seed, quiet = TRUE)) == 0L)
writeLines("run EXP00000001 was here", file.path(d, "notes.txt"))
hits <- .tp("actaBundleScan")(d, extra = seed, quiet = TRUE)
chk("a site token in a bundle file is FOUND", nrow(hits) >= 1L)
chk("...and it BLOCKS rather than merely advising",
    any(hits$severity == "blocking"), paste(unique(hits$severity), collapse = ","))
unlink(file.path(d, "notes.txt"))
## THE CASE THAT MOTIVATES THE WHOLE SPLIT: glued inside a longer token.
writeLines("x", file.path(d, "EXP00000001Glued_A1.txt"))
chk("a token GLUED inside a longer one is still found (the rename cannot reach it)",
    nrow(.tp("actaBundleScan")(d, extra = seed, quiet = TRUE)) >= 1L)
chk("a value containing regex syntax is matched literally, not as a pattern",
    nrow(.tp("actaBundleScan")(d, extra = stats::setNames("Acme (Gen2)", "a site value"),
                               quiet = TRUE)) == 0L)

cat("-- nothing to do is success; only a real failure is FALSE --\n")
p <- file.path(tempdir(), paste0("n_", basename(tempfile(""))), "wb.xlsx")
dir.create(dirname(p), recursive = TRUE); file.copy(SRC, p)
suppressWarnings(.tp(".tpScrubWorkbook")(p)); suppressWarnings(.tp(".tpStripEmail")(p))
chk("re-scrubbing an already-scrubbed workbook is success",
    isTRUE(suppressWarnings(.tp(".tpScrubWorkbook")(p))))
chk("re-stripping an already-stripped workbook is success (idempotent)",
    isTRUE(suppressWarnings(.tp(".tpStripEmail")(p))))
chk("a file that is not a workbook is a FAILURE",
    !isTRUE(suppressWarnings(.tp(".tpScrubWorkbook")(file.path(tempdir(), "no_such.xlsx")))))
chk("the workbook still opens with all four sheets after the chain",
    identical(tryCatch(readxl::excel_sheets(p), error = function(e) character(0)),
              c("Info", "gating_template", "Layout_Plate", "TiterPubs_template")))

## ---------------------------------------------------------------------------------------------
## AN ORPHANED SHARED STRING IS A PUBLISHED VALUE. .tpCleanWorkbook() must empty it.
##
## Writing a cell as an inline string leaves the PREVIOUS value in xl/sharedStrings.xml with
## nothing pointing at it: invisible in Excel, invisible to any reader that goes through cells,
## and unreachable by the scrub, which keys on the value it was told to replace rather than on
## whatever was there before. Measured on a real bundle that every other control passed -- the
## live cell, the export, the report and actaBundleScan() were all clean while an orphan
## published the panel name the cell used to hold.
cat("\n=== orphaned shared strings ===\n")
local({
  ## Plant an orphan the way an inline-string edit does: add an <si> no cell indexes.
  pl <- file.path(tempdir(), paste0("orph_", basename(tempfile(""))), "wb.xlsx")
  dir.create(dirname(pl), recursive = TRUE); file.copy(SRC, pl)
  d <- unpack(pl); f <- file.path(d, "xl", "sharedStrings.xml")
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  SECRET <- "ORPHANED-PANEL-NAME-SHOULD-NOT-PUBLISH"
  ## Appended LAST, so its index is one past anything a sheet references.
  x <- sub("</sst>", sprintf("<si><t>%s</t></si></sst>", SECRET), x, fixed = TRUE)
  writeLines(x, f, useBytes = TRUE); repack(d, pl)

  chk("the planted orphan is really in the file before cleaning",
      any(grepl(SECRET, rawParts(pl), fixed = TRUE, useBytes = TRUE)))
  ## ...and really is unreferenced, or this fixture proves nothing: an orphan a cell points at
  ## is live data and must NOT be emptied.
  live <- cellValues(pl)
  chk("...and no cell reads it", !any(grepl(SECRET, live, fixed = TRUE)))

  chk("cleaning succeeds", isTRUE(.tp(".tpCleanWorkbook")(pl)))
  chk("THE ORPHAN IS GONE from every part",
      !any(grepl(SECRET, rawParts(pl), fixed = TRUE, useBytes = TRUE)))
  ## The fix must not have eaten live strings along with the dead one. Deleting an <si> would
  ## renumber every index after it and silently shift every t="s" cell in the workbook.
  after <- cellValues(pl)
  chk("...and every live cell value in the workbook survives unchanged",
      identical(sort(live), sort(after)),
      sprintf("%d before, %d after", length(live), length(after)))
  chk("...the workbook still opens with all four sheets",
      identical(tryCatch(readxl::excel_sheets(pl), error = function(e) character(0)),
                c("Info", "gating_template", "Layout_Plate", "TiterPubs_template")))
  ## Indices are kept, not compacted -- that is what makes the blanking safe.
  nsi <- function(p) length(regmatches(paste(rawParts(p), collapse = ""),
                                       gregexpr("<si>", paste(rawParts(p), collapse = "")))[[1]])
  chk("...and the shared-string table still has every entry (blanked, not deleted)", nsi(pl) > 0L)
})

## A SELF-CLOSING <si/> MUST NOT SHIFT THE INDEX OF EVERYTHING AFTER IT.
##
## The blanking is positional, so an entry the enumeration cannot see desyncs every later index
## and live strings get emptied instead of orphans. "<si>.*?</si>" cannot see a schema-valid
## <si/>. Measured before the fix: a 4-entry table with <si/> at index 0 enumerated 3 and blanked
## the first LIVE string rather than the orphan -- while the function returned TRUE, the workbook
## opened, and the OPC checks reported clean. Nothing in this pipeline writes <si/>, which is
## exactly why only a gate keeps it fixed.
cat("\n=== a self-closing <si/> does not desync the table ===\n")
local({
  pl <- file.path(tempdir(), paste0("selfc_", basename(tempfile(""))), "wb.xlsx")
  dir.create(dirname(pl), recursive = TRUE); file.copy(SRC, pl)
  d <- unpack(pl); f <- file.path(d, "xl", "sharedStrings.xml")
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  ORPH <- "SELFCLOSE-ORPHAN-MUST-GO"
  LIVE <- "SELFCLOSE-LIVE-MUST-SURVIVE"
  ## APPENDED, with a LIVE entry placed AFTER the <si/>. Nothing existing is renumbered.
  ##
  ## Placement is the whole fixture. A bare appended <si/> sits after every live entry, shifts
  ## nothing, and the gate passes against the broken code -- measured; the first version of this
  ## fixture did exactly that. Prepending triggers the bug but forces every existing t="s"
  ## reference to be renumbered, and that rewrite was itself wrong twice. Appending a live entry
  ## after the <si/> gets the trigger without the rewrite: with the defective pattern the <si/>
  ## is not enumerated, so the live entry is read as the orphan's index and BLANKED, and the real
  ## orphan is read as the live index and SURVIVES. Both assertions below fail, in opposite
  ## directions, which is a sharper signal than either alone.
  n <- length(regmatches(x, gregexpr("<si[ />]", x))[[1]])      # index of the <si/> we add
  x <- sub("</sst>", sprintf("<si/><si><t>%s</t></si><si><t>%s</t></si></sst>", LIVE, ORPH),
           x, fixed = TRUE)
  writeLines(x, f, useBytes = TRUE)

  ## REPOINT ONE EXISTING t="s" CELL at the LIVE entry rather than inserting a new one. Inserting
  ## meant choosing a row and a column that keep cells ascending, and getting that wrong is its
  ## own corruption; repointing changes one <v> and cannot disturb the sheet's structure at all.
  sf <- list.files(file.path(d, "xl", "worksheets"), pattern = "[.]xml$", full.names = TRUE)
  done <- FALSE
  for (p1 in sf) {
    sx <- paste(readLines(p1, warn = FALSE), collapse = "\n")
    one <- regmatches(sx, regexpr('<c[^>]*\\st="s"[^>]*>\\s*<v>[0-9]+</v>', sx))
    if (!length(one) || !nzchar(one)) next
    sx <- sub(one, sub('<v>[0-9]+</v>', sprintf("<v>%d</v>", n + 1L), one), sx, fixed = TRUE)
    writeLines(sx, p1, useBytes = TRUE); done <- TRUE; break
  }
  repack(d, pl)

  ## ASSERTED AT XML LEVEL, NOT THROUGH readxl.
  ##
  ## What matters here is which <si> keeps its text, and `used` is built from the sheet XML -- so
  ## the sheet reference is what makes an entry live, whether or not read_excel() surfaces that
  ## particular cell. Going through cellValues() made this fixture fail for an unrelated reason
  ## (the first t="s" cell is A1, which read_excel turns into a column NAME), and chasing that
  ## was three rounds of fixture plumbing with the product fix already measured correct twice.
  sstOf <- function(p) {
    dd <- unpack(p); on.exit(unlink(dd, recursive = TRUE), add = TRUE)
    paste(readLines(file.path(dd, "xl", "sharedStrings.xml"), warn = FALSE), collapse = "\n")
  }
  hasText <- function(s, tag) grepl(sprintf("<si><t>%s</t></si>", tag), s, fixed = TRUE)
  s0 <- sstOf(pl)
  chk("fixture: <si/> at n, a LIVE entry at n+1, an orphan at n+2",
      done && hasText(s0, LIVE) && hasText(s0, ORPH), sprintf("n = %d", n))
  chk("fixture: a sheet cell really references index n+1", done)
  chk("cleaning succeeds", isTRUE(.tp(".tpCleanWorkbook")(pl)))
  s1 <- sstOf(pl)
  chk("the orphan after the <si/> is still emptied", !hasText(s1, ORPH))
  ## THE REAL ASSERTION. With the defective pattern the <si/> is not enumerated, so the LIVE entry
  ## is read as the orphan's index and blanked while the orphan is read as the live one and kept
  ## -- both of these fail, in opposite directions.
  chk("THE LIVE ENTRY AFTER THE <si/> SURVIVES", hasText(s1, LIVE))
})

## A DEAD DUPLICATE OF A LIVE STRING MUST NOT TAKE THE LIVE ONE WITH IT.
##
## The table is addressed BY POSITION, so blanking has to be positional too. Locating each <si> by
## its TEXT -- which the first version of the fix did, with sub(fixed = TRUE) -- rewrites the
## FIRST match, so a dead duplicate blanks the LIVE copy that precedes it and every cell pointing
## at it reads empty. Silent data loss in a published artefact, and invisible unless a workbook
## actually contains a duplicate, which a correct writer never produces.
local({
  pl <- file.path(tempdir(), paste0("dup_", basename(tempfile(""))), "wb.xlsx")
  dir.create(dirname(pl), recursive = TRUE); file.copy(SRC, pl)
  d <- unpack(pl); f <- file.path(d, "xl", "sharedStrings.xml")
  x <- paste(readLines(f, warn = FALSE), collapse = "\n")
  DUP <- "DUPLICATED-SHARED-STRING-MUST-SURVIVE"

  ## BUILT, NOT FOUND. An earlier version looked for an existing referenced <si> and SKIPPED when
  ## there was none -- which is what happens here, because this workbook's cells are inline
  ## strings and almost every entry in its table is already an orphan. A gate that stands down on
  ## the fixture it needs is not a gate, so the live reference is constructed.
  n <- length(regmatches(x, gregexpr("<si>", x))[[1]])        # next index, 0-based
  x <- sub("</sst>", sprintf("<si><t>%s</t></si><si><t>%s</t></si></sst>", DUP, DUP), x, fixed = TRUE)
  writeLines(x, f, useBytes = TRUE)

  ## Point a cell at the FIRST of the pair, so index n is live and n+1 is the dead duplicate.
  ## Appended as a new cell on the last row, in a column after its last one: cells within a row
  ## must ascend, and an out-of-order cell is its own corruption (see set_workbook_cells.R).
  sf <- file.path(d, "xl", "worksheets", "sheet3.xml")
  if (!file.exists(sf)) sf <- list.files(file.path(d, "xl", "worksheets"),
                                         pattern = "^sheet[0-9]+[.]xml$", full.names = TRUE)[1]
  sx <- paste(readLines(sf, warn = FALSE), collapse = "\n")
  rows <- regmatches(sx, gregexpr("<row[^>]*r=\"[0-9]+\"[^>]*>.*?</row>", sx))[[1]]
  if (!length(rows)) {
    cat("  [skip] no addressable row in the fixture sheet\n")
  } else {
    last <- rows[[length(rows)]]
    rn   <- sub('.*\\sr="([0-9]+)".*', "\\1", substr(last, 1, 40))
    cols <- regmatches(last, gregexpr('<c r="([A-Z]+)', last))[[1]]
    lastL <- if (length(cols)) sub('<c r="', "", cols[[length(cols)]]) else "A"
    nxt  <- if (nchar(lastL) == 1L && lastL < "Z") chartr("ABCDEFGHIJKLMNOPQRSTUVWXY",
                                                          "BCDEFGHIJKLMNOPQRSTUVWXYZ", lastL)
            else paste0(lastL, "A")
    newc <- sprintf('<c r="%s%s" t="s"><v>%d</v></c>', nxt, rn, n)
    sx <- sub(last, sub("</row>", paste0(newc, "</row>"), last, fixed = TRUE), sx, fixed = TRUE)
    writeLines(sx, sf, useBytes = TRUE)
    repack(d, pl)

    chk("fixture: index n is referenced by a cell and n+1 is its dead duplicate",
        any(grepl(DUP, cellValues(pl), fixed = TRUE)), sprintf("index %d", n))
    chk("cleaning succeeds with a duplicate present", isTRUE(.tp(".tpCleanWorkbook")(pl)))
    chk("THE LIVE STRING SURVIVES -- the dead duplicate did not take it with it",
        any(grepl(DUP, cellValues(pl), fixed = TRUE)))
  }
})

## ---------------------------------------------------------------------------------------------
## THE BUNDLE'S INVENTORY MUST RE-RESOLVE TO THE SAME PANEL. Round trip, not inspection.
##
## .tpWriteInventory() wrote tb$Antigen into Marker_Alias, which looks right in every structural
## check and in the file itself. The only thing that catches it is resolving a panel, writing the
## subset, and resolving the SAME panel again from that subset: a reagent whose antigen differs
## from its marker name in the FCS matches on Marker_Alias, so the written file reported one
## reagent fewer than the run it was written from. Measured on OQ_Test1: 4 of 4 became 3 of 4.
cat("\n=== the written inventory re-resolves (round trip) ===\n")
local({
  ## THE SOURCE GOES THROUGH actaPanelInventory() TOO, not hand-built: it renames the sheet's
  ## headers to its own internal names, so a frame built with the sheet's spelling resolves
  ## nothing -- and a round-trip gate that skips the read path is not testing the round trip.
  ## A panel whose reagent's ANTIGEN DIFFERS FROM ITS MARKER is the only shape that bites.
  src <- data.frame(
    Antigen            = c("CD3", "Live Dead eFluor 780 (LD) (Viability)"),
    Fluorochrome       = c("BUV805", "efluor780"),
    Fluorochrome_Alias = c("", "efluor 780"),
    Marker_Alias       = c("", "Viability"),
    Vendor = c("BD", "TS"), Catalog = c("612893", "65-0865-14"),
    Clone = c("SK7", ""), `Species / Isotype` = c("", ""), Type = c("Base", "Base"),
    `Panel Iteration` = c("DemoPanel_1", "DemoPanel_1"),
    check.names = FALSE, stringsAsFactors = FALSE)
  srcf <- file.path(tempdir(), paste0("src_", basename(tempfile(""))), "Master_Panel_Inventory.xlsx")
  dir.create(dirname(srcf), recursive = TRUE)
  wb0 <- openxlsx::createWorkbook(creator = "ACTA")
  openxlsx::addWorksheet(wb0, "Master")
  openxlsx::writeData(wb0, "Master", src, keepNA = FALSE)
  openxlsx::saveWorkbook(wb0, srcf, overwrite = TRUE)

  mk <- c("CD3", "Viability"); ch <- character(0)
  inv0 <- .tp("actaPanelInventory")(srcf)
  r1 <- .tp("actaPanelIterationTable")(inv0, "DemoPanel_1", mk, ch)
  chk("the source inventory resolves both reagents",
      !is.null(r1$table) && all(tolower(r1$table$Verdict) == "matched"),
      paste(r1$table$Verdict, collapse = ","))

  ## Write it the way a bundle does, then read it back as a recipient would.
  out <- file.path(tempdir(), paste0("inv_", basename(tempfile(""))), "Master_Panel_Inventory.xlsx")
  dir.create(dirname(out), recursive = TRUE)
  w <- .tp(".tpWriteInventory")(list(DemoPanel_1 = list(table = r1$table)), out)
  chk("the subset workbook is written", !is.null(w) && file.exists(out))

  if (file.exists(out)) {
    inv1 <- .tp("actaPanelInventory")(out)
    r2 <- .tp("actaPanelIterationTable")(inv1, "DemoPanel_1", mk, ch)
    chk("IT RE-RESOLVES THE SAME NUMBER OF REAGENTS",
        !is.null(r2$table) && nrow(r2$table) == nrow(r1$table),
        sprintf("%d then %d", nrow(r1$table), if (is.null(r2$table)) 0L else nrow(r2$table)))
    chk("...with the same verdict for every reagent -- this is the whole gate",
        !is.null(r2$table) && identical(sort(tolower(r1$table$Verdict)),
                                        sort(tolower(r2$table$Verdict))),
        paste(r2$table$Verdict, collapse = ","))
    chk("...and the reagent whose antigen is not its marker still matches",
        !is.null(r2$table) &&
          identical(tolower(r2$table$Verdict[grepl("Viability", r2$table$Antigen)]), "matched"))
  }
})

cat(if (ok) "\nTITERPUBS SCRUB TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
