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

cat(if (ok) "\nTITERPUBS SCRUB TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
