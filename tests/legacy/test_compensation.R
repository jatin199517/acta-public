suppressMessages({library(flowCore); library(readxl)})
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir(); h <- actaTestFunctions(vd)
ok <- TRUE; chk <- function(w, c) { if (!c) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (c) "ok" else "FAIL", w)) }
## THE SHIPPED SHAPE IS LONG, SO THE FIXTURE IS LONG. This helper used to build
## `data.frame(compensation = v)` -- a WIDE sheet ACTA has not shipped for versions -- so every
## assertion in this file passed against a layout the product never sees. Behind that green gate,
## actaCompMode() matched on COLUMN NAMES and returned "none" for every real workbook: OQ_Test2
## asked for csv, ran uncompensated, and its pre-flight said "pass -- Info!compensation is blank".
## A gate must exercise the artefact that ships. Wide is still asserted, as back-compat, below.
infLong <- function(v) data.frame(Category = "General", Variable = "compensation",
                                  Value = if (length(v) && is.na(v)) NA_character_ else v,
                                  Description = "Compensation to apply.",
                                  stringsAsFactors = FALSE)
infWide <- function(v) data.frame(compensation = v, stringsAsFactors = FALSE)
inf <- infLong
wd <- file.path(tempdir(), "compwd"); unlink(wd, recursive = TRUE); dir.create(wd, showWarnings = FALSE)
one <- function(r) r[[1]]
sq <- function(p, n = 3) { m <- diag(n); dimnames(m) <- list(paste0("C", 1:n), paste0("C", 1:n))
                           utils::write.csv(m, p) }

cat("=== mode handling ===\n")
r <- one(h$actaCompensationRecords(inf(NA), wd))
chk("blank -> pass, identity stated", r$status == "pass" && grepl("identity", r$detail))
r <- one(h$actaCompensationRecords(inf("spill"), wd))
chk("typo -> FAIL", r$status == "fail")
chk("typo names the three legal values", grepl("blank", r$detail) && grepl("self", r$detail) && grepl("csv", r$detail))
r <- one(h$actaCompensationRecords(inf("CSV"), wd))
chk("mode is case-insensitive (CSV)", grepl("no \\*CompMatrix\\*", r$detail) || r$status == "fail")

cat("\n=== csv: the three failure modes ===\n")
chk("0 files -> fail", one(h$actaCompensationRecords(inf("csv"), wd))$status == "fail")
sq(file.path(wd, "A_CompMatrix.csv"))
chk("1 square file -> pass", one(h$actaCompensationRecords(inf("csv"), wd))$status == "pass")
sq(file.path(wd, "B_compmatrix.CSV"))
r <- one(h$actaCompensationRecords(inf("csv"), wd))
chk("2 files -> fail (case-insensitive match)", r$status == "fail" && grepl("2 candidate", r$detail))
unlink(file.path(wd, "B_compmatrix.CSV"))
ns <- file.path(wd, "A_CompMatrix.csv")
utils::write.csv(matrix(1:6, nrow = 2, dimnames = list(c("a","b"), c("x","y","z"))), ns)
r <- one(h$actaCompensationRecords(inf("csv"), wd))
chk("non-square -> fail", r$status == "fail" && grepl("square", r$detail))
sq(ns)

cat("\n=== the override is what gets validated ===\n")
empty <- file.path(tempdir(), "emptywd"); dir.create(empty, showWarnings = FALSE)
ov <- file.path(tempdir(), "Chosen_CompMatrix.csv"); sq(ov)
chk("empty folder + override -> pass", one(h$actaCompensationRecords(inf("csv"), empty, override = ov))$status == "pass")
chk("override that does not exist -> fail",
    one(h$actaCompensationRecords(inf("csv"), empty, override = file.path(tempdir(), "nope.csv")))$status == "fail")

cat("\n=== staging moves the incumbent aside and restores it ===\n")
before <- sort(basename(h$actaCompCsvCandidates(wd)))
restore <- h$actaStageCompCsv(wd, ov)
during <- sort(basename(h$actaCompCsvCandidates(wd)))
chk("exactly one candidate while staged", length(during) == 1)
chk("and it is the override", identical(during, basename(ov)))
restore()
chk("incumbent restored afterwards", identical(sort(basename(h$actaCompCsvCandidates(wd))), before))
chk("override not left behind", !file.exists(file.path(wd, basename(ov))))
r2 <- h$actaStageCompCsv(wd, file.path(wd, "A_CompMatrix.csv"))   # override IS the incumbent
chk("staging is a no-op when override == incumbent",
    identical(sort(basename(h$actaCompCsvCandidates(wd))), before)); r2()

cat("\n=== self mode ===\n")
## Off `vd`, not the current directory: this line was the ONLY reason this script had to be run
## from inside the version folder.
fr <- normalizePath(file.path(actaTestCaseRoot(), "OQ_Test1", "Titration_FCS"))
r <- one(h$actaCompensationRecords(inf("self"), wd, fcs_root = fr, antibody = "Strep_PE"))
cat("   ", r$status, "--", substr(r$detail, 1, 68), "\n")
chk("self reports on the real files", r$status %in% c("pass", "fail"))
chk("self with no FCS root -> skip, not pass",
    one(h$actaCompensationRecords(inf("self"), wd, fcs_root = file.path(tempdir(), "nope")))$status == "skip")
cat("\n=== BOTH Info layouts resolve identically (the defect this file missed) ===\n")
for (v in c("csv", "self", "CSV", "spill")) {
  lw <- h$actaCompMode(infLong(v), quiet = TRUE); ww <- h$actaCompMode(infWide(v), quiet = TRUE)
  chk(sprintf("actaCompMode agrees on long/wide for %-6s (%s)", sQuote(v), lw), identical(lw, ww))
  chk(sprintf("actaCompLabel agrees on long/wide for %s", sQuote(v)),
      identical(h$actaCompLabel(infLong(v)), h$actaCompLabel(infWide(v))))
}
chk("a LONG sheet saying csv really resolves csv -- not 'none'",
    identical(h$actaCompMode(infLong("csv"), quiet = TRUE), "csv"))
chk("...and its pre-flight does NOT report the cell as blank",
    !grepl("blank", one(h$actaCompensationRecords(infLong("csv"), wd))$detail))

cat("\n=== the SHIPPED workbook, read the way the pipeline reads it ===\n")
## The strongest assertion available: not a fixture, the actual OQ_Test2 workbook. It declares
## csv and ships a matrix beside it, so anything other than "csv" here means the setting was
## dropped between the sheet and the run.
ocase <- file.path(actaTestCaseRoot(), "OQ_Test2")
owb   <- file.path(ocase, "OQ_Test2_Ab_Titration_Instructions.xlsx")
if (file.exists(owb)) {
  oi <- suppressMessages(readxl::read_excel(owb, sheet = "Info"))
  names(oi) <- h$actaStripHeaderAnnotation(names(oi))
  chk("OQ_Test2's Info sheet is the LONG layout", isTRUE(h$actaInfoIsLong(oi)))
  chk("OQ_Test2 declares compensation = csv", identical(h$actaInfoValue(oi, "compensation"), "csv"))
  chk("...and actaCompMode() agrees", identical(h$actaCompMode(oi, quiet = TRUE), "csv"))
  chk("...and actaCompLabel() is the cell, not the em-dash sentinel",
      identical(h$actaCompLabel(oi), "csv"))
  rr <- one(h$actaCompensationRecords(oi, wd = normalizePath(ocase)))
  chk("...and the pre-flight finds the matrix", rr$status == "pass" && grepl("CompMatrix", rr$detail))
} else cat("  [skip] OQ_Test2 workbook not installed.\n")

cat("\n=== a blank cell must not reach the report as a printable character ===\n")
## actaCompLabel() renders blank as an em dash FOR CAPTIONS. The report takes the cell instead,
## because a sentinel that is non-empty defeats every `nzchar()` blank test downstream -- which is
## how a literal U+2014 was typeset into the PDF's Compensation section.
chk("actaCompLabel renders a blank cell as the em-dash sentinel",
    identical(h$actaCompLabel(infLong(NA)), "\u2014"))
chk("actaInfoValue returns NA for the same cell, so the report can branch on it",
    is.na(h$actaInfoValue(infLong(NA), "compensation")))
## vd IS the pipeline folder in a package checkout; system.file() covers the installed case.
rmdp <- file.path(vd, "ACTA_Report.Rmd")
if (!file.exists(rmdp)) rmdp <- system.file("pipeline", "ACTA_Report.Rmd", package = "ACTA")
rmd <- if (nzchar(rmdp) && file.exists(rmdp)) readLines(rmdp, warn = FALSE) else character(0)
if (length(rmd)) {
  i <- grep("actaCompReport\\(compMode", rmd)
  chk("the report passes the CELL to actaCompReport(), not compLabel",
      length(i) == 1L && !any(grepl("raw\\s*=\\s*.*compLabel", rmd[i:min(i + 3L, length(rmd))])))
} else cat("  [skip] ACTA_Report.Rmd not reachable.\n")

cat(if (ok) "\nALL COMPENSATION TESTS PASS\n" else "\nFAILURES\n"); quit(status = if (ok) 0 else 1)
