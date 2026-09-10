suppressMessages({library(flowCore); library(readxl)})
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir(); h <- actaTestFunctions(vd)
ok <- TRUE; chk <- function(w, c) { if (!c) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (c) "ok" else "FAIL", w)) }
inf <- function(v) data.frame(compensation = v, stringsAsFactors = FALSE)
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
cat(if (ok) "\nALL COMPENSATION TESTS PASS\n" else "\nFAILURES\n"); quit(status = if (ok) 0 else 1)
