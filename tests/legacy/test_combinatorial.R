## Guards the `Combinatorial_group (n)` declaration added in 3_0: the per-value numeric test, the
## per-group reduction, and the conflict report.
##
## Two failure modes this exists for, both silent:
##   1. `is.numeric()` on the COLUMN. readxl types a whole column at once, so one stray text cell
##      anywhere in Layout_Plate makes the column character and `is.numeric(col)` is then FALSE for
##      EVERY row -- switching the flag off for groups that were declared correctly.
##   2. Reading the value off ONE well. Layout_Plate has a row per well; statsForExport keeps only
##      the maxSI row. A number filled in on some rows and not the winning one would report FALSE.
## Rscript <this> [version_dir]   -- defaults to 3_0.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

cat("=== the numeric test is PER VALUE ===\n")
chk("1 is numeric",              isTRUE(h$actaIsCombinatorialValue(1)))
chk("\"1\" is numeric",          isTRUE(h$actaIsCombinatorialValue("1")))
chk("\" 2 \" is numeric",        isTRUE(h$actaIsCombinatorialValue(" 2 ")))
chk("1.5 is numeric (it groups, it does not count)", isTRUE(h$actaIsCombinatorialValue(1.5)))
chk("\"yes\" is NOT",            isFALSE(h$actaIsCombinatorialValue("yes")))
chk("\"1a\" is NOT",             isFALSE(h$actaIsCombinatorialValue("1a")))
chk("\"\" is NOT",               isFALSE(h$actaIsCombinatorialValue("")))
chk("NA is NOT",                 isFALSE(h$actaIsCombinatorialValue(NA)))
chk("Inf is NOT",                isFALSE(h$actaIsCombinatorialValue(Inf)))

cat("=== THE TRAP: a character column must still resolve per value ===\n")
## This is what readxl hands back when any cell in the column is text. Under is.numeric(col) every
## one of these would be FALSE; per value, the real declarations survive.
col <- c("1", "1", "yes", "2", NA)
chk("a character column still yields TRUE for its numeric cells",
    identical(vapply(col, h$actaIsCombinatorialValue, logical(1), USE.NAMES = FALSE),
              c(TRUE, TRUE, FALSE, TRUE, FALSE)))
chk("and is.numeric() on that column is FALSE -- the mode being guarded against",
    isFALSE(is.numeric(col)))

cat("=== the per-group reduction ===\n")
r <- h$actaCombinatorialGroup(c("1", "1", "1"), "CCR7_combo")
chk("a group declared on every row", identical(r$value, "1") && isTRUE(r$is_combo))
r <- h$actaCombinatorialGroup(c(NA, "1", NA, NA), "CCR7_combo")
chk("PARTIAL filling still declares the group", identical(r$value, "1") && isTRUE(r$is_combo))
r <- h$actaCombinatorialGroup(c(NA, NA), "CCR7")
chk("nothing declared -> NA and FALSE", is.na(r$value) && isFALSE(r$is_combo))
r <- h$actaCombinatorialGroup(c("", "  ", NA), "CCR7")
chk("blanks and whitespace are not declarations", is.na(r$value) && isFALSE(r$is_combo))
r <- h$actaCombinatorialGroup(c("yes", "yes"), "CD14")
chk("non-numeric is reported VERBATIM with FALSE, not blanked",
    identical(r$value, "yes") && isFALSE(r$is_combo))
chk("  and it is not a conflict", is.na(r$conflict))

cat("=== a conflicting declaration ===\n")
r <- h$actaCombinatorialGroup(c("1", "2"), "CCR7_combo")
chk("two numbers in one group is a conflict", !is.na(r$conflict))
chk("  the conflict names the titration", grepl("CCR7_combo", r$conflict, fixed = TRUE))
chk("  and quotes both values", grepl("'1'", r$conflict) && grepl("'2'", r$conflict))
chk("  value degrades to NA rather than picking one", is.na(r$value) && isFALSE(r$is_combo))

cat("=== the shipped OQ workbooks carry the column ===\n")
for (n in 1:3) {
  f <- file.path(actaTestCaseRoot(), paste0("OQ_Test", n),
                 sprintf("OQ_Test%d_Ab_Titration_Instructions_%s.xlsx", n,
                         sub("_WIP$", "", basename(vd))))
  if (!file.exists(f)) { cat(sprintf("  [skip] OQ_Test%d workbook not found\n", n)); next }
  nms <- names(suppressMessages(readxl::read_excel(f, sheet = "Layout_Plate", n_max = 0)))
  chk(sprintf("OQ_Test%d Layout_Plate has Combinatorial_group (n)", n),
      "Combinatorial_group (n)" %in% nms)
}
## The annotation is stripped at read time, so the rest of the pipeline sees `Combinatorial_group`.
chk("the (n) annotation strips to the base name",
    identical(h$actaStripHeaderAnnotation("Combinatorial_group (n)"), "Combinatorial_group"))

cat(if (ok) "\nALL COMBINATORIAL CHECKS PASSED\n" else "\nSOME COMBINATORIAL CHECKS FAILED\n")
quit(status = if (ok) 0L else 1L)
