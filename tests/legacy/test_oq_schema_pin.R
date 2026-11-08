## A TRIPWIRE ON THE COUPLING BETWEEN Layout_Plate AND expected.json.
##
## WHY THIS EXISTS, AND IT IS NOT A STYLE CHOICE.
##
## `stats_cols` in each OQ case's expected.json is `ncol(res$stats)`. Layout_Plate columns ride
## into `stats` through pData, so ADDING OR REMOVING A LAYOUT COLUMN MOVES THAT NUMBER. It has
## moved four times:
##
##   41 -> 42  2_95   Combinatorial_group (n) added
##   42 -> 43  3.4.0  Costain_Panel_Iteration added
##   45 -> 46  3.6.0  Test_Material_Status added
##   46 -> 47  3.6.8  +Tissue_Type +Culture_Status -flowjo_transformation_arg
##
## Every one of those was discovered AFTER the fact and written into the schema_note as history.
## 3.6.8 is the one that got out: it shipped an OQ that reported FAIL on a completely correct
## analysis, and **no local gate could see it** -- `run_all.R` never runs an OQ case, so the only
## thing that compares against expected.json is the CI `oq` / `report-windows` job, which is
## `schedule || workflow_dispatch` and does not run on push. A green local suite said nothing.
##
## THE FIX IS NOT "RUN THE OQ HERE". That was excluded deliberately: the three cases take minutes
## each and need LaTeX, and a suite that takes an hour gets skipped, which is worse than this gap.
##
## THE FIX IS ALSO NOT TO COMPUTE stats_cols FROM THE COLUMN COUNT. The 3.6.8 move proves why:
## two columns were added and one removed, so the count went +1 while the schema changed by three
## columns. Anyone deriving 46+2=48 would have been confidently wrong. The number has to be
## MEASURED by running a case, and this file's job is only to make you notice that you must.
##
## So: both numbers are PINNED here. Change the Layout_Plate schema and this goes red, naming the
## one thing that actually resolves it -- run an OQ case and read the observed value.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages(library(readxl))
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
h <- actaTestFunctions(actaTestAppDir())
.f <- function(nm) if (!is.null(h[[nm]])) h[[nm]] else getFromNamespace(nm, "ACTA")

## ---- the two pinned numbers -------------------------------------------------------------------
## Update BOTH together, and only after re-measuring stats_cols from a real run.
LAYOUT_COLS <- 28L   # TEMPLATE_Ab_Titration_Instructions.xlsx, sheet Layout_Plate, as of 3.6.8
STATS_COLS  <- 47L   # ncol(res$stats), measured on OQ_Test1 against the published 3.6.8

HOWTO <- paste(
  "\n        TO RESOLVE: do NOT edit the pin to match. Run a case and read the observed value:",
  "\n          Rscript -e '.libPaths(c(\"<lib>\", .libPaths())); library(ACTA);",
  "\n                      r <- actaOQRun(\"<a copy of inst/extdata/oq_small/OQ_Test1>\")'",
  "\n        then read Outputs/log.txt for the 'Per-well statistics: column count' line, write that",
  "\n        number into all three expected.json AND into STATS_COLS here, and append to schema_note.")

cat("\n=== the shipped template's Layout_Plate ===\n")
tf <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                "TEMPLATE_Ab_Titration_Instructions.xlsx")
lp <- suppressMessages(readxl::read_excel(tf, sheet = "Layout_Plate", col_types = "text",
                                          .name_repair = "minimal"))
nm <- .f("actaStripHeaderAnnotation")(names(lp))
chk(sprintf("Layout_Plate has %d columns", LAYOUT_COLS), length(nm) == LAYOUT_COLS,
    sprintf("found %d: %s%s", length(nm), paste(nm, collapse = ", "), HOWTO))

## The four that moved the count, named so a diff shows which one changed.
for (c0 in c("Cell_Type", "Tissue_Type", "Sample_Status", "Culture_Status",
             "Combinatorial_group", "Costain_Panel_Iteration"))
  chk(sprintf("...including %s", c0), c0 %in% nm)
chk("...and NOT the retired flowjo_transformation_arg", !"flowjo_transformation_arg" %in% nm)

cat("\n=== every OQ case's expected.json agrees, and matches the pin ===\n")
cases <- sort(Sys.glob(file.path(ACTA_REPO_ROOT, "inst", "extdata", "oq_small", "OQ_Test*")))
chk("there are OQ cases to check at all", length(cases) > 0L, length(cases))
got <- integer(0)
for (d in cases) {
  f <- file.path(d, "expected.json")
  if (!file.exists(f)) { chk(sprintf("%s has an expected.json", basename(d)), FALSE); next }
  j <- jsonlite::fromJSON(f)
  v <- if (is.null(j$stats_cols)) NA_integer_ else as.integer(j$stats_cols)
  got <- c(got, v)
  chk(sprintf("%s stats_cols == %d", basename(d), STATS_COLS), identical(v, STATS_COLS),
      sprintf("found %s%s", v, HOWTO))
}
## THE SCHEMA IS THE SAME FOR EVERY CASE. A per-case difference means one file was updated and the
## others were not -- the shape of a half-finished fix, which is how 3.6.8's miss would have looked
## if only OQ_Test1 had been corrected.
chk("all cases agree with each other", length(unique(stats::na.omit(got))) <= 1L,
    paste(got, collapse = " / "))

cat("\n=== the schema_note records the history, so the next change has the context ===\n")
for (d in cases) {
  j <- jsonlite::fromJSON(file.path(d, "expected.json"))
  note <- if (is.null(j$schema_note)) "" else j$schema_note
  chk(sprintf("%s schema_note mentions the current count", basename(d)),
      grepl(as.character(STATS_COLS), note, fixed = TRUE),
      "the note should say WHY the number is what it is, not just the number")
}

cat(if (ok) "\nOQ SCHEMA PIN: all checks passed\n" else "\nOQ SCHEMA PIN: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
