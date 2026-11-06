## which_plate() reads the plate number off the FCS path, and the layout's PlateID is matched
## against what it returns. A wrong answer matches NO layout row, and the run then dies inside
## annotate_changeParam_pdWrite() with base R's "attempt to select less than one element in
## OneIndex" -- a message that names neither the file nor the well, and that greps for nowhere in
## this repo because it is compiled into libR.
##
## THE BUG THIS GATE EXISTS FOR. The test was `grepl("PLATE_2", toupper(path))`, an UNANCHORED
## substring. An instrument filename shaped `EXP00000001 Plate_2026_11_03_19_32_OLS-AF.fcs`
## uppercases to `...PLATE_2026...`, whose first seven characters ARE `PLATE_2` -- so every file in
## a correctly-labelled single-plate run resolved to plate 2, and 0 of 39 wells mapped. Any year
## beginning with 2 does it; `Plate_19xx` resolved to 1 only by the same accident.
##
## AND IT IS NOT ONLY FILE NAMES. `path<-paste0(getwd(),"/Titration_FCS")` is absolute and
## read.flowSet() keeps the whole path in $FILENAME, so a FOLDER anywhere above the run -- which on
## a synced drive is not the analyst's to rename -- poisoned every file in it.
##
## No fixture, no FCS on disk: the frames below carry a $FILENAME and nothing else, which is all
## which_plate() reads. That keeps this gate fast enough to stay in front of every plate change.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

suppressMessages(library(ACTA))
suppressMessages(library(flowCore))
wp <- ACTA:::which_plate

## A flowFrame carrying one event and one parameter -- the cheapest object which_plate() accepts.
frameNamed <- function(nm) {
  m <- matrix(1, nrow = 1L, ncol = 1L, dimnames = list(NULL, "FSC-A"))
  ff <- flowCore::flowFrame(m)
  ff@description$FILENAME <- nm
  ff
}
expect <- function(label, nm, want, ...) {
  got <- tryCatch(wp(frameNamed(nm), ...), error = function(e) paste("ERROR:", conditionMessage(e)))
  chk(label, length(got) == 1L && isTRUE(all.equal(as.numeric(got), as.numeric(want))),
      sprintf("path=%s  want=%s got=%s(len %d)", sQuote(nm), want,
              paste(format(got), collapse = ","), length(got)))
}

cat("-- a date in the name is not a plate token --\n")
expect("the reported filename resolves to plate 1",
       "261103_EXP00000001 Plate_2026_11_03_19_32_OLS-AF.fcs", 1)
expect("Plate_2030 is not plate 2",  "EXP1 Plate_2030_01_01.fcs", 1)
expect("Plate_2199 is not plate 2",  "EXP1 Plate_2199_01_01.fcs", 1)
## default=3 ON PURPOSE for the two below. Both paths contain the substring `PLATE_1`, so the old
## code answered 1 -- which is also the ordinary default, and an assertion of 1 would therefore have
## passed against the very bug it exists to catch. A non-default `default` is the only way the
## right answer and the wrong one are different numbers.
expect("Plate_1999 is not plate 1",  "EXP1 Plate_1999_01_01.fcs", 3, default = 3L)
expect("lower-case plate_2026 is not plate 2", "exp1 plate_2026_11_03.fcs", 1)

cat("-- a folder above the run cannot set the plate either --\n")
expect("an ancestor folder with a date resolves to plate 1",
       "/srv/synced/Plate_2026 run/Titration_FCS/Ab/x.fcs", 1)
expect("PLATE_2_experiment is not the PLATE_2 folder",
       "/srv/PLATE_2_experiment/Titration_FCS/Ab/x.fcs", 1)
expect("PLATE_10 is not plate 1", "/srv/Titration_FCS/PLATE_10/Ab/x.fcs", 3, default = 3L)
expect("a file literally named PLATE_2.fcs is not plate 2",
       "/srv/Titration_FCS/Ab/PLATE_2.fcs", 1)

cat("-- the real plate folders still resolve --\n")
expect("PLATE_1 folder", "/srv/Titration_FCS/PLATE_1/Strep_PE/x.fcs", 1)
expect("PLATE_2 folder", "/srv/Titration_FCS/PLATE_2/Strep_PE/x.fcs", 2)
expect("PLATE_3 folder", "/srv/Titration_FCS/PLATE_3/Strep_PE/x.fcs", 3)
expect("lower-case plate_2 folder",  "/srv/Titration_FCS/plate_2/Ab/x.fcs", 2)
expect("mixed-case Plate_3 folder",  "/srv/Titration_FCS/Plate_3/Ab/x.fcs", 3)
## file.sep is "/" on every platform R runs on, but a path TYPED by an operator or read out of a
## workbook can still arrive backslashed -- and it reaches here as $FILENAME unchanged.
expect("backslash-separated path",  "C:\\run\\Titration_FCS\\PLATE_2\\Ab\\x.fcs", 2)

cat("-- defaults and precedence --\n")
expect("a flat tree is plate 1", "/srv/Titration_FCS/Strep_PE/x.fcs", 1)
expect("the default argument is honoured when no token is present",
       "/srv/Titration_FCS/Strep_PE/x.fcs", 2, default = 2L)
expect("PLATE_1 wins when both appear", "/srv/PLATE_1/Titration_FCS/PLATE_2/Ab/x.fcs", 1)

cat("-- degenerate input still returns ONE number --\n")
## An FCS with no $FILENAME gave case_when() a zero-length condition, so `plate` came back
## zero-length, reached the layout filter and failed with the SAME OneIndex message by a different
## route. Length is the assertion here; the value only has to be the default.
expect("an absent $FILENAME falls back to the default", NULL, 1)
expect("an empty $FILENAME falls back to the default", "", 1)
expect("a bare file name with no directory", "x.fcs", 1)

cat(if (ok) "\nWHICH_PLATE TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
