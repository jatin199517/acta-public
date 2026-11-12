## SEED THE CANARY COLUMN: put the hazard corpus into a shipped fixture, permanently.
##
##     Rscript seed_canary_column.R <instructions.xlsx> [Clone]
##
## WHY. Six datasets and a full regression suite never saw the row-F defect because every value in
## every fixture was benign. test_boundary_probes.R pushes 22 hazardous shapes through four
## boundaries, but only in a unit test, against synthetic frames -- it cannot see what a real run
## does to them across the whole pipeline.
##
## So the hazards go INTO the fixture. `Clone` is empty on all 96 rows of OQ_Test1 and is pure
## metadata: no gate, no QC check, no stain index and no plot reads it, so seeding it cannot move
## a number. It DOES travel into TitrationExport, into the export's Layout_Plate audit copy, into
## the dashboard and into the bundle -- and check_metadata_identity.R compares every one of those
## against the workbook. One OQ run then probes 22 hazards across the entire published surface,
## every time anybody runs it, forever.
##
## A CASE BUILDER, NOT A GATE. It REWRITES a tracked workbook. Run it deliberately, then re-run
## the case and check_metadata_identity.R. Listed with the other builders in README.md.
##
## XML LEVEL ONLY, via set_workbook_cells.R -- never an openxlsx load/save round trip, which has
## corrupted cells in sheets nobody touched. Driven from R rather than the shell so that `=1+1`,
## `#N/A`, a trailing space and the XML entities reach the tool intact.
suppressMessages(library(readxl))
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

args  <- commandArgs(TRUE)
path  <- normalizePath(args[[1]], mustWork = TRUE)
col   <- if (length(args) > 1L) args[[2]] else "Clone"

## The corpus, kept in step with test_boundary_probes.R by hand -- deliberately NOT sourced from
## it, because this writes a tracked artefact and must not change because a test file changed.
##
## "007" AND NOT "001": OQ_Test1 ships SOP = "001", and a canary equal to it would trip
## .tpScrubMap()'s collision guard and stop every scrubbed run of this case. The guard is correct;
## the fixture simply must not pick a fight with it. Zero-padding is still exercised.
HAZARDS <- c(
  "F", "T", "TRUE", "NA", "NaN", "Inf",
  "007", "-007", "+42", "0x1A", "1E5", "3.90625E-2",
  "2026-10-23", "1/2", "#N/A", "=1+1",
  "00000000000001", "20250101123456789", "LOT 55 ",
  "a<b>&c\"d'e", "a\\Ed", "µg/mL é")

lp <- as.data.frame(suppressMessages(read_excel(path, sheet = "Layout_Plate", col_types = "text")))
nm <- sub("[[:space:]]*\\([^()]*\\)$", "", trimws(names(lp)))
j  <- match(col, nm)
if (is.na(j)) stop(sprintf("no column %s in Layout_Plate", col), call. = FALSE)
L  <- LETTERS[j]
used <- which(!is.na(lp[[1]]) & nzchar(trimws(as.character(lp[[1]]))))
if (length(used) < length(HAZARDS))
  stop(sprintf("%s has %d data rows, fewer than the %d hazards", basename(path),
               length(used), length(HAZARDS)), call. = FALSE)

existing <- unique(stats::na.omit(trimws(as.character(lp[[j]][used]))))
existing <- existing[nzchar(existing)]
if (length(existing))
  cat(sprintf("NOTE: %s already holds %d distinct value(s); they will be OVERWRITTEN: %s\n",
              col, length(existing), paste(utils::head(existing, 5), collapse = ", ")))

## A CANARY MUST NOT COLLIDE WITH A SCRUB TARGET. .tpScrubMap() is value-keyed and stops the run
## when a confidential value is not unique to its own column -- correctly. Check before writing,
## so this script cannot be the thing that breaks every scrubbed run of the case.
tgt <- intersect(c("Operator", "ELN_ID", "ELN_Name", "SOP", "Costain_Panel_Iteration"), nm)
clash <- character(0)
for (t in tgt) {
  v <- unique(stats::na.omit(trimws(as.character(lp[[match(t, nm)]][used]))))
  clash <- c(clash, intersect(v[nzchar(v)], HAZARDS))
}
if (length(clash))
  stop(sprintf(paste0("a hazard value is already in a SCRUBBED column (%s), so seeding it would ",
                      "trip the collision guard and stop every scrubbed run. Change the fixture ",
                      "cell or the corpus."), paste(unique(clash), collapse = ", ")), call. = FALSE)

tool <- file.path(ACTA_TEST_DIR, "set_workbook_cells.R")
rows <- used[seq_along(HAZARDS)] + 1L            # +1: the header occupies sheet row 1
for (i in seq_along(HAZARDS)) {
  st <- system2("Rscript", c(shQuote(tool), shQuote(path), "Layout_Plate", as.character(rows[i]),
                             shQuote(paste0(L, "=", HAZARDS[i]))),
                stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(st, "status")) && attr(st, "status") != 0L)
    stop(sprintf("set_workbook_cells failed on row %d: %s", rows[i],
                 paste(st, collapse = " ")), call. = FALSE)
}

## VERIFY BY READING BACK, not by trusting the writer. A seeder that reports success over a
## workbook it did not actually change is the same fault as everything else in this folder.
back <- as.data.frame(suppressMessages(read_excel(path, sheet = "Layout_Plate", col_types = "text")))
got  <- trimws(as.character(back[[j]][used[seq_along(HAZARDS)]]))
want <- trimws(HAZARDS)
bad  <- which(got != want)
for (i in bad) cat(sprintf("  [FAIL] row %d: wrote %s, read back %s\n", rows[i],
                           sQuote(want[i]), sQuote(got[i])))
cat(sprintf("%s: seeded %d hazard(s) into %s (sheet rows %d-%d), %d mismatch(es)\n",
            basename(path), length(HAZARDS), col, min(rows), max(rows), length(bad)))
quit(status = if (length(bad)) 1L else 0L)
