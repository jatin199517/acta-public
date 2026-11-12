## DIFFERENTIAL READ: every shipped workbook, through TWO independent readers.
##
##     Rscript differential_read.R
##
## WHY. The row-F defect was a string silently reinterpreted at a library boundary.
## test_pdata_restore.R probes nineteen such shapes through GatingSet() -- but that probe starts
## from a frame R already holds. It cannot see a value reinterpreted ON THE WAY IN, when readxl
## guessed the type of a workbook column, because a reader cannot detect its own guessing: it
## reports what it decided, confidently, and every downstream gate agrees with it.
##
## Two readers can. This dumps what readxl sees and hands it to openpyxl (differential_read.py),
## which exposes the cell's stored type. A disagreement is a cell where the type system, not the
## operator, chose what the value says.
##
## WHAT IT IS NOT. Not a style or formatting check -- both sides render through the same
## normalisation, so only differences in MEANING are reported. A whole number is "3" on both
## sides whether it was stored as 3 or 3.0.
##
## Minutes, and it needs python3 + openpyxl, so it is NOT in run_all.R. Run it when a workbook
## changes, and before a release.
suppressMessages(library(readxl))
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

norm <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | x == "NA"] <- NA_character_
  x
}
## Match differential_read.py's as_text(): a whole number renders without a trailing ".0", so the
## comparison is about meaning rather than float formatting.
render <- function(col) {
  if (is.logical(col)) return(ifelse(is.na(col), NA_character_, ifelse(col, "TRUE", "FALSE")))
  if (is.numeric(col))
    return(ifelse(is.na(col), NA_character_,
                  ifelse(col == floor(col) & abs(col) < 1e15,
                         format(col, scientific = FALSE, trim = TRUE, digits = 15),
                         as.character(col))))
  norm(col)
}

## ACTA_REPO_ROOT IS ALREADY THE PACKAGE DIRECTORY in this layout (.../ACTA/ACTA_package), not the
## repo root its name suggests -- appending "ACTA_package" doubles it and finds nothing.
pkg    <- ACTA_REPO_ROOT
books  <- c(list.files(file.path(pkg, "inst", "extdata", "oq_small"),
                       pattern = "[.]xlsx$", full.names = TRUE, recursive = TRUE),
            list.files(file.path(pkg, "inst", "pipeline", "Template"),
                       pattern = "[.]xlsx$", full.names = TRUE))
books <- books[!grepl("/Outputs/", books)]
if (!length(books)) { cat("no workbooks found under", pkg, "\n"); quit(status = 1L) }

## READ WITH READXL'S OWN GUESSING, deliberately -- not col_types = "text". Forcing text is what
## the rest of the suite does to get exact values; here the guessing IS the thing under test.
dump <- list()
for (f in books) {
  for (s in tryCatch(excel_sheets(f), error = function(e) character(0))) {
    d <- tryCatch(as.data.frame(suppressMessages(read_excel(f, sheet = s))),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d)) next
    cols <- lapply(d, render)
    dump[[length(dump) + 1L]] <- list(
      file = normalizePath(f), sheet = s, header = as.list(names(d)),
      ## unname(): a NAMED list serialises as a JSON object, and the Python side indexes rows
      ## POSITIONALLY to stay aligned with `header`. Without this each row is a dict and the
      ## comparison dies on a KeyError instead of comparing anything.
      rows = lapply(seq_len(nrow(d)), function(i) unname(lapply(cols, function(cc) cc[[i]]))))
  }
}
tmp <- file.path(tempdir(), "acta_diffread.json")
writeLines(jsonlite::toJSON(dump, auto_unbox = TRUE, null = "null", na = "null"), tmp)
cat(sprintf("readxl read %d sheet(s) from %d workbook(s)\n", length(dump), length(books)))

py <- file.path(this.path::this.dir(), "differential_read.py")
st <- system2("python3", c(shQuote(py), shQuote(tmp)), stdout = TRUE, stderr = TRUE)
cat(paste(st, collapse = "\n"), "\n")
code <- attr(st, "status"); if (is.null(code)) code <- 0L
## openpyxl missing exits 2. A reader that cannot read must not report clean -- that is the whole
## rule this family of gates exists to enforce, so it is a failure here too.
if (identical(as.integer(code), 2L)) {
  cat("\n** openpyxl is not installed, so NOTHING was differentially read **\n"); quit(status = 1L)
}
cat(if (code == 0L) "\nDIFFERENTIAL READ OK\n" else "\n** DIFFERENTIAL READ FOUND DISAGREEMENTS **\n")
quit(status = if (code == 0L) 0L else 1L)
