## EVERY BOUNDARY A STRING CROSSES, PROBED WITH THE SAME HAZARD CORPUS.
##
## WHY THIS FILE EXISTS. The row-F defect was an operator string silently reinterpreted by a
## library: "F" went into GatingSet() and "FALSE" came out. You cannot enumerate the defects of
## that class -- nobody enumerated row F -- but you CAN enumerate the boundaries, because a string
## only changes meaning where it crosses between representations.
##
## ACTA has seven such boundaries. Two were already probed:
##
##   1. workbook -> readxl type guessing       differential_read.R  (two readers, 36k cells)
##   2. flowSet pData -> GatingSet             test_pdata_restore.R (19 shapes, real GatingSet)
##   3. stats -> openxlsx write -> readxl      ** HERE **
##   4. stats -> jsonlite (the dashboard)      ** HERE **
##   5. workbook cell -> staged FCS filename   ** HERE **
##   6. map key -> XML rewrite (the scrub)     ** HERE ** (and the \E case in test_scrub_iteration)
##   7. stats -> LaTeX (the report PDF)        NOT COVERED -- see the note at the end.
##
## ONE CORPUS, SHARED. A boundary is only as well probed as the nastiest value pushed through it,
## and keeping one list means a hazard learned at one boundary is immediately tested at all of
## them. Every entry below is a shape that has actually changed meaning in some reader.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d; if (is.null(d)) d <- tryCatch(paste(as.character(detail), collapse = " "),
                                          error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages(library(ACTA))
suppressMessages(library(readxl))

## THE CORPUS. Each is a string an operator can legitimately type into a metadata cell.
HAZARDS <- c(
  row_F          = "F",              # the original defect: parsed as logical FALSE
  row_T          = "T",
  word_TRUE      = "TRUE",
  word_NA        = "NA",             # parsed as missing, which erases the value
  word_NaN       = "NaN",
  word_Inf       = "Inf",
  padded         = "001",            # leading zero is MEANING in an identifier
  padded_signed  = "-007",
  plus_signed    = "+42",
  hex_like       = "0x1A",           # R parses hex: becomes "26"
  sci_like       = "1E5",            # becomes "1e+05" or "100000"
  excel_sci      = "3.90625E-2",
  iso_date       = "2026-10-23",     # Excel makes this a DATE; readers render it differently
  slash_date     = "1/2",
  excel_error    = "#N/A",
  formula        = "=1+1",           # a writer may store this as a FORMULA, not text
  lead_zero_long = "00000000000001",
  long_digits    = "20250101123456789",   # past 2^53: two digit strings collapse to one double
  trailing_space = "LOT 55 ",
  xml_entities   = "a<b>&c\"d'e",    # must survive XML escaping intact
  regex_quote    = "a\\Ed",          # ends a \Q...\E span -- see .tpApplyRepl
  unicode        = "µg/mL é"
)

cat("=== boundary 3: stats -> openxlsx (writeTitrationExport) -> readxl ===\n")
## THE EXPORT IS THE PUBLISHED ARTEFACT. openxlsx decides a cell's TYPE from the R column, and a
## character column is supposed to stay text -- but "=1+1" as a formula or "TRUE" as a boolean
## would both arrive changed, in the file that goes to a CRO.
df <- data.frame(Alias = names(HAZARDS), StainType = "Stain", StainQty = seq_along(HAZARDS),
                 Probe = unname(HAZARDS), check.names = FALSE, stringsAsFactors = FALSE)
xp <- file.path(tempdir(), sprintf("bp_%s.xlsx", basename(tempfile(""))))
wrote <- tryCatch({ ACTA:::writeTitrationExport(df, xp); TRUE },
                  error = function(e) conditionMessage(e))
chk("the export writes", isTRUE(wrote), wrote)
if (isTRUE(wrote)) {
  back <- as.data.frame(suppressMessages(read_excel(xp, sheet = "TitrationExport",
                                                    col_types = "text")))
  for (nm in names(HAZARDS)) {
    want <- unname(HAZARDS[[nm]])
    got  <- back$Probe[match(nm, back$Alias)]
    ## trimws on the READ side only: Excel preserves the authored trailing space in the cell, but
    ## whether a reader hands it back is the reader's business, not the writer's.
    chk(sprintf("export round-trip: %-14s", nm),
        !is.na(got) && identical(trimws(got), trimws(want)),
        sprintf("wrote %s, read back %s", sQuote(want), sQuote(got)))
  }
}

cat("\n=== boundary 4: stats -> jsonlite (the dashboard's const DATA) ===\n")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  j   <- jsonlite::toJSON(as.list(HAZARDS), auto_unbox = TRUE)
  rt  <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  bad <- names(HAZARDS)[vapply(names(HAZARDS),
                               function(nm) !identical(as.character(rt[[nm]]),
                                                       unname(HAZARDS[[nm]])), TRUE)]
  chk("every hazard survives the JSON round-trip the dashboard uses",
      length(bad) == 0L, paste(bad, collapse = ", "))
} else chk("jsonlite available for the dashboard probe", FALSE, "jsonlite not installed")

cat("\n=== boundary 5: workbook cell -> staged FCS filename ===\n")
## .tpRenameToken() rewrites a confidential value inside a FILE NAME, on token boundaries. A
## hazard here does not change a value's meaning -- it corrupts the name of a data file, or
## silently fails to scrub it.
fn <- ACTA:::.tpRenameToken
for (nm in names(HAZARDS)) {
  v <- unname(HAZARDS[[nm]])
  ## A filename cannot contain a slash on any platform, so those two are out of scope by
  ## construction rather than by omission.
  if (grepl("[/\\\\]", v)) next
  base <- sprintf("EXP00000001_%s_A1.fcs", v)
  want <- "EXP00000001_REDACTED_A1.fcs"
  out  <- tryCatch(fn(base, stats::setNames(list("REDACTED"), v)),
                   error = function(e) paste("ERROR:", conditionMessage(e)))
  ## EXACT RESULT, not a substring search for the old value. A substring test fails on values
  ## that occur inside the REST of the name: "T" is in "REDACTED" and "001" is in "EXP00000001",
  ## so both reported a correct rename as a leak. The property wanted is that the TOKEN was
  ## replaced and nothing else moved, and only the whole string says that.
  chk(sprintf("filename rename: %-14s", nm),
      is.character(out) && length(out) == 1L && identical(out, want),
      sprintf("%s -> %s (wanted %s)", base, out, want))
}

cat("\n=== boundary 6: map key -> XML rewrite (.tpApplyRepl) ===\n")
## The scrub rewrites whole <t> elements by regex. A key containing regex syntax, XML entities or
## a \E must be matched LITERALLY and replaced exactly once -- see the measured \E case in
## test_scrub_iteration.R, where the pattern missed its own value AND hit an unrelated cell.
suppressMessages(library(openxlsx))
for (nm in names(HAZARDS)) {
  v <- unname(HAZARDS[[nm]])
  lp <- data.frame(Row = c("A", "A"), `Column (N)` = c(1, 2), Alias = c("CD3", "CD4"),
                   Operator = c(v, v), check.names = FALSE, stringsAsFactors = FALSE)
  f  <- file.path(tempdir(), sprintf("bx_%s.xlsx", paste(sample(letters, 8), collapse = "")))
  w  <- createWorkbook(); addWorksheet(w, "Layout_Plate"); writeData(w, "Layout_Plate", lp)
  saveWorkbook(w, f, overwrite = TRUE)
  res <- tryCatch({
    m <- ACTA:::.tpScrubMap(f)
    ACTA:::.tpApplyRepl(f, m)
    d <- as.data.frame(suppressMessages(read_excel(f, sheet = "Layout_Plate", col_types = "text")))
    unique(trimws(as.character(d$Operator)))
  }, error = function(e) paste("ERROR:", conditionMessage(e)))
  want <- unname(ACTA:::ACTA_SCRUB_CONFIDENTIAL[["Operator"]])
  ## openxlsx may not store the authored text verbatim (a formula, a number), so the ONLY thing
  ## asserted is the property that matters: the confidential value does not survive the scrub.
  chk(sprintf("scrub leaves nothing behind: %-14s", nm),
      length(res) == 1L && (identical(res, want) || !grepl(v, res, fixed = TRUE)),
      sprintf("Operator reads %s (wanted %s)", sQuote(paste(res, collapse = ",")), sQuote(want)))
}

cat("\n")
## BOUNDARY 7 IS NOT COVERED, and saying so is the point of listing it.
## stats -> LaTeX -> PDF needs a TeX engine and a full render, so it cannot live in a fast gate.
## The report's Info table goes through actaDeIdentifyText() and kableExtra, both of which can
## re-render a value. ci_report_render.R renders a report but asserts only that a PDF appeared.
## Closing it means extracting the PDF text with pdftools and asserting the corpus survives --
## queued, not done.
cat(if (ok) "BOUNDARY PROBES PASSED\n" else "FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
