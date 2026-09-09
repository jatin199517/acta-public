## ---------------------------------------------------------------------------------------------
## Build "Unit test 1": a standalone, publicly shareable copy of a single-group titration.
##
## Sanitises BOTH the workbook and the FCS keywords. The FCS files were the real exposure -- they
## embed the company name, an operator username, the programme name, the instrument serial, a donor
## code and the construct marker, none of which is visible from the layout sheet.
## ---------------------------------------------------------------------------------------------
suppressMessages({library(flowCore); library(readxl); library(openxlsx)})
src <- normalizePath(".", mustWork = TRUE)
ut  <- file.path(src, "Unit test 1")
dir.create(file.path(ut, "Template"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(ut, "Titration_FCS", "PLATE_1", "Strep_PE"), recursive = TRUE, showWarnings = FALSE)

## ---- 1. code + templates: verbatim copies, so the test drives the same pipeline --------------
for (p in c(list.files(src, pattern = "^ACTA_(Script|Functions|Dashboard).*[.]R$", full.names = TRUE),
            list.files(src, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE)))
  file.copy(p, file.path(ut, basename(p)), overwrite = TRUE)
file.copy(list.files(file.path(src, "Template"), pattern = "dashboard_template.*[.]html?$", full.names = TRUE),
          file.path(ut, "Template"), overwrite = TRUE)

## ---- 2. FCS: rewrite the identifying keywords, then rename the files -------------------------
## $PnN (detector names) are KEPT -- they are vendor names, carry nothing private, and $SPILLOVER
## is keyed on them. Only $PnS (the marker labels) needed changing, and only the construct one.
## The real marker label is the thing being removed, so it is not written down here -- it comes from
## the same gitignored sidecar the sanitisation gate uses (see verify_oq_sanitised.R). Without it
## this script would quietly copy the construct name straight into the "sanitised" fixture, so a
## missing sidecar is a hard stop, not a warning.
SCRUB_MARKER_MAP <- NULL
.tokfile <- file.path(this.path::this.dir(), ".acta_scrub_tokens.R")
if (file.exists(.tokfile)) source(.tokfile)
if (is.null(SCRUB_MARKER_MAP) || !length(SCRUB_MARKER_MAP))
  stop(".acta_scrub_tokens.R (beside tests/) must define SCRUB_MARKER_MAP, e.g.\n",
       '  SCRUB_MARKER_MAP <- c("<real marker label>" = "Antigen")\n',
       "Refusing to build a fixture that would carry the real $PnS label through.", call. = FALSE)
MARKER_MAP <- SCRUB_MARKER_MAP
SCRUB <- c("$INST" = "Anon", "$OP" = "operator", "$CYTSN" = "SN0000",
           "$PROJ" = "UNITTEST1_Titration_Strep-PE", "GROUPNAME" = "Unit test 1 Strep-PE")
fcs <- list.files(file.path(src, "Titration_FCS"), pattern = "[.]fcs$", recursive = TRUE, full.names = TRUE)
stopifnot(length(fcs) > 0)
## StainType per WELL, from the layout. The loader excludes any file with "Unstain" in its NAME, so
## the new filenames must keep that token or the unstained well becomes an extra analysable sample
## and the file/row counts stop matching. The filename is load-bearing, not decoration.
layRaw <- suppressMessages(read_excel(list.files(src, pattern = "Titration_Instructions.*[.]xlsx$", full.names = TRUE)[1],
                                     sheet = "Layout_Plate", .name_repair = "minimal"))
layRaw <- layRaw[!is.na(layRaw$Dirname), , drop = FALSE]
wellType <- setNames(trimws(as.character(layRaw$StainType)),
                     toupper(paste0(trimws(as.character(layRaw$Row)),
                                    trimws(as.character(layRaw[["Column (N)"]])))))
for (p in fcs) {
  ff  <- suppressWarnings(read.FCS(p, truncate_max_range = FALSE, transformation = FALSE))
  kw  <- keyword(ff)
  well <- toupper(trimws(kw[["$WELLID"]]))
  ## markers: swap only the mapped ones
  ms <- markernames(ff)
  if (length(ms)) { hit <- ms %in% names(MARKER_MAP)
                    if (any(hit)) { ms[hit] <- MARKER_MAP[ms[hit]]; markernames(ff) <- ms } }
  for (k in names(SCRUB)) if (k %in% names(kw)) keyword(ff)[[k]] <- SCRUB[[k]]
  ## read.FCS stamps FILENAME with the full SOURCE PATH, and write.FCS bakes it in -- so the
  ## "sanitised" file would carry the username and the OneDrive library name. Drop every keyword
  ## whose value looks like a local path, not just this one.
  kk <- keyword(ff)
  pathish <- names(kk)[vapply(kk, function(v)
    is.character(v) && length(v) == 1 && grepl("^(/|[A-Za-z]:\\\\)", v), logical(1))]
  for (k in pathish) keyword(ff)[[k]] <- NULL
  ## $FIL must match the new filename; the old one carried the donor code
  st <- wellType[[well]]
  newname <- sprintf("UT1_Strep-PE_%s_%s.fcs", well, if (is.null(st) || is.na(st)) "Stain" else st)
  keyword(ff)[["$FIL"]] <- newname
  suppressWarnings(write.FCS(ff, file.path(ut, "Titration_FCS", "PLATE_1", "Strep_PE", newname)))
}
cat(sprintf("[ut1] %d FCS rewritten\n", length(fcs)))

## ---- 3. layout workbook: sanitise and rename ------------------------------------------------
lay <- list.files(src, pattern = "Titration_Instructions.*[.]xlsx$", full.names = TRUE)[1]
out <- file.path(ut, "UNITTEST1_Ab_Titration_Instructions_2_87.xlsx")
file.copy(lay, out, overwrite = TRUE)
wb <- loadWorkbook(out)
setCell <- function(sheet, col, value) {
  raw <- suppressMessages(read_excel(out, sheet = sheet, col_names = FALSE, .name_repair = "minimal"))
  hdr <- as.character(unlist(raw[1, ])); j <- which(hdr == col)
  if (!length(j)) return(invisible(FALSE))
  n <- nrow(raw)
  writeData(wb, sheet, rep(value, n - 1), startCol = j, startRow = 2, colNames = FALSE)
  invisible(TRUE)
}
## Row-wise columns in Layout_Plate: only rewrite where the row is populated, so blank template
## rows stay blank rather than being filled with a fake value.
raw <- suppressMessages(read_excel(out, sheet = "Layout_Plate", col_names = FALSE, .name_repair = "minimal"))
hdr <- as.character(unlist(raw[1, ]))
dirCol <- which(hdr == "Dirname")
keep <- which(!is.na(raw[[dirCol]][-1])) + 1L      # excel rows with a populated Dirname
for (cn in c("ELN_ID" = "EXP00000001", "Operator" = "OP", "Test_Material" = "Test material X")) NULL
for (nm in c("ELN_ID", "Operator", "Test_Material")) {
  j <- which(hdr == nm); if (!length(j)) next
  val <- switch(nm, ELN_ID = "EXP00000001", Operator = "OP", Test_Material = "Test material X")
  ## EVERY row that currently holds a value, not only the rows with a Dirname: these columns are
  ## filled further down the sheet than the populated wells, so a Dirname-scoped rewrite left the
  ## real ELN ID sitting in the tail.
  col <- as.character(raw[[j]])[-1]
  rows <- which(!is.na(col) & nzchar(trimws(col))) + 1L
  for (r in rows) writeData(wb, "Layout_Plate", val, startCol = j, startRow = r, colNames = FALSE)
}
## filename column must match the renamed FCS
jf <- which(hdr == "filename")
if (length(jf)) {
  rowsW <- toupper(paste0(trimws(as.character(raw[[which(hdr == "Row")]][keep])),
                          trimws(as.character(raw[[which(hdr == "Column (N)")]][keep]))))
  stW <- trimws(as.character(raw[[which(hdr == "StainType")]][keep]))
  writeData(wb, "Layout_Plate", sprintf("UT1_Strep-PE_%s_%s.fcs", rowsW, stW),
            startCol = jf, startRow = min(keep), colNames = FALSE)
}
setCell("Info", "ELN_ID", "EXP00000001"); setCell("Info", "Operator", "OP")
saveWorkbook(wb, out, overwrite = TRUE)
cat("[ut1] layout written:", basename(out), "\n")
