## INDEPENDENT end-to-end check: what the workbook SAYS is what the export PUBLISHES.
##
## This is the check that was missing. The row-F defect was not a gating bug, a leak, or a
## regression against a previous run -- it was an authored value arriving changed in a published
## artefact, and nothing in the suite compared those two things. The regression gate compares run
## to run (so it would have agreed with itself about "FALSE" forever), and the leak sweep looks for
## secrets, not for truth.
##
## So: read the instructions workbook, read the export, and for every Layout_Plate column that also
## appears in TitrationExport / SI_stats / Pop_stats, require the published value for each well to
## be the value the operator wrote.
##
## Deliberately NOT written against ACTA's own readers -- the workbook is read with readxl and the
## export with readxl, both as TEXT, so neither side can be normalised into agreement.
##
##   Rscript check_metadata_identity.R <run_dir> [<run_dir> ...]
##
## ---------------------------------------------------------------------------------------------
## FOUR APERTURES, because the first version had one and it was not the one that mattered.
##
## Written in response to row F, this script opened exactly TWO files -- the instructions workbook
## and the TitrationExport -- and compared the Layout_Plate columns that also appear in the export.
## Row F was found by a reader of a published BUNDLE. The artefact where the defect was actually
## discovered was not checked, and neither were the audit copies sitting inside the export itself.
##
##   1. KEY-MATCHED, per well: TitrationExport / SI_stats / Pop_stats vs Layout_Plate. The original
##      check. Only columns that TRAVEL into the export, only rows that match a well.
##   2. AUDIT COPIES, cell for cell: the export carries its own Info / Layout_Plate /
##      gating_template sheets, same shape as the workbook's. Nothing compared them. This is the
##      WIDEST aperture by far -- every column including ones that never travel, every row
##      including wells with no FCS -- and the cheapest, because the shapes already match.
##   3. THE BUNDLE: the TiterPubs zip carries its own copy of the workbook and the export. It is
##      the artefact that goes to Zenodo and TiterPubs, and the one row F was found in.
##   4. THE DASHBOARD: its `const DATA = [...]` is structured JSON, so the published per-run
##      metadata is comparable without parsing HTML.
## ---------------------------------------------------------------------------------------------
suppressMessages(library(readxl))

## Columns whose export value is DERIVED and legitimately differs from the layout cell.
DERIVED <- c("name", "label", "Panel", "fcs_file", "Titration", "Markername", "Fl")

## Columns scrub_confidential rewrites by design; compared only on an unscrubbed run.
##
## ASK THE SCRUB, DO NOT RESTATE IT. A hand-written copy of this list omitted `Cell_Type`, which
## actaApplyScrub() also rewrites wherever a site has configured `acta.cell_type_rules`. The
## consequence was not a missed leak but the opposite and almost as bad: on a correctly
## de-identified run the check would have reported FAIL and PRINTED the internal code it was there
## to confirm had been removed -- a cry-wolf that also leaks, in the output most likely to be
## pasted into a ticket. Derived from the package, so the next column the scrub gains is covered
## without anyone remembering this file.
SCRUBBED <- local({
  fallback <- c("Operator", "ELN_ID", "ELN_Name", "SOP", "Cell_Type", "Costain_Panel_Iteration")
  v <- tryCatch(names(getFromNamespace("ACTA_SCRUB_CONFIDENTIAL", "ACTA")),
                error = function(e) NULL)
  ## Cell_Type and Costain_Panel_Iteration are scrubbed by separate mechanisms (site cell-type
  ## rules, and the iteration placeholder map), so they are not in that vector and are named here.
  if (is.null(v)) fallback else unique(c(v, "Cell_Type", "Costain_Panel_Iteration"))
})

norm <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "" | x == "NA"] <- NA_character_
  x
}
## A number may legitimately be re-rendered ("3.90625E-2" -> "0.0390625"); a string may not.
same_cell <- function(a, b) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.na(a) || is.na(b)) return(FALSE)
  if (identical(a, b)) return(TRUE)
  plain <- function(x) grepl("^\\s*[+-]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][+-]?[0-9]+)?\\s*$", x)
  padded <- function(x) grepl("^\\s*[+-]?0[0-9]", x)
  ## 15 SIGNIFICANT DIGITS MAX -- the same bound actaRestorePData() uses, and for the same reason:
  ## past 2^53 two different digit strings collapse to one double, so a numeric comparison would
  ## call a corrupted identifier equal and this gate would agree with the bug it exists to catch.
  d15 <- function(x) nchar(gsub("[^0-9]", "", x)) <= 15L
  if (plain(a) && plain(b) && !padded(a) && d15(a) && d15(b)) {
    ## EXACT EQUALITY, matching actaRestorePData()'s `na == nb`. all.equal() applies a 1.5e-8
    ## RELATIVE tolerance, which below the 15-digit bound is wide enough to call a corrupted
    ## identifier equal: 123456789 vs 123456790, or a 10-digit Lot_Num off by one, both pass it.
    ## The function this mirrors would restore those; with all.equal() the gate agreed with the
    ## bug instead of catching it.
    na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
    if (!is.na(na) && !is.na(nb) && na == nb) return(TRUE)
  }
  FALSE
}

## TWO OUTPUT SHAPES. run_acta() writes the export beside the workbook, in the run folder root;
## actaOQRun() writes it into Outputs/. Look in both rather than making the caller know which --
## a checker that silently finds nothing is the failure mode this whole file exists to prevent,
## so a directory with no export is a HARD FAIL below, never a quiet skip.
find1 <- function(d, pat) {
  for (dir in c(d, file.path(d, "Outputs"))) {
    if (!dir.exists(dir)) next
    f <- list.files(dir, pattern = pat, full.names = TRUE)
    if (length(f)) return(f[[1]])
  }
  NA_character_
}

## Info variables the scrub rewrites, asked of the package rather than restated -- same discipline
## as SCRUBBED above, and for the same reason.
SCRUBBED_INFO <- local({
  v <- tryCatch(names(getFromNamespace("ACTA_SCRUB_INFO", "ACTA")), error = function(e) NULL)
  c(if (is.null(v)) "Submitter_Email" else v, "scrub_eln_placeholder")
})

## APERTURE 2: the export's own audit copy of a workbook sheet, compared CELL FOR CELL.
##
## No keys and no column intersection: the two sheets have the same shape, so position IS the
## correspondence. That makes this the widest aperture in the file -- it sees columns that never
## travel into TitrationExport and rows for wells that produced no FCS, which the key-matched pass
## cannot reach by construction.
##
## A shape mismatch is a FAILURE, not a skip. An audit copy that is not the same shape as what it
## claims to copy has already lost the property being checked.
auditIdentity <- function(wb, xp, sheet, scrubbed, label) {
  a <- tryCatch(as.data.frame(suppressMessages(read_excel(wb, sheet = sheet, col_types = "text"))),
                error = function(e) NULL)
  b <- tryCatch(as.data.frame(suppressMessages(read_excel(xp, sheet = sheet, col_types = "text"))),
                error = function(e) NULL)
  ## An audit sheet the export does not carry is not this check's business -- but one the WORKBOOK
  ## does not carry, while the export does, is a value with no source and is reported.
  if (is.null(b)) return(list(n = 0L, bad = 0L, note = "absent from the export"))
  if (is.null(a)) return(list(n = 0L, bad = 1L, note = "present in the export, ABSENT from the workbook"))
  if (!identical(dim(a), dim(b)))
    return(list(n = 0L, bad = 1L,
                note = sprintf("shape %s vs %s", paste(dim(a), collapse = "x"),
                               paste(dim(b), collapse = "x"))))
  nm <- sub("\\s*\\([^()]*\\)$", "", trimws(names(a)))
  skipCol <- if (scrubbed && identical(sheet, "Layout_Plate")) nm %in% SCRUBBED else
             rep(FALSE, length(nm))
  n <- 0L; bad <- 0L
  for (j in seq_along(a)) {
    if (skipCol[j]) next
    x <- norm(a[[j]]); y <- norm(b[[j]])
    for (i in seq_along(x)) {
      ## Info is long (Variable/Value); a scrubbed run legitimately rewrites some of its VALUES,
      ## so those rows are skipped by variable name rather than the whole column.
      if (scrubbed && identical(sheet, "Info") && identical(nm[j], "Value") &&
          !is.na(a$Variable[i]) && trimws(a$Variable[i]) %in% SCRUBBED_INFO) next
      n <- n + 1L
      if (!same_cell(x[i], y[i])) {
        bad <- bad + 1L
        if (scrubbed)
          cat(sprintf("  [FAIL] %s %s!%s row %d: audit copy differs (values withheld -- scrubbed run)\n",
                      label, sheet, names(a)[j], i + 1L))
        else
          cat(sprintf("  [FAIL] %s %s!%s row %d: workbook=%-18s export=%s\n",
                      label, sheet, names(a)[j], i + 1L, x[i], y[i]))
      }
    }
  }
  list(n = n, bad = bad, note = NULL)
}

ok <- TRUE
## APERTURE 3: a bundle is a run folder in a zip. Unpack it and feed it back through the same
## comparison rather than writing a second, divergent one.
expand <- function(d) {
  z <- list.files(d, pattern = "TiterPubs.*[.]zip$", full.names = TRUE, recursive = TRUE)
  if (!length(z)) return(character(0))
  z <- z[order(file.mtime(z), decreasing = TRUE)][1]
  out <- file.path(tempdir(), paste0("mid_", basename(tempfile(""))))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  ex  <- tryCatch(suppressWarnings(utils::unzip(z, exdir = out)), error = function(e) character(0))
  dec <- tryCatch(nrow(utils::unzip(z, list = TRUE)), error = function(e) NA_integer_)
  ## A bundle that cannot be fully read must not be reported as checked -- the same rule
  ## actaBundleScan() learned when a truncated bundle scanned "clean".
  if (!length(ex) || is.na(dec) || length(ex) != dec) {
    cat(sprintf("  [FAIL] %s: the bundle could not be fully extracted (%d of %s)\n",
                basename(d), length(ex), format(dec)))
    ok <<- FALSE; return(character(0))
  }
  ## The bundle nests its payload one level down; find the folder holding the workbook.
  w <- list.files(out, pattern = "Ab_Titration_Instructions[.]xlsx$", full.names = TRUE,
                  recursive = TRUE)
  if (!length(w)) return(character(0))
  unique(dirname(w))
}

targets <- unlist(lapply(commandArgs(TRUE), function(d) c(d, expand(d))))
for (d in targets) {
  wb <- find1(d, "Ab_Titration_Instructions[.]xlsx$")
  xp <- find1(d, "TitrationExport.*[.]xlsx$")
  if (is.na(wb) || is.na(xp)) { cat(sprintf("%-16s ** workbook or export missing **\n", basename(d)))
                                ok <- FALSE; next }
  inf <- as.data.frame(suppressMessages(read_excel(wb, sheet = "Info", col_types = "text")))
  gv  <- function(v) { i <- which(!is.na(inf$Variable) & trimws(inf$Variable) == v)
                       if (length(i)) trimws(as.character(inf$Value[i[1]])) else NA_character_ }
  scrubbed <- isTRUE(toupper(gv("scrub_confidential")) == "TRUE")

  lp <- as.data.frame(suppressMessages(read_excel(wb, sheet = "Layout_Plate", col_types = "text")))
  ## STRIP ANY PARENTHETICAL SUFFIX, not a single letter. `\\([A-Za-z]\\)` handles "Column (N)"
  ## and "Fix (L)" but NOT "Channel ($PnN)" or "Markername ($PnS)" -- so `Channel` silently fell
  ## outside the comparison while this file claimed every travelling column was checked. Same
  ## shape as the defect the script was written for: a checker quietly covering less than it says.
  ## Mirrors ACTA's own actaStripHeaderAnnotation().
  names(lp) <- sub("\\s*\\([^()]*\\)$", "", trimws(names(lp)))     # "Column (N)" -> "Column"

  ## THE KEY DELIBERATELY EXCLUDES Row. Row is one of the values under test, so keying on it would
  ## hide exactly the defect this script exists to find -- a mangled Row simply fails to match and
  ## the row gets skipped. Alias + StainType + numeric StainQty identifies a well uniquely within a
  ## titration (the Costain and the Unstained both sit at quantity 0 and are told apart by type),
  ## and StainQty is compared numerically so Excel's "3.90625E-2" still matches "0.0390625".
  ##
  ## Alias + Column was tried first and was WORSE THAN USELESS: in OQ_Test2 three aliases share
  ## three plate columns, so every key matched several layout rows, every row was skipped, and the
  ## script reported "0 cell(s) compared ... OK". That is the same shape as the defect it is
  ## checking for -- a reader that reads nothing reporting clean -- which is why zero comparisons
  ## is now a hard failure below.
  mkkey <- function(d) paste(norm(d$Alias), norm(d$StainType),
                             suppressWarnings(as.numeric(norm(d$StainQty))))
  key_lp <- mkkey(lp)

  sheets <- intersect(c("TitrationExport", "SI_stats", "Pop_stats"), excel_sheets(xp))
  ncmp <- 0; nbad <- 0; nwell <- 0; nskip <- 0
  for (s in sheets) {
    ex <- as.data.frame(suppressMessages(read_excel(xp, sheet = s, col_types = "text")))
    if (!all(c("Alias", "StainType", "StainQty") %in% names(ex))) next
    key_ex <- mkkey(ex)
    cols <- setdiff(intersect(names(lp), names(ex)), c(DERIVED, if (scrubbed) SCRUBBED))
    for (i in seq_len(nrow(ex))) {
      j <- which(key_lp == key_ex[i])
      if (length(j) != 1L) { nskip <- nskip + 1; next }
      nwell <- nwell + 1
      for (cn in cols) {
        a <- norm(lp[[cn]][j]); b <- norm(ex[[cn]][i]); ncmp <- ncmp + 1
        if (!same_cell(a, b)) {
          nbad <- nbad + 1; ok <- FALSE
          ## ON A SCRUBBED RUN, NAME THE CELL AND NOT ITS CONTENTS. This output is the thing an
          ## operator pastes into a ticket or a bundle note, and a de-identification check that
          ## prints the values it is checking defeats itself. The address is enough to find it.
          if (scrubbed)
            cat(sprintf("  [FAIL] %s %s row %d: %-24s workbook and export differ (values withheld -- scrubbed run)\n",
                        basename(d), s, i, cn))
          else
            cat(sprintf("  [FAIL] %s %s row %d: %-24s workbook=%-18s export=%s\n",
                        basename(d), s, i, cn, a, b))
        }
      }
    }
  }
  ## ---- APERTURE 2: the audit copies, cell for cell ------------------------------------------
  nAudit <- 0L; nAuditBad <- 0L; notes <- character(0)
  for (s in c("Info", "Layout_Plate", "gating_template")) {
    r <- auditIdentity(wb, xp, s, scrubbed, basename(d))
    nAudit <- nAudit + r$n; nAuditBad <- nAuditBad + r$bad
    if (!is.null(r$note)) notes <- c(notes, sprintf("%s: %s", s, r$note))
    if (r$bad > 0L) ok <- FALSE
  }

  ## ---- APERTURE 4: the dashboard's published rows --------------------------------------------
  ## `const DATA = [...]` is JSON, so this needs no HTML parsing. Only the fields that come
  ## straight from the workbook are compared; everything else there is derived.
  nDash <- 0L; nDashBad <- 0L
  htm <- list.files(c(d, file.path(d, "Outputs")), pattern = "[.]html$", full.names = TRUE)
  if (length(htm) && requireNamespace("jsonlite", quietly = TRUE)) {
    txt <- paste(readLines(htm[[1]], warn = FALSE), collapse = "\n")
    m   <- regmatches(txt, regexpr("const DATA = \\[.*?\\];", txt))
    rows <- if (length(m))
      tryCatch(jsonlite::fromJSON(sub(";$", "", sub("const DATA = ", "", m)),
                                  simplifyVector = FALSE), error = function(e) NULL) else NULL
    if (!is.null(rows)) for (r in rows) {
      for (fld in c("eln_id", "operator")) {
        lpCol <- c(eln_id = "ELN_ID", operator = "Operator")[[fld]]
        if (!lpCol %in% names(lp) || is.null(r[[fld]])) next
        if (scrubbed && lpCol %in% SCRUBBED) next
        want <- unique(norm(lp[[lpCol]])); want <- want[!is.na(want)]
        nDash <- nDash + 1L
        if (!any(vapply(want, function(w) same_cell(w, norm(as.character(r[[fld]]))), TRUE))) {
          nDashBad <- nDashBad + 1L; ok <- FALSE
          cat(sprintf("  [FAIL] %s dashboard %s: published %s, workbook has %s\n",
                      basename(d), fld,
                      if (scrubbed) "(withheld)" else as.character(r[[fld]]),
                      if (scrubbed) "(withheld)" else paste(want, collapse = "/")))
        }
      }
    }
  }

  ## A run that compared nothing has PROVED nothing. Say so and fail.
  if (ncmp == 0L) { ok <- FALSE
    cat(sprintf("  [FAIL] %s: 0 cells compared -- %d export row(s) matched no unique layout row\n",
                basename(d), nskip)) }
  ## The audit pass is the wide one, so it gets the same treatment: zero audit cells over a run
  ## that HAS an export means the audit sheets went missing, which is itself the finding.
  if (nAudit == 0L) { ok <- FALSE
    cat(sprintf("  [FAIL] %s: 0 audit cells compared -- %s\n", basename(d),
                if (length(notes)) paste(notes, collapse = "; ") else "no audit sheets found")) }
  cat(sprintf("%-16s %-18s %5d keyed + %5d audit + %2d dash cell(s), %3d well-match(es), %d differ%s\n",
              basename(d), if (scrubbed) "(scrubbed)" else "(unscrubbed)",
              ncmp, nAudit, nDash, nwell, nbad + nAuditBad + nDashBad,
              if ((nbad + nAuditBad + nDashBad) == 0 && ncmp > 0 && nAudit > 0) "  OK" else "  ** FAIL **"))
  if (length(notes)) for (nt in notes) cat(sprintf("                   note: %s\n", nt))
}
cat(if (ok) "\nMETADATA IDENTITY OK\n" else "\n** METADATA IDENTITY FAILED **\n")
quit(status = if (ok) 0L else 1L)
