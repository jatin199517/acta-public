## ===========================================================================
## ACTA dashboard generator  ·  standalone
## ---------------------------------------------------------------------------
## Builds a self-contained HTML dashboard from ACTA titration exports.
##
## Run:  Rscript ACTA_Dashboard_2_86.R          (or source() it in RStudio)
##
## DELIBERATELY INDEPENDENT of ACTA_Script: the analysis never calls this, and
## this never sources ACTA_Functions.R. It is meant to be shareable on its own,
## so everything it needs is either in this file or in the exports it reads --
## no Master Panel Inventory, no lookup workbook, no network. The only inputs
## are the layout workbook's `Info` sheet (settings), an HTML template, and
## whatever ACTA has already written to disk.
##
## Settings come from Info. Headers may carry a "(L)"-style annotation, which is
## stripped, so `dashboard_dir (L)` and `dashboard_dir` both work:
##   dashboard_dir         where the .html is written. Blank -> this folder.
##   titration_export_dir  the folder whose IMMEDIATE SUBFOLDERS are the runs.
##                         Blank -> the ACTA root (this folder's parent).
##
## Each immediate subfolder of titration_export_dir is one run, scanned for a
## *TitrationExport*.xlsx, a report .pdf, and figures. Anything whose path
## contains "archive" (case-insensitive, at any depth) is ignored.
##
## Exports are combined with a FLEXIBLE row-bind: the dashboard carries the UNION
## of all columns ever seen, and a run whose export predates a column shows NA for
## it. That is what lets an older experiment sit beside a newer one without either
## being re-run.
## ===========================================================================

suppressWarnings(suppressMessages({
  library(readxl); library(dplyr); library(jsonlite)
}))

## --------------------------------------------------------------- where am I --
acta_dash_dir <- tryCatch(dirname(normalizePath(this.path::this.path())),
                          error = function(e) getwd())
setwd(acta_dash_dir)

## ------------------------------------------------------------------ helpers --
## Trailing "(...)" on a layout header is documentation for whoever fills the
## sheet in; strip it once so the code can refer to plain names. Any punctuation
## around the annotation goes too, because a header that has been through
## make.names() reads "miflowcyt_export.(L)" rather than "miflowcyt_export (L)".
stripAnn <- function(x) trimws(sub("[^A-Za-z0-9]*\\([^()]*\\)[^A-Za-z0-9]*$", "", x))

## Normalised comparison key: letters and digits only. Survives every spelling of a
## header that readxl, openxlsx or make.names might hand back.
normKey <- function(x) gsub("[^a-z0-9]", "", tolower(x))

## Every Info setting is optional, so a missing column returns NA rather than
## erroring. Exact normalised match first; failing that, a UNIQUE prefix match, which
## catches a header whose annotation survived as bare text ("miflowcyt_export..L.")
## because make.names() turned the parentheses into dots and left nothing to strip.
infoVal <- function(info, name) {
  if (is.null(info) || !nrow(info)) return(NA_character_)
  keys <- normKey(stripAnn(names(info)))
  want <- normKey(name)
  hit  <- match(want, keys)
  if (is.na(hit)) {
    cand <- which(startsWith(keys, want))
    hit  <- if (length(cand) == 1L) cand else NA_integer_
  }
  if (is.na(hit)) return(NA_character_)
  v <- info[[hit]][1]
  if (is.null(v) || length(v) == 0 || is.na(v)) return(NA_character_)
  v <- trimws(as.character(v))
  if (!nzchar(v) || toupper(v) == "NA") NA_character_ else v
}

## Excel round-trips a logical column badly -- one text cell turns the whole
## column to character -- so accept every spelling an analyst may produce.
isYes <- function(x) !is.na(x) && toupper(trimws(x)) %in% c("TRUE", "T", "YES", "Y", "1")

## UNIVERSAL exclusion, applied to the whole path at any depth, so an archived run
## nested inside a live folder is skipped too.
isArchived <- function(p) grepl("archive", p, ignore.case = TRUE)

## Path of `path` relative to `base`. Written out rather than taken from a package
## because the dashboard's links must be relative for the file to stay portable,
## and this keeps the script dependency-light.
relTo <- function(path, base) {
  p <- strsplit(normalizePath(path, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  b <- strsplit(normalizePath(base, winslash = "/", mustWork = FALSE), "/", fixed = TRUE)[[1]]
  i <- 0L
  while (i < min(length(p), length(b)) && p[i + 1L] == b[i + 1L]) i <- i + 1L
  up   <- rep("..", max(0L, length(b) - i))
  down <- if (i < length(p)) p[(i + 1L):length(p)] else character(0)
  paste(c(up, down), collapse = "/")
}

## First non-empty value, as a length-1 character. Exports carry NA freely.
one <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- trimws(as.character(x[!is.na(x)]))
  ## The literal string "NA" counts as empty: as.character() on an NA column produces it, and an
  ## older export can carry it as a real cell value. Treating it as data puts "NA" in the dashboard
  ## and, worse, uses it as a figure-matching key that matches nothing.
  x <- x[nzchar(x) & toupper(x) != "NA"]
  if (!length(x)) return("")
  x[1]
}

## First non-empty of several columns, for fields whose source name changed across versions.
firstOf <- function(d, ...) {
  for (nm in c(...)) { v <- one(if (nm %in% names(d)) d[[nm]] else NULL); if (nzchar(v)) return(v) }
  ""
}

## ------------------------------------------------------- settings from Info --
## ACTA_LAYOUT_DIR lets the workbook live somewhere other than beside this script -- needed when a
## diagnostic case supplies only its inputs and is driven by the working version's generator. The
## TEMPLATE still resolves beside this file, which is why an OQ folder needs no Template/ of its own.
acta_layout_dir <- { e <- Sys.getenv("ACTA_LAYOUT_DIR", unset = "")
                     if (nzchar(e)) normalizePath(e, mustWork = TRUE) else acta_dash_dir }
layoutFile <- grep("Titration_Instructions",
                   list.files(acta_layout_dir, pattern = "\\.xlsx$", full.names = TRUE), value = TRUE)
if (length(layoutFile) != 1)
  ## Same rename hint as the script: name the old spelling when it is what is actually present.
  stop(sprintf(paste0("ACTA dashboard: expected exactly one *Titration_Instructions*.xlsx in '%s'; ",
                      "found %d%s."), acta_layout_dir, length(layoutFile),
               if (length(layoutFile)) paste0(" (", paste(layoutFile, collapse = ", "), ")") else ""),
       call. = FALSE)
if (!"Info" %in% excel_sheets(layoutFile))
  stop(sprintf("ACTA dashboard: '%s' has no `Info` sheet; that is where the settings live.",
               layoutFile), call. = FALSE)
info <- suppressMessages(read_excel(layoutFile, sheet = "Info"))

## `dashboard_creation` was REMOVED from the instructions file in 2_87. It duplicated the app's own
## "generate dashboard" checkbox and sat in series with it, so a workbook cell could silently veto an
## explicit run-time choice -- and the app never read or displayed the flag, so there was nothing to
## explain the silence. The script now always builds when it is run, which is what "run the dashboard
## generator" should mean.
##
## The braces below are retained deliberately: they keep the 426-line body at its existing
## indentation. A top-level `{` block does not create a scope in R, so behaviour is identical, and
## dedenting instead would risk silently editing any multi-line string literal in there.
{

  ## ACTA_DASHBOARD_DIR overrides Info, the same override pattern ACTA_WORK_DIR uses in the script
  ## and the .Rmd. It is how the app makes its own path field authoritative for a session without
  ## writing to the workbook -- which we deliberately do not do, because it is an Excel file on
  ## OneDrive and a lock or sync conflict would fail or silently lose the save.
  outDir <- { e <- Sys.getenv("ACTA_DASHBOARD_DIR", unset = "")
              if (nzchar(e)) e else infoVal(info, "dashboard_dir") }
  outDir <- if (is.na(outDir)) acta_dash_dir else normalizePath(outDir, mustWork = FALSE)
  if (!dir.exists(outDir))
    stop(sprintf("ACTA dashboard: Info!dashboard_dir '%s' does not exist.", outDir), call. = FALSE)

  ## Two modes, and the blank case is deliberately the NARROW one:
  ##   set   -> that folder's IMMEDIATE SUBFOLDERS are the runs (the cumulative view)
  ##   blank -> THIS folder is the run, and only its own export is read
  ## Defaulting blank to the parent would silently pull in every sibling version folder, which is a
  ## very different dashboard from the one someone gets by just running the script.
  ## ACTA_EXPORT_DIR overrides Info -- see the note on ACTA_DASHBOARD_DIR above.
  scanRaw <- { e <- Sys.getenv("ACTA_EXPORT_DIR", unset = "")
            if (nzchar(e)) e else infoVal(info, "titration_export_dir") }
  wdMode  <- is.na(scanRaw)
  scanDir <- if (wdMode) acta_dash_dir else normalizePath(scanRaw, mustWork = FALSE)
  if (!dir.exists(scanDir))
    stop(sprintf("ACTA dashboard: Info!titration_export_dir '%s' does not exist.", scanDir),
         call. = FALSE)

  ## ------------------------------------------------------------- the template --
  ## Matched by PATTERN, not by a fixed name, so the HTML can be swapped for another
  ## as long as it keeps the two placeholders. Zero or several is fatal rather than
  ## a guess: silently picking one would make the output depend on file ordering.
  tplDir <- file.path(acta_dash_dir, "Template")
  tpl <- list.files(tplDir, pattern = "dashboard_template", ignore.case = TRUE, full.names = TRUE)
  tpl <- tpl[grepl("\\.html?$", tpl, ignore.case = TRUE) & !isArchived(tpl)]
  if (length(tpl) != 1)
    stop(sprintf(paste0("ACTA dashboard: expected exactly ONE *dashboard_template*.html in '%s'; ",
                        "found %d%s. Keep the one you want and move the rest out (or into an ",
                        "'Archive' subfolder, which is ignored)."),
                 tplDir, length(tpl),
                 if (length(tpl)) paste0(":\n  ", paste(basename(tpl), collapse = "\n  ")) else ""),
         call. = FALSE)
  tplTxt <- paste(readLines(tpl, warn = FALSE), collapse = "\n")

  ## ------------------------------------------------- pre-flight: the template --
  ## Checked BEFORE any scanning, so a broken template fails in a second rather than
  ## after minutes of file walking -- and fails with something actionable instead of a
  ## dashboard that opens to a blank table.
  ##
  ## The last check is the useful one: rather than hard-coding which element ids the
  ## markup must carry (which would break every time the template is redesigned), it
  ## asks the template to be consistent WITH ITSELF -- every id the script reaches for
  ## must exist in the markup. That survives a redesign but still catches a renamed or
  ## deleted element, which is the failure that produces an empty page.
  validateTemplate <- function(txt, path) {
    bad <- character(0)
    nm  <- basename(path)
    cnt <- function(pat, fixed = TRUE) {
      m <- gregexpr(pat, txt, fixed = fixed, ignore.case = !fixed)
      if (m[[1]][1] == -1) 0L else length(m[[1]])
    }
    if (!nzchar(trimws(txt))) return(sprintf("  %s: file is empty", nm))
    if (cnt("<html", FALSE) == 0 || cnt("</html>", FALSE) == 0)
      bad <- c(bad, "no <html> ... </html> skeleton -- is this an HTML file?")
    nOpen <- cnt("<script", FALSE); nClose <- cnt("</script>", FALSE)
    if (nOpen == 0)            bad <- c(bad, "no <script> block, so nothing would ever render")
    else if (nOpen != nClose)  bad <- c(bad, sprintf("<script> tags unbalanced: %d opening, %d closing",
                                                     nOpen, nClose))
    ## META and DATA are the contract. COLS/GROUPABLE are optional -- a template may fix
    ## its own columns -- but a duplicate of ANY of them is fatal, because substitution
    ## uses sub() (first match only) and a second mention would be left unreplaced.
    req <- c(META = "__META_JSON__", DATA = "__DATA_JSON__")
    opt <- c(COLS = "__COLS_JSON__", GROUPABLE = "__GROUPABLE_JSON__")
    for (v in names(c(req, opt))) {
      ph <- c(req, opt)[[v]]
      k  <- cnt(ph)
      if (k == 0 && v %in% names(req))
        bad <- c(bad, sprintf("%s is missing; the generator has nowhere to inject %s", ph, v))
      else if (k > 1)
        bad <- c(bad, sprintf("%s appears %d times; it must appear exactly once (sub() replaces only the first)",
                              ph, k))
      else if (k == 1 &&
               !grepl(sprintf("(const|let|var)[[:space:]]+%s[[:space:]]*=[[:space:]]*%s", v, ph), txt))
        bad <- c(bad, sprintf("%s is present but not assigned to a variable -- expected `const %s = %s;`",
                              ph, v, ph))
    }
    ## Self-consistency: every id the script selects must exist in the markup.
    sel <- unique(c(
      vapply(regmatches(txt, gregexpr('\\$\\("#[A-Za-z0-9_-]+"\\)', txt))[[1]],
             function(z) sub('^\\$\\("#', "", sub('"\\)$', "", z)), "", USE.NAMES = FALSE),
      vapply(regmatches(txt, gregexpr('getElementById\\("[A-Za-z0-9_-]+"\\)', txt))[[1]],
             function(z) sub('^getElementById\\("', "", sub('"\\)$', "", z)), "", USE.NAMES = FALSE)))
    ids <- unique(vapply(regmatches(txt, gregexpr('id="[A-Za-z0-9_-]+"', txt))[[1]],
                         function(z) sub('^id="', "", sub('"$', "", z)), "", USE.NAMES = FALSE))
    miss <- setdiff(sel, ids)
    if (length(miss))
      bad <- c(bad, sprintf("script targets #%s, but no element carries %s id",
                            paste(miss, collapse = ", #"),
                            if (length(miss) > 1) "those" else "that"))
    if (length(bad)) paste0("  ", nm, ": ", bad) else character(0)
  }

  tplProblems <- validateTemplate(tplTxt, tpl)
  if (length(tplProblems))
    stop(sprintf("ACTA dashboard: template pre-flight failed -- nothing was scanned. %d problem(s):\n%s",
                 length(tplProblems), paste(tplProblems, collapse = "\n")), call. = FALSE)
  message(sprintf("[ACTA dashboard] template OK: %s", basename(tpl)))

  ## ------------------------------------------------------------- scan the runs --
  ## The scan folder ITSELF counts as a run when an export sits directly in it, not only its
  ## subfolders. wdMode is true only when no scan folder was given at all, and the app always gives
  ## one -- so this used to be subfolders-only. Copying an export straight into the dashboard folder
  ## (what the app's "Copy destination" does when it points at the scan folder) therefore produced a
  ## dashboard that silently ignored the new file and indexed the older subfolder runs instead: the
  ## titer entered a minute earlier was simply absent. actaScanExports() has always counted a
  ## root-level export ("loose in the root"), so the pre-flight reported it as found -- the same
  ## pre-flight/generator divergence as the Outputs/ one, in the other direction.
  runs <- if (wdMode) scanDir else {
    subs <- list.dirs(scanDir, recursive = FALSE, full.names = TRUE)
    selfExp <- list.files(scanDir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE,
                          ignore.case = TRUE)
    if (length(selfExp[!isArchived(selfExp)])) c(scanDir, subs) else subs
  }
  runs <- runs[!isArchived(runs)]
  ## An EMPTY scan folder is a legitimate state, not an error: it is what you have before the first
  ## run is copied in. Stopping here refused to write anything, so the app could not show the
  ## operator a dashboard at all until a run existed. Warn and carry on -- the file still gets
  ## written, just with no rows.
  if (!length(runs))
    warning(sprintf(paste0("ACTA dashboard: no export in '%s' and no subfolders under it -- ",
                           "writing an empty dashboard."), scanDir), call. = FALSE)

  ## The export and the report are taken from the run's OWN folder plus ACTA_OUTPUT_SUBDIR, never
  ## recursively: ACTA writes both at the top level, the OQ runner collects them into Outputs/, and
  ## recursing further would sweep in exports from nested experiment folders that belong to a
  ## different run. Figures are the exception -- they live in Plots/.
  ## Defined here rather than imported: this script deliberately never sources ACTA_Functions.R
  ## (see the header -- it has to be shareable on its own). ACTA_Functions.R declares the same
  ## constant for the app's pre-flight; the two MUST stay in sync, because when they disagree the
  ## pre-flight passes and the dashboard comes out empty.
  OUTPUT_SUBDIR <- "Outputs"
  pick <- function(root, pattern, recursive) {
    roots <- if (recursive) root else c(root, file.path(root, OUTPUT_SUBDIR))
    f <- list.files(roots, pattern = pattern, recursive = recursive, full.names = TRUE,
                    ignore.case = TRUE)
    f[!isArchived(f)]
  }

  runInfo <- lapply(runs, function(r) list(
    name   = basename(r),
    root   = r,
    export = pick(r, "TitrationExport.*\\.xlsx$", FALSE),
    report = pick(r, "\\.pdf$",                    FALSE),
    figs   = pick(r, "\\.png$",                    TRUE)
  ))
  names(runInfo) <- vapply(runInfo, `[[`, "", "name")
  runInfo <- runInfo[vapply(runInfo, function(x) length(x$export) > 0, logical(1))]
  ## Likewise: nothing to index is not a failure. EMPTY_RUN short-circuits the read/bind/derive
  ## chain below, which all assumes at least one export, and jumps to writing the template with an
  ## empty DATA array. The template's own JS already handles a zero-row table ("No rows match.").
  EMPTY_RUN <- !length(runInfo)
  if (EMPTY_RUN)
    warning(sprintf(paste0("ACTA dashboard: no *TitrationExport*.xlsx found in %s -- writing an ",
                           "empty dashboard. Set Info!titration_export_dir (or the app's scan ",
                           "folder) to the folder whose subfolders hold the runs if you meant to ",
                           "pool several."),
                    if (wdMode) sprintf("'%s'", scanDir)
                    else sprintf("any of the %d subfolder(s) of '%s'", length(runs), scanDir)),
            call. = FALSE)

  ## Everything from here to the write assumes at least one export: it reads them, binds the
  ## union of their columns, derives the schema from the newest, and builds one record per row.
  ## With nothing scanned there is no schema to derive, which is why this used to stop. Skip the
  ## chain and fall through to the write with empty structures instead.
  if (!EMPTY_RUN) {
  ## -------------------------------------------------------- read + bind exports --
  ## bind_rows, not rbind: it takes the UNION of columns and fills the gaps with NA,
  ## so an export written before a column existed still lines up. rbind() would
  ## refuse outright, which would make every schema change retroactively break the
  ## whole dashboard.
  readExport <- function(path, runName) {
    d <- tryCatch(suppressMessages(read_excel(path, sheet = 1)),
                  error = function(e) NULL)
    if (is.null(d) || !nrow(d)) return(NULL)
    names(d) <- make.unique(stripAnn(names(d)))
    d <- mutate(d, across(everything(), as.character))   # one type per column across runs
    d$.run  <- runName
    d$.file <- path
    d
  }
  allRows <- bind_rows(lapply(names(runInfo), function(nm)
    bind_rows(lapply(runInfo[[nm]]$export, readExport, runName = nm))))
  if (is.null(allRows) || !nrow(allRows))
    stop("ACTA dashboard: every export found was empty or unreadable.", call. = FALSE)

  ## ------------------------------------------------------------- build records --
  ## COLUMNS ARE DERIVED FROM THE EXPORTS, not fixed in the template. Consequences, all intended:
  ##   * a column added to a future export shows up with no edit here or in the HTML;
  ##   * a column that exists in no export is never rendered, so nothing sits permanently blank;
  ##   * ordering follows the NEWEST export, with columns only older runs had appended after it.
  ## bind_rows already matched on NAME, so re-ordering columns in the export changes nothing about
  ## the pooled data -- only the display order, which is taken from the newest file on purpose.
  ##
  ## Per-well and internal columns are hidden: the export carries one row per antibody but keeps the
  ## winning well's raw statistics, which are meaningless as dashboard columns.
  ## "stainvolume" was left behind by the StainQty rename -- it matched nothing, so the per-well
  ## quantity silently became visible. Kept visible deliberately now, with StainUnit beside it, since
  ## the unit is meaningless without the quantity it qualifies.
  EXPORT_HIDE <- c("name", "pop", "percent", "staintype", "row", "column",
                   "plateid", "filename", "robustsd", "pos", "neg", "si", "maxsat",
                   "maxsat_stainqty", "fix", "perm", "eln_name", "alias", "dirname", "channel",
                   "gatingmethod", "label", "mefi", "mefi_channel", "count", "run", "file")
  ## Friendly labels for the columns we know about; anything unrecognised falls back to its own
  ## name with underscores opened up, so a new export column is readable without being listed here.
  ## The layout workbook's Info/Layout_Plate sheets name these ELN_Name/ELN_ID -- the ELN is not
  ## assumed to be Benchling, so the dashboard labels them generically.
  LABELS <- c(cat_num = "Catalog #", lot_num = "Lot", eln_id = "ELN ID", eln_name = "ELN name",
              test_material = "Titration material", e6_cells_per_well = "cells/well (e6)",
              scriptversion = "Script Ver", titration_status = "Titer status",
              qc_status = "Overall QC status", fl = "Fluorophore", markername = "Marker",
              acquisition_date = "Acquired", maxsi = "Max SI", maxsi_stainqty = "Max SI vol",
              ## Michaelis-Menten model fit. Spelled out rather than left as "LI ...": the abbreviation
              ## means nothing to a reader who did not write the pipeline, and these four are the
              ## columns most likely to be quoted at someone.
              mm_r2 = "Michaelis-Menten R\u00b2", mm_vmax = "Michaelis-Menten Vmax",
              mm_kd = "Michaelis-Menten Km", mm_90pct_vmax = "Michaelis-Menten 90% Vmax",
              ## Carries the micro sign, so its header opts out of uppercasing (see UNIT_COLS).
              well_volume_ul = "Well volume (\u00b5L)",
              ## One per QC check. Labelled from the check id, which is the stable key the export
              ## column is built from -- the prose statement is reworded freely, the id is not.
              qc_cells_fsc = "QC Cells FSC", qc_live = "QC Live",
              qc_single_cells = "QC Singlets", qc_costain_lowest = "QC Costain lowest",
              qc_min_events = "QC Min events")
  ## Headers that must keep their own case. text-transform:uppercase turns U+00B5 MICRO SIGN into
  ## U+039C GREEK CAPITAL MU, so "(uL)" renders as "(ML)" -- a 1000x error in a volume header.
  UNIT_COLS <- c("well_volume_ul")
  ## Columns rendered in the monospace/tabular face: identifiers, dates and figures, where a
  ## fixed advance makes them scannable down the column. Presentational only -- templates that
  ## define no `.mono` rule simply ignore the class. Carried here rather than in a template so
  ## every template gets it from the one schema.
  MONO <- c("cat_num", "lot_num", "clone", "expiration", "eln_id", "titer", "e6_cells_per_well",
            "scriptversion", "acquisition_date", "maxsi", "maxsi_stainqty", "costain_size",
            "mm_r2", "mm_vmax", "mm_kd", "mm_90pct_vmax", "well_volume_ul")
  keyOf   <- function(x) gsub("[^a-z0-9]+", "_", tolower(trimws(x)))
  labelOf <- function(x) { k <- keyOf(x); if (k %in% names(LABELS)) LABELS[[k]] else gsub("_", " ", x) }

  ## "Newest" = most recently written export. Simple, and it tracks the run that actually defines
  ## the current schema better than a version string would (versions are not always monotonic).
  newest    <- names(sort(vapply(runInfo, function(x) max(file.mtime(x$export)), 0), decreasing = TRUE))[1]
  newestCol <- names(readExport(runInfo[[newest]]$export[1], newest))
  allCol    <- names(allRows)
  ordered   <- c(newestCol, setdiff(allCol, newestCol))
  dispCol   <- ordered[!keyOf(ordered) %in% EXPORT_HIDE & !startsWith(ordered, ".")]

  ## Derived columns the exports cannot carry: the run grouping and the file links.
  ## Concatenation first, matching the report: the raw separation before the number derived from it.
  FIGCOLS <- list(list(k = "concat", label = "Concatenation"),
                  list(k = "si",     label = "Stain Index"),
                  list(k = "sat",    label = "Saturation"),
                  list(k = "plots",  label = "Flow plots"),
                  list(k = "report", label = "Report"),
                  list(k = "export", label = "Export"))
  markerKey <- keyOf(if ("Markername" %in% dispCol) "Markername" else
                     if ("Alias" %in% dispCol) "Alias" else "project")

  ## Info!dashboard_tabbed_by names the export column the dashboard groups its tabs by; blank means
  ## ScriptVersion. The template tabs on `project`, so that field carries the chosen value rather
  ## than the folder name -- which is why the tabs previously read as versions: `project` WAS the
  ## version folder. The folder itself is still shown, in its own column.
  ## Renamed from `tabbed_by` on 2026-08-19: the Info sheet is read by three different consumers and
  ## a bare "tabbed_by" gave no clue that it steers the DASHBOARD. Reads the new name and still
  ## accepts the old one, so an instructions workbook written before the rename keeps working.
  tabWant <- infoVal(info, "dashboard_tabbed_by")
  if (is.na(tabWant) || !nzchar(tabWant)) tabWant <- infoVal(info, "tabbed_by")
  if (is.na(tabWant)) tabWant <- "ScriptVersion"
  tabCol <- names(allRows)[match(keyOf(tabWant), keyOf(names(allRows)))]
  if (is.na(tabCol)) {
    warning(sprintf(paste0("ACTA dashboard: Info!dashboard_tabbed_by names '%s', which is not a column in any ",
                           "export; tabbing by run folder instead. Available: %s"),
                    tabWant, paste(setdiff(names(allRows), c(".run", ".file")), collapse = ", ")),
            call. = FALSE)
    tabLabel <- "Run"
  } else {
    tabLabel <- labelOf(tabCol)
  }

  cols <- c(
    list(list(k = "project", label = tabLabel, chip = "proj", filter = "select", sticky = 1)),
    lapply(dispCol, function(cn) {
      k <- keyOf(cn)
      d <- list(k = k, label = labelOf(cn), filter = "text")
      if (k == markerKey)          { d$cls <- "key"; d$sticky <- 2 }
      if (k %in% MONO && is.null(d$cls)) d$cls <- "mono"   ## never overrides the marker's "key"
      if (k == "notes")              d$cls   <- "notes"
      ## Panel lists every channel in the acquisition -- 31 of them on the ZE5 -- and td defaults to
      ## white-space:nowrap, so it rendered as one cell wide enough to push every later column off
      ## screen. Wraps within a bounded width instead.
      if (k == "panel")              d$cls   <- "panel"
      ## Every QC_* column, not just the roll-up: they all hold the same PASS/FAIL/SKIPPED
      ## vocabulary, so they all get the status chip. startsWith covers checks added later
      ## without another edit here -- actaQCColumns() already generates the columns that way.
      if (k == "qc_status" || startsWith(k, "qc_")) d$status <- TRUE
      if (k == "titration_status")   d$tstat  <- TRUE
      if (k %in% UNIT_COLS)          d$unit   <- TRUE
      if (k %in% c("vendor", "operator", "equipment", "scriptversion", "fix_perm", "qc_status",
                   "titration_status", "titer", "panel") || startsWith(k, "qc_"))
        d$filter <- "select"   ## three possible values -- a text box would be the wrong control
      d
    }),
    lapply(FIGCOLS, function(f) list(k = f$k, label = f$label, figs = f$k, filter = "none")),
    list(list(k = "parent", label = "Parent pop", filter = "select"),
         list(k = "folder", label = "Folder", folder = TRUE, filter = "none")))

  groupable <- c("project", "parent",
                 intersect(vapply(cols, `[[`, "", "k"),
                           c(markerKey, "fl", "eln_id", "panel", "vendor", "fix_perm", "qc_status")))
  groupable <- setNames(as.list(rep(1L, length(unique(groupable)))), unique(groupable))

  ## Figures are matched to a row by the run it came from plus that row's Dirname, which is exactly
  ## how ACTA names them:
  ##   SIPlot_<BID>_parent_<pop>_.png / SatPlot_<BID>_parent_<pop>_.png   (per run)
  ##   <Dirname>_<BID>_parent_<pop>_{histogram,pseudo,layout_<alias>}.png (per marker)
  col <- function(d, nm) if (nm %in% names(d)) d[[nm]] else rep(NA_character_, nrow(d))
  parentOf <- function(paths) {
    m <- regmatches(basename(paths), regexpr("parent_(.+?)_", basename(paths)))
    if (!length(m)) return("")
    sub("^parent_", "", sub("_$", "", m[1]))
  }

  records <- lapply(seq_len(nrow(allRows)), function(i) {
    r    <- allRows[i, ]
    ri   <- runInfo[[ r$.run ]]
    figs <- ri$figs
    bn   <- basename(figs)
    si   <- figs[grepl("^SIPlot",  bn, ignore.case = TRUE)]
    sat  <- figs[grepl("^SatPlot", bn, ignore.case = TRUE)]
    ## ConcatPlot is RUN-level, like SIPlot and SatPlot -- one figure per run, not one per antibody.
    ## Without this it fell into the per-antibody branch, then failed that branch's group-prefix
    ## match (its name starts "ConcatPlot_", not "CD19_") and was silently dropped: generated,
    ## exported, and invisible in the dashboard.
    conc <- figs[grepl("^ConcatPlot", bn, ignore.case = TRUE)]
    bid   <- one(col(r, "ELN_ID"))
    byBid <- function(x) { if (!nzchar(bid) || !length(x)) return(x)
                           k <- x[grepl(bid, basename(x), fixed = TRUE)]; if (length(k)) k else x }
    si <- byBid(si); sat <- byBid(sat); conc <- byBid(conc)
    perAb <- !(grepl("^SIPlot|^SatPlot|^ConcatPlot", bn, ignore.case = TRUE))
    tokOf <- function(x) vapply(regmatches(x, regexec("^(.+)_([A-Za-z0-9]+)_parent_", x)),
                                function(z) if (length(z) >= 2) z[2] else NA_character_, "")
    toks <- unique(na.omit(tokOf(bn[perAb])))
    key  <- firstOf(r, "Dirname", "Alias")
    if (!nzchar(key) && length(toks) == 1L) key <- toks[1]
    mine <- if (nzchar(key)) figs[perAb & startsWith(toupper(bn), toupper(paste0(key, "_")))]
            else character(0)
    rel  <- function(x) if (length(x)) vapply(x, relTo, "", base = outDir, USE.NAMES = FALSE) else character(0)

    ## Every displayed export column, under its normalised key.
    rec <- setNames(lapply(dispCol, function(cn) one(col(r, cn))), keyOf(dispCol))
    ## A pre-2_85 export has no Markername/Alias/Dirname (they were written as the string "NA"), so
    ## the marker cell would be blank -- and that is the column the table sorts on.
    if (markerKey %in% names(rec) && !nzchar(rec[[markerKey]])) rec[[markerKey]] <- key
    ## `project` is the TAB value (Info!dashboard_tabbed_by), falling back to the run folder when that column
    ## is absent or empty for this row -- an empty tab label would otherwise collapse rows into a
    ## nameless tab.
    tabVal <- if (!is.na(tabCol)) one(col(r, tabCol)) else ""
    ## The template's isOK() reads r.status, while the DISPLAYED column is keyed on the export's own
    ## name (qc_status). Mirroring the value into `status` keeps the "needs attention" count honest
    ## without the template having to know which export column carries QC. It is not in COLS, so it
    ## renders nowhere -- it exists purely for that predicate.
    c(rec, list(
      status  = if ("qc_status" %in% names(rec)) rec[["qc_status"]] else "",
      project = if (nzchar(tabVal)) tabVal else r$.run,
      ## I() keeps these AsIs so jsonlite never unboxes a single path into a bare string.
      concat = I(rel(conc)), si = I(rel(si)), sat = I(rel(sat)), plots = I(rel(mine)),
      report = I(rel(ri$report)), export = I(rel(ri$export)),
      parent = parentOf(c(mine, si)), folder = relTo(ri$root, outDir)))
  })

  } else {
    ## Empty but VALID inputs for the write below. cols = list() means the template renders no
    ## headers and its own "No rows match." branch covers the body, so the output is the template
    ## with nothing in it -- which is exactly what an empty scan folder should produce.
    records   <- list()
    cols      <- list()
    groupable <- setNames(list(), character(0))
    markerKey <- "markername"
    tabCol    <- NA_character_
    newest    <- NA_character_
  }
  ## ------------------------------------------------------------------- write it --
  meta <- list(
    generated = format(Sys.time(), "%Y-%m-%d %H:%M"),
    root      = scanDir,
    n_entries = length(unique(vapply(records, function(x) one(x[["eln_id"]]), ""))),
    n_runs    = length(runInfo),
    sort_key  = markerKey,
    ## Default grouping in the "All data" tab. ELN ID is the useful default -- one experiment per
    ## group -- falling back to the tab field when the exports carry no ELN_ID column.
    group_key = if ("eln_id" %in% vapply(cols, `[[`, "", "k")) "eln_id" else "project",
    tabbed_by = if (is.na(tabCol)) "run folder" else tabCol,
    schema_from = newest,
    source    = "ACTA titration exports"
  )
  txt <- tplTxt
  for (ph in c("__META_JSON__", "__DATA_JSON__", "__COLS_JSON__", "__GROUPABLE_JSON__")) {
    val <- switch(ph,
      "__META_JSON__"      = jsonlite::toJSON(meta,      auto_unbox = TRUE, null = "null"),
      "__DATA_JSON__"      = jsonlite::toJSON(records,   auto_unbox = TRUE, null = "null"),
      "__COLS_JSON__"      = jsonlite::toJSON(cols,      auto_unbox = TRUE, null = "null"),
      "__GROUPABLE_JSON__" = jsonlite::toJSON(groupable, auto_unbox = TRUE, null = "null"))
    ## `</script>` CANNOT SURVIVE INTO THE PAGE. Security review of 2_94, finding L-1:
    ## jsonlite::toJSON() escapes what JSON requires but leaves `<` and `>` alone, and this payload
    ## is substituted INSIDE a <script> block. A marker or sample name containing the literal text
    ## `</script>` would close that block early and whatever followed would be parsed as markup.
    ## Marker and sample names come off shared metadata sheets, so this is not purely self-inflicted
    ## even though the dashboard is a local file opened by its own author.
    ##
    ## Escaping the SLASH keeps the JSON byte-identical in meaning -- `<\/script>` and `</script>`
    ## are the same string to any JSON parser -- so nothing downstream needs to know. Done here
    ## rather than by asking toJSON for HTML escaping, because that would also rewrite legitimate
    ## `<`/`>` inside displayed values into \u003c and change what the dashboard shows.
    val <- gsub("</", "<\\/", as.character(val), fixed = TRUE)
    ## Only substitute placeholders the template actually uses, so an older template that fixes its
    ## own COLS still works -- it simply ignores the injected ones.
    if (grepl(ph, txt, fixed = TRUE)) txt <- sub(ph, val, txt, fixed = TRUE)
  }
  left <- grep("__(META|DATA|COLS|GROUPABLE)_JSON__", strsplit(txt, "\n")[[1]], value = TRUE)
  if (length(left))
    stop(sprintf("ACTA dashboard: placeholder(s) left unsubstituted in the output:\n  %s",
                 paste(trimws(left), collapse = "\n  ")), call. = FALSE)

  outFile <- file.path(outDir, "ACTA_Titration_Dashboard.html")
  writeLines(txt, outFile, useBytes = TRUE)
  ## Two shapes of summary. The detailed one names the schema and the pooled/hidden column split,
  ## which only exist when something was scanned -- allRows, ordered and dispCol are all products of
  ## the derive chain and are undefined on the empty path. Reporting "0 rows" there is the whole
  ## message worth printing anyway.
  if (EMPTY_RUN) {
    message(sprintf(paste0("[ACTA dashboard] %s\n  EMPTY -- no exports were found under '%s'.\n",
                           "  The template was written with no columns and no rows.\n",
                           "  template: %s"), outFile, scanDir, basename(tpl)))
  } else {
    message(sprintf(paste0("[ACTA dashboard] %s\n  %d row(s) from %d run(s), tabbed by %s\n",
                           "  %d column(s) shown, schema from '%s' (%d pooled, %d hidden as internal)\n",
                           "  template: %s"),
                    outFile, length(records), length(runInfo),
                    if (is.na(tabCol)) "run folder" else tabCol, length(cols), newest,
                    ncol(allRows), length(ordered) - length(dispCol), basename(tpl)))
  }
}
