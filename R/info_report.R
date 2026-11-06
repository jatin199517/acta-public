## THE INFO SHEET AS THE RUN ACTUALLY USED IT -- the table behind report section 6.
##
## Modelled on URSULA's `# 8 Info` section (ACFA, Report_URSULA.Rmd): one row per script-level
## variable, grouped by Category, with a SOURCE column saying where the value came from.
##
## SOURCE IS THE POINT OF THE EXERCISE, not the values -- those are already in the workbook and in
## the export's Info audit sheet. A run once reported a real costain panel as unknown because the
## panel inventory had silently fallen back to the shipped demo file; the value looked ordinary and
## nothing said it was a fallback. A Source of "shipped example" rather than "Info sheet" says it
## on the page, without anyone having to go and diagnose it.
##
## NO PARALLEL TABLE OF DEFAULTS, which is where this departs from URSULA. URSULA carries a
## `default` per variable because its variables are declared in code (.ACFA_INFO_VARS). ACTA's are
## declared in the WORKBOOK, which already ships Category, Variable and Description. Restating each
## default in R would be a second source of truth, and it would drift from the template the first
## time one of them changed -- silently, because nothing compares them. So a blank cell is reported
## as "not set" and the template's own Description ("Blank reads every event", "Blank means the
## folder holding...") is what tells the reader the fallback. A resolver that KNOWS what it picked
## overrides its row instead; that is what `resolved` is for.

## One row per Info variable: Category, Variable, Value, Source, Description.
##
## `resolved` is a named list keyed by Variable. Each element is either a plain string (the
## effective value, Source becomes "resolved") or a list(value=, source=) when the resolver can
## name its own provenance. It exists so a value the pipeline worked out at run time -- which
## inventory file it opened, say -- is reported as that, not as the blank the workbook holds.
actaInfoReport <- function(info, resolved = NULL) {
  blank <- function(x) is.na(x) || !nzchar(trimws(x)) || toupper(trimws(x)) == "NA"
  ## Zero-width characters are endemic in hand-maintained sheets -- the panel inventory carries a
  ## U+200B on the end of "Streptavidin" and the Info sheet is written the same way. Invisible in
  ## Excel, and it makes a `resolved` key fail to match its own row. Stripped once, on read.
  zw <- function(x) trimws(gsub("[\u200b-\u200d\ufeff\u00ad]", "", as.character(x)))

  if (is.null(info) || !is.data.frame(info) || !nrow(info)) return(NULL)

  if (actaInfoIsLong(info)) {
    nm  <- tolower(trimws(names(info)))
    col <- function(want) { j <- match(want, nm); if (is.na(j)) rep(NA_character_, nrow(info))
                            else trimws(as.character(info[[j]])) }
    d <- data.frame(Category = zw(col("category")), Variable = zw(col("variable")),
                    Value = col("value"), Description = col("description"),
                    stringsAsFactors = FALSE)
  } else {
    ## The legacy WIDE layout carries no Category or Description -- one header row, one value row.
    ## Reported anyway rather than skipped: an older workbook still deserves its settings on the
    ## page, it just gets them ungrouped and undescribed.
    d <- data.frame(Category = "General", Variable = zw(names(info)),
                    Value = trimws(as.character(unlist(info[1, ], use.names = FALSE))),
                    Description = NA_character_, stringsAsFactors = FALSE)
  }

  ## Spacer rows -- the template uses blank lines to separate Categories in Excel.
  d <- d[!vapply(d$Variable, blank, TRUE), , drop = FALSE]
  if (!nrow(d)) return(NULL)
  ## A Category only has to be written on its first row to read correctly in Excel.
  for (i in seq_len(nrow(d))) if (blank(d$Category[i]))
    d$Category[i] <- if (i == 1L) "General" else d$Category[i - 1L]

  d$Source <- ifelse(vapply(d$Value, blank, TRUE), "not set", "Info sheet")
  d$Value[vapply(d$Value, blank, TRUE)] <- NA_character_

  ## A variable's bare name without the "(N)" / "(L)" type hint, so `resolved` can be keyed by
  ## either -- the same aliasing actaInfoValue() does.
  bare <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", d$Variable))
  for (k in names(resolved %||% list())) {
    ## A NULL entry means "nothing resolved". `res[[k]] <- NULL` deletes the key so the pipeline
    ## never produces one, but list(k = NULL) does, and as.character(NULL) is character(0) --
    ## which made the data.frame() below die with "arguments imply differing number of rows".
    if (is.null(resolved[[k]])) next
    i <- which(tolower(d$Variable) == tolower(k) | tolower(bare) == tolower(k))
    if (!length(i)) next
    r <- resolved[[k]]
    if (is.list(r)) { d$Value[i[1]] <- r$value %||% d$Value[i[1]]
                      d$Source[i[1]] <- r$source %||% "resolved" }
    else           { d$Value[i[1]] <- as.character(r); d$Source[i[1]] <- "resolved" }
  }

  ## A RESOLVED VARIABLE THE SHEET DOES NOT CARRY IS APPENDED, NOT DROPPED. The loop above only
  ## overwrites rows that already exist, so on a legacy wide Info sheet -- which has no
  ## master_panel_inventory_path column at all -- the resolved inventory vanished silently. That
  ## is precisely the case the Source column was added for: a real run's report named the demo
  ## inventory in the panel caption and nowhere in the Info table.
  for (k in names(resolved %||% list())) {
    if (is.null(resolved[[k]])) next
    if (any(tolower(d$Variable) == tolower(k) | tolower(bare) == tolower(k))) next
    r <- resolved[[k]]
    d <- rbind(d, data.frame(
      Category = "Resolved at run time", Variable = k,
      Value  = if (is.list(r)) r$value %||% NA_character_ else as.character(r),
      Source = if (is.list(r)) r$source %||% "resolved" else "resolved",
      Description = "Not on this workbook's Info sheet; resolved while the run executed.",
      stringsAsFactors = FALSE))
  }

  ## THE HOME PREFIX IS REWRITTEN TO ~ BEFORE THIS TABLE LEAVES. Four Info variables hold
  ## absolute paths -- working_directory, dashboard_dir, titration_export_dir and
  ## master_panel_inventory_path -- and on a managed machine the account under /Users is the
  ## operator's EMAIL ADDRESS. The report already de-identifies its session-info block for exactly
  ## this reason and this table was not covered, so section 7 printed what section 8 was careful
  ## to hide. It matters more than it looks: the report travels inside the TiterPubs bundle.
  ##
  ## Done HERE rather than in the Rmd so every consumer of the table inherits it, not just the
  ## one chunk that happens to remember.
  for (cn in c("Value", "Description")) {
    i <- !is.na(d[[cn]])
    if (any(i)) d[[cn]][i] <- vapply(d[[cn]][i],
                                     function(v) tryCatch(actaDeIdentifyText(v), error = function(e) v), "")
  }

  rownames(d) <- NULL
  d[, c("Category", "Variable", "Value", "Source", "Description"), drop = FALSE]
}

## How actaPanelInventoryPath()'s provenance tag reads on the page. The tags themselves are
## internal ("info"/"run_folder"/"example"); these are what a reader of the PDF should see.
ACTA_INFO_SOURCE_LABEL <- c(info = "Info sheet", run_folder = "run folder",
                            lab_default = "lab default", example = "shipped example",
                            unknown = "resolved")

## The `resolved` entry for the master panel inventory: the FILE that was actually opened and the
## provenance tag that says why. Returns NULL when no inventory was resolved, so the row falls back
## to whatever the workbook said.
actaInfoResolvedInventory <- function(path, source = NULL) {
  if (is.null(path) || !length(path) || is.na(path) || !nzchar(as.character(path))) return(NULL)
  src <- source %||% attr(path, "acta_inventory_source") %||% "unknown"
  list(value  = basename(as.character(path)),
       source = unname(ACTA_INFO_SOURCE_LABEL[src] %||% "resolved"))
}
