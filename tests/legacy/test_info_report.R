## actaInfoReport(): the Info sheet as the run used it, with a SOURCE column.
##
## The Source column is the whole point -- it is what would have said "shipped example" when a run
## silently matched its costain panel against the demo inventory. These assertions are about
## provenance, not about values.

ok <- TRUE
## AN ASSERTION THAT ERRORS IS A FAILURE, NOT THE END OF THE RUN. `pass` is a promise, so forcing
## it inside tryCatch catches a predicate that throws -- which is exactly what the bug this gate
## guards does. Without this the script aborted on the first such assertion and every later one
## went unreported, so a falsification run showed one problem where there were several.
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d %||% tryCatch(paste(as.character(detail), collapse = " "), error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

suppressMessages(library(ACTA))
rep_   <- ACTA:::actaInfoReport
resInv <- ACTA:::actaInfoResolvedInventory

long <- data.frame(
  Category    = c("General", "General", NA,        "TiterPubs", "Provenance"),
  Variable    = c("miflowcyt_export (L)", "Downsample_to (N)", NA, "titerpubs_export",
                  "master_panel_inventory_path"),
  Value       = c("TRUE", "", NA, "FALSE", ""),
  Description = c("writes the metadata sheet", "Blank reads every event", NA,
                  "builds a bundle", "Blank falls back"),
  stringsAsFactors = FALSE)

d <- rep_(long)
chk("a blank spacer row is dropped, not reported as a variable", nrow(d) == 4L, nrow(d))
chk("a populated cell is sourced to the Info sheet",
    d$Source[d$Variable == "miflowcyt_export (L)"] == "Info sheet")
chk("a blank cell is 'not set', NOT silently shown as a value",
    d$Source[d$Variable == "Downsample_to (N)"] == "not set")
chk("...and its Value is NA so the renderer can label it",
    is.na(d$Value[d$Variable == "Downsample_to (N)"]))
chk("Description survives for the reader to learn the fallback",
    grepl("every event", d$Description[d$Variable == "Downsample_to (N)"]))
chk("a Category inherited from the row above is filled in",
    d$Category[d$Variable == "titerpubs_export"] == "TiterPubs", d$Category)
chk("columns are exactly Category/Variable/Value/Source/Description",
    identical(names(d), c("Category","Variable","Value","Source","Description")), names(d))

## THE CASE THIS EXISTS FOR. A blank path in the workbook, resolved at run time to the shipped
## demo inventory, must NOT read as "not set" -- it must name the file and say it was a fallback.
p <- "/x/y/TEMPLATE_Master_Panel_Inventory.xlsx"
attr(p, "acta_inventory_source") <- "example"
d2 <- rep_(long, resolved = list(master_panel_inventory_path = resInv(p)))
row <- d2[d2$Variable == "master_panel_inventory_path", ]
chk("a resolved inventory names the file that was opened",
    identical(row$Value, "TEMPLATE_Master_Panel_Inventory.xlsx"), row$Value)
chk("...and its Source says 'shipped example', not 'Info sheet' and not 'not set'",
    identical(row$Source, "shipped example"), row$Source)
chk("...and the absolute path is NOT published, only the basename",
    !grepl("/x/y", row$Value, fixed = TRUE), row$Value)

p2 <- "/lab/260519_Master_Panel_Inventory.xlsx"; attr(p2, "acta_inventory_source") <- "info"
chk("an Info-sheet inventory is labelled 'Info sheet'",
    identical(resInv(p2)$source, "Info sheet"), resInv(p2)$source)
p3 <- "/run/Master_Panel_Inventory.xlsx"; attr(p3, "acta_inventory_source") <- "run_folder"
chk("a run-folder inventory is labelled 'run folder'",
    identical(resInv(p3)$source, "run folder"), resInv(p3)$source)
chk("no inventory resolves to NULL, leaving the workbook's own row alone",
    is.null(resInv(NA_character_)) && is.null(resInv(NULL)))

## Resolution must key off the bare name too -- the sheet writes "Downsample_to (N)".
d3 <- rep_(long, resolved = list(Downsample_to = "20000"))
chk("resolved matches a Variable written with its (N)/(L) type hint",
    identical(d3$Value[d3$Variable == "Downsample_to (N)"], "20000"),
    d3$Value[d3$Variable == "Downsample_to (N)"])

## A legacy WIDE Info sheet still reports, ungrouped rather than skipped.
wide <- data.frame(working_directory = NA_character_, MM_R2_threshold = "0.9",
                   stringsAsFactors = FALSE)
d4 <- rep_(wide)
chk("a legacy wide Info sheet still produces a table", !is.null(d4) && nrow(d4) == 2L)
chk("...with its populated value sourced to the Info sheet",
    d4$Source[d4$Variable == "MM_R2_threshold"] == "Info sheet")
chk("...and its blank one 'not set'", d4$Source[d4$Variable == "working_directory"] == "not set")

chk("an empty Info sheet returns NULL rather than erroring",
    is.null(rep_(long[0, ])) && is.null(rep_(NULL)))

## ---- a resolved variable the sheet does not carry ---------------------------------------------
## THE CASE THAT BROKE ON EXP26000001. Its Info sheet is the legacy 9-column wide layout, which
## has no master_panel_inventory_path column, so overwriting-only meant the resolved inventory
## was dropped and the Info table said nothing at all -- in exactly the situation the Source
## column exists to expose.
wide2 <- data.frame(working_directory = NA_character_, MM_R2_threshold = "0.9",
                    stringsAsFactors = FALSE)
pex <- "/x/TEMPLATE_Master_Panel_Inventory.xlsx"; attr(pex, "acta_inventory_source") <- "example"
d5 <- rep_(wide2, resolved = list(master_panel_inventory_path = resInv(pex)))
chk("a resolved variable absent from the sheet is APPENDED, not dropped",
    "master_panel_inventory_path" %in% d5$Variable, d5$Variable)
r5 <- d5[d5$Variable == "master_panel_inventory_path", ]
chk("...carrying the file that was opened", r5$Value == "TEMPLATE_Master_Panel_Inventory.xlsx", r5$Value)
chk("...and its real Source", r5$Source == "shipped example", r5$Source)
chk("...under a Category that says it was not on the sheet",
    r5$Category == "Resolved at run time", r5$Category)
chk("...with a Description saying so", grepl("Not on this workbook", r5$Description), r5$Description)
chk("the sheet's own rows are untouched", nrow(d5) == 3L && "MM_R2_threshold" %in% d5$Variable, nrow(d5))
chk("a resolved variable the sheet DOES carry is still overwritten, not duplicated",
    sum(rep_(long, resolved = list(Downsample_to = "20000"))$Variable == "Downsample_to (N)") == 1L)
chk("'lab default' is a known source label",
    identical(unname(ACTA:::ACTA_INFO_SOURCE_LABEL[["lab_default"]]), "lab default"))

## A NULL resolved entry must be skipped, not turned into a zero-row data.frame. The pipeline
## cannot make one (res[[k]] <- NULL deletes the key) but list(k = NULL) can, and it died with
## "arguments imply differing number of rows: 1, 0".
## BOTH BRANCHES. A key matching no row exits the overwrite loop early anyway, so it only
## exercises the append guard -- the first version of this assertion passed against a build with
## the overwrite guard deleted. The key below MATCHES a row, which is the only way in.
chk("a NULL resolved entry whose key MATCHES a row is skipped, not an error",
    identical(rep_(long, resolved = list(Downsample_to = NULL))$Value, rep_(long)$Value))
chk("...and one whose key matches nothing is not appended",
    !"nope" %in% rep_(long, resolved = list(nope = NULL))$Variable)

## Zero-width characters are endemic in hand-maintained sheets and make a resolved key miss its
## own row -- invisible in Excel, so nothing on screen explains the mismatch.
zwl <- data.frame(Category = "General", Variable = "Downsample_to (N)\u200b", Value = "",
                  Description = "d", stringsAsFactors = FALSE)
chk("a zero-width space in a Variable name is stripped on read",
    rep_(zwl)$Variable == "Downsample_to (N)", rep_(zwl)$Variable)
chk("...so a resolved key still matches that row, instead of appending a duplicate",
    nrow(rep_(zwl, resolved = list(Downsample_to = "20000"))) == 1L &&
      rep_(zwl, resolved = list(Downsample_to = "20000"))$Value == "20000")

## THE HOME PREFIX MUST NOT REACH THE PAGE. Four Info variables hold absolute paths, and on a
## managed machine the account under /Users is the operator's email address. The report travels
## inside the TiterPubs bundle, so this is a disclosure path, not a cosmetic one. The session-info
## block was already de-identified; this table was not, so section 7 printed what section 8 hid.
hp <- path.expand("~")
acct <- tryCatch(Sys.info()[["user"]], error = function(e) "")
dl <- rep_(data.frame(Category = "General",
                      Variable = c("working_directory", "dashboard_dir", "keywords"),
                      Value = c(file.path(hp, "CloudStorage/Runs/Analysis"), file.path(hp, "Dash"), "none"),
                      Description = NA_character_, stringsAsFactors = FALSE))
chk("no Value carries the home prefix", !any(grepl(hp, dl$Value, fixed = TRUE)), dl$Value)
chk("...nor the account name",
    !nzchar(acct) || !any(grepl(acct, dl$Value, fixed = TRUE)), dl$Value)
chk("...and the path is still readable, rewritten to ~",
    any(grepl("^~/CloudStorage/Runs/Analysis$", dl$Value)), dl$Value)
chk("a value with no path in it is untouched", "none" %in% dl$Value, dl$Value)
## An APPENDED resolved row must be de-identified too -- it is added after the sheet's own rows,
## so a de-identification pass placed before the append would miss exactly this one.
dr <- rep_(data.frame(Category = "General", Variable = "keywords", Value = "none",
                      Description = NA_character_, stringsAsFactors = FALSE),
           resolved = list(some_path = file.path(hp, "Runs", "x.xlsx")))
chk("an appended resolved value is de-identified too",
    nrow(dr) == 2L && !any(grepl(hp, dr$Value, fixed = TRUE)), dr$Value)

cat(if (ok) "\nINFO REPORT TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
