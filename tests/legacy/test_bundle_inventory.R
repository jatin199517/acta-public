## .tpWriteInventory(): the inventory a bundle carries so it can re-run itself.
##
## THE DEFECT, measured 2026-11-21 across all seven deposited bundles: 0 of 15 TITRATED reagents
## appeared in the inventory their own bundle shipped. The function was written from `panels`,
## which holds co-stain panel iterations, and a titrated reagent is by definition absent from its
## own co-stain panel -- so the one reagent the submission is ABOUT was the one it could not
## describe. Re-running a deposited bundle and diffing the templates it rebuilt against the ones
## inside it showed `Antibody host species`, `Isotype` and `Clonality` going from populated to
## BLANK, identically on 3.6.11, 3.6.12 and 3.6.13. Nothing published was wrong; the round trip
## lost it, which is the one thing a bundle exists to support.
##
## `Clonality` had no column at all and `Species / Isotype` was never written, so even a CO-STAIN
## reagent lost its isotype on re-run.
##
## THE TRAP THIS TEST EXISTS TO HOLD: the titrated row must carry NO `Panel Iteration`. Give it
## one and the re-run reads the titrated reagent as a member of its own co-stain panel, which is
## precisely what .tpSelfCostain() refuses -- ACTA would build a bundle that its own gate rejects.
## Section 4 is that assertion.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, not the installed namespace.
fns   <- actaTestFunctions()
wrInv <- fns$.tpWriteInventory
rdInv <- fns$actaPanelInventory
byCat <- fns$.tpInvByCatalog
lotOf <- fns$actaLotConcentration

## ---- the fixture: the shape actaPanelInventory() really returns --------------------------------
## `612936` is the TITRATED reagent and has NO iteration, which is how the lab master holds such a
## row and why the filtered frame cannot see it. The unfiltered rows ride on an attribute.
mkInv <- function() {
  all <- data.frame(
    antigen = c("CD3", "CD4"), fluorochrome = c("BUV805", "BV421"), fl_alias = c("", ""),
    marker_alias = c("CD3", "CD4"), vendor = c("BD Biosciences", "BD Biosciences"),
    catalog = c("612893", "612936"), clone = c("SK7", "SK3"),
    clonality = c("monoclonal", "monoclonal"),
    isotype = c("Mouse IgG1", "Mouse IgG1, kappa"),
    type = c("Base", "Variable"), iteration = c("Iteration1", NA_character_),
    stringsAsFactors = FALSE)
  inv <- all[!is.na(all$iteration), , drop = FALSE]
  attr(inv, "acta_inventory_all") <- all
  attr(inv, "acta_lot_info") <- data.frame(catalog = "612936", lot = "5315516",
                                           concentration = "200", stringsAsFactors = FALSE)
  inv
}
mkPanels <- function() list(Iteration1 = list(table = data.frame(
  Antigen = "CD3", Fluorochrome = "BUV805", Matched_to = "CD3", Catalog = "612893",
  Vendor = "BD Biosciences", Clone = "SK7", Type = "Base", stringsAsFactors = FALSE)))
mkStats <- function() data.frame(Alias = "CD4", Markername = "CD4", Cat_Num = "612936",
                                 Vendor = "BD Biosciences", Lot_Num = "5315516", Clone = "SK3",
                                 stringsAsFactors = FALSE)

inv <- mkInv()
## CONTROL on the fixture itself: the titrated reagent must be invisible to the FILTERED frame,
## or the whole defect is not being reproduced and every assertion below is vacuous.
chk("fixture: titrated reagent is absent from the filtered inventory",
    !("612936" %in% as.character(inv$catalog)))
chk("fixture: ...but present in the unfiltered rows",
    "612936" %in% as.character(attr(inv, "acta_inventory_all")$catalog))

out <- tempfile(fileext = ".xlsx")
p <- wrInv(mkPanels(), out, stats = mkStats(), inv = inv)

## ---- 1. the file ------------------------------------------------------------------------------
cat("\n-- 1. what gets written --\n")
chk("an inventory is written", !is.null(p) && file.exists(out))
shts <- if (file.exists(out)) readxl::excel_sheets(out) else character(0)
chk("it has a Master sheet", "Master" %in% shts, paste(shts, collapse = ","))
chk("...and a Lot_info sheet", "Lot_info" %in% shts, paste(shts, collapse = ","))
d <- if (file.exists(out))
  suppressMessages(readxl::read_excel(out, sheet = "Master", col_types = "text",
                                      .name_repair = "minimal")) else data.frame()
chk("a Clonality column exists", "Clonality" %in% names(d), paste(names(d), collapse = ","))
chk("both reagents are written", nrow(d) == 2L, nrow(d))

## ---- 2. the titrated reagent is in it ---------------------------------------------------------
cat("\n-- 2. the titrated reagent --\n")
ti <- if (nrow(d)) which(as.character(d$Catalog) == "612936") else integer(0)
chk("the titrated reagent has a row", length(ti) == 1L, length(ti))
if (length(ti) == 1L) {
  chk("...typed Variable, not Base", identical(as.character(d$Type[ti]), "Variable"),
      d$Type[ti])
  chk("...carrying Clonality from the inventory",
      identical(as.character(d$Clonality[ti]), "monoclonal"), d$Clonality[ti])
  chk("...and Species / Isotype",
      identical(as.character(d[["Species / Isotype"]][ti]), "Mouse IgG1, kappa"),
      d[["Species / Isotype"]][ti])
}
pi_ <- if (nrow(d)) which(as.character(d$Catalog) == "612893") else integer(0)
chk("the co-stain row keeps its isotype too", length(pi_) == 1L &&
      identical(as.character(d[["Species / Isotype"]][pi_]), "Mouse IgG1"),
    if (length(pi_)) d[["Species / Isotype"]][pi_] else "no row")

## ---- 3. ACTA can read its own file back -------------------------------------------------------
cat("\n-- 3. round trip through ACTA's own reader --\n")
back <- tryCatch(rdInv(out), error = function(e) NULL)
chk("actaPanelInventory() reads it", !is.null(back) && nrow(back) >= 1L)
if (!is.null(back)) {
  hit <- byCat(back, "612936")
  chk("the titrated reagent is found by catalogue", !is.null(hit) && nrow(hit) == 1L)
  if (!is.null(hit)) {
    chk("...its clonality survives", identical(as.character(hit$clonality[1]), "monoclonal"))
    chk("...its isotype survives",
        identical(as.character(hit$isotype[1]), "Mouse IgG1, kappa"))
  }
  chk("the lot concentration resolves",
      identical(lotOf(attr(back, "acta_lot_info"), "612936", "5315516"), "200"),
      lotOf(attr(back, "acta_lot_info"), "612936", "5315516"))
}

## ---- 4. THE TRAP: the titrated reagent must not join its own co-stain panel --------------------
cat("\n-- 4. it must not become its own co-stain --\n")
chk("the written titrated row has a BLANK Panel Iteration",
    length(ti) == 1L && (is.na(d[["Panel Iteration"]][ti]) ||
                         !nzchar(trimws(as.character(d[["Panel Iteration"]][ti])))),
    if (length(ti)) d[["Panel Iteration"]][ti] else "no row")
if (!is.null(back))
  chk("...so a re-run's panel frame does not contain it",
      !("612936" %in% as.character(back$catalog)),
      paste(as.character(back$catalog), collapse = ","))

## ---- 5. the package is internally consistent --------------------------------------------------
## Hand-rolled OPC: every sheet named in workbook.xml needs a relationship, a Content_Types
## override and a real part. An inconsistent package is what makes Excel offer to repair a file
## that every R reader opens without complaint.
cat("\n-- 5. OPC package consistency --\n")
if (file.exists(out)) {
  nm <- utils::unzip(out, list = TRUE)$Name
  wbx <- paste(readLines(unz(out, "xl/workbook.xml"), warn = FALSE), collapse = "")
  rel <- paste(readLines(unz(out, "xl/_rels/workbook.xml.rels"), warn = FALSE), collapse = "")
  ctx <- paste(readLines(unz(out, "[Content_Types].xml"), warn = FALSE), collapse = "")
  ids <- regmatches(wbx, gregexpr('r:id="[^"]+"', wbx))[[1]]
  chk("every sheet in workbook.xml has a relationship",
      length(ids) > 0 && all(vapply(sub('r:id="([^"]+)"', "\\1", ids),
                                    function(i) grepl(sprintf('Id="%s"', i), rel, fixed = TRUE),
                                    logical(1))), paste(ids, collapse = " "))
  tgt <- sub('.*Target="([^"]+)".*', "\\1",
             regmatches(rel, gregexpr('Target="[^"]+"', rel))[[1]])
  chk("every relationship target is a real part in the zip",
      length(tgt) > 0 && all(file.path("xl", tgt) %in% nm),
      paste(setdiff(file.path("xl", tgt), nm), collapse = " "))
  chk("every worksheet part has a Content_Types override",
      all(vapply(grep("^xl/worksheets/", nm, value = TRUE),
                 function(s) grepl(sprintf('PartName="/%s"', s), ctx, fixed = TRUE),
                 logical(1))))
  chk("every declared sheet actually opens",
      all(vapply(shts, function(s)
        !inherits(tryCatch(suppressMessages(readxl::read_excel(out, sheet = s)),
                           error = function(e) e), "error"), logical(1))))
}

## ---- 6. CONTROLS: the new rows really come from `stats` ----------------------------------------
## Without this the suite could not tell the fix from a fixture that happened to contain the row.
cat("\n-- 6. controls --\n")
o2 <- tempfile(fileext = ".xlsx")
invisible(wrInv(mkPanels(), o2, stats = NULL, inv = inv))
d2 <- suppressMessages(readxl::read_excel(o2, sheet = "Master", col_types = "text",
                                          .name_repair = "minimal"))
chk("stats = NULL writes the panel row only", nrow(d2) == 1L, nrow(d2))
chk("...and no Lot_info sheet", !("Lot_info" %in% readxl::excel_sheets(o2)))
## A lot nobody recorded must not invent a sheet.
s3 <- mkStats(); s3$Lot_Num <- "NOT_A_REAL_LOT"
o3 <- tempfile(fileext = ".xlsx")
invisible(wrInv(mkPanels(), o3, stats = s3, inv = inv))
chk("an unmatched lot writes no Lot_info sheet",
    !("Lot_info" %in% readxl::excel_sheets(o3)),
    paste(readxl::excel_sheets(o3), collapse = ","))
chk("...but the titrated reagent is still written", nrow(suppressMessages(
  readxl::read_excel(o3, sheet = "Master", col_types = "text",
                     .name_repair = "minimal"))) == 2L)
## MEASURED ON A REAL RUN: the lab inventory lists both lots and records no concentration for
## either. That wrote a Lot_info sheet whose only informative column was empty, which reads as
## "ACTA looked this up and this is the answer". A matching row is not the same as a value.
i4 <- mkInv()
.l4 <- attr(i4, "acta_lot_info"); .l4$concentration <- NA_character_
attr(i4, "acta_lot_info") <- .l4
o4 <- tempfile(fileext = ".xlsx")
invisible(wrInv(mkPanels(), o4, stats = mkStats(), inv = i4))
chk("a matching lot with a BLANK concentration writes no Lot_info sheet",
    !("Lot_info" %in% readxl::excel_sheets(o4)),
    paste(readxl::excel_sheets(o4), collapse = ","))

cat(sprintf("\n%s\n", if (ok) "BUNDLE INVENTORY: all checks passed" else "BUNDLE INVENTORY: FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
