## The 3.6.8 schema change: the Cell_Type rename, the template version gate, and -- the reason
## this file exists -- PROOF THAT THE SITE SCRUB RULES STILL FIRE.
##
## WHY. Renaming Layout_Plate!Test_Material -> Cell_Type touches two guarded reads that FAIL OPEN:
##
##   R/panel_inventory.R        if ("Cell_Type" %in% names(df) && length(actaCellTypeRules()))
##   R/TiterPubs_template_mapper.R  if ("Cell_Type" %in% names(lp) && length(actaCellTypeRules()))
##
## Miss either and the guard is simply FALSE: no error, no warning, every site rule silently
## stops applying, and a bundle reports clean because the scrub never ran. That is the single
## worst outcome this codebase can produce, and a passing test suite would have said nothing --
## no shipped fixture sets these rules, so nothing exercised the path at all.
##
## So the assertions below SET REAL RULES and check the substitution actually happened, on both
## readers, rather than checking that a column name appears somewhere in the source.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
suppressMessages({library(readxl); library(zip)})
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
h <- actaTestFunctions(actaTestAppDir())
.f <- function(nm) if (!is.null(h[[nm]])) h[[nm]] else getFromNamespace(nm, "ACTA")

## THE PATTERN IS FICTIONAL ON PURPOSE. The first version of this file used this site's REAL
## programme-code prefix, and the release build's scrub gate refused to publish -- correctly: a
## test destined for a public repo had a site token hardcoded in five places. What is under test
## is "does a site rule fire at all", for which any pattern serves, so the one here matches
## nothing any lab would own. See the sidecar token list for why the gate could see it and a
## generic sweep could not.
RULES <- list(c(pattern = "ACME-[0-9]{2,4}|ZZPROG", replacement = "Human T cells"),
              c(pattern = "PBMC",              replacement = "Human PBMCs"))

cat("\n=== the site Cell_Type rules still fire after the rename ===\n")
local({
  old <- options(acta.cell_type_rules = RULES); on.exit(options(old), add = TRUE)
  mapped <- .f("actaMapCellType")(c("ACME-1234 product", "frozen PBMC", "Whole blood"))
  chk("a programme-code material is renamed by the site rule",
      identical(mapped[1], "Human T cells"), mapped[1])
  chk("a PBMC material is renamed by the site rule",
      identical(mapped[2], "Human PBMCs"), mapped[2])
  chk("an unmatched material is passed through unchanged, not blanked",
      identical(mapped[3], "Whole blood"), mapped[3])

  ## READER 1: actaApplyScrub() on a layout frame. This is the one that reaches the export,
  ## the report and the bundled Layout_Plate.
  df <- data.frame(Cell_Type = c("ACME-1234 product", "frozen PBMC"),
                   Operator = c("SC", "SC"), stringsAsFactors = FALSE)
  out <- suppressMessages(.f("actaApplyScrub")(df))
  chk("actaApplyScrub() rewrites the Cell_Type COLUMN (reader 1 of 2)",
      identical(as.character(out$Cell_Type), c("Human T cells", "Human PBMCs")),
      paste(out$Cell_Type, collapse = " | "))

  ## FALSIFICATION, INLINE: with the column under its OLD name, the guard must not fire. This is
  ## what a half-finished rename would look like, and it must look different from success.
  df_old <- data.frame(Test_Material = c("ACME-1234 product"), stringsAsFactors = FALSE)
  out_old <- suppressMessages(.f("actaApplyScrub")(df_old))
  chk("...and a frame still using the OLD column name is NOT rewritten (so the guard is real)",
      identical(as.character(out_old$Test_Material), "ACME-1234 product"),
      paste(out_old$Test_Material, collapse = " | "))
})

cat("\n=== the renamed option is enforced, not silently ignored ===\n")
local({
  old <- options(acta.test_material_rules = RULES, acta.cell_type_rules = NULL)
  on.exit(options(old), add = TRUE)
  e <- tryCatch({ .f("actaCellTypeRules")(); NULL }, error = function(e) conditionMessage(e))
  chk("the retired option name STOPS the run rather than being ignored", !is.null(e))
  chk("...and the message names the new option", !is.null(e) && grepl("acta.cell_type_rules", e))
})
local({
  old <- options(acta.test_material_rules = RULES, acta.cell_type_rules = RULES)
  on.exit(options(old), add = TRUE)
  r <- tryCatch(.f("actaCellTypeRules")(), error = function(e) NULL)
  chk("both set is fine -- the new one wins, no error", !is.null(r) && length(r) == 2L)
})

cat("\n=== the template version gate ===\n")
local({
  mkinfo <- function(...) { kv <- list(...)
    data.frame(Category = "General", Variable = names(kv),
               Value = unlist(kv, use.names = FALSE), stringsAsFactors = FALSE) }
  g <- .f("actaAssertTemplateVersion")
  layout_new <- data.frame(Cell_Type = "x", stringsAsFactors = FALSE)
  layout_old <- data.frame(Cell_Type = "x", flowjo_transformation_arg = "pos=4.5",
                           stringsAsFactors = FALSE)

  chk("a 3.6.8 workbook passes",
      isTRUE(tryCatch(g(mkinfo(Template_Version = "3.6.8"), layout_new), error = function(e) FALSE)))
  chk("a NEWER template passes too",
      isTRUE(tryCatch(g(mkinfo(Template_Version = "3.7.0"), layout_new), error = function(e) FALSE)))

  e1 <- tryCatch({ g(mkinfo(Template_Version = "3.6.7"), layout_new); NULL },
                 error = function(e) conditionMessage(e))
  chk("an OLDER version is refused", !is.null(e1))
  chk("...and the message says to download the current template",
      !is.null(e1) && grepl("current template", e1))

  e2 <- tryCatch({ g(mkinfo(working_directory = ""), layout_new); NULL },
                 error = function(e) conditionMessage(e))
  chk("a workbook with NO Template_Version is refused", !is.null(e2))

  ## The shape test, which the version cell cannot do: a workbook that CLAIMS 3.6.8 while still
  ## carrying the retired Layout_Plate column -- a hand-edited version cell, or a pasted layout.
  e3 <- tryCatch({ g(mkinfo(Template_Version = "3.6.8"), layout_old); NULL },
                 error = function(e) conditionMessage(e))
  chk("a 3.6.8 claim WITH the retired Layout_Plate column is still refused", !is.null(e3))

  ## Both sheets at once is ambiguous and must error rather than pick a winner.
  e4 <- tryCatch({ g(mkinfo(Template_Version = "3.6.8", flowjo_transformation_arg = "pos=4.5"),
                     layout_old); NULL }, error = function(e) conditionMessage(e))
  chk("the field present in BOTH sheets errors instead of guessing",
      !is.null(e4) && grepl("BOTH", e4))
})

cat("\n=== the shipped workbooks actually carry the new schema ===\n")
local({
  tf <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                  "TEMPLATE_Ab_Titration_Instructions.xlsx")
  lp <- as.data.frame(suppressMessages(readxl::read_excel(tf, sheet = "Layout_Plate",
          col_types = "text", .name_repair = "minimal")), check.names = FALSE)
  nm <- .f("actaStripHeaderAnnotation")(names(lp))
  for (want in c("Cell_Type", "Tissue_Type", "Sample_Status", "Culture_Status"))
    chk(sprintf("TEMPLATE Layout_Plate has %s", want), want %in% nm)
  chk("TEMPLATE Layout_Plate no longer has flowjo_transformation_arg",
      !"flowjo_transformation_arg" %in% nm)
  chk("TEMPLATE Layout_Plate no longer has Test_Material", !"Test_Material" %in% nm)

  inf <- as.data.frame(suppressMessages(readxl::read_excel(tf, sheet = "Info",
           col_types = "text", .name_repair = "minimal")), check.names = FALSE)
  gv <- function(k) { j <- which(!is.na(inf$Variable) & inf$Variable == k)
                      if (!length(j)) "<ABSENT>" else
                      if (is.na(inf$Value[j[1]])) "<BLANK>" else inf$Value[j[1]] }
  chk("TEMPLATE Info!Template_Version is 3.6.8", identical(gv("Template_Version"), "3.6.8"))
  chk("TEMPLATE Info!flowjo_transformation_arg exists", gv("flowjo_transformation_arg") != "<ABSENT>")
  chk("TEMPLATE Info!Collection_Software exists", gv("Collection_Software") != "<ABSENT>")
  chk("TEMPLATE Info!Sample_Details exists", gv("Sample_Details") != "<ABSENT>")
  chk("TEMPLATE Info!Tissue_Fallback kept", gv("Tissue_Fallback") != "<ABSENT>")
  chk("TEMPLATE Info!Cell_Type_Fallback kept", gv("Cell_Type_Fallback") != "<ABSENT>")
  chk("TEMPLATE Info!Tissue REMOVED (it is per-well now)", gv("Tissue") == "<ABSENT>")
  chk("TEMPLATE Info!Fresh_or_Frozen REMOVED", gv("Fresh_or_Frozen") == "<ABSENT>")

  gt <- as.data.frame(suppressMessages(readxl::read_excel(tf, sheet = "gating_template",
          col_types = "text", .name_repair = "minimal")), check.names = FALSE)
  act <- gt[!is.na(gt$alias) & !startsWith(trimws(gt$alias), "#"), , drop = FALSE]
  rg <- act$gating_method[trimws(act$alias) == "Reagent"]
  chk("TEMPLATE's ACTIVE reagent gate is gate_mind_fmeans_costain",
      identical(rg, "gate_mind_fmeans_costain"), paste(rg, collapse = ", "))

  mi <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                  "TEMPLATE_Master_Panel_Inventory.xlsx")
  chk("TEMPLATE inventory has a Lot_info sheet", "Lot_info" %in% readxl::excel_sheets(mi))
  mm <- as.data.frame(suppressMessages(readxl::read_excel(mi, sheet = "Master",
          col_types = "text", .name_repair = "minimal")), check.names = FALSE)
  chk("TEMPLATE inventory Master has Clonality", "Clonality" %in% names(mm))
})

cat("\n=== Collection Software: the CREATOR keyword is reduced to the menu term ===\n")
local({
  tpl <- .f("actaTiterPubsTemplate")()
  if (!nzchar(tpl) || !file.exists(tpl)) { cat("  [skip] no template\n"); return() }
  menus <- .f("actaTiterPubsMenus")(tpl)
  red <- .f(".tpCollectionSw")
  ## WHY THIS IS REDUCED AND NOT PUBLISHED VERBATIM. Their menu holds bare names -- Diva,
  ## SpectroFlo, CytExpert, ID7000, Everest -- while the FCS CREATOR keyword carries a version
  ## ("SpectroFlo 3.3.0" on every Cytek file measured). Verbatim, the value is OFF MENU on every
  ## single Cytek run, which is worse than the model lookup it replaced. Found by an end-to-end
  ## check that read the WRITTEN workbook; a unit test asserting on the returned vector would
  ## have called "SpectroFlo 3.3.0" correct and shipped it.
  chk("'SpectroFlo 3.3.0' reduces to the menu term",
      identical(red("SpectroFlo 3.3.0", menus), "SpectroFlo"), red("SpectroFlo 3.3.0", menus))
  chk("...and the reduced value IS on their menu",
      .f(".tpOnMenu")("SpectroFlo", "Collection Software", menus))
  chk("a bare menu name is unchanged",
      identical(red("Diva", menus), "Diva"), red("Diva", menus))
  chk("software their menu does not list passes through for row 3, not blanked",
      identical(red("AcmeAcquire 9", menus), "AcmeAcquire 9"), red("AcmeAcquire 9", menus))
  chk("a blank keyword stays blank", is.na(red("", menus)))
  ## Longest-first, so a future menu entry cannot be shadowed by a shorter prefix of itself.
  fake <- list(`Collection Software` = c("SpectroFlo", "SpectroFlo SE"))
  chk("the LONGEST matching option wins, not the first",
      identical(red("SpectroFlo SE 1.2", fake), "SpectroFlo SE"), red("SpectroFlo SE 1.2", fake))
})

cat("\n=== a missing inventory column SKIPS, it does not error ===\n")
local({
  tmp <- file.path(tempdir(), paste0("inv_", basename(tempfile("")), ".xlsx"))
  d <- data.frame(Antigen = "CD3", Catalog = "612893", `Panel Iteration` = "1",
                  check.names = FALSE, stringsAsFactors = FALSE)   # no Clonality, no Lot_info
  openxlsx::write.xlsx(d, tmp, sheetName = "Master")
  inv <- tryCatch(.f("actaPanelInventory")(tmp), error = function(e) structure(NULL, err = e))
  chk("an inventory with NO Clonality column still reads", !is.null(inv))
  chk("...and clonality comes back as NA rather than missing",
      !is.null(inv) && "clonality" %in% names(inv) && all(is.na(inv$clonality)))
  chk("...and a workbook with no Lot_info sheet yields NULL lot info, not an error",
      !is.null(inv) && is.null(attr(inv, "acta_lot_info")))
})

cat(if (ok) "\nPASS\n" else "\nFAIL\n")
quit(status = if (ok) 0L else 1L)
