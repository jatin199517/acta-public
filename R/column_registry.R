## =================================================================================================
## THE WORKBOOK COLUMN REGISTRY
## -------------------------------------------------------------------------------------------------
## One declaration of every column and setting ACTA reads out of the instructions workbook, and the
## ONLY place that knowledge is written down. tests/legacy/test_column_registry.R holds the shipped
## template, the OQ fixtures and the source against it.
##
## WHY THIS EXISTS, AND WHY IT IS NOT DOCUMENTATION.
##
## The workbook is the whole input surface: three sheets, 28 + 31 + 16 named things, read by FOUR
## different readers (actaInfoValue() in the package, infoVal()/resolvePath() in the Shiny app,
## infoVal() in the standalone dashboard, and bare `metadata_file$X` in the pipeline). Nothing
## connected them. A column could be added to the template with no code behind it, read by code and
## absent from the template, or renamed in one reader and not the others, and every one of those is
## SILENT -- each reader is written to return NA for a name it cannot find, because every Info
## setting is legitimately optional.
##
## That is the shape that has already cost this project: a reader that cannot read reports "clean".
## The registry turns each of those into a test failure instead.
##
## FOUND BY THE FIRST RUN OF THAT TEST, and both were invisible to every other gate:
##   * ACTA_App.R read `Info!ELN_ID` to name the archive folder. ELN_ID moved to Layout_Plate and
##     no template has carried the Info row since; the lookup therefore always returned NA and every
##     app-driven copy archived into `Archived/<stamp>_run`.
##   * `Info!scrub_eln_placeholder` is read, validated and can stop a run, and appears in no
##     template -- so the only way to discover it was to read the source.
##
## WHAT A ROW MEANS
##   sheet    Info | Layout_Plate | gating_template | TiterPubs_template
##   name     the BARE name -- no annotation. actaBareName() is what makes that safe.
##   hint     the annotation the shipped template writes in brackets, "" for none. This records what
##            the workbook SAYS, not what it ought to say: see the `(n)` note below.
##   col_req  the COLUMN must exist, or pre-flight fails. Asserted by deleting it and running the
##            real pre-flight -- not by grepping for the check.
##   req      the cell must be FILLED. Asserted against all three OQ fixtures.
##   status   current   in the shipped template, and meant to be.
##            site_only read by code, deliberately NOT offered to an operator.
##            legacy    still accepted so an older workbook keeps working, superseded by something
##                      else, and NOT in the template. A legacy row names its replacement in `note`.
##   note     one line, for the operator.
##
## THE `status` FIELD IS NOT BOOKKEEPING. Three of the four disagreements this registry found on its
## first run were deliberate back-compat that nothing had written down -- `tabbed_by`, and the Info
## fallbacks for ELN_ID / ELN_Name. Undeclared, each one is indistinguishable from the fourth, which
## was a genuine defect. Declaring them is what makes the next disagreement mean something.
##
## col_req AND req ARE DIFFERENT QUESTIONS and the template must eventually say both. `Fix` is a
## required COLUMN whose blank cell means FALSE; `StainQty` is required to be filled or there is no
## titration axis. 3.6.13's `(req)` marker is the second one.
## =================================================================================================

## THE CLOSED ANNOTATION VOCABULARY. A header may carry one of these, or a choices list written as
## "a | b | c". Anything else is a typo, and test_column_registry.R refuses it.
##
## $PnS / $PnN are in the set because the template genuinely uses them: those two columns hold FCS
## keyword values, and naming the keyword is the clearest hint either column can carry.
ACTA_COLUMN_HINTS <- c("L", "N", "path", "col", "list", "$PnS", "$PnN")

## THE ONE NON-CANONICAL ANNOTATION IN THE SHIPPED TEMPLATE, pinned by name rather than tolerated by
## a loosened rule. Layout_Plate writes `Combinatorial_group (n)` where every other numeric column
## writes `(N)`. It is harmless today -- actaBareName() is case-blind about what it strips -- so
## fixing it means editing a workbook, which is a 3.6.13 change. Pinned here so that:
##   * a SECOND lowercase hint fails the test rather than joining it, and
##   * when 3.6.13 canonicalises it, the test fails until this list is emptied.
ACTA_COLUMN_HINT_CASE_DEVIATIONS <- c("Combinatorial_group")

.actaReg <- function(sheet, name, hint = "", col_req = FALSE, req = FALSE, note = "",
                     status = "current")
  data.frame(sheet = sheet, name = name, hint = hint, col_req = col_req, req = req,
             status = status, note = note, stringsAsFactors = FALSE)

actaColumnRegistry <- function() {
  r <- function(...) .actaReg(...)
  out <- rbind(

    ## ---------------------------------------------------------------------------- Layout_Plate --
    ## 96 rows, one per well. col_req here is what actaPrevalidateRecords() fails on.
    r("Layout_Plate", "Row",                   "",      TRUE,  TRUE,  "Plate row letter, A-H."),
    r("Layout_Plate", "Column",                "N",     TRUE,  TRUE,  "Plate column number, 1-12."),
    r("Layout_Plate", "PlateID",               "N",     TRUE,  TRUE,  "Plate number; also the FCS subfolder token."),
    r("Layout_Plate", "ELN_Name",              "",      TRUE,  FALSE, "Experiment name. Per well, so a run may carry more than one."),
    r("Layout_Plate", "ELN_ID",                "",      TRUE,  FALSE, "Experiment id. THE one source -- the Info sheet does not hold it."),
    r("Layout_Plate", "Markername",            "$PnS",  FALSE, FALSE, "Titrated marker, as the FCS $PnS keyword spells it. This column OR Channel."),
    r("Layout_Plate", "Channel",               "$PnN",  FALSE, FALSE, "Detector, as the FCS $PnN keyword spells it. This column OR Markername."),
    r("Layout_Plate", "Dirname",               "",      TRUE,  TRUE,  "FCS subfolder for this titration. At least one well must name one."),
    r("Layout_Plate", "Alias",                 "",      FALSE, FALSE, "Short label for the titrated reagent; names the plot and the export row."),
    r("Layout_Plate", "Vendor",                "",      FALSE, FALSE, "Antibody supplier."),
    r("Layout_Plate", "Cat_Num",               "",      FALSE, FALSE, "Catalogue number. Joins this reagent to the Master Panel Inventory."),
    r("Layout_Plate", "Clone",                 "",      FALSE, FALSE, "Antibody clone."),
    r("Layout_Plate", "Lot_Num",               "",      FALSE, FALSE, "Antibody lot."),
    r("Layout_Plate", "Cell_Type",             "",      FALSE, FALSE, "Material stained; falls back to Info!Cell_Type_Fallback."),
    r("Layout_Plate", "Tissue_Type",           "",      FALSE, FALSE, "Tissue of origin; falls back to Info!Tissue_Fallback."),
    r("Layout_Plate", "Sample_Status",         "",      FALSE, FALSE, "Fresh / Frozen / Thawed."),
    r("Layout_Plate", "Culture_Status",        "",      FALSE, FALSE, "Cultured / Uncultured."),
    r("Layout_Plate", "e6_cells_per_well",     "N",     FALSE, FALSE, "Millions of cells per well."),
    r("Layout_Plate", "Fix",                   "L",     TRUE,  FALSE, "TRUE if fixed. Blank reads as FALSE, but the COLUMN must be present."),
    r("Layout_Plate", "Perm",                  "L",     TRUE,  FALSE, "TRUE if permeabilised. Blank reads as FALSE."),
    r("Layout_Plate", "StainQty",              "N",     TRUE,  TRUE,  "Antibody amount for this well. The titration axis."),
    r("Layout_Plate", "StainUnit",             "",      FALSE, FALSE, "Unit for StainQty, e.g. uL or ug."),
    r("Layout_Plate", "StainType",             "",      TRUE,  TRUE,  "Test / Costain / Unstained. Costain sets the gate floor."),
    r("Layout_Plate", "Operator",              "",      TRUE,  FALSE, "Who stained the plate. Per well."),
    r("Layout_Plate", "SOP",                   "",      FALSE, FALSE, "Governing SOP. Scrubbed to SOP-000 when scrub_confidential is on."),
    r("Layout_Plate", "Well_volume_uL",        "",      FALSE, FALSE, "Staining volume, for the concentration axis."),
    r("Layout_Plate", "Combinatorial_group",   "n",     FALSE, FALSE, "Number grouping co-titrated reagents. Blank = not combinatorial."),
    r("Layout_Plate", "Costain_Panel_Iteration","",     FALSE, FALSE, "Which Master Panel Inventory costain panel these wells used."),

    ## ------------------------------------------------------------------------------------ Info --
    ## Long sheet: Category | Variable | Value | Description. The Info sheet as a whole is OPTIONAL
    ## -- every setting has a documented default -- so NOTHING here is col_req. That is also why a
    ## misspelt Variable is silent, and why this registry matters most on this sheet.
    r("Info", "Template_Version",         "",     FALSE, TRUE,  "Template version. Gated as a FLOOR: ACTA requires 3.6.8 or later."),
    r("Info", "working_directory",        "",     FALSE, FALSE, "Run root. Blank = the workbook's own folder."),
    r("Info", "compensation",             "",     FALSE, FALSE, "none / self / a spillover .csv path."),
    r("Info", "miflowcyt_export",         "L",    FALSE, FALSE, "TRUE writes the MiFlowCyt metadata appendix."),
    r("Info", "dashboard_dir",            "",     FALSE, FALSE, "Where the dashboard .html is written."),
    r("Info", "titration_export_dir",     "",     FALSE, FALSE, "Folder whose immediate subfolders are runs, for the dashboard."),
    r("Info", "dashboard_tabbed_by",      "",     FALSE, FALSE, "Export column the dashboard tabs by."),
    r("Info", "MM_R2_threshold",          "",     FALSE, FALSE, "Minimum R-squared for a Michaelis-Menten fit to be reported. Default 0.9."),
    r("Info", "Downsample_to",            "N",    FALSE, FALSE, "Events per file to downsample to. Blank = no downsampling."),
    r("Info", "flowjo_transformation_arg","",     FALSE, FALSE, "Biexponential arguments. RUN-LEVEL since 3.6.8 -- parsed, never evaluated."),
    r("Info", "keywords",                 "",     FALSE, FALSE, "Extra FCS keywords to carry into the export."),

    ## ---- read by code, deliberately not offered in the template ----
    r("Info", "scrub_eln_placeholder",    "",     FALSE, FALSE,
      "Stand-in id used when scrub_confidential renames files. Site option: validated, and a bad value stops the run.",
      "site_only"),

    ## ---- accepted so an older workbook keeps working ----
    ## These are NOT dead code and NOT defects. Each is a documented back-compat path with a named
    ## replacement, and each was invisible until the registry asked who reads what.
    r("Info", "tabbed_by",                "",     FALSE, FALSE,
      "Superseded by dashboard_tabbed_by (renamed 2026-08-19). Read only when that is blank. OQ_Test1 still carries it, which is what exercises the path.",
      "legacy"),
    r("Info", "ELN_ID",                   "",     FALSE, FALSE,
      "Superseded by Layout_Plate!ELN_ID, which is per well. Read by .mfcElnRef() only when the stats carry none.",
      "legacy"),
    r("Info", "ELN_Name",                 "",     FALSE, FALSE,
      "Superseded by Layout_Plate!ELN_Name. Read by .mfcElnRef() only when the stats carry none.",
      "legacy"),

    r("Info", "titerpubs_export",         "",     FALSE, FALSE, "TRUE builds the TiterPubs deposit bundle."),
    r("Info", "scrub_confidential",       "",     FALSE, FALSE, "TRUE removes site identifiers before anything is named. Leave it on for any run that may be published."),
    r("Info", "Organization/Affiliation", "",     FALSE, FALSE, "Submitting organisation, for the bundle."),
    r("Info", "Submitter_Name",           "",     FALSE, FALSE, "Depositor name."),
    r("Info", "Submitter_ORCID",          "",     FALSE, FALSE, "Depositor ORCID."),
    r("Info", "Submitter_Email",          "",     FALSE, FALSE, "Depositor email. PUBLISHED in the bundle."),
    r("Info", "Quality_Control_Measures", "",     FALSE, FALSE, "Free text, published."),
    r("Info", "Collection_Software",      "",     FALSE, FALSE, "Acquisition software and version."),
    r("Info", "Target_Species",           "",     FALSE, FALSE, "Species of the stained material."),
    r("Info", "Cell_Type_Fallback",       "",     FALSE, FALSE, "Used when Layout_Plate!Cell_Type is UNRECOGNISED."),
    r("Info", "Sample_Details",           "",     FALSE, FALSE, "Free text about the material, published."),
    r("Info", "Tissue_Fallback",          "",     FALSE, FALSE, "Used when Layout_Plate!Tissue_Type is EMPTY. Different rule from Cell_Type_Fallback."),
    r("Info", "References",               "",     FALSE, FALSE, "Citation(s) for the bundle."),

    r("Info", "Incubation_Time",          "",     FALSE, FALSE, "Stain time in minutes."),
    r("Info", "Incubation_Temperature_C", "",     FALSE, FALSE, "Stain temperature in Celsius."),
    r("Info", "Preparation_Protocol",     "",     FALSE, FALSE, "How the material was prepared."),
    r("Info", "Protocol_reference",       "",     FALSE, FALSE, "URL or citation for the staining protocol."),
    r("Info", "Equipment_Make",           "",     FALSE, FALSE, "Overrides $CYT, which is unreliable: Bio-Rad writes it, BD writes nothing."),
    r("Info", "Equipment_Model",          "",     FALSE, FALSE, "Overrides $CYT."),
    r("Info", "master_panel_inventory_path","",   FALSE, FALSE, "Master Panel Inventory workbook. Blank = look beside the instructions."),

    ## -------------------------------------------------------------------------- gating_template --
    ## THE ONE SHEET READ RAW. openCyto matches the first ten columns BY EXACT NAME, so none of them
    ## may ever carry an annotation -- test_column_registry.R asserts that, and it is the reason
    ## actaStripHeaderAnnotation() is NOT applied to this sheet.
    r("gating_template", "alias",                 "", TRUE,  TRUE,  "openCyto. Node name. One row's alias must be Reagent or Antibody."),
    r("gating_template", "pop",                   "", TRUE,  TRUE,  "openCyto. Which side of the gate to keep."),
    r("gating_template", "parent",                "", TRUE,  TRUE,  "openCyto. Parent node."),
    r("gating_template", "dims",                  "", TRUE,  TRUE,  "openCyto. Channel(s) the gate is drawn in."),
    r("gating_template", "gating_method",         "", TRUE,  TRUE,  "openCyto. Gating function."),
    r("gating_template", "gating_args",           "", FALSE, FALSE, "openCyto. Arguments to it. Unknown keys are silently ignored."),
    r("gating_template", "collapseDataForGating", "", FALSE, FALSE, "openCyto."),
    r("gating_template", "groupBy",               "", FALSE, FALSE, "openCyto."),
    r("gating_template", "preprocessing_method",  "", FALSE, FALSE, "openCyto."),
    r("gating_template", "preprocessing_args",    "", FALSE, FALSE, "openCyto."),
    r("gating_template", "default_gating_args_reference",        "", FALSE, FALSE, "ACTA. Documentation only; nothing reads it."),
    r("gating_template", "default_preprocessing_args_reference", "", FALSE, FALSE, "ACTA. Documentation only; nothing reads it."),
    r("gating_template", "help_notes",            "", FALSE, FALSE, "ACTA. Documentation only; nothing reads it."),
    r("gating_template", "Layout_Report",         "", FALSE, FALSE, "ACTA flag: include this node's plot in the report."),
    r("gating_template", "Layout_Export",         "", FALSE, FALSE, "ACTA flag: include this node in the layout export."),
    r("gating_template", "Stats_Export",          "", FALSE, FALSE, "ACTA flag: include this node's stats in the export."),

    ## ----------------------------------------------------------------------- TiterPubs_template --
    ## Per-reagent overrides for the deposit. Absent or blank, every field takes its documented
    ## default -- the sheet exists for the first run that needs to contradict one.
    r("TiterPubs_template", "Alias",                   "", FALSE, FALSE, "Joins to Layout_Plate!Alias."),
    r("TiterPubs_template", "Target_Location",         "", FALSE, FALSE, "Surface or Intracellular. Default Surface; decides the fixation answer."),
    r("TiterPubs_template", "Secondary_Conjugate",     "", FALSE, FALSE, "For an indirectly conjugated reagent."),
    r("TiterPubs_template", "Secondary_Supplier",      "", FALSE, FALSE, "Secondary antibody supplier."),
    r("TiterPubs_template", "Secondary_Cat_Num",       "", FALSE, FALSE, "Secondary antibody catalogue number."),
    r("TiterPubs_template", "Secondary_Concentration", "", FALSE, FALSE, "Secondary antibody concentration.")
  )
  rownames(out) <- NULL
  out
}

## The ten columns openCyto matches by exact name. Separated from the registry's `sheet` field
## because the distinction is not "which sheet" but "who reads it" -- these are the ones ACTA may
## never rename, annotate or reorder, whatever it does to its own six.
ACTA_OPENCYTO_COLUMNS <- c("alias", "pop", "parent", "dims", "gating_method", "gating_args",
                           "collapseDataForGating", "groupBy", "preprocessing_method",
                           "preprocessing_args")

## Layout_Plate needs ONE OF these, not both -- actaPrevalidateRecords()'s `marker_column` check.
## An either/or cannot be expressed as a per-row col_req flag, so it is named here and asserted
## separately.
ACTA_MARKER_OR_CHANNEL <- c("Markername", "Channel")

## A row's `status` says whether it belongs in the shipped template. Only "current" does.
ACTA_COLUMN_STATUS <- c("current", "site_only", "legacy")

## ACTA's own three gating_template columns, as opposed to openCyto's ten. These are read through
## actaTemplateFlag(), which matches CASE-INSENSITIVELY and then on the hint-stripped name -- so the
## OQ fixtures' lowercase `stats_export` and a future `Stats_Export (L)` both resolve. openCyto's
## ten get neither courtesy, and must never be given one.
ACTA_GATING_FLAG_COLUMNS <- c("Layout_Report", "Layout_Export", "Stats_Export")
