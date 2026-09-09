options(repos = c(CRAN = "https://cloud.r-project.org"))
if(!requireNamespace("BiocManager")){
  install.packages("BiocManager", quietly=TRUE)
}
if(!requireNamespace("purrr")){
  install.packages("purrr", quietly=TRUE)
}
##BiocManager::install("flowMeans")
##remotes::install_github("RGLab/ggcyto")
library(purrr)
## Attached packages, and ONLY the ones actually called. Setup.R parses this vector out
## of the source, so it is also the install manifest -- every name here is something a new user
## is made to download. Audited 2026-08-19 against every exported function referenced anywhere in
## the codebase: ggthemes, writexl, flowStats, patchwork, ggpubr, future, furrr and concaveman had
## ZERO call sites and were removed. flowStats is worth a note -- openCyto does NOT import it, so
## dropping it was verified by running both OQ cases (which use singletGate, gate_flowclust_2d and
## flowMeans) rather than assumed from the dependency graph.
listOfLibrary<-c("flowMeans","this.path","lubridate","dplyr","tidyr","readxl","openxlsx","rstudioapi","ggplot2","flowCore","ggcyto","openCyto","flowWorkspace","grDevices","BiocManager","stringr","knitr","kableExtra","diptest","mclust","ggplate")
walk(listOfLibrary, ~{
  if (!requireNamespace(.x, quietly = TRUE)) {
    message("Trying CRAN for: ", .x)
    tryCatch(install.packages(.x), error = function(e) NULL)
  }
  if (!requireNamespace(.x, quietly = TRUE)) {
    message("CRAN failed for: ", .x, ". Trying Bioconductor...")
    tryCatch(BiocManager::install(.x, ask = FALSE, update = FALSE),
             error = function(e) NULL)
  }
  library(.x, character.only = TRUE)
})

## Robust working-directory set for RStudio *and* Positron / VS Code. IMPORTANT: in
## Positron you must SOURCE the file -- running code line-by-line in the console gives
## no script context (both this.path() and rstudioapi return nothing).
##  - this.path() (NORMALISED, absolute) is the primary: correct under Source, Rscript
##    AND knit. Do NOT use this.path(original=TRUE) here: when the .Rmd sources this
##    script by a RELATIVE name, original=TRUE hands back that bare filename, dirname()
##    is then "." and the setwd() below silently becomes a NO-OP -- the run inherits
##    whatever folder the caller happened to be in. That is how an OLD version folder's
##    ACTA_Functions.R got sourced into a 2_82 run ("unused argument (label = ...)").
##  - original=TRUE is kept only as a fallback for Positron, where it can return a
##    *percent-encoded file:// URI* (e.g. file:///a%40b/c%20d/ACTA_Script.R) that must be both
##    URLdecode()d (%40->@, %20->space, %26->&) and stripped of its scheme.
##  - rstudioapi is the LAST resort: it reports the file focused in the EDITOR, which may
##    belong to a different version folder -- hence the validation below.
##  - ACTA_WORK_DIR, if it already exists in the evaluation environment, OVERRIDES all of the
##    above. That is the hook run_acta() uses: sourcing this script into a fresh environment with
##    the folder pre-set means the caller does not depend on this.path() resolving under whatever
##    harness is driving it (Shiny, testthat, a scheduler). Nothing sets it in a normal Source or
##    knit, so the interactive path below is unchanged.
acta_path <- NA_character_
acta_dir_override <- if (exists("ACTA_WORK_DIR", inherits = TRUE)) get("ACTA_WORK_DIR") else NULL
if (!is.null(acta_dir_override)) {
  acta_path <- file.path(normalizePath(acta_dir_override, mustWork = TRUE), "__acta_work_dir__")
}
if (is.na(acta_path))
  acta_path <- tryCatch(this.path(), error = function(e) NA_character_)
if (length(acta_path) != 1 || is.na(acta_path) || !nzchar(acta_path)) {
  acta_path <- tryCatch(this.path(original = TRUE), error = function(e) NA_character_)
}
if (length(acta_path) != 1 || is.na(acta_path) || !nzchar(acta_path)) {
  acta_path <- tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
      rstudioapi::getSourceEditorContext()$path else NA_character_
  }, error = function(e) NA_character_)
}
if (length(acta_path) != 1 || is.na(acta_path) || !nzchar(acta_path)) {
  stop("Could not resolve the script location. In Positron/VS Code you must SOURCE the file (the line-by-line console has no path context).")
}
## Only URLdecode/strip when it really is a file:// URI -- a plain path may legitimately
## contain a '%', which URLdecode would corrupt.
acta_dir <- if (grepl("^file://", acta_path))
  dirname(sub("^file://(localhost)?", "", URLdecode(acta_path))) else dirname(acta_path)
acta_dir <- normalizePath(acta_dir, mustWork = TRUE)
setwd(acta_dir)
## Source the helpers by ABSOLUTE path out of the script's OWN folder -- never a bare
## list.files(path=".") that depends on the inherited working directory.
## CODE vs WORK directory. ACTA_CODE_DIR, when set, is where the helpers (and the .Rmd, and the
## dashboard generator) live; acta_dir remains where the DATA and OUTPUTS live. They are the same
## folder in normal use. The split exists so a diagnostic case can carry only its inputs and be
## driven by the working version's code, instead of holding a second copy that has to be kept in
## sync -- a duplicate that silently drifts is worse than no test at all.
acta_code_dir <- if (exists("ACTA_CODE_DIR", inherits = TRUE))
  normalizePath(get("ACTA_CODE_DIR"), mustWork = TRUE) else acta_dir
## The helper library arrives ONE OF TWO WAYS, and which one is not a preference -- it is a fact
## about how ACTA was deployed:
##   sibling file   a version folder holds ACTA_Functions.R beside this script  -> source() it
##   namespace      installed as a package; the helpers are in the ACTA namespace and inst/pipeline/
##                  deliberately has no ACTA_Functions.R to source
## "Exactly one or die" is KEPT for the sibling case -- picking up an archived version's helpers is a
## real failure mode and the two fail-fast checks below exist because of it. Zero is only acceptable
## when the namespace can supply them instead.
ACTAFunctionFile<-list.files(path=acta_code_dir, pattern="^ACTA_Function.*\\.R$", full.names=TRUE)
if (length(ACTAFunctionFile) > 1)
  stop(sprintf("Expected at most one ACTA_Function*.R in the code folder '%s'; found %d.",
               acta_code_dir, length(ACTAFunctionFile)))
if (length(ACTAFunctionFile) == 1) {
  message("ACTA: sourcing helpers from ", ACTAFunctionFile)
  source(ACTAFunctionFile, local=TRUE)
} else if (requireNamespace("ACTA", quietly=TRUE)) {
  message("ACTA: helpers from the installed ACTA package (", utils::packageVersion("ACTA"), ")")
  library(ACTA)
} else {
  stop(sprintf(paste0("No ACTA_Function*.R in the code folder '%s' and the ACTA package is not ",
                      "installed, so the helper library cannot be loaded from either source."),
               acta_code_dir), call. = FALSE)
}
## The report render shells out to a LaTeX engine, and kpsewhich/perl live in the system
## directories -- so a PATH missing them turns every correct number into a failed PDF. Repaired here,
## once the helpers are available. See actaRepairPath().
invisible(actaRepairPath())

## Fail fast, and legibly, if an OUTDATED helpers file was picked up (e.g. from an
## archived version folder) instead of erroring deep inside the plot lapply with
## "unused argument (label = abLabel)".
if (!"label" %in% names(formals(makeGateLayoutGreatAgain)))
  stop(sprintf(paste0("Outdated ACTA_Functions.R sourced from '%s' -- it predates the 2_82 ",
                      "caption/subtitle API (makeGateLayoutGreatAgain has no `label` argument). ",
                      "Check that the script is running from its own version folder."),
               ACTAFunctionFile))
## Same fail-fast for the vendor-agnostic scatter matchers, which the boundary map and the
## pseudocolor y dim both depend on -- without this the run would die mid-loop with a bare
## "could not find function isScatterChannel".
## Plain lexical lookup inside tryCatch, NOT exists(): with `source(local=TRUE)` the helpers may
## land in a knitr chunk environment, and exists() called from inside an *apply frame searches
## that frame's enclosure (namespace:base -> global) and would not see them.
if (inherits(tryCatch(list(isScatterChannel, pickForwardScatter), error = function(e) e), "error"))
  stop(sprintf(paste0("Outdated ACTA_Functions.R sourced from '%s' -- it predates the vendor-agnostic ",
                      "scatter-channel matchers (isScatterChannel / pickForwardScatter). ",
                      "Check that the script is running from its own version folder."),
               ACTAFunctionFile))

# Reading metadata file
path<-paste0(getwd(),"/Titration_FCS")
## Discovery lives in actaFindInstructions() -- see its note for the anchoring bug and for the two
## filenames (Excel's `~$` lock file, macOS's `._` resource fork) that an anchored pattern still
## matches. It used to be spelled out here AND in the OQ runner, and the two copies drifted.
metadata_file_name<-actaFindInstructions()
## EXACTLY ONE, with a message that names the 2_87 rename.
##
## There was no check here at all: an empty match left metadata_file_path as just the folder path and
## the failure surfaced further down as a generic excel_sheets() error about a file that does not
## exist. Two matches were worse -- the path became a VECTOR and the error was stranger still.
##
## The rename hint matters because every existing 2_87 folder holds the file under its old name.
## No back-compat shim, in line with the workbook being version-locked: the file is renamed once,
## deliberately, and the error says so instead of sending someone hunting for a file that is sitting
## right there spelled differently.
if (length(metadata_file_name) != 1) {
  oldName <- grep("Titration_Layout", list.files(pattern="[.]xlsx$"), value=TRUE)
  stop(sprintf(paste0("Expected exactly one *Titration_Instructions*.xlsx in '%s'; found %d.%s"),
               getwd(), length(metadata_file_name),
               if (length(oldName))
                 sprintf(paste0("\n  Found '%s' instead. As of 2_87 the layout workbook is called the ",
                                "INSTRUCTIONS file: rename Titration_Layout -> Titration_Instructions ",
                                "and re-run."), oldName[1])
               else if (!length(metadata_file_name)) "\n  No .xlsx in this folder matches that pattern."
               else sprintf("\n  Matched: %s", paste(metadata_file_name, collapse=", "))),
       call. = FALSE)
}
metadata_file_path<-paste0(getwd(),"/",metadata_file_name) ## This picks up the location of the file
## Single layout sheet as of 2_85: the workbook has ONE sheet named `Layout_Plate` (it was
## "Layout Plate 1" + "Layout Plate 2" row-bound together before). Fail with a clear message
## rather than readxl's generic error if an older two-sheet workbook is used by mistake.
if (!"Layout_Plate" %in% excel_sheets(metadata_file_path))
  stop(sprintf(paste0("'%s' has no `Layout_Plate` sheet (found: %s). From 2_85 the layout is a ",
                      "SINGLE sheet named `Layout_Plate`; a pre-2_85 workbook with 'Layout Plate 1'/",
                      "'Layout Plate 2' needs the second sheet removed and the first renamed."),
               basename(metadata_file_path),
               paste(excel_sheets(metadata_file_path), collapse=", ")))
metadata_file<-read_excel(metadata_file_path, sheet="Layout_Plate")
## Headers carry self-documenting annotations -- `Markername ($PnS)`, `Fix (L)`, `StainQty (N)`
## -- which are stripped here so every read site downstream uses the plain name. See
## actaStripHeaderAnnotation(). Excel note: the sheet is a TABLE (Cmd+T), so a header renamed in
## the sheet must also be renamed in xl/tables/table1.xml or Excel reports the file as damaged.
names(metadata_file)<-actaStripHeaderAnnotation(names(metadata_file))
gating_template<-read_excel(metadata_file_path,sheet="gating_template")
## Run-level settings sheet. Only `compensation` is read from it so far; the sheet is optional so
## a workbook without it still runs (actaCompMode() returns "none" for a NULL info table).
infoSheet<-if ("Info" %in% excel_sheets(metadata_file_path))
             read_excel(metadata_file_path, sheet="Info") else NULL
## Info headers are annotated the same way Layout_Plate's are (`dashboard_creation (L)`), so they
## get the same strip -- otherwise a reader keyed on `compensation` would miss `compensation (T)`.
if (!is.null(infoSheet)) names(infoSheet)<-actaStripHeaderAnnotation(names(infoSheet))
## "csv" | "self" | "none" -- see actaCompensate(). Resolved ONCE for the whole run, because a
## titration compares wells to each other and compensating them differently would invalidate
## every cross-well comparison the report makes.
## --- optional per-RUN event cap -------------------------------------------------------------
## Info!Downsample_to: a whole number of events to read from each FCS file. BLANK/NA means read
## everything, which is the historical behaviour and the default in every shipped workbook.
## Validated HERE, before any data is touched, so a typo cannot get as far as the loader.
actaDownsampleTo <- local({
  v <- actaInfoValue(infoSheet, "Downsample_to")
  if (is.na(v)) return(NA_integer_)
  n <- suppressWarnings(as.numeric(v))
  if (!is.finite(n) || n < 1 || n != floor(n))
    stop(sprintf(paste0("Info!Downsample_to must be a whole number of events, or blank for no ",
                        "downsampling; got '%s'."), v), call. = FALSE)
  as.integer(n)
})
if (!is.na(actaDownsampleTo))
  message(sprintf("[ACTA] Downsample_to = %s events per FCS file (random subsample, seeded).",
                  format(actaDownsampleTo, big.mark = ",")))

## --- RNG ------------------------------------------------------------------------------------
## gate_flowclust_2d (and flowMeans) initialise randomly, so two runs over identical data used to
## disagree -- %Live drifted by ~2.5 points between renders, which makes a report impossible to
## regenerate. Pinning the seed makes a run reproducible.
## Re-set at the top of EVERY antibody iteration, not just here: seeding once would make each
## group's gates depend on how many groups ran before it, so adding an antibody would silently
## move the gates of the ones after it.
ACTA_SEED <- 123
set.seed(ACTA_SEED)

compMode<-actaCompMode(infoSheet)
## Marker column: `Markername` as of 2_85, `Dim` before that. Resolved once here rather than per
## antibody so the deprecation notice prints once, not once per group.
## Matched by NORMALISED name, so the headers' `($PnS)` / `($PnN)` suffixes are free to change and
## a pre-2_85 `Dim` sheet still resolves. See actaLayoutCol().
markerColName <-actaLayoutCol(metadata_file, ACTA_MARKER_COLS)
channelColName<-actaLayoutCol(metadata_file, ACTA_CHANNEL_COLS)
if (is.na(markerColName) && is.na(channelColName))
  stop("Layout_Plate has no marker column (`Markername ($PnS)`, or the legacy `Dim`) and no ",
       "`Channel ($PnN)` column; one of them must name the titrated parameter. Found: ",
       paste(names(metadata_file), collapse=", "))
if (!is.na(markerColName) && grepl("^dim$", tolower(trimws(markerColName))))
  message("[ACTA] Layout_Plate uses the legacy `Dim` header; it is read as `Markername ($PnS)`. ",
          "Rename it when convenient -- the suffix records which FCS slot the column matches: ",
          "`Markername ($PnS)` is markernames(), `Channel ($PnN)` is colnames().")
## Caption form of the same cell, shown on the layout plots so a reviewer can tell from the
## figure alone what the gate was drawn on. See actaCompLabel().
compLabel<-actaCompLabel(infoSheet)

## --- openCyto gating-template setup (shared across all antibodies) ----
gating_template<-gating_template |>
  ## Trim stray whitespace from every text cell FIRST. Excel forces a leading space
  ## when a cell value starts with + or - (e.g. the `pop` column "+/-"), and openCyto
  ## rejects ANY whitespace in `pop`/method names -- so strip it here. (Alternatively,
  ## in Excel prefix the value with an apostrophe -- '+/- -- to store it as text.)
  mutate(across(where(is.character), trimws)) |>
  ## Drop "commented-out" reference/example rows and stray blank rows: any row whose
  ## `alias` is blank/NA or starts with '#' is IGNORED by the whole pipeline (gt_gating,
  ## marker-row detection, and plot scaffolding). Use a leading '#' on the alias to keep
  ## an example row in the sheet for reference without it being read.
  dplyr::filter(!is.na(alias) & nzchar(alias) & !startsWith(alias, "#")) |>
  mutate(gating_method=gsub("(?i)^gate_flowclust[._]([12])d$", "gate_flowclust_\\1d",
                            gating_method, perl=TRUE),
         gating_method=ifelse(tolower(gating_method)=="gate_singlet", "singletGate",
                              gating_method),
         gating_args=gsub('"', "'", gating_args))

## ---- Per-population opt-in flags, CAPTURED BEFORE the openCyto column strip below ----------
## The strip keeps only openCyto's recognised columns, so layout_report / layout_export /
## stats_export must be lifted out FIRST. Reading them off the stripped template instead makes
## actaTemplateFlag() see an ABSENT column, which defaults to TRUE -- so a layout_report=FALSE
## would be silently ignored and the population would still be plotted. (That is exactly the bug
## observed on 2026-08-06: Single_cells set FALSE still appeared in the report.)
##
## Matched CASE-INSENSITIVELY, and for the same reason: the sheet's headers were lower-cased
## after these columns shipped as layout_report / layout_export / stats_export, and an exact
## any_of() match silently drops a renamed column -- which does not error, it just defaults every
## population back to TRUE. Accepting either spelling keeps the archived per-experiment workbooks
## working too. actaTemplateFlag() resolves the name the same way.
gatingFlags<-gating_template |>
  dplyr::select(dplyr::any_of("alias"),
                dplyr::any_of(names(gating_template)[
                  tolower(names(gating_template)) %in%
                    c("layout_report","layout_export","stats_export")]))

gating_template<-gating_template |>
  ## Keep ONLY openCyto's recognised columns before building the template. This lets you
  ## add free-form documentation columns to the sheet (e.g. `default_gating_args` listing
  ## every arg a method supports with its default, or notes) purely as a reminder -- they
  ## are dropped here and never reach gt_gating. any_of() tolerates a sheet that omits some
  ## optional columns.
  dplyr::select(dplyr::any_of(c("alias","pop","parent","dims","gating_method","gating_args",
                                "collapseDataForGating","groupBy","preprocessing_method","preprocessing_args")))
## Register the custom flowMeans gating plugin (defined in ACTA_Functions.R).
## tryCatch keeps re-sourcing within one R session from erroring on re-registration.
tryCatch(register_plugins(fun=gate_flowmeans, methodName="gate_flowmeans"),
         error=function(e) message("gate_flowmeans registration skipped: ", conditionMessage(e)))
tryCatch(register_plugins(fun=gate_flowmeans_costain, methodName="gate_flowmeans_costain"),
         error=function(e) message("gate_flowmeans_costain registration skipped: ", conditionMessage(e)))
tryCatch(register_plugins(fun=gate_auto_costain_dip, methodName="gate_auto_costain_dip"),
         error=function(e) message("gate_auto_costain_dip registration skipped: ", conditionMessage(e)))
tryCatch(register_plugins(fun=gate_auto_costain_gmm, methodName="gate_auto_costain_gmm"),
         error=function(e) message("gate_auto_costain_gmm registration skipped: ", conditionMessage(e)))
tryCatch(register_plugins(fun=gate_auto2_costain, methodName="gate_auto2_costain"),
         error=function(e) message("gate_auto2_costain registration skipped: ", conditionMessage(e)))
## Preprocessing plugin: computes the per-group (per-Dirname) Costain floor and
## feeds it to the *_costain gating methods via pp_res. type="preprocessing" is
## preprocessing method registration.
tryCatch(register_plugins(fun=pp_costain_floor, methodName="pp_costain_floor", type="preprocessing"),
         error=function(e) message("pp_costain_floor registration skipped: ", conditionMessage(e)))
## Preprocessing plugin for gate_auto2_costain: Costain floor + group-level
## static-vs-flowMeans decision (any sample >sat_threshold % positive -> static).
tryCatch(register_plugins(fun=pp_auto2_costain, methodName="pp_auto2_costain", type="preprocessing"),
         error=function(e) message("pp_auto2_costain registration skipped: ", conditionMessage(e)))
## gate_manual: returns the interactively pre-drawn gate from .acta_manual_cache (drawn
## by drawManualGates() before pass-2 gt_gating). pop="+/-" -> openCyto builds <alias>+/-.
tryCatch(register_plugins(fun=gate_manual, methodName="gate_manual"),
         error=function(e) message("gate_manual registration skipped: ", conditionMessage(e)))
## Identify the per-antibody marker gate row (alias "Reagent"/"Antibody", case-
## insensitive). Its x/first dim is filled PER ANTIBODY from the layout `Dim` column
## (resolved to a channel via `boundary`); any dim authored in the sheet for that row
## (e.g. "SSC-A"/"FSC-A") is kept as the y/second dim. Because that x dim differs per
## antibody, the gatingTemplate is (re)built inside the per-antibody loop below rather
## than once here. If no such row exists the template is antibody-independent and the
## per-antibody build is a harmless no-op patch.
## A quadrant method reads `pop`, not `alias` -- a row written the other way round dies inside
## openCyto with "subscript out of bounds" after every gate has been computed. Rewritten here, before
## anything is read. See actaQuadPopFix().
.quadFix <- actaQuadPopFix(gating_template)
gating_template <- .quadFix$template
for (.n in .quadFix$notes) message("[ACTA] ", .n)

## Reject pop values ACTA cannot represent BEFORE any data is touched -- see actaUnsupportedPops().
.badPops <- actaUnsupportedPops(gating_template$pop, gating_template$alias)
if (length(.badPops))
  stop(sprintf("gating_template contains unsupported pop value(s).\n%s",
               paste(.badPops, collapse = "\n")), call. = FALSE)
## Every row the scaffold plots is drawn as a 2D panel, so its `dims` must name two channels. One
## dim reached computePlotParams() as (x, NA) and stopped the run with "subscript out of bounds"
## AFTER the whole tree had been gated. Checked HERE, on the RAW sheet: before any FCS is read, and
## before the marker row's dims is rewritten per antibody (which would make it look 2D). The gate is
## not affected by the second dim -- see actaDimsNot2D().
.dims2D <- actaDimsNot2D(gating_template$dims, gating_template$alias)
for (.n in .dims2D$notes) message("[ACTA] ", .n)
## `fatal` now covers only what actaPlotDims() cannot rescue; a one-dim row is a NOTE.
if (length(.dims2D$fatal))
  stop(sprintf("gating_template row(s) have unusable dims.\n%s",
               paste(.dims2D$fatal, collapse = "\n")), call. = FALSE)
markerGateRowIdx <- which(tolower(trimws(gating_template$alias)) %in% c("reagent","antibody"))
if (length(markerGateRowIdx) != 1)
  stop(sprintf("gating_template must contain exactly one marker gate row (alias 'Reagent' or 'Antibody'); found %d. Marker gating is now fully template-driven.", length(markerGateRowIdx)))
reagentMethod <- gating_template$gating_method[markerGateRowIdx]  ## template gating method, used only for plot/stat labels
## Full provenance for the MARKER row, for the p_hist / p_pseudo captions -- the same treatment
## the layout plots get from strat[]. Those two plots show the marker gate, so their caption
## should name every argument that placed it, not just the method. One row, one template, so it
## is resolved once here rather than per antibody. wrapToWidth() reflows it at plot time, since
## only then is the device width known.
markerCaption <- gateCaption(gating_template[markerGateRowIdx, ], compensation=compLabel)
### Reading flowset per target
## The loop's unit of work is ONE TITRATED MARKER, which is not the same as one FCS folder:
## combinatorial titration puts several markers in the same wells, so one Dirname can carry several
## series. actaTitrationGroups() turns the layout's long rows into that unit and validates them.
## `antibody` stays the KEY -- what names plot files, keys qcList and labels the dashboard -- and it
## is still the Dirname for every workbook with one Alias per Dirname, so nothing moves for those.
actaGroups <- actaTitrationGroups(metadata_file, channel_col = channelColName)
if (!nrow(actaGroups))
  stop("Layout_Plate has no usable Dirname values, so there is nothing to titrate.", call. = FALSE)
antibody <- actaGroups$key
if (isTRUE(attr(actaGroups, "combinatorial")))
  message(sprintf(paste0("[ACTA] combinatorial titration: %d series across %d FCS folder(s), keyed ",
                         "by Alias where a folder carries more than one -- %s."),
                  nrow(actaGroups), length(unique(actaGroups$dirname)),
                  paste(sprintf("%s/%s", actaGroups$dirname, actaGroups$key), collapse = ", ")))
## Marker column is `Markername` as of 2_85, `Dim` before it. Read here by name so a legacy sheet
## still resolves; `markerColName` is set just above.
Target<-if (is.na(markerColName)) character(0) else unique(metadata_file[[markerColName]])
Target<-Target[!is.na(Target)] ## Must match a markername ($PnS); see resolveMarkerChannel()
## Layout sanity check up front, before any FCS is read: duplicate StainQty in a titration
## series and a missing Costain well are hard errors; >1 Costain / >1 Unstained only warn.
## See validateLayoutGroups() for why each severity was chosen.
actaPrevalidate(metadata_file, gating_template, infoSheet, actaGroups,
                fcs_root = path,
                marker_col = markerColName, channel_col = channelColName)
## Biexp maxValue read from the instrument ($PnR), ONCE for the whole run and applied to every
## group -- see resolveInstrumentMaxValue(). Any disagreement between files is a hard error,
## because a per-group transform would make Stain Indices incomparable within one experiment.
actaMaxValue <- resolveInstrumentMaxValue(
  list.files(path, pattern = "\\.fcs$", recursive = TRUE, full.names = TRUE))
message(sprintf("ACTA: biexp maxValue = %s from $PnR (%.2f decades)",
                format(actaMaxValue, scientific = FALSE), log10(actaMaxValue)))
## One transformer for the whole run: used both to transform the GatingSet and to build the
## decade axis breaks, so the axis can never drift from the transform actually applied.
## Transform parameters come from the layout when set, defaults otherwise. maxValue stays with
## $PnR. See actaTransArgs().
## actaGroups, not `antibody`: this is a run-level check over every row belonging to any group,
## and `antibody` now holds titration KEYS, which never match Dirname for a combinatorial folder --
## the match came back empty and an empty match is the "use defaults" path, so the sheet's transform
## was ignored without a word.
actaTransArg  <- actaTransArgs(metadata_file, actaGroups)
actaTrans <- do.call(actaBiexpTrans, c(list(maxValue = actaMaxValue), actaTransArg))
if (length(actaTransArg))
  message("[ACTA] biexp args from layout: ",
          paste(sprintf("%s=%s", names(actaTransArg), unlist(actaTransArg)), collapse = ", "))
## The transform AS APPLIED, for the report: actaBiexpTrans()'s own defaults, overridden by whatever
## the layout set, with maxValue prepended because it comes from $PnR and never from the sheet.
## Read off formals() rather than restated, so the table cannot drift from the function signature.
actaTransEff <- as.list(formals(actaBiexpTrans))
actaTransEff <- actaTransEff[setdiff(names(actaTransEff), "maxValue")]
actaTransEff <- lapply(actaTransEff, function(x) if (is.language(x) || is.symbol(x)) eval(x) else x)
actaTransEff <- c(list(maxValue = actaMaxValue), modifyList(actaTransEff, actaTransArg))
## Marker gating and its Costain floor are fully defined in the gating_template sheet
## (Reagent/Antibody row + pp_costain_floor, costain_quantile in preprocessing_args).

## --- gate_manual (interactive marker gating) setup ------------------------------------
## If the marker row's method is `gate_manual`, the marker gate is DRAWN interactively
## with flowGate (rectangle/polygon) AFTER the auto template builds its parent, via a
## two-pass gt_gating (drawManualGates() + the gate_manual plugin). Scope is driven by the
## marker row's `groupBy` column:
##   groupBy = a column (e.g. Dirname) = ONE gate per group value (Dirname => per antibody).
##   groupBy = EMPTY                   = PER-SAMPLE, one gate per well (warned about below).
## Drawn gates persist under gate_dir and are reused on re-run unless manualGateRedraw=TRUE.
manualGateRedraw <- FALSE
gate_dir <- file.path(getwd(), "Manual_Gates")
markerIsManual <- tolower(trimws(reagentMethod)) == "gate_manual"
manualGroupBy <- ""
if (markerIsManual) {
  manualGroupBy <- trimws(as.character(gating_template$groupBy[markerGateRowIdx]))
  if (length(manualGroupBy) != 1 || is.na(manualGroupBy)) manualGroupBy <- ""
  if (!nzchar(manualGroupBy)) {   # empty groupBy -> per-sample
    nTot <- nrow(dplyr::filter(metadata_file, !is.na(Dirname) & Dirname %in% actaGroups$dirname & !isLayoutValue(StainType, "Unstained")))
    warning(sprintf(paste0("gate_manual with EMPTY groupBy = PER-SAMPLE: you will draw an individual gate for every ",
                           "population -- %d samples total across the dataset. To draw ONE gate per group instead, ",
                           "put a grouping column (e.g. 'Dirname') in the marker row's groupBy column."), nTot))
  }
}

stats<-data.frame()
## Concatenated per-event marker values for ConcatPlot, one entry per antibody group. Collected in
## the loop because that is the only place the GATED data exists; the plot itself is built after the
## loop so all groups share one faceted figure, exactly as SIPlot does.
concatData<-list()
## Events kept per well for ConcatPlot. The figure is a 2D density, so past this the binning is
## already saturated while memory keeps climbing -- 21 wells x 3 groups of full-size files would be
## millions of rows for a picture that looks identical. Seeded, so the plot is reproducible.
CONCAT_MAX_PER_WELL <- 20000L
plotList_hist<-list()
plotList_pseudo<-list()
list_message<-list()
## One named list of gate-layout plots per antibody (one entry per gating_template
## population). Replaces the old plotList / plotLayout / plotLayout2 patchworks -- every
## plot now gets its own report page.
plotLayoutList<-list()
## Populations flagged layout_export=TRUE. SEPARATE from plotLayoutList because layout_report and layout_export are
## independent opt-ins: a population can be exported as a PNG without being printed in the
## report, and vice versa. The .Rmd reads plotLayoutList only; the ggsave loop reads this.
plotExportList<-list()
plotDims<-list()
## Report-facing plots are PAGINATED; the PNG exports are not. A page holds at most
## REPORT_FACETS_PER_PAGE facets in a fixed REPORT_NCOL-wide grid, so every report page has
## identical panel geometry, while Plots/*.png keep the whole series in one image (a PNG has
## no page, so facetLayout() already gives it constant panel size at any facet count).
REPORT_FACETS_PER_PAGE <- 12L
REPORT_NCOL            <- 4L
reportHist<-list(); reportPseudo<-list(); reportLayout<-list(); reportDims<-list()
## Accumulates the per-population stats for rows flagged stats_export=TRUE, across all antibodies.
## Written once after the loop, and ONLY if at least one row was flagged -- see StatsExport.
statsExportAll<-NULL
## Report QC, one entry per antibody (see actaQCChecks()). Rendered by the .Rmd's "QC" section;
## nothing here ever stops the run -- a failed check is information for the analyst, not a fault.
qcList<-list()
panelList<-list()
panelStr<-list()
## Marker-gate parent per antibody group, for the labels and filenames written after this loop.
parentPopByGroup <- character(0)
elnByGroup <- character(0)
## The analyst's own combinatorial declaration, per titration. Kept per group for the same reason
## elnByGroup is: the export is assembled after the loop and needs THIS group's value, not whatever
## the last iteration left behind. See actaCombinatorialGroup().
comboByGroup <- character(0)
## seq_along, not 1:length(): on an empty vector `1:length(x)` is c(1, 0) and the body runs
## TWICE with meaningless indices instead of not at all.
for(q in seq_along(antibody)){
  set.seed(ACTA_SEED)   ## per-group, so gates do not depend on how many groups ran before
  ## Marker gating is fully template-driven now (gating_template Reagent/Antibody row
  ## + pp_costain_floor). The layout Gating_method / Costain_quantile / Parent_pop
  ## columns have been retired and are no longer read here.
  ## `Markername` names the column for what it holds -- the value markernames() returns ($PnS).
  ## `Dim` is the pre-2_85 header for the same thing and is still read, so existing layouts and the
  ## per-experiment folders keep working untouched. Markername wins if a sheet somehow has both.
  ## THIS titration's rows. Filtering on Dirname alone was right only while a Dirname held one
  ## marker; with two it silently takes whichever comes first. actaTitrationRowMask() is the single
  ## definition of membership, Alias fallback included.
  .abDir  <- actaGroups$dirname[q]
  .abRows <- actaTitrationRowMask(metadata_file, .abDir, actaGroups$alias[q])
  targetName <- if (is.na(markerColName)) character(0) else metadata_file[[markerColName]][.abRows]
  targetName <- unique(targetName[!is.na(targetName)])   ## drop NA (Costain / blank wells) so the lookup gets one clean value
  ## Optional `Channel` column (2_85): names the detector ($PnN) directly, for panels where the
  ## markername route is unusable. any_of()-style guard so a workbook without the column -- i.e.
  ## every layout written before now -- still reads.
  channelName <- if (!is.na(channelColName)) metadata_file[[channelColName]][.abRows] else character(0)
  channelName <- channelName[!is.na(channelName)]
  aliasName <- actaGroups$alias[q]
  ## PLATE_1 only as of 2_85. The old code also read PLATE_2 and rbind2()'d it on, but with a
  ## single `Layout_Plate` sheet there are no plate-2 metadata rows for those wells to match:
  ## annotate_changeParam_pdWrite() would find no layout row, and the run would fail later with
  ## something obscure. So PLATE_2 is not read -- and if such a folder exists we WARN rather
  ## than skip it silently, since that would quietly drop real acquisitions.
  ## Dirname -> folder match is literal and case-insensitive, and accepts EITHER tree:
  ##   Titration_FCS/<antibody>/          (flat, 2_85 onward -- PLATE_* is gone from the layout)
  ##   Titration_FCS/PLATE_1/<antibody>/  (legacy, still read)
  ## Paths come back relative to Titration_FCS, so read.flowSet() below reads from that one root and
  ## both trees flow through unchanged. See actaFindAntibodyFcs().
  list_fcs1<-actaFindAntibodyFcs(path, .abDir)   ## the FOLDER == the Dirname, not the key
  l1<-length(list_fcs1)
  ## The FOLDER in the paths, the KEY in the label. They are the same thing for a single-marker
  ## folder and not for a combinatorial one, and quoting the key as a path sends the reader looking
  ## for a directory that was never searched -- the same confusion the pre-flight had.
  if(l1 == 0) stop(sprintf(paste0("No FCS files found for titration '%s'. Looked for '%s/<%s>/*.fcs' ",
                                  "and '%s/PLATE_1/<%s>/*.fcs'."),
                           antibody[q], path, .abDir, path, .abDir))
  extraPlates<-actaPlateExtras(path)
  if(length(extraPlates)){
    ## Emitted as a message() AND a warning() on purpose. A normal run raises >10 warnings from
    ## ggcyto/ggplot deprecations, at which point R collapses them to "There were N warnings"
    ## and the TEXT is never shown -- so a warning alone would itself be silent, which defeats
    ## the point of flagging dropped acquisitions. message() always prints; warning() keeps the
    ## right semantics and stays visible to warnings() and to tryCatch.
    extraMsg <- sprintf(paste0("ACTA: Titration_FCS contains %s, which is NOT read -- the layout is a ",
                               "single `Layout_Plate` sheet, so there are no plate-2 rows for those ",
                               "wells to match. Put the antibody folders directly under Titration_FCS ",
                               "(or under PLATE_1) and add their wells to the layout if they belong ",
                               "to this run."),
                        paste(extraPlates, collapse=", "))
    message(extraMsg)
    warning(extraMsg, call.=FALSE)
  }

  ## Read every event, or a seeded RANDOM subsample of Downsample_to events per file.
  ##
  ## PER FILE, not one which.lines for the whole set: read.flowSet() applies a single which.lines to
  ## every file, and flowCore ERRORS -- "cannot take a sample larger than the population" -- as soon
  ## as n exceeds a file's $TOT. Real runs have short wells: OQ_Test1 has one of 970 events against a
  ## 60,000 median. A set-wide value would therefore either crash or, if clamped to the smallest
  ## file, silently downsample the whole run to 970. A file with fewer than n events is read whole.
  ##
  ## `which.lines` given a single number is flowCore's OWN random-sample facility, so the sampling is
  ## its behaviour rather than something ACTA reimplements, and the seed set immediately below makes
  ## it reproducible -- same seed, same events, which is what the regression gate depends on.
  ##
  ## $TOT is deliberately NOT rewritten: it keeps reporting what the instrument acquired, and the
  ## number of events actually read is nrow(). The report's Downsample section states both.
  ##
  ## The no-downsampling branch is the ORIGINAL read.flowSet() call, untouched, so a run without
  ## Downsample_to set cannot move by a single bit.
  fsImported<-if (is.na(actaDownsampleTo)) {
    read.flowSet(list_fcs1, path=path, truncate_max_range = FALSE)
  } else {
    set.seed(ACTA_SEED)
    .frames<-lapply(list_fcs1, function(.fn){
      .p  <-file.path(path, .fn)
      .kw <-tryCatch(read.FCSheader(.p)[[1]], error=function(e) character(0))
      ## read.FCSheader returns a NAMED CHARACTER VECTOR, so [[ ]] on an absent name errors --
      ## hence the %in% check rather than a bare lookup.
      .tot<-if ("$TOT" %in% names(.kw)) suppressWarnings(as.integer(.kw[["$TOT"]])) else NA_integer_
      .wl <-if (!is.na(.tot) && .tot > actaDownsampleTo) actaDownsampleTo else NULL
      read.FCS(.p, which.lines=.wl, truncate_max_range = FALSE)
    })
    as(setNames(.frames, basename(list_fcs1)), "flowSet")
  }
  ## Events per sample -- READ (nrow) and ACQUIRED ($TOT) -- for the report's Downsample section and
  ## the OQ assertion. Recorded even when no downsampling happened, so the section can say so.
  ## Both are needed to tell "downsampling worked" from "this file never had that many events":
  ## which.lines leaves $TOT alone, so the pair is read == min(n, acquired). $TOT comes from the
  ## keywords already in memory, so this costs no extra file I/O on the normal path.
  actaEventsRead<-c(if (exists("actaEventsRead", inherits=FALSE)) actaEventsRead else NULL,
                    setNames(as.integer(fsApply(fsImported, nrow)), sampleNames(fsImported)))
  actaEventsAcquired<-c(if (exists("actaEventsAcquired", inherits=FALSE)) actaEventsAcquired else NULL,
                        setNames(as.integer(fsApply(fsImported, function(.fr){
                          .k<-flowCore::keyword(.fr)
                          if ("$TOT" %in% names(.k)) suppressWarnings(as.integer(.k[["$TOT"]])) else NA_integer_
                        })), sampleNames(fsImported)))

  ## THIS TITRATION'S LAYOUT ROWS ONLY. annotate_changeParam_pdWrite() matches each FCS to its
  ## layout row by PlateID + Row + Column -- the well -- and a combinatorial well has one row per
  ## co-titrated marker. Handed the whole sheet it would match N of them, and both the flowFrame
  ## description and pData would come back N-deep, breaking the one-row-per-sample assumption that
  ## everything downstream rests on. Scoped to the titration, each well matches exactly one row
  ## again, which is what it always did for a single-marker folder -- so this is a no-op there.
  fs<-annotate_changeParam_pdWrite(fsImported, metadata_file[.abRows, , drop = FALSE])
  fs<-fs[!isLayoutValue(pData(fs)$StainType, "Unstained")] ## Removes unstained sample (case-insensitive).
  ### Transformation
  notChannels<-c("AF-A","Time","INFO")
  channels<-setdiff(colnames(fs), notChannels)
  
  ## (2_85) The positional `mkname` fallback that used to sit here -- assigning a hardcoded Cytek
  ## Aurora panel by acquisition order when $PnS was absent -- has been removed. It was a manual
  ## override for one panel and is no longer relevant; marker names come from the FCS.
  ## Per-sample gating via the shared openCyto gatingTemplate (built above).
  list_channels<-list()
  list_index<-list()

  ## Creating gating set. The biexp is applied to the GatingSet as a registered TRANSFORMER
  ## (2_84), not by pre-transforming the flowSet as before: the transformed values are the
  ## same either way, but registering it is what lets ggcyto relabel the axes in raw units
  ## (0, 10^2 ... 10^6) via axis_*_inverse_trans() instead of showing linear channel numbers.
  ## Compensation FIRST, then the biexp transformer: un-mixing is a linear operation on raw
  ## channel values, so it has to happen before any non-linear transform. Identity in the "none"
  ## branch, so fs_comp is a compensated set in every case and there is one downstream path.
  ## NAMING: `fs_comp` is a flowCore **flowSet**, like the `fs` it comes from (read.flowSet
  ## above). The pipeline only becomes cytoset/cytoframe-backed inside GatingSet(), which copies
  ## the data into an h5-backed cytoset. Deliberate as of 2_85: at ~175 MB per antibody group the
  ## out-of-core backing a cytoset buys is not worth rewriting annotate_changeParam_pdWrite(),
  ## whose failure mode is silently wrong pData rather than an error. actaCompensate() is
  ## generic over both, so a later migration would not touch this line.
  fs_comp<-actaCompensate(fs, compMode, wd=getwd())
  gs<-GatingSet(fs_comp) ## Declares the empty gating set
  ## GatingSet() reparses numeric-looking pData strings and drops leading zeros ("001" -> "1"),
  ## which corrupts identifiers -- Lot_Num above all. Restore them from the flowSet, which is still
  ## correct here. See actaRestorePData().
  gs<-actaRestorePData(gs, fs_comp, quiet=TRUE)
  gs<-transform(gs, actaTransformerList(channels, actaTrans))
  ## Transformed view of the data, for the downstream axis-limit helpers that used to be
  ## handed the pre-transformed flowSet.
  ## cs_, not fs_: gs_pop_get_data() returns a **cytoset** of cytoframes (the GatingSet's own
  ## h5-backed copy), not the flowSet that went in.
  cs_comp_trans<-gs_pop_get_data(gs)
  ## Boundary dataframe: Channel <-> Target/marker map, used downstream for the
  ## antibody-channel lookup and plot dimensions (independent of the gating tree).
  costainPosition<-which(isLayoutValue(pData(gs)$StainType, "Costain"))[1] ## [1]: defensive against >1 Costain well per antibody group (e.g. one per titration row) -- only need one sample to look up channel/marker names, since the panel is the same for all wells in this group
  gsLength<-length(gs)
  ## Prefer the LAST non-Costain sample, as before -- but only among samples that actually carry
  ## $PnS markers. An Unstained file frequently has NONE (11 of 12 files in one internal run have them;
  ## the Unstained has zero), and if such a file landed on this index then markernames() came back
  ## empty, `boundary` held nothing but scatter, and EVERY marker name in the template fell through
  ## resolveDimsToChannels() as a bogus channel -- the "can't find <marker>" failure, arrived at
  ## positionally. Falls back to the old arithmetic only if no sample has markers at all, so the
  ## error then comes from the dims pre-flight below rather than from here.
  .nMark<-vapply(seq_along(gs), function(i) length(markernames(gs[[i]])), integer(1))
  .cand<-setdiff(which(.nMark>0), costainPosition)
  if (!length(.cand)) .cand<-which(.nMark>0)
  boundaryIndex<-if (length(.cand)) .cand[[length(.cand)]] else
                 ifelse(costainPosition==gsLength,gsLength-1,gsLength)
  ## Scatter needs its own rows: markernames() only returns channels that carry a $PnS, and
  ## instruments generally leave scatter undescribed, so scatter would otherwise be absent from
  ## the map and any template `dims` naming it would fail to resolve. The rows are DERIVED from
  ## this file's channels rather than hardcoded to c("FSC-A","FSC-H","SSC-A","SSC-H") -- those
  ## literals only exist on an Aurora/BD/CytoFLEX-style instrument, and a ZE5 (FS00-A, SS02-A)
  ## or Guava (FSC-HLin) run would land four rows in `boundary` that match no real channel while
  ## its actual scatter channels stayed missing. setdiff() keeps this a no-op for any scatter
  ## channel that DID come back from markernames().
  scatterChannels<-setdiff(colnames(gs)[isScatterChannel(colnames(gs))], names(markernames(gs[[boundaryIndex]])))
  boundary<-data.frame(Channel=names(markernames(gs[[boundaryIndex]])), Target=markernames(gs[[boundaryIndex]]), row.names=NULL) |>
    add_row(Channel=scatterChannels, Target=scatterChannels)
  ## (2_85) A `channel <- boundary$Channel[grepl(toupper(antibody[q]), ...)]` line sat here. It
  ## matched the DIRNAME ("cParp_Donor1") against markernames ("cPARP") as a substring pattern,
  ## which can never hit -- the pattern is the longer string -- so it always produced
  ## character(0). Nothing read `channel` before the correct assignment further down
  ## (`channel <- list_channels$ab[2]`, which pop.MeFI()/pop.rsd() rely on), so it was dead.
  size<-length(gs)
  sampling<-c(round(size/6), round(2* size/6), round(3*size/6), round(4*size/6), round(5*size/6), size)
  forTestPlot<-unique(sampling)

  ## Build THIS antibody's gatingTemplate. For the marker gate row, set the x/first
  ## dim to the layout `Dim`-derived channel; keep any sheet-authored dim as y (2D gate).
  gt_this <- gating_template
  markerParent <- NA_character_; markerDimsCh <- NULL
  ## Resolved ONCE, here, and reused by the plot/stat code below. It used to be three separate
  ## copies of the same grepl() against boundary$Target, which is how a Channel column would have
  ## been honoured in one place and ignored in the other two.
  markerChannel <- resolveMarkerChannel(targetName, channelName, boundary, colnames(gs),
                                        group = antibody[q], dim_col = markerColName)
  if (length(markerGateRowIdx) >= 1) {
    for (mr in markerGateRowIdx) {
      yDim <- trimws(as.character(gt_this$dims[mr]))                 # sheet-authored dim -> y (second)
      yDim <- if (is.na(yDim) || !nzchar(yDim)) "" else trimws(strsplit(yDim, ",")[[1]][1])
      gt_this$dims[mr] <- if (nzchar(yDim)) paste(markerChannel, yDim, sep = ", ") else markerChannel
    }
    if (markerIsManual) {
      ## Manual marker: capture parent + resolved dims for the interactive draw. The row
      ## stays in the FULL template (gate_manual plugin returns the drawn gate in pass 2).
      markerParent <- gt_this$parent[markerGateRowIdx]
      markerDimsCh <- resolveDimsToChannels(gt_this$dims[markerGateRowIdx], boundary)
    }
  }
  ## PASS 1: auto template (exclude the manual marker row) -> builds up to the parent.
  gt_auto <- if (markerIsManual) gt_this[-markerGateRowIdx, , drop = FALSE] else gt_this
  ## PRE-FLIGHT: every `dims` token must name a marker or a channel THIS data has. openCyto would
  ## otherwise discover it deep into gating -- after the layout plots are on disk -- see
  ## actaUnresolvedDims(). Fail here, before anything is written.
  .badDims <- actaUnresolvedDims(gt_auto$dims, gt_auto$alias, boundary$Target, colnames(gs))
  if (length(.badDims))
    stop(sprintf(paste0("Antibody '%s': the gating_template names dimension(s) this data does not ",
                        "have.\n%s\nMarkers in this panel: %s\nChannels in this panel: %s"),
                 antibody[q], paste(.badDims, collapse = "\n"),
                 paste(sort(unique(boundary$Target)), collapse = ", "),
                 paste(colnames(gs), collapse = ", ")), call. = FALSE)
  acta_gt_csv <- tempfile(fileext = ".csv")
  write.csv(gt_auto, acta_gt_csv, row.names = FALSE, na = "")
  gt_gating(gatingTemplate(acta_gt_csv), gs)
  recompute(gs) ## Recomputes statistics
  ## PASS 2 (manual only): draw the marker gate interactively, then re-run the FULL
  ## template -- openCyto skips the existing auto nodes and builds <alias>+/- from the
  ## drawn gate returned by the gate_manual plugin.
  if (markerIsManual) {
    ## groupBy drives scope: a column (e.g. Dirname) -> one gate per group value; empty ->
    ## per-sample. drawManualGates() picks the brightest well per group to draw on.
    .acta_manual_cache$gates <- drawManualGates(gs, dims = markerDimsCh, parent = markerParent,
                                                groupBy = manualGroupBy, gate_dir = gate_dir,
                                                redraw = manualGateRedraw)
    acta_gt_full_csv <- tempfile(fileext = ".csv")
    write.csv(gt_this, acta_gt_full_csv, row.names = FALSE, na = "")
    gt_gating(gatingTemplate(acta_gt_full_csv), gs)
    recompute(gs)
  }

  ## A gate_quad_* row leaves its four populations named after the CHANNELS
  ## ("BUV496-A+Alexa Fluor 532-A-"). Rename them to the dims (marker) names -- see
  ## actaRenameQuadNodes(). This has to happen BEFORE the scaffold below, because the scaffold, the
  ## layout plots and StatsExport all key on node NAMES; renaming afterwards would leave three
  ## different spellings of the same population in one run.
  .quadRenamed <- actaRenameQuadNodes(gs, gt_this, boundary, group = antibody[q])
  if (nrow(.quadRenamed))
    message(sprintf("[ACTA] '%s': quadrant populations renamed to their markers -- %s.", antibody[q],
                    paste(sprintf("%s -> %s", .quadRenamed$from, .quadRenamed$to),
                          collapse = "; ")))

  ## Plot scaffolding (channels, adaptive bins, axis limits, node indices) for the
  ## template populations, consumed by the layout plots further down.
  ## Plot scaffolding derived generically from the gating_template populations: for
  ## each template row, resolve its dims to channels (via `boundary`), locate its node
  ## in the gating tree, and precompute adaptive bins + axis limits on its parent.
  ## No per-population hardcoding -- add a row to the sheet and it flows through here
  ## and into the layout plots below.
  ## Per-alias CAPTION for the layout plots: the whole gating_template row, not just the
  ## method name -- see gateCaption(). gt_this already holds every openCyto column.
  strat<-setNames(vapply(seq_len(nrow(gt_this)),
                         function(i) gateCaption(gt_this[i, ], compensation=compLabel),
                         character(1)), gt_this$alias)
  popPaths<-gs_get_pop_paths(gs)
  ## Scaffold/layout-plot only the single-population hierarchy rows. EXCLUDE the marker
  ## gate row(s): pop "+/-" produces two nodes named <alias>+/<alias>- (not <alias>), so
  ## the alias has no matching single node -> index becomes NA and the layout plot fails
  ## with "NA not found!". The marker +/- populations are plotted separately below
  ## (p_hist / p_pseudo via list_index$markerpos/markerneg).
  scaffoldRows<-setdiff(seq_len(nrow(gt_this)), markerGateRowIdx)
  ## ONE ROW CAN PRODUCE SEVERAL POPULATIONS, and not under the alias -- a `+/-+/-` quadrant row
  ## expands to six, named from the dims. See actaTemplateRowNodes(). The scaffold is therefore keyed
  ## by NODE name rather than by row alias, with `scafRowAlias` remembering which row each node came
  ## from so the per-row flags and captions can still be looked up.
  popScaffold<-list(); scafRowAlias<-character(0)
  for (.i in scaffoldRows) {
    .parent<-gt_this$parent[.i]
    ## The parent's PATH as well as its name: a multi-population row needs to pick its own children
    ## out of the tree, and a basename match alone would also match a same-named population under a
    ## different branch.
    .parentPath<-if (identical(tolower(trimws(.parent)), "root")) "root" else {
      .h<-popPaths[basename(popPaths) == .parent]
      if (length(.h) == 1L) .h else "root"
    }
    .chans <-resolveDimsToChannels(gt_this$dims[.i], boundary)
    ## A row naming ONE dim gets the instrument's forward scatter as its DISPLAY y axis -- see
    ## actaPlotDims(). Without it computePlotParams() received NA and the run died with "subscript
    ## out of bounds" after the whole tree was gated.
    .chans <-actaPlotDims(.chans, colnames(gs), gt_this$alias[.i])
    ## parent and dims belong to the ROW, so every node it produces shares the same plot
    ## parameters -- computed once here rather than once per node.
    .prm   <-computePlotParams(gs, .parent, .chans[1], .chans[2], adaptiveBins(gs, .parent))
    .nodes <-actaTemplateRowNodes(gt_this$alias[.i], gt_this$pop[.i], gt_this$dims[.i], .chans)
    ## A predicted node that is not in the tree is SKIPPED with a message. Previously the alias was
    ## looked up unconditionally, so a miss became index = NA and the layout plot died with the
    ## opaque "NA not found!" -- a visible skip is strictly better than that.
    .miss  <-setdiff(.nodes, basename(popPaths))
    if (length(.miss))
      message("[ACTA] gating_template row ", .i, " (alias '", gt_this$alias[.i], "', pop '",
              gt_this$pop[.i], "'): no population named ",
              paste(sprintf("'%s'", .miss), collapse=", "), " in the gating tree -- not plotted.")
    .have<-intersect(.nodes, basename(popPaths))
    if (length(.have) > 1L) {
      ## ONE plot for a row that yields several populations. A quadrant IS a single gate, and
      ## rendering it as four separate single-population panels of the same 2D plot is not what a
      ## quadrant looks like in FlowJo or anywhere else -- it was also the first thing the user said
      ## was wrong. All of the row's gates go on one plot, which is the quadrant.
      ##
      ## The two 1D intermediates of the `+/-+/-` expansion (<dim>+ for each dim) are EXCLUDED from
      ## the drawing: their cutpoints are already the edges of the four rectangles, so they would add
      ## two redundant lines and two marginal percentages to an otherwise readable panel. They remain
      ## real populations in the tree and still feed StatsExport; they just get no plot of their own.
      .tk  <-trimws(strsplit(paste0(as.character(gt_this$dims[.i]), ""), ",")[[1]])
      ## Named by the ROW, since no single population name describes the set. alias "*" is openCyto's
      ## "name these yourself" marker and would make a nonsense plot title and filename.
      ## Resolved BEFORE .drawn, which needs it -- see below.
      .al  <-trimws(as.character(gt_this$alias[.i]))
      ## Redundant quadrant intermediates are excluded, the row's own +/- pair never is --
      ## see actaDrawnNodes(), which carries the reasoning and is unit-tested.
      .drawn<-actaDrawnNodes(.have, gt_this$dims[.i], .al)
      .nm  <-if (nzchar(.al) && .al != "*" && !is.na(.al)) .al else
               paste(.tk[nzchar(.tk)], collapse = "_")
      popScaffold[[.nm]]<-list(alias    = .nm,
                               parent   = .parent,
                               channels = .chans,
                               ## Restricted to CHILDREN OF THIS ROW'S PARENT: a bare basename match
                               ## would also pick up a same-named population elsewhere in the tree.
                               index    = which(dirname(popPaths) ==
                                                  (if (identical(.parentPath, "root")) "/" else .parentPath) &
                                                basename(popPaths) %in% .drawn),
                               params   = .prm)
      scafRowAlias[.nm]<-gt_this$alias[.i]
    } else {
      for (.nd in .have) {
        popScaffold[[.nd]]<-list(alias    = .nd,
                                 parent   = .parent,
                                 channels = .chans,
                                 index    = which(basename(popPaths) == .nd)[1],
                                 params   = .prm)
        scafRowAlias[.nd]<-gt_this$alias[.i]
      }
    }
  }

  ## ---- Per-population opt-in flags -----------------------------------------------------------
  ## Read from `gatingFlags` (lifted out of the sheet BEFORE the openCyto column strip) -- NOT
  ## from gt_this, which no longer carries them. Default TRUE everywhere (absent column, blank
  ## cell and "NA" cell all mean TRUE), so a layout file written before these columns existed
  ## behaves exactly as it always did. See actaTemplateFlag().
  ##   layout_report = TRUE -> plot enters plotLayoutList and is printed as a report page
  ##   layout_export = TRUE -> plot is ggsave()d to Plots/
  ##   stats_export  = TRUE -> population contributes count/percent/MeFI/robustSD to StatsExport
  ## A plot is BUILT if layout_report OR layout_export is TRUE, so an export-only population
  ## still renders; a population with both FALSE is never built at all (each layout plot is a
  ## faceted ggcyto render, so skipping is a real saving).
  flagLayout<-actaTemplateFlag(gatingFlags, "layout_report")
  flagExport<-actaTemplateFlag(gatingFlags, "layout_export")
  flagStats <-actaTemplateFlag(gatingFlags, "stats_export")
  scafAliases<-names(popScaffold)
  ## Flags and captions are authored per ROW; a row that expands to several populations applies its
  ## own flags and caption to all of them. Re-keyed from row alias to NODE name here so every
  ## consumer further down keeps indexing by the name it plots.
  .byNode<-function(v){ out<-unname(v[scafRowAlias[scafAliases]]); names(out)<-scafAliases; out }
  flagLayoutN<-.byNode(flagLayout); flagExportN<-.byNode(flagExport); flagStatsN<-.byNode(flagStats)
  stratN<-.byNode(strat)
  ## Flags are meaningless on a row that yields no single population node -- the marker "+/-"
  ## row is excluded from scaffoldRows above and is marked NA in the sheet for that reason.
  ## Skip such rows QUIETLY (an NA there is documentation, not a mistake) but name any row the
  ## user explicitly set TRUE, since that request cannot be honoured.
  ## Compared against the ROW aliases that produced nodes, not against the node names: an expanded
  ## row is authored as alias "*" and would otherwise report itself as having no population.
  .nonScaffold<-setdiff(gt_this$alias[!is.na(gt_this$alias)], unique(scafRowAlias))
  for (fl in list(list(n="layout_report", v=flagLayout), list(n="layout_export", v=flagExport),
                  list(n="stats_export",  v=flagStats))) {
    .exp<-attr(fl$v, "explicit")
    .bad<-intersect(.nonScaffold, names(.exp)[.exp])
    if (length(.bad))
      message("[ACTA] ", fl$n, "=TRUE ignored for row(s) with no single population node: ",
              paste(unique(.bad), collapse=", "),
              " (the marker '+/-' row is plotted as p_hist/p_pseudo instead).")
  }
  wantLayout<-scafAliases[which(flagLayoutN[scafAliases])]
  wantExport<-scafAliases[which(flagExportN[scafAliases])]
  wantStats <-scafAliases[which(flagStatsN[scafAliases])]
  buildAliases<-scafAliases[scafAliases %in% union(wantLayout, wantExport)]  # keep sheet order
  message("[ACTA] ", antibody[q], " -- populations: ", length(scafAliases),
          " | Layout ", length(wantLayout), " | Export ", length(wantExport),
          " | Stats ", length(wantStats),
          if (!length(buildAliases)) "  (no layout plots will be built)" else "")

  ## NOTE: Cells, Single_cells and Live are now produced by the openCyto template
  ## (gt_gating above). Manual Single/Live gate construction removed.

  ## Marker positive/negative gating is applied directly on the Live parent below.
  ## flowMeans & Costain based gate setting
  ## [1] is the pseudocolor y dim (forward scatter, area preferred), [2] the titrated marker.
  ## Resolved from the instrument via pickForwardScatter() instead of the literal "FSC-A", which
  ## does not exist on a ZE5 ("FS00-A") or a Guava ("FSC-HLin").
  list_channels$ab<-c(pickForwardScatter(colnames(gs)), markerChannel)
  
  key<-plotLimits(cs_comp_trans, channels)
  temp_df<-pData(gs)  |>  mutate(median_pos=0,median_neg=0,rsd_neg=0,SI=0)
  sn<-sampleNames(gs)
  ## Parent population for the marker plots/stats: the marker gate row's OWN parent, read from the
  ## template. Was hardcoded "Live", which is right only while the marker gate hangs directly off
  ## Live. On one internal run the marker sits under CD3+/MarkerB-, so the histogram, the pseudocolor and
  ## ConcatPlot were drawing ALL Live events while the percentages beside them came from the marker
  ## nodes under MarkerB- -- a figure that disagrees with its own labels, and no error to say so.
  ## Also feeds the "Parent pop:" subtitles and the _parent_<pop>_ filenames, so those now name the
  ## population actually plotted.
  parentPop<-trimws(as.character(gt_this$parent[markerGateRowIdx]))
  if (!nzchar(parentPop) || is.na(parentPop)) parentPop<-"root"
  ## Kept PER GROUP as well: everything after the loop needs this group's value, not the last
  ## group's. See actaParentLabel().
  parentPopByGroup[antibody[q]] <- parentPop
  channel<-list_channels$ab[2]
  ## pop.MeFI()/pop.rsd() are handed to gs_pop_get_stats(type=) below, which calls them with the
  ## flowFrame and nothing else -- so the channel and the transform have to be published to them
  ## here rather than read out of this scope. See actaSetStatContext().
  actaSetStatContext(channel, actaTrans)
  ## Per-antibody descriptor (reagent + test material). Built ONCE here, BEFORE the plots,
  ## so the same string serves as the per-antibody plot subtitle AND as the SIPlot/SatPlot
  ## facet `label` further down (it used to be built only in temp_df, too late for the
  ## plots). These fields are constant within a Dirname group; warn if that ever breaks.
  ## EVERY field the descriptor prints has to be named here: distinct() keeps ONLY the columns it
  ## is given, so a field added to abLabel but not to this list resolves to NULL and prints as
  ## nothing at all. Clone was added on 2026-08-27 and did exactly that -- the strip, the two
  ## marker plots and the layout titles all silently omitted it, because actaTitleStamp() cannot
  ## tell "this workbook has no Clone" from "this frame no longer carries the column".
  abMeta<-metadata_file[.abRows, ] |>
    distinct(Cat_Num, Vendor, Lot_Num, Clone, Test_Material, e6_cells_per_well)
  if(nrow(abMeta)!=1)
    warning(sprintf("Antibody '%s': reagent metadata is not constant across its wells (%d distinct combinations); using the first.", antibody[q], nrow(abMeta)))
  abMeta<-abMeta[1,]
  ## Two variants of the same descriptor:
  ##  abLabel     - MULTI-LINE and carries the marker gating method. Used ONLY as the
  ##                SIPlot/SatPlot facet-strip `label`: the STRIP is where the method has to live
  ##                on those plots -- the SI caption is spent on the linear-MFI note -- and the \n
  ##                keeps narrow strips readable.
  ##  abLabelLine - SINGLE-LINE and WITHOUT the gating method. Used for the per-antibody plot
  ##                subtitles (histogram / pseudocolor / gate layouts), where the method is
  ##                already in the caption -- and, on a layout page, the caption shows THAT
  ##                population's own method (e.g. gate_flowmeans for Live), which would
  ##                contradict the marker method if it were repeated in the subtitle.
  ## Line breaks: "Cat:" starts its own line, and a "||" is dropped wherever a newline now
  ## follows it -- a separator at the end of a line separates nothing. The padding spaces that
  ## only existed to sit either side of those removed "||" go with them, since a trailing or
  ## leading space would offset the centring of the facet strip text.
  ## Clone rides with the catalogue number: both name the REAGENT, where Lot names the vial and
  ## Test sample names what it was run on. Emitted through actaTitleStamp() so that a layout with
  ## no Clone column, or a blank cell, drops the separator too -- "Cat: 123 || Clone: " reads as
  ## missing data rather than as a field this workbook never fills.
  abLabel<-paste0(aliasName, " ", channel, " (", reagentMethod, ")\n",
                  "Cat: ", abMeta$Cat_Num, actaTitleStamp(abMeta$Clone, "Clone: ", " || "), "\n",
                  abMeta$Vendor, " || ", "Lot: ", abMeta$Lot_Num, "\n",
                  "Test sample: ", abMeta$Test_Material, " at ", abMeta$e6_cells_per_well, "e6/well")
  abLabelLine<-paste0(aliasName, " ", channel, " || Cat: ", abMeta$Cat_Num, " || ", abMeta$Vendor,
                      " || Lot: ", abMeta$Lot_Num, " || Test sample: ", abMeta$Test_Material,
                      " at ", abMeta$e6_cells_per_well, "e6/well")
  ## Marker +/- populations, resolved from the marker ROW by structure -- see actaMarkerNodes().
  ## The previous version took the first "+"/"-" suffixed node anywhere in the tree, which silently
  ## selected an INTERMEDIATE gate whenever one was also pop = "+/-".
  mkNodes <- actaMarkerNodes(gs, gt_this, antibody[q])
  posNode <- mkNodes$pos
  negNode <- mkNodes$neg
  list_index$markerpos<-mkNodes$pos_index
  list_index$markerneg<-mkNodes$neg_index
  recompute(gs) ## Recomputes statistics
  bins_ab<-adaptiveBins(gs, parentPop)
  ab_params<-computePlotParams(gs, parentPop, list_channels$ab[2], list_channels$ab[1], bins_ab)
  ## Facet grid AND device size for this antibody: both ncol and nrow scale with the number of
  ## stain volumes so every panel keeps a constant size and aspect (see facetLayout()). Counted
  ## from distinct rounded StainQty -- exactly what facet_wrap() bins on below.
  fLay <- facetLayout(dplyr::n_distinct(round(as.numeric(pData(gs)$StainQty), 3)))
  message(sprintf("ACTA: %-22s %2d facet(s) -> %d col x %d row grid, %.1f x %.1f in device",
                  antibody[q], fLay$facets, fLay$ncol, fLay$nrow, fLay$width, fLay$height))
  ## THIS GROUP's notebook entry, from THIS titration's rows. Read from the whole sheet it was
  ## (a) a vector whenever a layout drew its wells from more than one entry, and (b) picking up
  ## ELN_IDs from rows with no Dirname at all -- unused plate wells, 80 of the 108 in the run that
  ## found this. The histogram/pseudocolor titles below are per antibody, so the per-group value is
  ## also the CORRECT one to print. See actaElnLabel().
  BID<-actaElnLabel(metadata_file$ELN_ID[.abRows])
  ## Kept per group for the same reason parentPopByGroup is: the per-antibody filenames written
  ## after the loop need this group's entry, not whatever the last iteration left behind.
  elnByGroup[antibody[q]]<-BID
  ## Reduced across THIS titration's rows, not read off one well -- Layout_Plate has a row per well
  ## and the declaration is per titration. Pre-flight has already made a conflicting declaration
  ## fatal, so reaching the warn branch here means the column changed under a running analysis;
  ## degrade to "not declared" rather than picking one of two numbers.
  .cg <- actaCombinatorialGroup(
           if ("Combinatorial_group" %in% names(metadata_file))
             metadata_file$Combinatorial_group[.abRows] else character(0),
           antibody[q])
  if (!is.na(.cg$conflict)) warning(.cg$conflict, call. = FALSE)
  comboByGroup[antibody[q]]<-.cg$value
  ## Operator/analyst, appended to the plot titles after the Benchling ID. collapse= keeps
  ## this a SINGLE string even if a run lists more than one operator: a length>1 value here
  ## would vectorise every downstream paste()/ggsave() -- the same failure mode that once
  ## produced the duplicate "_NA_" exports and plots.
  OPR<-paste(unique(metadata_file$Operator[!is.na(metadata_file$Operator)]), collapse=", ")
  ## Biexp axes never show less than 10^4 (+10% headroom) -- see biexpFloorLimits(). The same
  ## clamped range feeds ggcyto_par_set and the decade breaks so they cannot disagree.
  histXlim <- biexpFloorLimits(c(key$min[key$channel == list_channels$ab[2]],
                                 1.1*key$max[key$channel == list_channels$ab[2]]),
                               list_channels$ab[2], actaTrans)
  ## AND A FLOOR, because biexpFloorLimits() only raises the ceiling. `key$min` is the MINIMUM
  ## ACROSS WELLS of each well's p0.01 (plotLimits()), and one bad well takes the axis with it:
  ## on DATASET-1 the pooled p0.01 of APC-A is raw -401 (transformed +749) while one well's own
  ## p0.01 reached raw ~-200,000, putting key$min at -90,192 -- 95.2% of every panel empty and the
  ## whole density in a spike at the right edge. actaBiexpFloor() bounds the negative band at a
  ## share of the positive span; see it for why a percentile alone cannot do this job.
  .histFloorRaw <- histXlim[1]
  histXlim[1] <- actaBiexpFloor(histXlim[1], histXlim[2], actaTrans)
  if (histXlim[1] > .histFloorRaw)
    message(sprintf(paste0("[ACTA] %s histogram x floor guarded: %.0f -> %.0f (raw %.0f -> %.0f)",
                           " -- one well's p0.01 would have given the negative band %.0f%% of ",
                           "the panel."),
                    list_channels$ab[2], .histFloorRaw, histXlim[1],
                    actaTrans$inverse(.histFloorRaw), actaTrans$inverse(histXlim[1]),
                    100 * (actaTrans$transform(0) - .histFloorRaw) /
                          (histXlim[2] - .histFloorRaw)))
  ## The plot chains are captured as quoted EXPRESSIONS, not built inline, so the report can
  ## re-evaluate them against a SUBSET of the samples for pagination (see below). eval() picks
  ## up whatever `gs` and `facetNcol` are bound in the calling frame, which keeps the ~40-line
  ## ggcyto chains in one place instead of duplicating them or plumbing a dozen parameters
  ## through a function. Everything else the chain closes over -- histXlim, ab_params,
  ## bins_ab, list_index -- is computed ONCE over the FULL sample set above and is therefore
  ## identical on every page: that is what stops each page self-scaling to its own subset.
  histExpr <- quote({
    p <- ggcyto(gs,
                     aes(x = .data[[list_channels$ab[2]]]), 
                     subset = parentPop) + 
      geom_histogram(aes(y = after_stat(density)), 
                     bins = 500, 
                     fill = "#3498D0", 
                     color = "white", 
                     alpha = 0.5,
                     linewidth = 0.01) +
      geom_density(color = "red", linewidth = 0.5) +
      ## Percentages moved OFF the baseline, up to where p_pseudo carries them.
      ## geom_stats' own `adjust` cannot do this on a histogram: stat_position.filter() excludes the
      ## "density" dimension from the adjustment (`names(centroids) != "density"`), so the label
      ## stays pinned near y=0 whatever adjust is set to -- measured at 6% up the panel, against 50%
      ## for the pseudocolor. location="plot"/"data" error outright on a 1D density.
      ## Passing y through to the underlying geom_label overrides the computed position, and Inf is
      ## the one value that means the same thing in every panel -- necessary because scales="free"
      ## gives each panel its own density maximum, so any absolute y would be right in one panel and
      ## off-scale in the next. vjust then hangs the label below the panel top.
      geom_stats(gs_get_pop_paths(gs)[c(list_index$markerneg,list_index$markerpos)], type="percent",
                 fill="white", color="black", alpha=0.9, size=5, y = Inf, vjust = 1.6) +
      ## scales="free": BOTH axes are per-panel. On a density histogram that reads better -- a
      ## dim well's peak is not flattened by the brightest well setting a shared y maximum. It
      ## also removes any cross-page coupling when the report paginates: with no shared y range
      ## there is nothing for a page subset to rescale, so pages need no y pinning.
      facet_wrap(~round(as.numeric(StainQty),3), ncol = facetNcol, scales = "free") +
      ## Draw the marker gate boundary the ggcyto-native way (like p_pseudo): geom_gate
      ## reads the per-sample gate from the GatingSet and facets correctly. On a 1D
      ## histogram the [bound, Inf) gate renders as a vertical line at `bound`.
      geom_gate(gs_pop_get_gate(gs, gs_get_pop_paths(gs)[list_index$markerpos]),
                colour = "#27AE60", linewidth = 0.6, linetype = "dashed") +
      theme_bw(base_size = 11) +
      theme(
        strip.background = element_rect(fill = "steelblue"),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, , face="plain", size=12),
        axis.text.y = element_text(hjust = 0, vjust = 0, , face="plain", size=12),
        strip.text = element_text(color = "white", face = "bold", size=12),
        panel.grid.minor = element_blank(),
        plot.subtitle=element_text(face="italic",size=12),
        plot.caption=element_text(face="italic",size=actaCaptionSize(captionW), hjust=0),
        plot.title=element_text(size=14, face="bold")
      ) +
      ## Reagent/test-material info (abLabel) now rides in the subtitle after the parent
      ## pop; the gating method moved to the caption.
      labs(title = paste0(paste(aliasName,list_channels$ab[2], " Histogram |",BID,"|",OPR),
                          actaTitleStamp(abMeta$Clone, "Clone ")),
           subtitle=paste0("Parent pop: ",parentPop," || ",abLabelLine),
           caption=wrapToWidth(markerCaption, captionW, font_size=actaCaptionSize(captionW), frac=0.86),
           x = list_channels$ab[2],
           y = "Frequency") +
      ggcyto_par_set(limits = list(x = histXlim))
    ## Relabel the biexp x-axis in raw units (0, 10^2 ... 10^6) FlowJo-style. Added AFTER the
    ## chain: ggcyto's `+` silently ignores a list of layers (see addBiexpAxes()). y is a
    ## density here, so only x is inverted.
    ## Extra room on the LEFT only. The negative percentage sits at the centroid of the negative
    ## gate, which on a Costain/unstained well is a sliver hard against the left edge -- at the
    ## default 5% expansion "99.0%" was clipped to "9.0%". Expansion adds drawn margin without
    ## changing the limits, so no data, gate or label position moves; there is just room for it.
    p <- addBiexpAxes(p, xch = list_channels$ab[2], trans = actaTrans, maxValue = actaMaxValue,
                           xlim = histXlim, x_expand = expansion(mult = c(0.16, 0.05)))
    p
  })
  pseudoExpr <- quote({
    ## list_channels$ab[1], not a literal "FSC-A": same channel addBiexpAxes() is told about
    ## below, so the aes, the axis label and the linear/biexp decision can never disagree.
    p <- ggcyto(gs, aes(x=.data[[list_channels$ab[2]]], y=.data[[list_channels$ab[1]]]), subset=parentPop) + flow_bin2d(binwidth=ab_params$binwidth) + geom_gate(gs_pop_get_gate(gs,gs_get_pop_paths(gs)[list_index$markerneg])) +
      geom_gate(gs_pop_get_gate(gs,gs_get_pop_paths(gs)[list_index$markerpos])) +
      ##geom_point(alpha=0.1, size=1, shape=19) +
      theme_bw(base_size = 14) +
      ## scales="free_x": x stays per-panel (each stain volume gets its own biexp range) but y is
      ## FIXED across panels, so the y labels collapse to the leftmost column and the page matches
      ## the SI/Saturation plots. Trade-off: one shared y range means the brightest well sets the
      ## scale for all panels.
      facet_wrap(~round(as.numeric(StainQty),3), ncol = facetNcol, scales = "free_x") +
      ## Reagent/test-material info (abLabel) now rides in the subtitle after the parent
      ## pop; the gating method moved to the caption.
      labs(title = paste0(paste(aliasName,list_channels$ab[2], " Pseudocolor |", BID,"|",OPR),
                          actaTitleStamp(abMeta$Clone, "Clone ")),
           subtitle=paste0("Parent pop: ",parentPop," | ",abLabelLine),
           caption=wrapToWidth(markerCaption, captionW, font_size=actaCaptionSize(captionW), frac=0.86),
           x = list_channels$ab[2],
           y = list_channels$ab[1]) +
      ## percent AND count -- see the note in makeGateLayoutGreatAgain(). The histogram above keeps
      ## percent only, on purpose: its labels sit on top of the density curve.
      geom_stats(gs_get_pop_paths(gs)[c(list_index$markerneg,list_index$markerpos)], type=c("percent","count"), fill="white", color="black", alpha=0.9, size=5) +
      theme_bw() +
      theme(
        strip.background = element_rect(fill = "steelblue"),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, , face="plain", size=12),
        axis.text.y = element_text(hjust = 0, vjust = 0, , face="plain", size=12),
        strip.text = element_text(color = "white", face = "bold", size=12),
        panel.grid.minor = element_blank(),
        ## Drop the bin-count colour bar: the density ramp is self-evident on a pseudocolor and
        ## the legend only eats panel width. flow_bin2d() still defines the scale.
        legend.position = "none",
        plot.subtitle=element_text(face="italic",size=12),
        plot.caption=element_text(face="italic",size=actaCaptionSize(captionW), hjust=0),
        plot.title=element_text(size=14, face="bold")
      ) +
      ggcyto_par_set(limits = list(x=pseudoXlim, y=ab_params$ylim))
    ## x is the titrated marker (biexp) -> decade ticks; y is forward scatter (linear) and is
    ## deliberately left alone by addBiexpAxes(), which recognises it via isScatterChannel().
    p <- addBiexpAxes(p, xch = list_channels$ab[2], ych = list_channels$ab[1],
                             trans = actaTrans, maxValue = actaMaxValue,
                             xlim = pseudoXlim, ylim = ab_params$ylim)
    p
  })
  ## Both x-limit vectors are computed ONCE over the full sample set, before any plot is
  ## built, and are closed over by the quoted chains -- so every page shares them.
  pseudoXlim <- biexpFloorLimits(ab_params$xlim, list_channels$ab[2], actaTrans)
  ## Same guard as the histogram, and needed for the same reason: computePlotParams() takes the
  ## MIN ACROSS SAMPLES of each sample's p0.005 and then pads by 10% of the RAW range, so a single
  ## deep-negative well sets the x axis for every facet of the pseudocolor too.
  pseudoXlim[1] <- actaBiexpFloor(pseudoXlim[1], pseudoXlim[2], actaTrans)
  ## Full-series plots for the PNG exports: every facet together, sized by facetLayout().
  ## facetNcol / captionW are the two things the quoted chains read from THIS frame: the column
  ## count and the device width the caption is wrapped against. Both are rebound below for the
  ## narrower report pages, which is why the chains must not reach for fLay$ncol directly.
  facetNcol <- fLay$ncol
  captionW  <- fLay$width
  p_hist   <- eval(histExpr)
  p_pseudo <- eval(pseudoExpr)
  ## --- report pages -------------------------------------------------------------------------
  ## At or under the threshold this is ONE page and the full plot is reused verbatim, so a
  ## typical run costs nothing extra and looks exactly as before. Above it, each page is a fresh
  ## build against gs[idx] -- reusing the limits/bins computed over the full set, so the axes and
  ## bin widths are identical on every page and a gate cannot appear to move between pages.
  pageIdx <- facetPages(pData(gs)$StainQty, REPORT_FACETS_PER_PAGE)
  rDims   <- facetLayout(min(fLay$facets, REPORT_FACETS_PER_PAGE), ncol_fixed = REPORT_NCOL)
  if (length(pageIdx) == 1L) {
    reportHist[[q]]   <- list(p_hist)
    reportPseudo[[q]] <- list(p_pseudo)
    reportDims[[q]]   <- fLay          # unpaginated: report uses the same device as the export
  } else {
    message(sprintf("ACTA: %-22s %2d facets > %d/page -> report split into %d pages of <=%d",
                    antibody[q], fLay$facets, REPORT_FACETS_PER_PAGE, length(pageIdx), REPORT_FACETS_PER_PAGE))
    facetNcol <- REPORT_NCOL
    captionW  <- rDims$width
    reportHist[[q]]   <- lapply(pageIdx, function(ix) { gs <- gs[ix]; eval(histExpr) })
    reportPseudo[[q]] <- lapply(pageIdx, function(ix) { gs <- gs[ix]; eval(pseudoExpr) })
    reportDims[[q]]   <- rDims
    facetNcol <- fLay$ncol
    captionW  <- fLay$width
  }
  ## Template marker nodes are named "<alias>+"/"<alias>-" (e.g. Reagent+/Reagent-).
  ## Recode them back to the internal "pos"/"neg" the downstream SI maths expects.
  temp_df_parent<-gs_pop_get_stats(gs,posNode,type="percent") |>
    mutate(pop="pos")
  temp_df_rsd<-gs_pop_get_stats(gs,negNode,type=pop.rsd) |>
    select(!pop)
  temp_df_MFI<-gs_pop_get_stats(gs,c(posNode,negNode),type=pop.MeFI) |>
    mutate(pop=recode(pop, !!posNode:="pos", !!negNode:="neg")) |>
    pivot_wider(names_from="pop",
                values_from="MeFI")
  temp_df <-temp_df_rsd |> 
    left_join(temp_df_MFI) |> 
    left_join(temp_df_parent) |> 
    mutate(SI=(pos-neg)/(2*robustSD)) |> 
    rename("name"="sample") |> 
    left_join(pData(gs)) |> 
    dplyr::filter(!is.na(Dirname)) |> ## guards against wells with no matching layout row (e.g. undocumented/extra FCS files) leaking into stats/plots
    ##dplyr::filter(StainType!="Costain") |> 
    mutate(maxSI=max(SI), maxSI_StainQty=StainQty[SI==maxSI])  |> 
    mutate(maxSat=max(percent), maxSat_StainQty=StainQty[percent==maxSat])  |> 
    mutate(Fl=markerChannel)  |>
    mutate(GatingMethod=reagentMethod) |>
    mutate(label=abLabel) |> ## same string as the per-antibody plot subtitles (built above)
    ## The titration key, on every row. Without it the joins below have nothing that identifies a
    ## SERIES: Dirname names the folder, which two co-titrated markers share. Equal to Dirname in
    ## every workbook with one Alias per folder.
    mutate(Titration=antibody[q])
  ## Histogram and pseudocolor are kept as SEPARATE plots (one report page each) --
  ## the former as.ggplot(p_hist)/as.ggplot(p_pseudo) patchwork squeezed both onto one
  ## page and is gone.
  plotList_hist[[q]]<-p_hist
  plotList_pseudo[[q]]<-p_pseudo
  ## ---- ConcatPlot data ------------------------------------------------------------------------
  ## FlowJo-style concatenation: every well of this group stacked into one frame, marker intensity
  ## against the stain quantity. Values come from the PARENT population of the marker gate, so the
  ## plot shows the same events the Stain Index is computed on. No gates are drawn on it.
  ## Values are in biexp-TRANSFORMED space (the transformer is registered on the GatingSet), which
  ## is what makes the y axis relabel into raw units below.
  concatData[[q]] <- local({
    qty <- suppressWarnings(as.numeric(pData(gs)$StainQty))
    rows <- lapply(seq_along(gs), function(i) {
      ## SAY when a well is dropped. This used to swallow the error and return numeric(0), so a
      ## well vanished from the concatenation plot with nothing said -- and a wrong `channel` or a
      ## missing parent population looks exactly like a well that legitimately had no events.
      v <- tryCatch(as.numeric(flowCore::exprs(gh_pop_get_data(gs[[i]], parentPop))[, channel]),
                    error = function(e) { message("[ACTA] ConcatPlot: ", sampleNames(gs)[i],
                                                  " dropped -- could not read '", channel,
                                                  "' on '", parentPop, "': ", conditionMessage(e))
                                          numeric(0) })
      if (!length(v)) message("[ACTA] ConcatPlot: ", sampleNames(gs)[i],
                              " contributes no events at '", parentPop, "'.")
      if (!length(v) || is.na(qty[i])) return(NULL)
      if (length(v) > CONCAT_MAX_PER_WELL) {
        set.seed(ACTA_SEED)
        v <- v[sort(sample.int(length(v), CONCAT_MAX_PER_WELL))]
      }
      data.frame(label = abLabel, StainQty = qty[i], value = v, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })
  stats<-rbind(stats,temp_df) |>
    relocate(SI, .after=`maxSI_StainQty`)
  ### Adding layouts -- one gate plot per gating_template population (generic).
  ## Keep these as RAW ggcyto objects -- do NOT wrap in as.ggplot(). ggcyto's as.ggplot()
  ## is NOT idempotent and does not drop the "ggcyto" class, so a pre-converted plot gets
  ## converted a SECOND time when ggsave()/print() dispatches print.ggcyto on it, and the
  ## second pass fails with "object 'name' not found" (its resolved data no longer carries
  ## the sample `name` column). Up to 2_81 this was hidden because the converted plots were
  ## wrapped in a patchwork, which draws its elements as plain ggplots; printing/saving them
  ## directly (one plot per page, since 2_82) exposes it. ggsave() and the report's print()
  ## both convert a raw ggcyto exactly once, which is what we want.
  ## Build ONLY the populations someone asked for (Layout or Export). plotsBuilt is the single
  ## source; plotLayoutList / plotExportList are views onto it, so a plot is never rendered twice.
  plotsBuilt<-setNames(lapply(buildAliases, function(al)
    makeGateLayoutGreatAgain(gs, popScaffold[[al]], aliasName, list_channels$ab[2], stratN[al],
                       label=abLabelLine, benchlingID=BID, operator=OPR, clone=abMeta$Clone,
                       trans=actaTrans, maxValue=actaMaxValue, layout_ncol=fLay$ncol,
                       caption_width=fLay$width)), buildAliases)
## Parsed from the SCRIPT's own filename, so it tracks the version folder automatically -- and it
## must look in the CODE dir, not the working directory. Those differ when a diagnostic case
## supplies only its inputs: globbing the wd found no script, ScriptVersion came back empty, and
## the export died with an opaque "In argument: `ScriptVersion = ScriptVersion`".
ScriptVersion<-str_extract(list.files(acta_code_dir, pattern="ACTA_Script.*\\.R$"),
                           "\\d{1,3}_\\d{1,3}")
ScriptVersion<-ScriptVersion[!is.na(ScriptVersion)]
if (!length(ScriptVersion))
  stop(sprintf(paste0("Could not read a version from any ACTA_Script*.R in the code folder '%s'. ",
                      "The export and the dashboard are both keyed on it."), acta_code_dir),
       call. = FALSE)
ScriptVersion<-ScriptVersion[1]
  ## ---- StatsExport rows for this antibody (populations flagged stats_export=TRUE) -----------------
  ## count + percent from openCyto, MeFI + robustSD from ACTA's own pop.MeFI()/pop.rsd(). Note
  ## those two read `channel` from THIS scope -- i.e. they report the TITRATED REAGENT's median
  ## and MAD *within* each flagged gate, not a statistic of the gate's own dims.
  if (length(wantStats)) {
    ## unlist(), not vapply(character(1)): a quadrant scaffold entry carries the indices of ALL FOUR
    ## of its populations, so this must flatten rather than insist on one node per entry -- it failed
    ## with "values must be length 1" the moment a quadrant row was flagged stats_export. All four
    ## quadrants belong in StatsExport; it is only the PLOT that is one panel.
    statsNodes<-unlist(lapply(wantStats, function(al) popPaths[popScaffold[[al]]$index]),
                       use.names = FALSE)
    ## No renaming needed: each type already returns its own value column -- "count",
    ## "percent", and (from the helpers' names(res)<-) "MeFI" and "robustSD".
    thisStats<-gs_pop_get_stats(gs, statsNodes, type="count") |>
      left_join(gs_pop_get_stats(gs, statsNodes, type="percent"), by=c("sample","pop")) |>
      left_join(gs_pop_get_stats(gs, statsNodes, type=pop.MeFI),  by=c("sample","pop")) |>
      left_join(gs_pop_get_stats(gs, statsNodes, type=pop.rsd),   by=c("sample","pop")) |>
      rename("name"="sample") |>
      left_join(pData(gs), by="name") |>
      dplyr::filter(!is.na(Dirname)) |>   ## same guard as `stats`: drop wells with no layout row
      mutate(Population=basename(pop), MeFI_channel=channel, GatingMethod=reagentMethod,
             label=abLabel, ScriptVersion=ScriptVersion)
    statsExportAll<-dplyr::bind_rows(statsExportAll, thisStats)
  }
  msgText<-paste0("Calculation & Plotting completed for ",antibody[q])
  ## Keep the population plots as a NAMED LIST (one entry per gating_template population)
  ## instead of the old first-half/second-half wrap_plots() patchworks: the report prints
  ## one population per page, so any population count works without splitting.
  ## Two views onto plotsBuilt. Order follows the sheet, so report pages and PNGs come out in
  ## gating_template order rather than flag order.
  plotLayoutList[[q]]<-plotsBuilt[buildAliases[buildAliases %in% wantLayout]]
  plotExportList[[q]]<-plotsBuilt[buildAliases[buildAliases %in% wantExport]]
  ## Layout populations shown in the REPORT get per-page rebuilds when paginating. Only the
  ## Layout-flagged ones -- Export-only populations never pay for pages they don't appear on.
  layoutAliases<-buildAliases[buildAliases %in% wantLayout]
  reportLayout[[q]]<-if (length(pageIdx) == 1L) {
    lapply(setNames(layoutAliases, layoutAliases), function(al) list(plotsBuilt[[al]]))
  } else {
    lapply(setNames(layoutAliases, layoutAliases), function(al)
      lapply(pageIdx, function(ix)
        makeGateLayoutGreatAgain(gs[ix], popScaffold[[al]], aliasName, list_channels$ab[2],
                                 stratN[al], label=abLabelLine, benchlingID=BID, operator=OPR,
                                 clone=abMeta$Clone,
                                 trans=actaTrans, maxValue=actaMaxValue, layout_ncol=REPORT_NCOL,
                                 caption_width=rDims$width)))
  }
  ## Dims travel with the plots: the PNG exports and the report must render at the SAME size the
  ## grid was designed for, otherwise the panels get re-squashed on output.
  plotDims[[q]]<-fLay
  ## Report QC for THIS antibody, captured while its GatingSet is still live. Collected per
  ## antibody rather than read off the leftover `gs` after the loop, which would only ever
  ## describe the LAST antibody. Non-fatal by design -- see actaQCChecks().
  ## temp_df is THIS group's per-sample SI (with StainType), which the Costain check needs.
  qcList[[antibody[q]]]<-actaQCChecks(gs, si = temp_df)
  ## Panel AS ACQUIRED, straight off this group's FCS: $PnN -> $PnS. Reported rather than derived
  ## from the layout, so the table shows what the instrument recorded, not what the sheet claims.
  ## Time is dropped -- it is a parameter but not part of the panel. A channel with no $PnS (scatter,
  ## and any detector left unlabelled at acquisition) shows an em dash rather than being hidden, so
  ## the table doubles as a check that the panel was annotated.
  .pnS <- markernames(gs[[boundaryIndex]])
  .pnN <- colnames(gs)
  .mk  <- unname(.pnS[match(.pnN, names(.pnS))])
  .mk[is.na(.mk) | !nzchar(trimws(.mk))] <- "\u2014"
  panelList[[antibody[q]]] <- data.frame(Channel = .pnN, Markername = .mk,
                                         stringsAsFactors = FALSE)[!grepl("^time$", .pnN, ignore.case = TRUE), ]
  ## Same one-line shape the dashboards have always used for a panel -- "Marker (Channel)", comma
  ## joined -- so the export column drops straight into that column without reformatting.
  ## Only LABELLED channels: scatter carries no $PnS and is not part of the panel.
  .lab <- panelList[[antibody[q]]][panelList[[antibody[q]]]$Markername != "\u2014", , drop = FALSE]
  panelStr[[antibody[q]]] <- if (nrow(.lab))
    paste(sprintf("%s (%s)", .lab$Markername, .lab$Channel), collapse = ", ") else NA_character_
  ## FIRST group's first stained file, kept for the MiFlowCyt Instrument section ($PnV detector
  ## voltages) and the panel. Any file in the run would do -- voltages and panel are per acquisition,
  ## not per well -- but pinning the first makes the metadata reproducible instead of depending on
  ## whichever group happened to run last.
  if (!exists("actaRepFcs", inherits = FALSE)) actaRepFcs <- file.path(path, list_fcs1[[1]])
  list_message[[q]]<-msgText
  message(msgText)
}

## RUN-LEVEL parent labels. SIPlot, SatPlot and ConcatPlot are one figure across every group, so they
## get the shared parent when the groups agree and an explicit per-group list when they do not --
## rather than whatever the group loop left in `parentPop`, which is the LAST group's. The filename
## variant collapses a disagreement to "mixed" instead of spelling out three groups in a path.
parentPopLabel <- actaParentLabel(parentPopByGroup)
parentPopFile  <- actaParentLabel(parentPopByGroup, for_file = TRUE)
## RUN-LEVEL ELN label, on the same footing and for the same reason -- `BID` at this point holds the
## LAST group's entry. Taken from `stats`, which is one row per ANALYSED well: that scope excludes
## the unused plate wells the raw column would have swept in, and cannot disagree with the numbers
## the figures are drawn from. The file variant joins with "+" so a two-entry run still produces one
## filename. See actaElnLabel().
BID     <- actaElnLabel(stats$ELN_ID)
BIDfile <- actaElnLabel(stats$ELN_ID, for_file = TRUE)
if (length(unique(parentPopByGroup)) > 1L)
  message(sprintf(paste0("[ACTA] the antibody groups gate under DIFFERENT parent populations (%s); ",
                         "run-level figures are labelled '%s' and filed as '_parent_%s_'."),
                  paste(sprintf("%s: %s", names(parentPopByGroup), parentPopByGroup),
                        collapse = "; "), parentPopLabel, parentPopFile))

AxisLabels<-unique(sort(as.numeric((stats$`StainQty`))))
## Axis TICK TEXT at 5 significant figures. The NUMERIC AxisLabels is left exactly as it is: every
## position lookup keys on it -- match() for the maxSI and ConcatPlot x positions, actaVolToPos() for
## the 90%Vmax line, and the factor levels on SatPlot -- so rounding the values themselves would
## shift vlines and points off their wells. Only the printed text changes.
##
## Serial dilutions produce values like 0.009765625, eleven characters that crowded the axis on an
## 11-well titration. Formatted per element rather than as a vector, because format() pads a vector
## to a common width and that reintroduces the crowding it is meant to fix. scientific = FALSE keeps
## small dilutions readable as decimals.
AxisLabelsDisp <- vapply(AxisLabels, function(v)
  format(signif(v, 5), scientific = FALSE, trim = TRUE, drop0trailing = TRUE), "")
yLimits<-unique(stats$maxSI)
headerSize<-max(min(16,length(Target)*5.3),14)
## NOTE: SIPlot / SatPlot titles deliberately do NOT get the operator appended -- both already
## carry "Analyst: <operator>" in their subtitle, so repeating it in the title would duplicate
## it on the same panel. Only the histogram/pseudocolor titles, which have no analyst anywhere,
## gained it.

## Facet geometry for BOTH summary plots, on the same footing as the flow plots: facetLayout()
## searches ncol so the grid's aspect matches the landscape page box (9.00 x 5.72in) instead of
## stacking rows under a hardcoded ncol=3. Computed ONCE, here, before either plot is built --
## `siLay$ncol` feeds facet_wrap() and `siLay$width/height` feed ggsave() and the .Rmd chunks, so
## the declared size can never drift from the actual grid (the old si_ncol had to be kept in sync
## with facet_wrap by hand, per its own comment).
## These facet by `label`, i.e. ONE PANEL PER ANTIBODY -- so the facet count is the number of
## antibodies in the run, not the number of stain volumes.
siLay <- facetLayout(dplyr::n_distinct(stats$label))
message(sprintf("ACTA: SI/Sat facets=%d -> %dx%d grid, %.2f x %.2f in",
                siLay$facets, siLay$ncol, siLay$nrow, siLay$width, siLay$height))

## Per-facet reference data for SatPlot, same reasoning as SIPlot's: drawn from `stats` these lines
## were stamped once per well, so an alpha'd stroke rendered solid. percent/maxSat are scaled to %
## in the plot's own mutate(), so they are scaled here too.
satLab <- unique(as.data.frame(stats)[, c("label", "maxSat", "maxSat_StainQty")])
satLab$maxSat <- as.numeric(satLab$maxSat) * 100
satLab$xpos   <- as.character(satLab$maxSat_StainQty)

## Unit for the quantity axis. StainQty is deliberately unit-agnostic -- it can be a volume, a
## concentration or a dilution factor -- so the UNIT comes from the layout rather than being
## hardcoded, and the axis says what was actually pipetted. "uL" only as a fallback for a workbook
## written before the column existed.
stainUnit <- {
  u <- if ("StainUnit" %in% names(stats)) trimws(as.character(stats$StainUnit)) else character(0)
  u <- unique(u[!is.na(u) & nzchar(u)])
  if (length(u) == 1) u else if (length(u) > 1) paste(u, collapse = "/") else "uL"
}
stainQtyLab <- sprintf("Stain Qty (%s)", stainUnit)

SatPlot<-stats |>
  mutate(StainQty=factor(StainQty, levels=AxisLabels), percent=percent*100, maxSat=maxSat*100) |>
  ##dplyr::filter(StainType!="Costain") |> 
  ggplot(aes(x = `StainQty`, y = percent, fill = Titration)) +
  ## Both lines stay on SatPlot -- unlike SIPlot, the horizontal one marks the saturation plateau,
  ## which is the read-out itself rather than just the height of the tallest point.
  geom_hline(data = satLab, aes(yintercept = maxSat), inherit.aes = FALSE,
             linetype = "3312", color = "grey20") +
  geom_vline(data = satLab, aes(xintercept = xpos), inherit.aes = FALSE,
             linetype = "3312", color = "grey20") +
  ## Foot of the vline, and the LEFT end of the hline. x/y = -Inf pins each to its panel edge, which
  ## is scale-independent -- necessary because facet_wrap(scales="free_y") gives every panel its own
  ## y range.
  geom_text(data = satLab, aes(x = xpos, y = -Inf, label = "x@saturation"), inherit.aes = FALSE,
            angle = 90, hjust = -0.05, vjust = -0.4, size = 2.6, color = "grey20") +
  geom_text(data = satLab, aes(x = -Inf, y = maxSat, label = "saturation"), inherit.aes = FALSE,
            hjust = -0.06, vjust = -0.5, size = 2.6, color = "grey20") +
  geom_jitter(width = 0, alpha = 0.8, size = 5, stroke = 1, shape = 23,height = 0) +  # Enhanced settings
  ##facet_wrap(~ paste0(Target, " ", Fl, " (", GatingMethod, ")"), scales = "free_y", ncol=3) +
  facet_wrap(~label, scales = "free_y", ncol=siLay$ncol) +
  scale_x_discrete(breaks = AxisLabels, labels = AxisLabelsDisp) +
  ## Matches SIPlot: 20% headroom at the top so an edge-pinned label clears the data.
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  labs(
    title = paste0("[Saturation] ", BID), x=stainQtyLab, y="% of Parent",
    subtitle= paste0("Parent pop: ",parentPopLabel," || Analyst: ",unique(stats$Operator))) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, size=12, , face="plain"),
        axis.text.y = element_text(size=12, face="italic"),
        legend.position = "none",
        panel.border = element_rect(color = "grey20", linewidth = NA, size = 0.8, linetype = "solid"), 
        strip.text = element_text(size = 8, face = "bold", color="navy"),
        plot.title = element_text(size = headerSize - 1, face = "bold"),
        axis.title.x = element_text(size=13, face="bold"),
        axis.title.y = element_text(size=13, face="bold"),
        strip.background.x = element_rect(fill = "gray90"), # Background for outer strips
        strip.background.y = element_rect(fill = "#D99311"), # Background for inner strips
        panel.spacing = unit(0.4, "lines"), # Adjust spacing between panels
        plot.subtitle = element_text(size = headerSize-2-1, color = "gray50", face="italic")
  )
SatPlot

## ---- Michaelis-Menten model per antibody -------------------------------------------------------
## SI = Vmax*V/(Km+V). Fitted on EVERY point in the SI plot, Costain included: it is the only
## control still present (Unstained is dropped before gating) and it is the observation at
## volume 0, which the isotherm passes through by construction.
mmFits  <- actaMichaelisMentenFits(stats, group_cols = c("Titration", "label"))
## SAY why a fit was refused. actaMichaelisMenten() already works the reason out -- "SI peaks at
## 2.5 uL then declines -- not a saturating isotherm" -- and it went nowhere: the SIPlot label reads
## a bare "Michaelis-Menten: no fit" and the export shows four empty MM_ columns. A refused fit is
## usually the RIGHT answer (an SI series that turns over is not an isotherm, and reporting a Vmax
## for it would be worse than reporting nothing), but an empty cell cannot say that, so the operator
## is left unable to tell a real finding from a broken pipeline. Measured on an internal run, where the
## question "is the NA fixed?" could have been answered by this one line.
for (.i in which(!isTRUE_vec(mmFits$MM_ok)))
  message(sprintf("[ACTA] %s -- no Michaelis-Menten fit: %s. maxSI/maxSI_StainQty still apply.",
                  mmFits$Titration[.i],
                  if (is.na(mmFits$MM_note[.i])) "reason not recorded" else mmFits$MM_note[.i]))
## Every Michaelis-Menten element -- the model curve, the 90% Vmax vline, that vline's foot label and the
## R2 box -- draws from ONE colour/alpha pair, so they cannot drift apart.
## MM_INK is the same colour with the alpha PRE-MULTIPLIED, needed for geom_label: its `alpha`
## argument applies to the FILL, not the text, so an alpha= there would make the white box
## translucent and let the vline show through the glyphs again. Baking alpha into the colour keeps
## the box opaque while matching the text and border to the curve.
MM_COL   <- "navy"
MM_ALPHA <- 0.5
MM_INK   <- adjustcolor(MM_COL, alpha.f = MM_ALPHA)
mmCurve <- actaMichaelisMentenCurve(mmFits, AxisLabels)
## Annotation text per facet. Placed at Inf/Inf (top-right corner) rather than at computed
## coordinates, because facet_wrap(scales="free_y") gives every panel its own y range -- Inf is
## the only position that means the same thing in all of them.
## One short line only -- R2 when the model fitted, otherwise "no fit". Vmax and 90% Vmax live in
## the export; MM_note is kept on mmFits for diagnosis but is deliberately not drawn, so a narrow
## facet cannot be overflowed by a sentence.
## Annotation x: 80% of the way across the axis, right-aligned, so a fixed 20% of the panel width
## stays blank to its right. hjust at x=Inf could not express this -- that offset is in text-width
## units, not panel units, so it drifts with the label's own length.
liLab <- mmFits
liLab$xpos <- 1 + 0.8 * (length(AxisLabels) - 1)
liLab$lab <- ifelse(isTRUE_vec(mmFits$MM_ok),
                    ## U+00B2 SUPERSCRIPT TWO. A literal character rather than plotmath: parse=TRUE
                    ## would need atop() to keep the two lines, which changes the line spacing and
                    ## cannot carry the "no fit" string through the same ifelse().
                    sprintf("Michaelis-Menten R\u00b2 = %.3f\nx@90%% Vmax = %s uL",
                            mmFits$MM_R2, signif(mmFits$MM_90pct_Vmax, 3)),
                    "Michaelis-Menten: no fit")

## x is the CATEGORY POSITION, not the volume: the dilution series is drawn evenly spaced (see
## actaMichaelisMentenCurve()), and a continuous scale over those positions is what lets a fitted curve be
## overlaid at all -- a discrete scale rejects the continuous x the curve needs. breaks/labels below
## reproduce the previous scale_x_discrete() appearance exactly.
## Reference-line data, ONE ROW PER FACET. Drawing these from `stats` would stamp an identical line
## once per well -- 11 overlapping strokes, which makes an alpha'd line render solid.
maxLab <- unique(as.data.frame(stats)[, c("label", "maxSI_StainQty")])
maxLab$xpos <- match(round(as.numeric(maxLab$maxSI_StainQty), 10), round(AxisLabels, 10))
maxLab <- maxLab[!is.na(maxLab$xpos), , drop = FALSE]
## The 90% Vmax volume generally falls BETWEEN two doses, hence actaVolToPos(). A fit whose value
## lands outside the tested series returns NA and simply gets no line -- the number is still in the
## annotation and the export, but the plot never implies a dose that was not run.
liVline <- mmFits[isTRUE_vec(mmFits$MM_ok), c("label", "MM_90pct_Vmax"), drop = FALSE]
liVline$xpos <- actaVolToPos(liVline$MM_90pct_Vmax, AxisLabels)
liVline <- liVline[!is.na(liVline$xpos), , drop = FALSE]
## Foot labels are ROTATED 90deg, each running up from the axis beside its own line. That attacks the
## real cause of collision: a horizontal label is ~10 characters wide, so it overlaps because its
## footprint is wide relative to the line spacing. Rotated, the footprint is one text-height, and it
## also hugs its own line -- so when two lines are close there is no ambiguity about which label
## belongs to which.
##
## On top of that the pair is pushed APART: each label sits on the side of its line facing AWAY from
## the other line, and the offset grows as the lines converge, so even coincident lines leave a
## readable gap. Direction is computed per facet rather than assumed -- 90% Vmax is usually below
## maxSI but is not guaranteed to be.
liVline$xpos_max <- maxLab$xpos[match(liVline$label, maxLab$label)]
.liGap   <- abs(liVline$xpos - liVline$xpos_max)
.liDelta <- pmax(0.18, (0.60 - .liGap) / 2)          # >= 0.6 apart when the lines coincide
.liDelta[!is.finite(.liDelta)] <- 0.18
.liDir   <- ifelse(!is.finite(liVline$xpos_max) | liVline$xpos <= liVline$xpos_max, -1, 1)
liVline$xlab <- liVline$xpos + .liDir * .liDelta
## maxSI's label takes the opposite side of its own line, so the two point away from each other.
.mi <- match(maxLab$label, liVline$label)
maxLab$xlab <- maxLab$xpos + ifelse(is.na(.mi), 0.18, -.liDir[.mi] * .liDelta[.mi])

## SAVE SETUP, all of it, above the first caller. ConcatPlot is the first run-level figure written,
## and this block used to sit ~120 lines BELOW it: ggsaveIf() was undefined, ACTA_WRITE_PLOTS (which
## it reads) was undefined, and Plots/ did not exist yet. Only the missing directory actually
## surfaced, because run_acta() happens to pre-set ACTA_WRITE_PLOTS in the knit environment -- a
## standalone Source of the script would have failed on the variable first.
ACTA_WRITE_PLOTS <- if (exists("ACTA_WRITE_PLOTS", inherits = TRUE)) isTRUE(get("ACTA_WRITE_PLOTS")) else TRUE
## Braces required: at top level a brace-less `if` body ends at the newline, so a bare `else` on
## the next line is a parse error -- the same trap the .Rmd notes for knitr chunks.
if (ACTA_WRITE_PLOTS) {
  dir.create(path = "Plots/", showWarnings = FALSE)
} else {
  message("[ACTA] PNG export disabled (ACTA_WRITE_PLOTS = FALSE); the titration export is still written.")
}

## Defined HERE rather than further down: ConcatPlot is the first run-level figure saved, and the
## helper has no dependencies, so its old position below simply made the first caller fail.
ggsaveIf <- function(...) if (ACTA_WRITE_PLOTS) ggplot2::ggsave(...) else invisible(NULL)

## ---- ConcatPlot ------------------------------------------------------------------------------
## Deliberately NOT ggcyto: ggcyto facets per sample, and the whole point here is one panel per
## antibody with the wells side by side along x. Plain ggplot over the extracted values instead,
## with addBiexpAxes() relabelling only the y axis -- x is a well INDEX, not a channel.
## Fixed scales, unlike SIPlot's free_y: every panel is fluorescence in the same transformed space,
## so a shared y axis is what makes the groups comparable at a glance.
ConcatPlot <- local({
  df <- do.call(rbind, concatData)
  if (is.null(df) || !nrow(df)) return(NULL)
  df$xpos <- match(round(df$StainQty, 10), round(AxisLabels, 10))
  df <- df[!is.na(df$xpos), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  ## X JITTER is what makes this a pseudocolour plot rather than a stack of coloured rectangles.
  ##
  ## Every event in a well shares one StainQty, so without jitter each column is a single x bin --
  ## a 1-D histogram rendered as a colour strip, with no second dimension for the density to spread
  ## across. That is why the first version looked banded: the geom and the palette were already
  ## identical to p_pseudo's, but p_pseudo has a genuinely continuous x and this did not. FlowJo does
  ## the same thing on a concatenated axis: it spreads events across the column before shading.
  ##
  ## NORMAL spread, not uniform -- this is what produces the tapered oval p_pseudo shows.
  ## Uniform jitter gives every y-slice the same density right out to the column edge, which renders
  ## as a hard-edged RECTANGLE. A real 2D cloud is densest in the middle and thins toward its
  ## boundary, and FlowJo's concatenated axis spreads events the same way. sd = halfW/2.5 puts
  ## ~99% of events inside the column; the rest are clamped to the edge rather than dropped, so no
  ## event is silently lost from a density plot.
  set.seed(ACTA_SEED)
  halfW <- 0.40                                   # column half-width; leaves a gap between wells
  jit <- stats::rnorm(nrow(df), 0, halfW / 2.5)
  df$xjit <- df$xpos + pmax(-halfW, pmin(halfW, jit))
  ## ~38 bins across a column, and ~110 up the range: the taper needs enough x resolution to read
  ## as a curve rather than a staircase. 220 y bins left visible gaps: instrument data
  ## is quantised, so at that resolution many bins fall between actual channel values.
  xBW <- (2 * halfW) / 38

  ## THE NEGATIVE TAIL OWNS BOTH THE AXIS AND THE BIN HEIGHT, and a handful of events can take
  ## them both. Measured 2026-08-28: 12 events of 674,069 (0.002%) reached raw -52,576, which biexp
  ## maps to -23,159 -- 24,159 units below zero against 8,186 for the ENTIRE positive range
  ## 0..5.1e6, because biexp compresses the decades and leaves the negative region near-linear. So
  ## one event three decades below the noise costs three times the whole positive axis: 74.7% of
  ## the panel was empty. And because yBW derives from the same range, every bin was ~3x taller
  ## than it needed to be -- the outliers were coarsening the density everywhere, not just wasting
  ## space.
  ##
  ## THE FLOOR IS PER FACET, AND THE LOWEST ONE WINS. The first version took
  ## `max(pooled 0.001 quantile, trans(-1e3))` and it was wrong twice over -- reported on
  ## DATASET-2, 2026-09-03:
  ##
  ##   * the FIXED RAW CAP assumed negatives live above raw -1000. A dim channel after unmixing
  ##     sits BELOW zero: Antigen Pacific Blue-A had a negative population whose MEDIAN was -341
  ##     transformed (zero is at 1000), so a floor at 394 hid 86% OF THAT FACET, and HLA-DR BUV395
  ##     lost 16%. That is not trimming an outlier tail, it is amputating the population the Stain
  ##     Index is measured against -- strictly worse than the cosmetic problem the cap was added to
  ##     solve, and silent.
  ##   * `max()` made the adaptive arm INERT. The pooled 0.001 quantile was -1974, generous enough
  ##     to have shown Pacific Blue almost entirely; the cap threw it away.
  ##
  ## A POOLED quantile is also the wrong statistic for a shared scale: a dim facet is only 1/n of
  ## the pool, so its own negative can be a rounding error in the pooled tail. Per facet, minimum
  ## across facets, is what a shared axis actually needs -- and it still excludes the original
  ## outliers, because those 12 events were 0.002% of THEIR OWN facet, far beyond its 0.1%.
  ##
  ## Applied with coord_cartesian, NOT scale_y_continuous(limits=): limits DROP rows before the
  ## stat runs, which would change the binning and therefore what the colours mean. coord zooms
  ## after, so the density is computed on every event and only the view is clipped.
  yTop   <- max(df$value, na.rm = TRUE)
  .perFacet <- tapply(df$value, df$label, function(v)
                        unname(stats::quantile(v, 0.001, na.rm = TRUE)))
  yFloor <- suppressWarnings(min(unlist(.perFacet), na.rm = TRUE))
  if (!is.finite(yFloor) || yFloor >= yTop) yFloor <- min(df$value, na.rm = TRUE)
  ## GUARDED, same helper as the two marker axes. The per-facet quantile is the right statistic
  ## for facets that are whole titration SERIES, but it is still a percentile on a near-linear
  ## negative arm: on DATASET-1 the APC facet's own q0.001 is -7,149 and would take 64% of the
  ## shared panel to show 0.14% of the events. The guard caps the negative band; `.hid` below
  ## then reports what that costs each facet, and warns past 0.5%.
  .yFloorQ <- yFloor
  yFloor   <- actaBiexpFloor(yFloor, yTop, actaTrans)
  ## Taking the MINIMUM of the per-facet quantiles bounds the loss at 0.1% of every facet: the one
  ## facet that sets the floor loses its own 0.1%, and every other facet, whose quantile is higher,
  ## loses less. That turns the unbounded silent amputation (86% of one panel) into a per-facet
  ## bound -- EXCEPT where actaBiexpFloor() above then raises the floor, which trades a little more
  ## of the deepest facet for a panel that is not 64% empty. So the bound is measured per facet
  ## rather than asserted, and warned on past 0.5%: a bound that lives only in a comment is a
  ## bound nobody checks.
  .hid <- vapply(split(df$value, df$label), function(v) 100 * mean(v < yFloor, na.rm = TRUE),
                 numeric(1))
  ## The facet label IS the four-line strip text, so quoting it whole spills the message over four
  ## lines; the first line is the marker + channel, which is all a log line needs.
  .facet1 <- function(x) sub("\n.*$", "", x)
  ## Two different claims, so two different messages. Asserting the 0.1% bound when the guard has
  ## just overridden the quantile would be a false statement in the log.
  if (yFloor > .yFloorQ)
    message(sprintf(paste0("[ACTA] ConcatPlot y floor %.0f (raw %.0f) -- GUARDED up from the ",
                           "per-facet 0.1%% quantile %.0f (raw %.0f), which would have given the ",
                           "negative band %.0f%% of the panel. Worst facet now hides %.2f%% of ",
                           "its events (%s)."),
                    yFloor, actaTrans$inverse(yFloor), .yFloorQ, actaTrans$inverse(.yFloorQ),
                    100 * (actaTrans$transform(0) - .yFloorQ) / (yTop - .yFloorQ),
                    max(.hid), .facet1(names(.hid)[which.max(.hid)])))
  else
    message(sprintf(paste0("[ACTA] ConcatPlot y floor %.0f (raw %.0f), set by '%s' -- the lowest ",
                           "of %d per-facet 0.1%% quantiles, so no facet has more than 0.1%% of ",
                           "its events below the view."),
                    yFloor, actaTrans$inverse(yFloor),
                    .facet1(names(.perFacet)[which.min(unlist(.perFacet))]),
                    length(.perFacet)))
  if (any(.hid > 0.5)) warning(sprintf(paste0("ConcatPlot y floor hides %.2f%% of '%s' -- the ",
                                              "per-facet 0.1%% bound is broken; the axis is ",
                                              "clipping a population, not an outlier tail."),
                                       max(.hid), .facet1(names(.hid)[which.max(.hid)])),
                            call. = FALSE)
  yBW <- (yTop - yFloor) / 110
  if (!is.finite(yBW) || yBW <= 0) yBW <- 1
  concatCapSize <- actaCaptionSize(siLay$width)
  p <- ggplot(df, aes(x = xjit, y = value)) +
    ## The SAME helper p_pseudo uses, so the two figures share one colour ramp and one binning
    ## behaviour -- a hand-rolled gradient here was the other half of why they did not match.
    flow_bin2d(binwidth = c(xBW, yBW)) +
    facet_wrap(~label, ncol = siLay$ncol) +
    coord_cartesian(ylim = c(yFloor, NA)) +
    scale_x_continuous(breaks = seq_along(AxisLabels), labels = AxisLabelsDisp) +
    labs(title = paste0("[Concatenation] ", BID), x = stainQtyLab,
         y = "Marker fluorescence (a.u.)",
         subtitle = paste0("Parent pop: ", parentPopLabel, " || Analyst: ", unique(stats$Operator)),
         ## Just the sampling cap. "No gates applied" was redundant -- the y axis is raw marker
         ## fluorescence and no gate is drawn -- and compensation belongs on the figures where a gate
         ## depends on it, not here. "per FCS" rather than "per well": the cap applies to each FILE
         ## read, which is the unit a reader can check against the folder.
         caption = wrapToWidth(sprintf("Concatenated events, up to %s per FCS.",
                                       format(CONCAT_MAX_PER_WELL, big.mark = ",")),
           siLay$width, font_size = concatCapSize, frac = 0.86)) +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white"),
          axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, size = 12, face = "plain"),
          axis.text.y = element_text(size = 12, face = "italic"),
          ## AXIS TITLES, matching SIPlot and SatPlot. This theme set the axis TEXT to match them and
          ## then said nothing about the axis TITLES, so those fell through to theme_minimal()'s
          ## default -- plain, 11pt -- while the other two run-level figures carry bold 13pt. Three
          ## figures that sit next to each other in the report, with one of them captioning its axes
          ## in a different weight.
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          legend.position = "none",
          panel.border = element_rect(color = "grey20", linewidth = NA, size = 0.8,
                                      linetype = "solid"),
          ## Strip styling copied from SatPlot verbatim, so the three run-level figures read as one
          ## set rather than three near-misses: same size, same navy, same grey strip band.
          strip.text = element_text(size = 8, face = "bold", color = "navy"),
          strip.background.x = element_rect(fill = "gray90"),
          strip.background.y = element_rect(fill = "#D99311"),
          plot.subtitle = element_text(face = "italic", size = headerSize - 2 - 1, color = "gray50"),
          plot.caption = element_text(face = "italic", size = concatCapSize, hjust = 0),
          plot.title = element_text(size = headerSize - 1, face = "bold"))
  ## `channel` here is the LAST group's marker channel, which is fine and worth stating: the biexp
  ## y scale is a function of trans + maxValue only, and the channel name is used solely as
  ## isFluorChannel()'s gate inside addBiexpAxes(). Any fluorescence channel yields the same axis --
  ## which is also why one shared y scale across panels is legitimate. xch = NULL because x is a
  ## well index, not a channel, and must keep its own breaks.
  addBiexpAxes(p, xch = NULL, ych = channel, trans = actaTrans, maxValue = actaMaxValue)
})
if (!is.null(ConcatPlot))
  ggsaveIf(paste0("Plots/ConcatPlot_",BIDfile,"_parent_",parentPopFile,"_.png"),
           ConcatPlot, width = siLay$width, height = siLay$height, dpi = 300, limitsize = FALSE)

## ---- Plate map ------------------------------------------------------------------------------
## Layout_Plate drawn as the plate it describes, one figure per PlateID. Built HERE rather than in
## the report for the same reason ConcatPlot is: the script owns the figures, the report prints
## them, and the Plots/ export and the PDF then show the identical object.
##
## FILENAME `PlateMap_<BenchlingID>_<PlateID>.png`, which follows the run-level convention: the
## other run-level figures are `<Kind>_<BenchlingID>_parent_<pop>_.png`, so the KIND leads and the
## id follows. There is no `_parent_` segment because a plate map has no gating parent -- it is the
## workbook, not a population. The PlateID is part of the name rather than decoration: a run can
## span several plates, and without it they would overwrite each other.
##
## Nothing here can stop the run: the plate map is a re-presentation of the workbook and reads no
## FCS, so a failure to draw it must not cost a completed analysis.
PlateMapList <- tryCatch(make_plate_layout_great_again(metadata_file, eln = BIDfile),
                         error = function(e) {
                           warning(sprintf("Plate map skipped: %s", conditionMessage(e)),
                                   call. = FALSE); list() })
for (.pid in names(PlateMapList))
  ggsaveIf(paste0("Plots/PlateMap_", BIDfile, "_", gsub("[^A-Za-z0-9._-]", "_", .pid), ".png"),
           PlateMapList[[.pid]], width = ACTA_PLATE_FIG$width, height = ACTA_PLATE_FIG$height,
           dpi = ACTA_PLATE_FIG$dpi, limitsize = FALSE)

SIPlot<-stats |> 
  mutate(StainQty=match(round(as.numeric(StainQty), 10),
                           round(AxisLabels, 10))) |> 
  ##dplyr::filter(StainType!="Costain") |> 
  ggplot(aes(x = `StainQty`, y = SI, fill = Dirname)) +
  ## maxSI vline, foot-labelled. The horizontal maxSI line was removed: the value it marked is the
  ## height of the highest point, which the point itself already shows.
  geom_vline(data = maxLab, aes(xintercept = xpos), inherit.aes = FALSE,
             linetype = "5122", color = "grey20") +
  ## angle=90 with y=-Inf: hjust runs ALONG the rotated baseline (vertically on screen), so a small
  ## negative value lifts the string clear of the axis; vjust centres it across `xlab`.
  geom_text(data = maxLab, aes(x = xlab, y = -Inf, label = "x@maxSI"), inherit.aes = FALSE,
            angle = 90, hjust = -0.05, vjust = 0.5, size = 2.6, color = "grey20") +
  ## 90% Vmax vline: same linetype, coloured to match the model curve so the pair reads as one
  ## object. Its label sits on the opposite side of its line from the maxSI label -- maxSI is
  ## usually the top dose (hard right) and 90% Vmax lower, so the two cannot collide.
  geom_vline(data = liVline, aes(xintercept = xpos), inherit.aes = FALSE,
             linetype = "5122", color = MM_COL, alpha = MM_ALPHA) +
  geom_text(data = liVline, aes(x = xlab, y = -Inf, label = "x@90% Vmax"), inherit.aes = FALSE,
            angle = 90, hjust = -0.05, vjust = 0.5, size = 2.6,
            color = MM_COL, alpha = MM_ALPHA) +
  geom_jitter(width = 0, alpha = 0.8, size = 5, stroke = 1, shape = 21,height = 0) +  # Enhanced settings
  ## Fitted isotherm. inherit.aes=FALSE: the base plot maps fill=Dirname, which a line has no use
  ## for and which would otherwise split the curve.
  ## No curve at all when the fit failed: mmCurve only carries groups with MM_ok, so a group that
  ## could not be fitted contributes no rows and simply has no line.
  {if (!is.null(mmCurve))
     geom_line(data = mmCurve, aes(x = x, y = SI), inherit.aes = FALSE,
               linetype = "13", color = MM_COL, alpha = MM_ALPHA, linewidth = 0.8)} +
  ## geom_label for the opaque white fill, which stops the 90% Vmax vline showing through the
  ## glyphs. ONE layer: geom_label's `colour` drives the border AND the glyphs, so stacking two
  ## layers to colour them differently printed grey glyphs under navy ones, and the antialiased
  ## edges read as an outline on the text. With border and text sharing MM_INK the glyphs are a
  ## single flat colour, and the box still carries its stroke via label.size.
  geom_label(data = liLab, aes(x = xpos, y = Inf, label = lab), inherit.aes = FALSE,
             hjust = 1, vjust = 1.25, size = 2.6, lineheight = 0.95,
             fill = "white", colour = MM_INK, label.size = 0.3,
             label.padding = unit(0.2, "lines"), label.r = unit(0.1, "lines")) +
  ##facet_wrap(~ paste0(Target, " ", Fl, " (", GatingMethod, ")"), scales = "free_y", ncol=3) +
  facet_wrap(~label, scales = "free_y", ncol=siLay$ncol) +
  scale_x_continuous(breaks = seq_along(AxisLabels), labels = AxisLabelsDisp) +
  ## 20% headroom at the top so the Michaelis-Menten annotation, which is pinned to y = Inf, clears the
  ## highest data point instead of sitting on top of it. Bottom keeps ggplot's usual 5%.
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
  labs(
    ## "- Linear" dropped from the title: it read as a plot variant rather than as a statement about
    ## how the MFI was computed. The caption states it instead, which is where a reader looks for a
    ## methodological note -- and it still marks that these Stain Indices are not on the same footing
    ## as pre-2_86 ACTA output, whose column names are deliberately unchanged.
    title = paste0("[Stain Index] ", BID), x=stainQtyLab, y="Stain Index",
    subtitle= paste0("Parent pop: ",parentPopLabel," || Analyst: ",unique(stats$Operator)),
    ## Same shape as the per-antibody captions: note plus run-level provenance, wrapped to the
    ## figure width and left-aligned by the theme below. Gating method omitted for the same reason
    ## as ConcatPlot -- it is per-panel here and each facet strip already states it.
    caption = wrapToWidth(sprintf(
      "MFI calculated on untransformed (linear) values. || Compensation: %s", compLabel),
      siLay$width, font_size = actaCaptionSize(siLay$width), frac = 0.86)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white"),
        axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1, size=12, , face="plain"),
        axis.text.y = element_text(size=12, face="italic"),
        legend.position = "none",
        panel.border = element_rect(color = "grey20", linewidth = NA, size = 0.8, linetype = "solid"), 
        strip.text = element_text(size = 8, face = "bold", color="grey10"),
        plot.title = element_text(size = headerSize - 1, face = "bold"),
        axis.title.x = element_text(size=13, face="bold"),
        axis.title.y = element_text(size=13, face="bold"),
        strip.background.x = element_rect(fill = "grey90"), # Background for outer strips
        strip.background.y = element_rect(fill = "#D99311"), # Background for inner strips
        panel.spacing = unit(0.4, "lines"), # Adjust spacing between panels
        plot.subtitle = element_text(size = headerSize-2-1, color = "gray50", face="italic"),
        ## Matches the per-antibody plots. Absent, the caption inherited ggplot's default right
        ## alignment, so the one methodological note in the figure sat in a different place from
        ## every other caption in the report.
        plot.caption = element_text(face = "italic", size = actaCaptionSize(siLay$width), hjust = 0),
  )
SIPlot
## PNG export is OPT-OUT (the app exposes it as a checkbox, checked by default). The titration
## export is NOT optional -- it is the file the dashboard consumes, so nothing gates it.
## Note the coupling: the dashboard links to these PNGs, so a run written with plots off produces
## dashboard rows whose figure cells are empty. The PDF report is unaffected either way, because
## it re-renders the plot OBJECTS rather than reading the files.
## ggsave / .Rmd chunk dimensions for the summary plots, taken STRAIGHT from the siLay computed
## next to the plots themselves (see facetLayout() there) -- same mechanism as the flow plots.
## Replaces the old hardcoded si_ncol<-3 + si_panel_w/h<-3.5 block, which (a) had to be kept in
## sync with facet_wrap's ncol by hand and (b) stacked every antibody past the 3rd into a new
## ROW. On a landscape page only ~5.72in of height is usable, so height was always the binding
## constraint under keepaspectratio and each extra row shrank the WHOLE figure: at 12 antibodies
## panels rendered at ~37% of their intended size, with axis text to match. Letting facetLayout()
## widen the grid instead keeps panels far closer to full size.
si_width  <- siLay$width
si_height <- siLay$height
## "_Linear" dropped from the filename with "- Linear" from the title. The dashboard matches on the
## ^SIPlot prefix and its own comment already documented the name as SIPlot_<BID>_parent_<pop>_.png,
## so the file had drifted from the documented convention; this puts them back in step.
ggsaveIf(paste0("Plots/SIPlot_",BIDfile,"_parent_",parentPopFile,"_.png"),
       SIPlot,
       height = si_height,
       width  = si_width,
       unit   = "in",
       dpi    = 300)
ggsaveIf(paste0("Plots/SatPlot_",BIDfile,"_parent_",parentPopFile,"_.png"),
       SatPlot,
       height = si_height,
       width  = si_width,
       unit   = "in",
       dpi    = 300)
## Fix_Perm: single fixation/permeabilisation descriptor for the export, derived from the
## layout's Fix/Perm flags by fixPermLabel() (which rejects Perm-without-Fix and blank flags).
if (!all(c("Fix","Perm") %in% names(stats)))
  stop(sprintf(paste0("The layout sheets must provide `Fix` and `Perm` columns (missing: %s). ",
                      "They sit immediately before StainQty -- copy them in from the workbook ",
                      "in Template/."),
               paste(setdiff(c("Fix","Perm"), names(stats)), collapse=", ")))
## Acquisition_date and Equipment ride in from pData (set per FCS file inside
## annotate_changeParam_pdWrite from $DATE and from $CYT + $CYTSN), so the export records when
## each winning well was acquired and on which instrument -- Acquisition_date is distinct from
## the render date in the export filename.
## Operator-facing block, in THIS order, with everything else following in its existing order.
## These are the columns the analyst reads and completes, so the export styles them maroon/pink
## and every other column blue (see writeTitrationExport()).
## `Titer` and `Titration_Status` are emitted DELIBERATELY BLANK for the analyst to fill in on
## the exported sheet: neither exists in the layout and nothing in the pipeline reads them back,
## so they are created here rather than carried through pData.
## Titer is only interpretable next to the volume it was pipetted into and the cell load it
## stained, so all three sit together in the operator-facing block rather than the titer being
## quoted per-100uL by convention. e6_cells_per_well is pulled forward from the derived block.
## SOP sits directly after Operator, matching where it now sits in Layout_Plate. Being IN this
## vector is what gives it the maroon/pink operator-facing header rather than the navy applied to
## derived columns -- membership here drives both the position (via the relocate() below) and the
## colour (via `primary = exportLeadCols`).
## `Combinatorial_group (n)` is in the block because the analyst supplies it, exactly like ELN_ID
## and SOP -- membership drives both the maroon header and the position. `Combinatorial (L)` is NOT:
## ACTA derives it, so it takes the navy palette while sitting immediately after the value it is
## derived from. Same split as QC_Status, and for the same reason -- position and colour are set
## independently, by the relocate() and by `primary =` respectively.
exportLeadCols <- c("Cat_Num","Vendor","Lot_Num","Clone","Titer","Well_volume_uL",
                    "e6_cells_per_well","Titration_Status",
                    "Test_Material","ELN_ID","Fix_Perm","Operator","SOP",
                    "Combinatorial_group (n)")
statsForExport<-stats |> dplyr::filter(SI==maxSI) |>
  mutate(ScriptVersion=ScriptVersion,
         Fix_Perm=fixPermLabel(Fix, Perm, ids=name),
         Titer=NA_character_, Titration_Status=NA_character_,
         ## The declaration, verbatim, and the logical derived from it. Keyed on Titration for the
         ## same reason QC_Status and Panel are: comboByGroup is named by the titration key, which
         ## is NOT the Dirname once a folder carries more than one marker.
         `Combinatorial_group (n)`=unname(comboByGroup[as.character(Titration)]),
         `Combinatorial (L)`=vapply(unname(comboByGroup[as.character(Titration)]),
                                    actaIsCombinatorialValue, logical(1), USE.NAMES=FALSE),
         ## One PASS/FAIL per antibody, rolled up from that group's report QC (actaQCStatus()).
         ## Keyed on Dirname because qcList is named by it. A group with no QC entry reads FAIL,
         ## matching the rule that an unrun check is not a passed one.
         ## Panel is per antibody group, keyed on Dirname exactly like QC_Status.
         Panel=unlist(lapply(as.character(Dirname), function(.ab)
                        if (!is.null(panelStr[[.ab]])) panelStr[[.ab]] else NA_character_)),
         QC_Status=vapply(as.character(Dirname), function(.ab)
                            if (!is.null(qcList[[.ab]])) actaQCStatus(qcList[[.ab]]) else "FAIL",
                          character(1), USE.NAMES=FALSE)) |>
  ## any_of() so a column missing from stats degrades to "not in the lead block" rather than
  ## erroring. Fix/Perm stay adjacent to each other right after the block; Fix_Perm now sits
  ## inside it, per the requested column order.
  ## QC_Status sits directly after the operator-facing block but is NOT part of it: it is derived
  ## by ACTA, not filled in by the analyst, so it takes the navy secondary palette while keeping its
  ## place next to Titration_Status's neighbourhood. Order and colour are set independently --
  ## position comes from this relocate(), colour from `primary = exportLeadCols` below.
  ## Per-check QC, one column per check (QC_<id>), joined per antibody group. The roll-up
  ## (QC_Status) stays as it was -- these say WHICH check failed, which an overall PASS/FAIL cannot.
  left_join(
    do.call(dplyr::bind_rows, lapply(names(qcList), function(.ab)
      ## qcList is keyed by the TITRATION key, so the column has to say so. It was called Dirname
      ## and joined on Dirname, which is the same thing only while a folder holds one marker.
      data.frame(Titration = .ab, as.data.frame(actaQCColumns(qcList[[.ab]]), check.names = FALSE),
                 check.names = FALSE, stringsAsFactors = FALSE))),
    by = "Titration") |>
  relocate(any_of(c(exportLeadCols, "Combinatorial (L)", "QC_Status", "Panel", "label",
                    "Acquisition_date", "Equipment", "Fix", "Perm"))) |>
  relocate(dplyr::starts_with("QC_"), .after = any_of("QC_Status")) |>
  ## StainUnit next to the quantity it qualifies. It is meaningless on its own, and `stats` column
  ## order comes from the merge/join chain rather than from Layout_Plate, so the two arrived five
  ## columns apart (Row, Column, Markername and Dirname in between). The DASHBOARD inherits this
  ## order -- it reads the newest export's columns -- so fixing it here fixes both.
  relocate(any_of("StainUnit"), .after = any_of("StainQty")) |>
  ## Michaelis-Menten outputs joined per antibody and pushed to the END of the sheet, after everything
  ## else. Derived, so they take the navy secondary palette (they are absent from exportLeadCols).
  ## Order: fit quality, then the two fitted parameters, then the value derived from Km. MM_Kd is
  ## the volume at HALF Vmax, so MM_90pct_Vmax == 9 * MM_Kd by construction -- reporting both makes
  ## that relationship checkable straight from the sheet.
  ##
  ## MM_note travels WITH them, last. It is the reason a fit was refused, and it is populated exactly
  ## when the other four are blank and blank exactly when they are populated -- never both -- so it
  ## reads as the explanation for the gap rather than as a mostly-empty column. Without it the sheet
  ## shows four empty cells and cannot say whether that means "the antibody's SI turns over, which no
  ## saturating isotherm can describe" (usually the case, and a real finding) or "the pipeline broke".
  ## That ambiguity is what prompted "is the failed MM model fixed?" on 2026-08-19; the answer was in
  ## `mmFits` the whole time.
  left_join(mmFits[, c("Titration","MM_R2","MM_Vmax","MM_Kd","MM_90pct_Vmax","MM_note")],
            by="Titration") |>
  relocate(any_of(c("MM_R2","MM_Vmax","MM_Kd","MM_90pct_Vmax","MM_note")), .after = last_col())
## MOVED ahead of the titration export on 2026-08-19: the export's MiFlowCyt appendix sheet reads
## `actaMfc`, so assembling it afterwards left the sheet looking for an object that did not exist
## yet ("object 'actaMfc' not found"). Everything it needs -- stats, statsExportAll, panelList,
## the transform arguments, the resolved compensation mode -- is complete by this point.
## ---- MiFlowCyt metadata ---------------------------------------------------------------------
## ONE assembler, data only, consumed by the report (which typesets it) and later by the
## FlowRepository export (which serialises it) -- see actaMiFlowCyt(). Built HERE because this is the
## first point where every input exists: `stats` (per well), `statsExportAll` (per population),
## panelList, the transform arguments and the resolved compensation mode.
##
## It is MiFlowCyt-STRUCTURED, not MiFlowCyt-COMPLETE: purpose, experiment variables, organization,
## conclusions, specimen source/collection and instrument QC legitimately live in the ELN and are
## emitted as explicit references rather than blanks. `standard$completeness` records that, so no
## consumer can present the result as a compliant submission on its own.
actaMfc <- actaMiFlowCyt(
  stats              = stats,
  info               = infoSheet,
  gating_template    = gating_template,
  qcList             = qcList,
  ## Per-population count / percent / MeFI / robustSD -- the same table StatsExport writes, which is
  ## MiFlowCyt's "gating statistics" and was the one argument with no producer.
  gate_stats         = statsExportAll,
  comp               = list(mode = compMode, raw = compLabel),
  trans_args         = actaTransEff,
  max_value          = actaMaxValue,
  script_version     = ScriptVersion,
  representative_fcs = if (exists("actaRepFcs", inherits = FALSE)) actaRepFcs else NA_character_,
  panel              = if (length(panelList)) panelList[[1]] else NULL,
  downsample         = actaDownsampleReport(actaDownsampleTo,
                                            if (exists("actaEventsRead", inherits = FALSE))
                                              actaEventsRead else integer(0)))

exportName<-paste0(str_replace_all(str_replace_all(lubridate::today(),"-",""),"^(\\d{2})",""),"_",BIDfile,"_TitrationExport_",ScriptVersion,".xlsx")
## Formatted export: the operator-facing lead block in orchid/purple, all derived columns in
## blue, every cell centred with a thin black border, Aptos Narrow 12 throughout -- see
## writeTitrationExport(). writexl cannot style cells, hence openxlsx.
## Audit copies of the instructions workbook, appended after the data sheet so the export is
## self-contained: the plate map, the gating template and the run settings that produced these
## numbers travel with them.
##
## Re-read from the FILE rather than reused from the objects already in memory: `gating_template`
## has been filtered and rewritten by this point (comment rows dropped, method aliases normalised),
## and an audit copy has to be what the workbook actually says, not what the pipeline made of it.
##
## col_types = "text" so every cell is carried across exactly as stored. That also sidesteps the
## coercion that silently turned a zero-padded "001" into "1" -- an audit trail is the last place
## that should happen.
auditSheets <- local({
  want <- c("Info", "gating_template", "Layout_Plate")
  have <- intersect(want, excel_sheets(metadata_file_path))
  setNames(lapply(have, function(sh)
    as.data.frame(suppressMessages(read_excel(metadata_file_path, sheet = sh, col_types = "text")),
                  check.names = FALSE)), have)
})
## Info!miflowcyt_export = TRUE adds a MiFlowCyt appendix sheet, FIRST among the extra sheets because
## it is the substantive one -- the other three are verbatim copies of the workbook. This is the first
## thing that has ever READ that flag: it has sat in all four instructions files unconsumed.
##
## The appendix numbers its items per MIFLOWCYT 1.0, which is NOT the numbering this report uses --
## the standard has four sections with reagents as 2.4 and data analysis as 4. See
## actaMiFlowCytAppendix(). A submission appendix has to match the standard it claims to follow.
##
## GOVERNANCE: this exists so OTHERS can submit their data to FlowRepository. It assembles run
## metadata -- ELN ID, operator, vendor/catalogue/lot, test material -- into one submission-shaped
## table, so it is not for in-house work, and the flag is FALSE in the working copy and the template.
actaMfcExport <- toupper(trimws(actaInfoValue(infoSheet, "miflowcyt_export") %||% "")) %in%
                   c("TRUE", "T", "YES", "1")
if (actaMfcExport) {
  .app <- actaMiFlowCytAppendix(actaMfc)
  if (!is.null(.app) && nrow(.app)) {
    auditSheets <- c(list(MiFlowCyt_metadata = .app), auditSheets)
    message(sprintf("[ACTA] Info!miflowcyt_export = TRUE -- MiFlowCyt appendix sheet added (%d rows).",
                    nrow(.app)))
  }
}
## ---- Pop_stats and SI_stats, sheets of the titration export --------------------------------------
## One workbook, three granularities: TitrationExport is one row per antibody (its best-SI well),
## SI_stats is one row per WELL, Pop_stats is one row per well AND population. They used to be two
## files -- the per-population table was a separate StatsExport workbook -- which meant handing
## somebody two files and explaining which was which.
##
## Built HERE, before the write, for the same reason actaMfc is: writeTitrationExport() takes its
## extra sheets as an argument, so anything assembled after the call cannot be in the file.
##
## Pop_stats: per-population count / percent / MeFI / robustSD for every gating_template row flagged
## stats_export=TRUE, carrying the layout metadata joined from pData. Empty when nothing is flagged,
## in which case the sheet is omitted rather than written blank.
if (!is.null(statsExportAll) && nrow(statsExportAll)) {
  popStatsLeadCols <- c("Population","pop","Dirname","StainQty","StainType","count","percent",
                        "MeFI","robustSD","MeFI_channel")
  ## Slimmed the same way SI_stats is, and for the same reason: now that both sheets live in the
  ## TitrationExport workbook, repeating the reagent and run metadata on every population row
  ## duplicates what the TitrationExport sheet already carries once per antibody. What stays is what
  ## is per WELL or per POPULATION and therefore nowhere else -- the population's identity, its plate
  ## position, the stain quantity, the four statistics, and the gate that produced it.
  popStatsKeep <- c(popStatsLeadCols,
                    "Alias","Markername","Channel","name","PlateID","Row","Column","StainUnit",
                    "GatingMethod","label")
  popStats <- statsExportAll |> dplyr::select(any_of(popStatsKeep)) |>
    relocate(any_of(popStatsLeadCols))
  message("[ACTA] Pop_stats: ", nrow(popStats), " row(s) x ", ncol(popStats), " column(s), ",
          length(unique(popStats$Population)), " population(s).")
} else {
  popStats <- NULL
  message("[ACTA] Pop_stats skipped -- no gating_template row is flagged stats_export=TRUE.")
}

## SI_stats: the Stain Index calculation per well, and the per-group values derived from it. An
## SI-FOCUSED SUBSET of `stats` rather than all of it: the reagent and run metadata (vendor, lot,
## clone, ELN, operator, SOP, instrument, fix/perm) is already on the TitrationExport sheet, so
## repeating it here would be the duplication this consolidation is meant to remove. What is kept is
## everything that is per WELL and therefore NOT on that sheet -- the plate position, the stain
## quantity, the positive/negative medians, the robust SD and the SI itself -- plus the per-group
## maxima and the Michaelis-Menten fit, joined from mmFits because they live per Dirname.
siLeadCols <- c("Titration","Dirname","Alias","Markername","Fl","name","PlateID","Row","Column",
                "StainType","StainQty","StainUnit",
                "pos","neg","robustSD","SI","percent",
                "maxSI","maxSI_StainQty","maxSat","maxSat_StainQty","GatingMethod","label")
siStats <- stats |>
  left_join(mmFits[, c("Titration","MM_R2","MM_Vmax","MM_Kd","MM_90pct_Vmax","MM_note")],
            by = "Titration") |>
  ## any_of() throughout: a column absent from `stats` degrades to "not in this sheet" instead of
  ## erroring, which is how the rest of the export treats its optional columns.
  dplyr::select(any_of(c(siLeadCols, "MM_R2","MM_Vmax","MM_Kd","MM_90pct_Vmax","MM_note"))) |>
  arrange(across(any_of(c("Dirname","StainQty"))))
message("[ACTA] SI_stats: ", nrow(siStats), " well(s) x ", ncol(siStats), " column(s).")

## Order in the workbook: the summary first, then the two statistics sheets, then the metadata
## appendix, then the verbatim workbook copies.
auditSheets <- c(Filter(Negate(is.null), list(SI_stats = siStats, Pop_stats = popStats)), auditSheets)

writeTitrationExport(statsForExport, exportName, primary = exportLeadCols, audit = auditSheets)
for(i in seq_along(plotList_hist)){
  ## THIS antibody's parent. These files belong to one group, so a run-level label would name the
  ## wrong population whenever the groups gate under different parents.
  .pp <- actaParentLabel(parentPopByGroup[antibody[i]], for_file = TRUE)
  ## ...and THIS group's notebook entry, for the same reason: these three files belong to one
  ## antibody, so naming them after every entry in the run would misattribute them.
  .eln <- actaElnLabel(elnByGroup[antibody[i]], for_file = TRUE)
  filename_hist<-paste0("Plots/",antibody[i],"_",.eln,"_parent_",.pp,"_histogram.png")
  filename_pseudo<-paste0("Plots/",antibody[i],"_",.eln,"_parent_",.pp,"_pseudo.png")
  ggsaveIf(filename_hist,plotList_hist[[i]], height=plotDims[[i]]$height, width=plotDims[[i]]$width, unit="in", dpi=300)
  ggsaveIf(filename_pseudo,plotList_pseudo[[i]], height=plotDims[[i]]$height, width=plotDims[[i]]$width, unit="in", dpi=300)
  ## One PNG per gating_template population flagged layout_export=TRUE, named by its alias (was: two
  ## fixed plotLayout1/plotLayout2 half-and-half patchwork images). Driven by plotExportList,
  ## NOT plotLayoutList -- Export and Layout are independent opt-ins, so a population can be
  ## saved without being printed in the report.
  for(al in names(plotExportList[[i]])){
    filename_layout<-paste0("Plots/",antibody[i],"_",.eln,"_parent_",.pp,"_layout_",
                            gsub("[^A-Za-z0-9._-]","_",al),".png")
    ggsaveIf(filename_layout,plotExportList[[i]][[al]], height=plotDims[[i]]$height, width=plotDims[[i]]$width, unit="in", dpi=300)
  }
}

## The separate StatsExport workbook is GONE as of 2_89: its table is the Pop_stats sheet of the
## titration export, assembled above. One file to hand over instead of two that had to be explained.

## Update titration dashboard 
## Run the update FLow dashboard.command file manually.

## Manual markdown export
##acta_dir <- if (grepl("^file://", acta_path))
##  dirname(sub("^file://(localhost)?", "", URLdecode(acta_path))) else dirname(acta_path)
##acta_dir <- normalizePath(acta_dir, mustWork = TRUE)
##setwd(acta_dir)
##ReportMarkdownFile<-grep("\\.Rmd$",list.files(pattern="Report"), value = TRUE)
##rmarkdown::render(ReportMarkdownFile)

## ======================= ANALYSIS-COMPLETE SENTINEL =======================
## Deliberately the LAST statement in this script, and the only thing that proves the analysis ran to
## the end. With report = TRUE the .Rmd SOURCES this script, so an analysis error is caught by
## run_acta()'s render tryCatch and is indistinguishable from a LaTeX failure: the run returned
## ok = TRUE, the app said "report_failed", and the completion message still announced the titration
## export -- one left over from a PREVIOUS run. run_acta() reads this flag instead of assuming.
ACTA_ANALYSIS_COMPLETE <- TRUE
