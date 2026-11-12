## Master panel inventory lookup: where a costain panel's catalogue and vendor come from.
##
## A titration workbook already names the panel a reagent was costained in
## (Layout_Plate!Costain_Panel_Iteration). A lab that keeps a panel inventory already records what
## is IN that panel -- antigen, fluorochrome, vendor, catalogue, clone. Joining the two means the
## run can report the costain panel's reagents without anybody retyping them into the layout, and
## the retyping is where the errors were.
##
## PROVENANCE, NOT QC. This distinguishes ACTA from ACFA, where the same lookup gates a result
## because the panel feeds patient data. Here a mismatch is REPORTED in the table and changes
## nothing else: no gate fails, no number moves, no artefact is withheld. An inventory that is out
## of date is a documentation problem, and failing a titration over one would be the wrong trade.
##
## Info!master_panel_inventory_path points at the workbook. Blank, missing, or unreadable all mean
## "no inventory": the table is skipped and the run is otherwise identical.

## The inventory's own column names, as the shipped template writes them. Matched case- and
## space-insensitively so a lab that renamed a header slightly still resolves.
## EVERY ENTRY IS OPTIONAL. The reader below resolves each name against the sheet's headers and
## fills an ABSENT one with NA rather than failing, so a lab that deletes a column it does not
## keep gets a blank field, not a stopped run. That is deliberate: the inventory is provenance,
## not QC, and the columns a site maintains are the site's business.
##
## `clonality` added 3.6.8 -- reagent-level, so it belongs here and not on the Info sheet, which
## is run-level. It replaces the old inference "a named Clone means monoclonal", which was ACTA
## asserting a fact the operator never stated and was wrong for a polyclonal with a catalogue
## designation in the Clone field.
ACTA_PI_COLS <- c(antigen = "Antigen", fluorochrome = "Fluorochrome",
                  fl_alias = "Fluorochrome_Alias", marker_alias = "Marker_Alias",
                  vendor = "Vendor", catalog = "Catalog", clone = "Clone",
                  clonality = "Clonality",
                  isotype = "Species / Isotype", type = "Type",
                  iteration = "Panel Iteration")

## Zero-width characters are endemic in hand-maintained inventories -- the real one carries a
## U+200B on the end of "Streptavidin", which is invisible in Excel and makes every exact match
## fail. Stripped on read, once, so nothing downstream has to know.
.piNorm <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u200B-\u200D\uFEFF\u00AD]", "", x)  # ZWSP, ZWNJ, ZWJ, BOM, soft hyphen
  x <- gsub("\u00A0", " ", x)  # non-breaking space
  trimws(gsub("[[:space:]]+", " ", x))
}

## WHERE THE INVENTORY LIVES. An explicit Info path wins; otherwise the shipped example template
## is used, so the feature demonstrates itself on a fresh install instead of silently doing
## nothing. Relative paths resolve against the run folder, which is how every other path in the
## workbook behaves.
## WHERE A PATH CAME FROM DECIDES WHAT AN UNMATCHED ITERATION MEANS, so every branch below says
## so. "info" and "run_folder" are AUTHORITATIVE -- somebody chose that file for this run, so an
## iteration missing from it is a real discrepancy worth reporting. "example" is a FALLBACK
## nobody chose, and an iteration missing from it says nothing at all about the lab's inventory.
## Collapsing the two is what made a real run report its real panel as unknown; see the
## unknown_iteration / example_only split in actaPanelIterationTable().
.piSrc <- function(path, src) { attr(path, "acta_inventory_source") <- src; path }

actaPanelInventoryPath <- function(info = NULL, version_dir = NULL, code_dir = NULL) {
  p <- if (is.null(info)) NA_character_ else actaInfoValue(info, "master_panel_inventory_path")
  if (!is.na(p) && nzchar(p)) {
    if (!grepl("^(/|[A-Za-z]:)", p) && !is.null(version_dir)) {
      cand <- file.path(version_dir, p)
      if (file.exists(cand)) return(.piSrc(normalizePath(cand, winslash = "/"), "info"))
    }
    if (file.exists(p)) return(.piSrc(normalizePath(p, winslash = "/"), "info"))
    warning(sprintf("Info!master_panel_inventory_path points at '%s', which does not exist; the panel inventory table is skipped.", p),
            call. = FALSE)
    return(NA_character_)
  }
  ## AN INVENTORY SITTING IN THE RUN FOLDER WINS over the shipped example. This is what makes a
  ## TiterPubs bundle self-describing: the bundle carries the panel it used, so a reader who
  ## re-runs it resolves the same reagents instead of being silently matched against whatever
  ## example happens to ship with their ACTA install -- or, with the iteration scrubbed to
  ## "Iteration1", against nothing at all.
  if (!is.null(version_dir) && dir.exists(version_dir)) {
    loc <- list.files(version_dir, pattern = "Master_Panel_Inventory.*[.]xlsx$", full.names = TRUE)
    loc <- loc[!grepl("^([~][$]|[.]_)", basename(loc))]
    if (length(loc)) return(.piSrc(normalizePath(loc[1], winslash = "/"), "run_folder"))
  }
  ## A LAB-WIDE DEFAULT, so finding the inventory does not depend on every workbook carrying an
  ## absolute path. A real run is why: its Info sheet was the legacy 9-column layout, which has no
  ## master_panel_inventory_path column at all, so the report told the operator to set a row that
  ## their workbook cannot hold -- while the lab's inventory did contain the panel all along.
  ## Migrating every old workbook is a bandaid; one default is not.
  ##
  ## The ENV VAR WINS over the option: the option is the persistent setting (a line in .Rprofile),
  ## the variable is the one-off override for a single run or a CI job.
  ## TRIMMED: a cell or env var holding only spaces is not a setting, and treating it as one
  ## produced a warning quoting '   ' at the operator.
  labDefault <- trimws(Sys.getenv("ACTA_MASTER_PANEL_INVENTORY", unset = ""))
  if (!nzchar(labDefault)) {
    o <- getOption("acta.master_panel_inventory", default = NULL)
    labDefault <- if (is.null(o)) "" else trimws(as.character(o)[1])
  }
  if (nzchar(labDefault)) {
    ## file.exists() IS TRUE FOR A DIRECTORY. Without the dir.exists() guard a path pointing at a
    ## folder was accepted and tagged "lab default", and the failure surfaced later as a readxl
    ## complaint about 'raw = FALSE' -- a confusing error a long way from its cause.
    if (file.exists(labDefault) && !dir.exists(labDefault))
      return(.piSrc(normalizePath(labDefault, winslash = "/"), "lab_default"))
    if (dir.exists(labDefault))
      warning(sprintf(paste0("The lab default panel inventory ('%s') is a folder, not a workbook, ",
                             "so it was skipped."), labDefault), call. = FALSE)
    else
    ## Said out loud rather than quietly falling through to the demo: a default that is set and
    ## wrong is a configuration error, and the whole point of this branch is to stop the demo
    ## standing in for the lab's inventory without anyone noticing.
      warning(sprintf(paste0("The lab default panel inventory ('%s') does not exist, so it was skipped. ",
                           "Set ACTA_MASTER_PANEL_INVENTORY or options(acta.master_panel_inventory=) ",
                           "to a readable file."), labDefault), call. = FALSE)
  }
  ## The shipped example, found the same way the TiterPubs template is.
  pat <- "^TEMPLATE_Master_Panel_Inventory\\.xlsx$"
  cand <- character(0)
  if (!is.null(code_dir) && nzchar(code_dir))
    cand <- c(cand, file.path(code_dir, "Template"), file.path(dirname(code_dir), "Template"))
  inst <- tryCatch(system.file("pipeline", "Template", package = "ACTA"), error = function(e) "")
  if (nzchar(inst)) cand <- c(cand, inst)
  for (d in cand) {
    if (!dir.exists(d)) next
    f <- list.files(d, pattern = pat, full.names = TRUE)
    if (length(f)) return(.piSrc(normalizePath(f[1], winslash = "/"), "example"))
  }
  NA_character_
}

## The inventory as a tidy frame, or NULL. Never errors: a malformed inventory is a reason to skip
## one report table, not to lose a run that is otherwise complete.
actaPanelInventory <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  d <- tryCatch(as.data.frame(suppressMessages(
         readxl::read_excel(path, sheet = 1, col_types = "text"))),
       error = function(e) { warning(sprintf("panel inventory '%s' could not be read: %s",
                                             basename(path), conditionMessage(e)), call. = FALSE)
                             NULL })
  if (is.null(d) || !nrow(d)) return(NULL)
  key <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(.piNorm(x))))
  have <- key(names(d))
  out <- list()
  for (nm in names(ACTA_PI_COLS)) {
    j <- match(key(ACTA_PI_COLS[[nm]]), have)
    out[[nm]] <- if (is.na(j)) rep(NA_character_, nrow(d)) else .piNorm(d[[j]])
  }
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  out <- out[!is.na(out$iteration) & nzchar(out$iteration), , drop = FALSE]
  ## SET AFTER THE SUBSET, NOT BEFORE. `[.data.frame` keeps names and row.names and drops
  ## everything else, so attaching these any earlier loses them silently and every caption goes
  ## back to not naming its file.
  attr(out, "acta_inventory_path") <- as.character(path)
  src <- attr(path, "acta_inventory_source")
  attr(out, "acta_inventory_source") <- if (is.null(src)) "unknown" else src
  ## PER-LOT DETAIL RIDES ALONG AS AN ATTRIBUTE, not a second argument threaded through four
  ## call sites. Same shape as the two attributes above, and it keeps actaTiterPubsValues()'s
  ## signature stable -- one workbook, one read, one object.
  attr(out, "acta_lot_info") <- .piLotInfo(path)
  out
}

## The inventory's Lot_info sheet, or NULL. Per-LOT facts, which the Master sheet cannot hold:
## Master has one row per reagent, and a lab buys the same catalogue number many times.
##
## OPTIONAL AT EVERY LEVEL, like every other inventory column. No sheet, no columns, no matching
## row, or a blank cell all mean the same thing downstream -- the field publishes empty. An
## inventory that does not track concentrations is not a broken inventory.
ACTA_PI_LOT_COLS <- c(catalog = "Catalog", lot = "Lot",
                      concentration = "Concentration (ug/mL)")

.piLotInfo <- function(path) {
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (!"Lot_info" %in% sheets) return(NULL)
  d <- tryCatch(as.data.frame(suppressMessages(
         readxl::read_excel(path, sheet = "Lot_info", col_types = "text"))),
       error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(NULL)
  key <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(.piNorm(x))))
  have <- key(names(d))
  out <- list()
  for (nm in names(ACTA_PI_LOT_COLS)) {
    j <- match(key(ACTA_PI_LOT_COLS[[nm]]), have)
    out[[nm]] <- if (is.na(j)) rep(NA_character_, nrow(d)) else .piNorm(d[[j]])
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

## The concentration recorded for one catalogue number AND lot. Both must match: a concentration
## from the wrong lot is worse than none, which is the whole reason this sheet is keyed by lot.
## THE EARLIEST MATCHING ROW WINS, so a sheet that has accumulated duplicate entries for one lot
## resolves deterministically instead of depending on read order.
actaLotConcentration <- function(lots, catalog, lot) {
  if (is.null(lots) || !nrow(lots)) return(NA_character_)
  k <- function(z) gsub("[^a-z0-9]", "", tolower(as.character(z)))
  if (is.na(catalog) || is.na(lot)) return(NA_character_)
  hit <- which(k(lots$catalog) == k(catalog) & k(lots$lot) == k(lot))
  if (!length(hit)) return(NA_character_)
  v <- lots$concentration[hit[1]]
  if (is.na(v) || !nzchar(trimws(v))) NA_character_ else trimws(v)
}

## The reagents of ONE panel iteration.
actaPanelReagents <- function(inv, iteration) {
  if (is.null(inv) || !nrow(inv) || is.na(iteration) || !nzchar(.piNorm(iteration))) return(NULL)
  hit <- tolower(inv$iteration) == tolower(.piNorm(iteration))
  if (!any(hit)) return(inv[0, , drop = FALSE])
  inv[hit, , drop = FALSE]
}

## Does a panel reagent correspond to one of the run's measured markers?
##
## Three ways a name can agree, tried in order, because a panel inventory and an FCS header are
## written by different people for different reasons:
##   antigen       "CD14" in the panel is "CD14" in $PnS
##   marker alias  the inventory's own shorthand, which is what a layout usually copies
##   fluorochrome  "BUV615" in the panel is the "BUV615-A" detector -- the match that catches a
##                 panel whose antigen naming differs but whose colour is unambiguous
## A detector suffix (-A/-H/-W) is dropped before comparing; nothing else is normalised away,
## because a looser match would start reporting agreement that is not there.
## PUNCTUATION AND SPACING ARE NOT INFORMATION HERE. The inventory writes "BV421" where the
## cytometer writes "BV 421-A", and "Alexa Fluor 532" where another file might say
## "AlexaFluor532". Comparing on letters and digits alone makes those agree without making
## anything else agree.
.piSquash <- function(x) gsub("[^a-z0-9]", "", tolower(as.character(x)))

.piMatches <- function(reagent, markers, channels) {
  stripped <- sub("-(A|H|W)$", "", c(markers, channels))
  pool_raw <- unique(stats::na.omit(c(markers, channels, stripped)))
  pool <- .piSquash(pool_raw)

  ## Antigen and the inventory's own alias are compared whole: "CD14" is CD14 or it is not.
  for (c0 in unique(stats::na.omit(c(reagent$antigen, reagent$marker_alias))))
    if (nzchar(c0) && .piSquash(c0) %in% pool) return(c0)

  fl <- reagent$fluorochrome
  if (is.na(fl) || !nzchar(fl)) return(NA_character_)
  flq <- .piSquash(fl)
  if (flq %in% pool) return(fl)

  ## A detector often carries a PREFIX the panel does not: the viability channel is
  ## "FVD eFluor 780-A" for a reagent the inventory calls "eFluor 780". A suffix match catches
  ## that, and the four-character floor is what stops it being reckless -- without it the
  ## two-letter "PE" would match "PE-Cy5" and "PE-CF594", reporting a tandem as its own parent
  ## dye. At four characters the shortest real fluorochrome names ("BV421", "BUV496") still
  ## resolve while nothing short enough to collide does.
  if (nchar(flq) >= 4L) {
    hit <- which(endsWith(pool, flq))
    if (length(hit)) return(fl)
  }
  NA_character_
}

## THE TABLE THAT GOES BESIDE REPORT ITEM 2.4.
##
## One row per reagent in the declared panel iteration, each marked matched or no match against
## the markers and channels this run actually measured. Matched rows carry the catalogue and
## vendor, which is the point of the whole exercise: a costain panel's provenance, recorded
## without anyone typing it into the layout.
##
## Returns a list so the caller can distinguish "no inventory configured" from "the declared
## iteration is not in the inventory" -- the second is worth saying out loud, the first is not.
##
## AN UNMATCHED ITERATION IS ONLY NEWS IF SOMEBODY CHOSE THE FILE. When the inventory is the
## shipped example -- a fallback reached because the workbook names no inventory and none sits in
## the run folder -- its three demo panels are not the lab's inventory and the honest answer is
## "none configured", not "your panel is unknown". Reported as example_only for exactly that.
##
## This cost a real diagnosis: a run declaring an iteration the demo does not hold was matched
## file, and the report said the iteration "is not in the master panel inventory" while the real
## inventory held it all along. The caption was true of the file consulted and false of the file
## meant -- which is why every branch below now names the file.
actaPanelIterationTable <- function(inv, iteration, markers, channels) {
  if (is.na(iteration) || !nzchar(.piNorm(iteration)))
    return(list(status = "none", iteration = NA_character_, table = NULL))
  iteration <- .piNorm(iteration)
  if (is.null(inv))
    return(list(status = "no_inventory", iteration = iteration, table = NULL))
  src  <- attr(inv, "acta_inventory_source")
  file <- basename(as.character(attr(inv, "acta_inventory_path")))
  if (!length(file) || is.na(file) || !nzchar(file)) file <- NA_character_
  rg <- actaPanelReagents(inv, iteration)
  if (is.null(rg) || !nrow(rg))
    return(list(status = if (identical(src, "example")) "example_only" else "unknown_iteration",
                iteration = iteration, table = NULL, source = src, file = file))

  markers  <- unique(.piNorm(stats::na.omit(markers)))
  channels <- unique(.piNorm(stats::na.omit(channels)))
  hits <- vapply(seq_len(nrow(rg)), function(i) .piMatches(rg[i, ], markers, channels), "")
  tab <- data.frame(
    Antigen     = rg$antigen,
    Fluorochrome = rg$fluorochrome,
    Matched_to  = ifelse(is.na(hits), NA_character_, hits),
    Verdict     = ifelse(is.na(hits), "no match", "matched"),
    ## Catalogue and vendor are shown for MATCHED rows only. An unmatched row's catalogue is not
    ## evidence about this run -- printing it would invite it to be read as one.
    Catalog     = ifelse(is.na(hits), NA_character_, rg$catalog),
    Vendor      = ifelse(is.na(hits), NA_character_, rg$vendor),
    Clone       = ifelse(is.na(hits), NA_character_, rg$clone),
    Type        = rg$type,
    stringsAsFactors = FALSE)
  tab <- tab[order(tab$Verdict != "matched", tab$Antigen), , drop = FALSE]
  list(status = "ok", iteration = iteration, table = tab, source = src, file = file,
       n_matched = sum(tab$Verdict == "matched"), n_total = nrow(tab))
}

## The caption, written once and used by BOTH the report table and the TiterPubs figure, so the
## two cannot drift into describing the same thing differently.
actaPanelIterationCaption <- function(res) {
  if (is.null(res) || identical(res$status, "none")) return(NA_character_)
  ## NAME THE FILE, ALWAYS. "not in the master panel inventory" sent somebody to check an
  ## inventory that was never opened; "not in <file>" would have ended it in one glance.
  f <- if (is.null(res$file) || is.na(res$file)) "the master panel inventory" else sprintf("'%s'", res$file)
  switch(res$status,
    no_inventory      = sprintf("Costain panel iteration '%s' -- no master panel inventory configured, so its reagents could not be resolved. Set Info!master_panel_inventory_path to resolve them.", res$iteration),
    ## NAMES BOTH WAYS TO FIX IT. The Info row is useless advice to a legacy wide workbook, which
    ## has no such column -- which is what made this caption unactionable on a real run.
    example_only      = sprintf(paste0("Costain panel iteration '%s' -- no master panel inventory is configured for this run, so its reagents could not be resolved. ",
                                       "Only the shipped example (%s) was available, and it holds demonstration panels, not this lab's. ",
                                       "Set Info!master_panel_inventory_path, or point ACTA at the lab's inventory once with ",
                                       "options(acta.master_panel_inventory=) or the ACTA_MASTER_PANEL_INVENTORY environment variable."),
                                res$iteration, f),
    unknown_iteration = sprintf("Costain panel iteration '%s' is not in %s, so its reagents could not be resolved.", res$iteration, f),
    ## The REAGENTS IN SCOPE are named when there is more than one panel on the plate. Without
    ## them two tables captioned only by iteration name look like two views of the same thing,
    ## and a reader cannot tell which titration each one belongs to.
    ## The SOURCE FILE is named here too. This table is provenance, and provenance that does not
    ## say where it came from is half a record -- a reader comparing two runs needs to know
    ## whether they consulted the same inventory.
    ok                = sprintf("Costain panel '%s' from %s: %d of %d reagents matched.%s",
                                res$iteration, f, res$n_matched, res$n_total,
                                ## The reagent list is long -- eight of them on a routine plate --
                                ## and only earns its space when the plate carries MORE THAN ONE
                                ## panel, where two tables captioned alike are otherwise
                                ## indistinguishable. The caller sets `reagents` only then.
                                if (length(res$reagents))
                                  sprintf(" Titrated here: %s.", paste(res$reagents, collapse = ", ")) else ""),
    NA_character_)
}


## Strip the instrument SERIAL from an Equipment string, keeping the model.
##
## ACTA writes Equipment as "<model> (<serial>)" -- "Aurora (SN0000)" -- because the serial is the
## thing that tells two identical cytometers apart in a validated environment. It is also an asset
## identifier, and a public titration repository has no use for it: the MODEL is what makes a
## result interpretable, the serial only says which room it happened in.
##
## Applied when Info!titerpubs_export is TRUE, at the point the stats table is built, so the
## export, the report and the bundle all carry the same scrubbed value. Scrubbing only the bundle
## copy would leave the serial in the PDF inside it, which is the one place nobody would look.
actaScrubEquipmentSerial <- function(x) {
  v <- as.character(x)
  out <- trimws(sub("\\s*\\([^()]*\\)\\s*$", "", v))
  ## A value with no parenthetical is returned unchanged rather than blanked -- not every site
  ## records a serial, and an empty Equipment column is worse than one without a serial.
  ifelse(is.na(v) | !nzchar(trimws(v)), v, ifelse(nzchar(out), out, v))
}

## ---- Info!scrub_confidential -----------------------------------------------------------------
## OFF BY DEFAULT, and only consulted when titerpubs_export is TRUE. Without it the submission
## carries everything the run recorded, which is the right default: a lab that has cleared a
## dataset for release should not have its identifiers silently rewritten.
##
## THE REPLACEMENTS ARE FIXED STRINGS, NOT REDACTIONS. "JD" and the fixture ELN are obviously
## placeholder-shaped, so a reader can tell at a glance that a field was substituted rather than
## wondering whether "JD" is somebody's actual initials. Blanking instead would make the export
## look incomplete and lose the fact that there WAS an operator.
##
## Cell_Type is deliberately NOT here (user's call, 2026-10-13): it describes what was stained
## and a reader needs it, so a site that cannot publish its sample labels should edit them in the
## layout rather than have ACTA guess a substitute.
##
## SOP JOINED THE LIST IN 3.6.11, and the argument against it lost on evidence. The objection was
## that another lab's SOP id need not be confidential, so scrubbing it for everyone overreaches.
## What settled it: `SOP` is free text, labs name the document rather than numbering it, and three
## published bundles went out carrying an internal SOP document TITLE in `Layout_Plate!SOP` and
## `TitrationExport!SOP`. The field is near-useless to a reader either way -- an SOP id means
## nothing to anyone outside the lab that wrote it -- so the asymmetry is total: scrubbing costs a
## reader nothing, and not scrubbing it leaks by default. The toggle is still opt-in, so a lab that
## wants its SOP published simply leaves scrub_confidential off.
## SOP IS BLANKED, NOT PLACEHELD, and that is deliberate. The other three carry placeholder-shaped
## values so a reader can see a field was substituted rather than wonder whether "JD" is somebody's
## initials. SOP is different: an SOP identifier means NOTHING to anyone outside the lab that wrote
## it, so there is nothing for a placeholder to preserve -- and a placeholder is actively worse
## here, because "SOP-000" is itself a string that the value-keyed rewrite below could later match.
## A blank says "withheld" and carries no second-order risk.
ACTA_SCRUB_CONFIDENTIAL <- c(
  Operator                = "JD",
  ELN_ID                  = "EXP26000001",
  ELN_Name                = "ELN",
  SOP                     = "")

## INFO-SHEET fields scrubbed alongside the layout ones. Separate vector because these live on a
## different sheet and reach different artefacts: the Info sheet is copied VERBATIM into the
## bundle's instructions workbook and into the export's Info audit sheet, so a submitter address
## left here is published twice more than the submission itself needs. The TiterPubs maintainer
## asked for it to be kept out of both (2026-10-22).
##
## It stays ON the Info sheet so one workbook still configures the whole run; scrub_confidential
## is what decides whether the real address leaves the building. The placeholder is a working
## address shape rather than a blank, so the filled template still validates against their form.
ACTA_SCRUB_INFO <- c(
  Submitter_Email         = "johndoe@anonymouscompany.com")

## SITE RULES FOR Cell_Type, AND THEY ARE NOT SHIPPED.
##
## Cell_Type is free text a lab writes for itself, and the strings it holds are exactly the
## ones a public submission should not carry -- internal product codes, donor identifiers, mixture
## notation. A site that publishes needs to map its own vocabulary onto plain language, and the
## mapping is SITE-SPECIFIC: the codes in it identify the organisation as surely as its name does.
##
## So ACTA ships NO RULES. The default below is empty, a public install renames nothing, and the
## patterns live in the operator's own R profile:
##
##   options(acta.cell_type_rules = list(
##     c(pattern = "<site regex>", replacement = "Human PBMCs"), ...))
##
## FIRST MATCH WINS, in the order given, because real materials hit more than one rule -- a mix of
## primary cells and a drug product matches both a "primary cells" rule and a "product" rule, and
## the order is the site's decision about which identity to publish. Documented rather than
## sorted by specificity, because "most specific" is not well defined for free text.
##
## Applied only when scrub_confidential is on: these exist for publication, and a local run should
## keep the operator's own words.
ACTA_CELL_TYPE_RULES <- list()

## RENAMED IN 3.6.8, AND THE OLD NAME STOPS THE RUN RATHER THAN BEING IGNORED.
##
## The option was `acta.test_material_rules`, named for the Layout_Plate column that is now
## `Cell_Type`. A site that set the old name in its R profile and upgraded would, under a silent
## rename, get a run where NOTHING MATCHED and every rule quietly stopped firing -- a bundle that
## reports clean because the scrub never ran, which is the one failure this whole layer exists to
## prevent. There is no safe default here: honouring the old name hides the drift, ignoring it
## publishes unscrubbed material. So it is a hard stop with the fix in the message.
##
## Deliberately NOT a silent alias. See the note on template version-locking: this project does
## not ship backward-compatibility shims that make a stale configuration look like a working one.
actaCellTypeRules <- function() {
  old <- getOption("acta.test_material_rules", default = NULL)
  new <- getOption("acta.cell_type_rules",     default = NULL)
  if (!is.null(old) && length(old) && (is.null(new) || !length(new)))
    stop("options(acta.test_material_rules = ...) was renamed to options(acta.cell_type_rules = ",
         "...) in ACTA 3.6.8, along with Layout_Plate!Test_Material -> Cell_Type. The old option ",
         "is set and the new one is not, so NO site rules would be applied and a bundle could ",
         "publish unscrubbed material labels. Rename the option in your R profile to continue.",
         call. = FALSE)
  if (is.null(new) || !length(new)) return(ACTA_CELL_TYPE_RULES)
  new
}

## MARKERS THAT MUST NEVER REACH A PUBLIC BUNDLE.
##
## Some reagents in a costain panel are not publishable at all -- an engineered-receptor detection
## reagent identifies the construct, and through it the programme, no matter how the run is
## scrubbed. Renaming cannot fix that: the panel composition is the disclosure.
##
## SHIPPED EMPTY, like the Cell_Type rules and for the same reason -- the patterns name the
## site's own constructs. Set in the operator's profile:
##
##   options(acta.bundle_blocked_markers = c("<construct regex>", ...))
##
## Matched against the panel AS ACQUIRED (the $PnS and $PnN keywords in the FCS), not against the
## layout: the layout names only the reagent being titrated, so a blocked marker sitting in the
## costain panel is invisible there. The FCS is what actually went to the instrument.
ACTA_BUNDLE_BLOCKED_MARKERS <- character(0)

actaBundleBlockedMarkers <- function() {
  o <- getOption("acta.bundle_blocked_markers", default = NULL)
  if (is.null(o) || !length(o)) return(ACTA_BUNDLE_BLOCKED_MARKERS)
  as.character(o)
}

## Every parameter NAME in a set of FCS files. Read from the HEADER so this costs nothing and can
## run before any data is loaded -- the whole point is to stop before the analysis, not after it.
##
## BOTH $PnS AND $PnN, because which one holds the marker is the acquisition template's choice,
## not a standard. $PnS is the stain name on a BD or Bio-Rad template and is what this read; $PnN
## is the detector, usually `BV421-A`. But a template that was never given stain names leaves $PnS
## absent or blank and the operator types the marker into $PnN instead -- and an FCS re-exported
## by third-party software routinely carries the marker in $PnN with $PnS empty. In either case
## this function returned nothing for that parameter and the gate passed a panel it was written to
## refuse. A detector name cannot match a construct regex, so reading both costs no false
## positives and closes the only way the marker can hide.
##
## `files` IS READ IN FULL. It used to truncate at 50 with no word to anyone: a 96-well plate is
## 96 files, so a blocked reagent sitting in the back half of the plate was never looked at. The
## cap is still here as a bound on a pathological input, but exceeding it now STOPS -- a gate that
## silently inspects part of its input is worse than one that refuses, because it reports a pass.
## Header reads are a few milliseconds each, so a full plate costs well under a second.
actaFcsMarkerNames <- function(files, max_files = 1000L, strict = FALSE) {
  if (!length(files)) return(character(0))
  if (length(files) > max_files)
    stop(sprintf(paste0("actaFcsMarkerNames(): %d files exceeds max_files = %d. Raise max_files ",
                        "rather than scanning part of the set -- a partial scan reports a pass."),
                 length(files), max_files), call. = FALSE)
  bad <- character(0)
  out <- unlist(lapply(files, function(f) {
    k <- tryCatch(flowCore::read.FCSheader(f)[[1]], error = function(e) NULL)
    ## A FILE THAT CANNOT BE READ IS NOT A FILE WITH NO MARKERS. Under `strict` the caller is a
    ## gate and must hear about it; left silent, one unreadable FCS is indistinguishable from a
    ## clean one -- the fail-open shape this package has now met ten times.
    if (is.null(k)) { bad <<- c(bad, basename(f)); return(character(0)) }
    v <- unlist(k[grepl("^\\$P[0-9]+[SN]$", names(k))])
    trimws(as.character(v))
  }))
  if (strict && length(bad))
    stop(sprintf(paste0("actaFcsMarkerNames(): could not read the FCS header of %s. The panel ",
                        "could not be established, so it must not be reported as allowed."),
                 paste(utils::head(bad, 5), collapse = ", ")), call. = FALSE)
  unique(out[nzchar(out) & !is.na(out)])
}

## HARD STOP, not a warning and not a silent skip.
##
## A warning scrolls past and the bundle still gets written; skipping the bundle quietly leaves
## an operator who asked for one wondering where it went. Neither is the right answer when the
## cost of being wrong is publishing a construct. The run stops and says which marker and which
## pattern, so the choice -- drop the reagent, or turn titerpubs_export off -- is the operator's
## and is made deliberately.
actaAssertBundleAllowed <- function(files, enabled, patterns = actaBundleBlockedMarkers()) {
  if (!isTRUE(enabled)) return(invisible(NULL))
  ## PDFTOOLS IS LOAD-BEARING FOR A BUNDLE, AND ONLY FOR A BUNDLE. The leak scan fails closed on a
  ## PDF it cannot read -- correctly, because the report is the one bundle artefact with no scrub
  ## pass of its own -- so without pdftools EVERY bundle would be built, judged unreadable, deleted
  ## and reported as a leak. Measured on a real 30-file bundle: 1 blocking finding, bundle deleted,
  ## 100% of the time. Caught HERE instead, where the run stops before any output is written and
  ## the message says what to install.
  ##
  ## NOT MOVED TO Imports. pdftools needs poppler, and making a heavy system dependency
  ## load-bearing at package-load time is exactly how ACTA 3.0 shipped unloadable on any Mac
  ## without XQuartz -- see .flowmeans_bound(), which uses flowMeans:: for that reason. Someone who
  ## never exports a bundle must not need poppler to run a titration.
  if (!requireNamespace("pdftools", quietly = TRUE))
    stop(paste0("titerpubs_export = TRUE, but the 'pdftools' package is not installed. The bundle's ",
                "leak scan reads the report PDF, and a bundle whose PDF cannot be read must not be ",
                "published -- so the run stopped before writing any output. Install it with ",
                "install.packages(\"pdftools\"), or set Info!titerpubs_export = FALSE to analyse ",
                "this run locally."), call. = FALSE)
  if (!length(patterns)) return(invisible(NULL))
  ## strict = TRUE: from here on this function IS the gate, so an FCS whose header will not parse
  ## stops the run rather than contributing silence to the allow decision.
  ##
  ## NO FILE CAP HERE, deliberately. The cap exists to bound a pathological input for ordinary
  ## callers; for the gate, "there are a lot of files" is not a reason to skip the check, and
  ## refusing a legitimate large run would be a worse answer than reading it (a header read is a
  ## few milliseconds, so even several thousand files cost seconds).
  mk <- actaFcsMarkerNames(files, max_files = Inf, strict = TRUE)
  ## ZERO MARKERS IS NOT A CLEAN PANEL. With patterns set, an empty read means the panel could not
  ## be established, and the old `return(invisible(NULL))` turned that into a pass. Same shape as
  ## .bsUnread(): a reader that cannot read must not report "nothing found".
  ##
  ## A BACKSTOP, NOT THE LIVE PATH. Reading $PnN made this unreachable from a readable file -- the
  ## FCS standard requires $PnN for every parameter, so a file that parses always yields at least
  ## its detector names, and a file that does not parse is stopped on by the strict read above
  ## with a better message (it names the file). test_blocked_markers.R pins that reasoning, so a
  ## change narrowing the read back to $PnS makes this branch live again rather than silent.
  if (!length(mk))
    stop(sprintf(paste0("titerpubs_export = TRUE and blocked markers are configured, but no ",
                        "parameter names could be read from the %d FCS file(s) in this run, so ",
                        "the panel could not be checked. The run stopped before any output was ",
                        "written rather than publishing an unchecked panel."), length(files)),
         call. = FALSE)
  ## A PATTERN THAT CANNOT BE EVALUATED IS AN ERROR, NOT A PASS -- the rule .bsHits() already
  ## states for the leak scan. This swallowed the compile failure and returned character(0), so a
  ## site whose construct pattern had a stray bracket published the construct with the gate green.
  hits <- unlist(lapply(patterns, function(p) {
    m <- tryCatch(mk[grepl(p, mk, perl = TRUE, ignore.case = TRUE)],
                  error = function(e) stop(sprintf(paste0(
                    "acta.bundle_blocked_markers pattern '%s' could not be evaluated (%s). A ",
                    "pattern that cannot run must not be read as 'this panel is publishable'."),
                    p, conditionMessage(e)), call. = FALSE))
    if (length(m)) sprintf("%s (matched '%s')", paste(m, collapse = ", "), p) else character(0)
  }))
  if (!length(hits)) return(invisible(NULL))
  stop(sprintf(paste0("titerpubs_export = TRUE, but this panel carries a marker that must not be ",
                      "published: %s. The bundle was NOT built and the run stopped before any ",
                      "output was written. Either remove the reagent from the panel, or set ",
                      "Info!titerpubs_export = FALSE to analyse it locally."),
               paste(hits, collapse = "; ")), call. = FALSE)
}

## Map one Cell_Type string through the site rules. Returns it unchanged when nothing matches,
## so an unrecognised material is published as written rather than silently blanked -- a reader
## can see it was not mapped, which is the honest failure.
actaMapCellType <- function(x, rules = actaCellTypeRules()) {
  if (!length(rules)) return(x)
  out <- as.character(x)
  for (i in seq_along(out)) {
    v <- out[i]
    if (is.na(v) || !nzchar(trimws(v))) next
    for (r in rules) {
      pat <- r[["pattern"]]; rep <- r[["replacement"]]
      if (is.null(pat) || is.null(rep)) next
      if (isTRUE(tryCatch(grepl(pat, v, perl = TRUE, ignore.case = TRUE), error = function(e) FALSE))) {
        out[i] <- rep; break
      }
    }
  }
  out
}

## Replace the scrubbed Info fields in place. Returns the sheet unchanged when the scrub is off,
## and names what it replaced, because a silent substitution in a submission is worse than none.
actaApplyInfoScrub <- function(info, quiet = FALSE) {
  if (is.null(info) || !is.data.frame(info) || !nrow(info)) return(info)
  long <- actaInfoIsLong(info)
  hit <- character(0)
  for (k in names(ACTA_SCRUB_INFO)) {
    repl <- ACTA_SCRUB_INFO[[k]]
    if (long) {
      j <- which(tolower(trimws(as.character(info$Variable))) == tolower(k))
      if (!length(j)) next
      was <- trimws(as.character(info$Value[j[1]]))
      ## A BLANK IS FILLED, NOT SKIPPED. The scrubbed state of this field is the placeholder, and
      ## a submission with an empty Email fails the maintainer's form -- a legacy workbook that
      ## never carried the row would otherwise produce exactly that.
      if (identical(was, repl)) next
      info$Value[j[1]] <- repl; hit <- c(hit, k)
    } else {
      nm <- tolower(trimws(names(info)))
      j <- which(nm == tolower(k) | trimws(sub("\\s*\\([^()]*\\)\\s*$", "", nm)) == tolower(k))
      if (!length(j)) next
      was <- trimws(as.character(info[[j[1]]][1]))
      if (identical(was, repl)) next
      info[[j[1]]][1] <- repl; hit <- c(hit, k)
    }
  }
  if (length(hit) && !isTRUE(quiet))
    message(sprintf("[ACTA] scrub_confidential: Info sheet %s replaced", paste(hit, collapse = ", ")))
  info
}

## Costain_Panel_Iteration is handled separately because it is NUMBERED, not constant: a plate can
## carry several panels, and collapsing them all to one label would merge distinct panels into one
## in the report and in the submission. Distinct values become Iteration1, Iteration2, ... in order
## of appearance.
ACTA_SCRUB_ITERATION_PREFIX <- "Iteration"

## The same three FCS keywords, which carry the operator and the programme independently of the
## layout. Scrubbed into the BUNDLE'S COPIES only -- the run's own FCS are never rewritten.
ACTA_SCRUB_FCS_KEYWORDS <- c(
  "$OP"   = "JD",
  "$PROJ" = "Titration",
  "$INST" = "Anon")

## Is the scrub switched on? TRUE only when BOTH flags are set: scrub_confidential alone means
## nothing, because there is no submission to scrub for.
actaScrubConfidential <- function(info) {
  on <- function(k) toupper(trimws(actaInfoValue(info, k) %||% "")) %in% c("TRUE", "T", "YES", "1")
  isTRUE(on("titerpubs_export")) && isTRUE(on("scrub_confidential"))
}

## THE PUBLISHED ELN ID, which a site may want to MEAN something.
##
## The default replaces every run's ELN_ID with one constant, which is right when the only goal is
## to hide it -- and wrong when a set of datasets is published together. Six bundles all reading
## EXP26000001 give a reviewer no way to tell which DOI belongs to which dataset, and that is
## exactly what a public repository needs them to do.
##
## Info!scrub_eln_placeholder overrides it per run. Blank or absent keeps the old constant, so
## nothing changes for anyone who does not set it.
##
## READ ONCE AND PASSED AROUND, never re-derived. The workbook scrub and the export scrub are
## different code paths over the same run; if they resolved this separately they could disagree,
## and the failure would be a bundle whose instructions sheet and export name different
## experiments -- the kind of inconsistency no structural check looks for.
actaScrubElnPlaceholder <- function(info = NULL, current = NULL) {
  v <- if (is.null(info)) NA_character_ else actaInfoValue(info, "scrub_eln_placeholder")
  v <- if (length(v) != 1L || is.na(v)) "" else trimws(as.character(v))
  if (!nzchar(v) || identical(toupper(v), "NA")) return(unname(ACTA_SCRUB_CONFIDENTIAL[["ELN_ID"]]))
  ## A placeholder long enough to look like prose is a mistake worth catching here rather than in
  ## a published file.
  if (nchar(v) > 64L)
    stop(sprintf("Info!scrub_eln_placeholder is %d characters; that is a label, not an id.",
                 nchar(v)), call. = FALSE)
  ## THIS VALUE BECOMES PART OF FILE NAMES, so it is constrained to what a file name may hold.
  ##
  ## Not defence in depth -- it is the only check on two real sinks. Before 3.6.4 every replacement
  ## in the scrub map was a hardcoded constant; this is the first operator-supplied one, and the
  ## map is what renames the staged FCS files (.tpStageFcs -> .tpRenameToken) and what names the
  ## bundle zip. .tpStageFcs() cleans "/" and ":" out of each leaf BEFORE the rename is applied, so
  ## a placeholder slips past that guard entirely: "../../x" yields a staged path that escapes the
  ## bundle directory, where dir.create(recursive = TRUE) and write.FCS/file.copy(overwrite = TRUE)
  ## then act on it. A ":" merely ships a name that cannot be unpacked on Windows, which is the
  ## breakage .tpStageFcs()'s own comment exists to prevent.
  ##
  ## XML safety needs no check here -- every workbook-part writer goes through .tpEsc() -- so this
  ## is about paths alone. Refused rather than rewritten: silently turning "a/b" into "a_b" would
  ## publish an id the operator never chose, and this value exists to be recognised by a reader.
  if (grepl("[^A-Za-z0-9._-]", v))
    stop(sprintf(paste0("Info!scrub_eln_placeholder is '%s'. It becomes part of file names in the ",
                        "bundle, so it may hold only letters, digits, dot, underscore and hyphen."),
                 v), call. = FALSE)
  ## A PLACEHOLDER EQUAL TO THE REAL ID PUBLISHES THE REAL ID, AND NO GATE CAN SEE IT.
  ##
  ## This is the one way the feature can defeat the scrub it belongs to, and it is invisible by
  ## construction rather than by oversight: .tpApplyRepl() drops any key whose replacement equals
  ## itself (OQ_Test2's `ELN -> ELN` taught it to, or a clean workbook failed), so the rewrite
  ## becomes a no-op; .tpReplSurvivors() is then asked about the FILTERED key list and never looks
  ## for the value; and .tpSiteTokens() excludes placeholder values from the seeded scan on
  ## purpose. Three independent checks all decline, correctly, for the same reason.
  ##
  ## WARNED, NOT REFUSED. Someone whose ELN id is not itself confidential may set this deliberately
  ## to publish it while still scrubbing Operator and ELN_Name -- that is a legitimate use, and
  ## stopping would be the fail-always trap this project has already been bitten by more than once.
  ## So it says so, loudly, every run, and leaves the decision where it belongs.
  cur <- unique(trimws(as.character(stats::na.omit(current))))
  cur <- cur[nzchar(cur)]
  if (length(cur) && v %in% cur)
    warning(sprintf(paste0("Info!scrub_eln_placeholder is '%s', which is already the ELN_ID in ",
                           "this run -- so the scrub will publish the real identifier unchanged ",
                           "and the bundle gate cannot flag it. Change it if that is not what ",
                           "you intend."), v), call. = FALSE)
  v
}

## Apply the column replacements to a stats/export frame. Columns that are absent are skipped
## rather than created: a workbook that predates a column should not gain one from a scrub.
actaApplyScrub <- function(df, quiet = FALSE, eln = NULL) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(df)
  ## `eln` is the resolved placeholder from actaScrubElnPlaceholder(). NULL keeps the constant,
  ## so every existing caller behaves exactly as before.
  repl <- ACTA_SCRUB_CONFIDENTIAL
  if (!is.null(eln) && length(eln) == 1L && !is.na(eln) && nzchar(eln)) repl[["ELN_ID"]] <- eln
  hit <- character(0)
  for (cn in names(repl)) {
    if (!cn %in% names(df)) next
    was <- unique(stats::na.omit(as.character(df[[cn]])))
    if (!length(was)) next
    df[[cn]] <- repl[[cn]]
    hit <- c(hit, sprintf("%s (%d value(s))", cn, length(was)))
  }

  ## THE PANEL ITERATION IS RENAMED, AND THE OLD NAME IS KEPT.
  ##
  ## The iteration is the key into the master panel inventory, so replacing it before the lookup
  ## runs means the lookup finds nothing and the costain panel's reagents -- catalogue and vendor,
  ## none of it confidential -- vanish from both the report and the submission. Measured: the
  ## report said "'Iteration1' is not in the master panel inventory" and the bundle's Panel
  ## iteration sheet came out empty.
  ##
  ## So the ORIGINAL is carried on the frame as an attribute. The lookup uses it; everything
  ## printed uses the placeholder. The panel's composition is published, the panel's internal name
  ## is not -- which is the point of the switch.
  map <- character(0)
  if ("Costain_Panel_Iteration" %in% names(df)) {
    v <- trimws(as.character(df$Costain_Panel_Iteration))
    orig <- unique(v[!is.na(v) & nzchar(v)])
    if (length(orig)) {
      lab <- paste0(ACTA_SCRUB_ITERATION_PREFIX, seq_along(orig))
      map <- stats::setNames(orig, lab)              # placeholder -> original
      df$Costain_Panel_Iteration <- ifelse(is.na(v) | !nzchar(v), NA_character_,
                                           lab[match(v, orig)])
      hit <- c(hit, sprintf("Costain_Panel_Iteration (%d panel(s) -> %s)",
                            length(orig), paste(lab, collapse = ", ")))
    }
  }
  ## Site Cell_Type mapping, applied with the rest of the scrub. Reported like every other
  ## substitution: a published material label that silently changed is worse than one that did not.
  if ("Cell_Type" %in% names(df) && length(actaCellTypeRules())) {
    was <- as.character(df$Cell_Type)
    now <- actaMapCellType(was)
    if (!identical(was, now)) {
      df$Cell_Type <- now
      hit <- c(hit, sprintf("Cell_Type (%s)",
                            paste(sprintf("'%s' -> '%s'", unique(was[was != now]),
                                          unique(now[was != now])), collapse = ", ")))
    }
  }
  attr(df, "acta_iteration_map") <- map
  if (length(hit) && !isTRUE(quiet))
    message(sprintf("[ACTA] scrub_confidential: replaced %s", paste(hit, collapse = ", ")))
  df
}
