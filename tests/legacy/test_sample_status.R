## The sample-description block: Cell_Type / Tissue_Type / Sample_Status / Culture_Status.
##
## WHY THIS NEEDS A GATE OF ITS OWN, AND WHY IT CHANGED SHAPE IN 3.6.8.
##
## It used to assert that Sample_Status MAPPED onto TiterPubs' `Fresh or Frozen` menu. It no
## longer does, deliberately: all four of that menu's options state TWO facts -- thaw state AND
## whether the cells were cultured before staining -- so publishing a bare `Frozen` as
## "Frozen, thawed and incubated with antibody" asserted NO CULTURE STEP, which no operator had
## said. Every run shipped that claim.
##
## The facts now travel as themselves. Three things have to hold, and each can break without the
## other two noticing:
##
##   1. THE FOUR COLUMNS EXIST, in every shipped workbook, together and in order. A reader who
##      sees the material without its tissue or its state has a fraction of the fact.
##   2. THEY REACH THE EXPORT. With the submission column unmapped, the bundled export is the
##      ONLY place these facts still exist -- so membership of the export lead block is now
##      load-bearing rather than cosmetic.
##   3. THE SUBMISSION COLUMN STAYS EMPTY. Not "usually empty": a future edit that quietly
##      restores the composition would put the fabricated claim straight back.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d %||% tryCatch(paste(as.character(detail), collapse = " "), error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
## THE CODE MUST COME FROM THIS TREE, NOT FROM WHATEVER IS INSTALLED.
##
## This file used to `library(ACTA)` and reach in with `ACTA:::`, so every assertion below was
## measured against the installed release rather than the checkout under test. Measured 2026-11-07
## during the 3.6.8 schema change: four assertions failed describing 3.6.7's behaviour while the
## working tree already had the new one. Same rule, and same reason, as the note at the top of
## test_titerpubs_scrub.R. actaTestFunctions() sources R/ from this tree; the installed copy is
## only a fallback for a context that has no inst/.
h <- actaTestFunctions(actaTestAppDir())
.f <- function(nm) if (!is.null(h[[nm]])) h[[nm]] else getFromNamespace(nm, "ACTA")

## ---- 1. the shipped workbooks carry it, beside Cell_Type, defaulted ------------------------
WBS <- c("inst/pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test1/OQ_Test1_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test2/OQ_Test2_Ab_Titration_Instructions.xlsx",
         "inst/extdata/oq_small/OQ_Test3/OQ_Test3_Ab_Titration_Instructions.xlsx")
for (wb in WBS) {
  p <- file.path(ACTA_REPO_ROOT, wb)
  if (!file.exists(p)) { cat("  [skip] not in this layout:", basename(wb), "\n"); next }
  lp <- suppressMessages(readxl::read_excel(p, sheet = "Layout_Plate", col_types = "text"))
  nm <- names(lp)
  j  <- match("Cell_Type", nm); i <- match("Sample_Status", nm)
  ti <- match("Tissue_Type", nm); ci <- match("Culture_Status", nm)
  chk(sprintf("%s has all four sample columns", basename(dirname(p))),
      !any(is.na(c(j, ti, i, ci))), paste(c(j, ti, i, ci), collapse = "/"))
  if (any(is.na(c(j, ti, i, ci)))) next
  ## ORDER, not merely presence: Cell_Type, Tissue_Type, Sample_Status, Culture_Status.
  chk("...contiguous and in order", identical(c(ti, i, ci), c(j + 1L, j + 2L, j + 3L)),
      sprintf("%d/%d/%d/%d", j, ti, i, ci))
  ## Filled exactly where Cell_Type is: a blank plate row stays blank in BOTH.
  chk("...filled on every row that names a test material",
      identical(is.na(lp[[i]]), is.na(lp[[j]])),
      sprintf("%d vs %d filled", sum(!is.na(lp[[i]])), sum(!is.na(lp[[j]]))))
  chk("...and defaults to Frozen",
      length(unique(stats::na.omit(lp[[i]]))) == 1L &&
        identical(unique(stats::na.omit(lp[[i]]))[[1]], "Frozen"),
      paste(unique(stats::na.omit(lp[[i]])), collapse = "/"))
}

## ---- 2. the export keeps the two columns together ----------------------------------------------
lead <- .f("ACTA_EXPORT_LEAD_COLS")
chk("the export lead block keeps all four together, in sheet order",
    identical(lead[match("Cell_Type", lead) + 0:3],
              c("Cell_Type", "Tissue_Type", "Sample_Status", "Culture_Status")),
    paste(lead[match("Cell_Type", lead) + 0:3], collapse = " / "))
## An older workbook has no such column, and the export's lead block is a FIXED schema -- the
## script fills it with NA rather than letting the column vanish. Asserted on the source because
## the alternative is a full pipeline run.
scr <- readLines(file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Script.R"), warn = FALSE)
chk("the pipeline backfills all three with NA when a layout predates them",
    any(grepl('c("Tissue_Type", "Sample_Status", "Culture_Status")', scr, fixed = TRUE)))

## ---- 3. the submission column is UNMAPPED, and must stay that way --------------------------
tpl   <- .f("actaTiterPubsTemplate")()
menus <- if (nzchar(tpl) && file.exists(tpl)) .f("actaTiterPubsMenus")(tpl) else list()
FIELD <- "Fresh or Frozen"

## Their menu still exists -- we are choosing not to use it, which is different from it being gone.
chk("the submission template still HAS a 'Fresh or Frozen' menu", length(menus[[FIELD]]) > 0L,
    length(menus[[FIELD]]))

## ...and every one of its options states a culture fact. This is the measurement the decision
## rests on: if their menu ever gains a culture-neutral option, the trade-off changes and this
## assertion is where that will surface.
chk("every option states a culture fact, so ANY value asserts one",
    all(grepl("cultured|incubated with antibody", menus[[FIELD]])),
    paste(menus[[FIELD]], collapse = " | "))

pub <- function(status = NULL, culture = NULL, info = data.frame(x = 1)) {
  row <- data.frame(Markername = "CD4", Alias = "CD4", stringsAsFactors = FALSE)
  if (!is.null(status))  row$Sample_Status  <- status
  if (!is.null(culture)) row$Culture_Status <- culture
  unname(.f("actaTiterPubsValues")(row, info, menus = menus)[[FIELD]])
}

for (cs in list(NULL, "Frozen", "Fresh", "Frozen, cultured",
                "Frozen, thawed and incubated with antibody")) {
  lab <- if (is.null(cs)) "<no column>" else cs
  chk(sprintf("Sample_Status '%s' publishes NOTHING", lab), is.na(pub(cs)), pub(cs))
}
chk("a filled Culture_Status does not resurrect it either",
    is.na(pub("Frozen", "not cultured")), pub("Frozen", "not cultured"))

## The retired Info cell must not be a back door: a workbook that still carries
## Info!Fresh_or_Frozen (an un-migrated one, or a hand-edit) publishes nothing either.
chk("a surviving Info!Fresh_or_Frozen is NOT used as a fallback",
    is.na(pub(NULL, NULL, data.frame(Variable = "Fresh_or_Frozen",
                                     Value = "Frozen, thawed and incubated with antibody",
                                     stringsAsFactors = FALSE))))

## And the alias table that did the composing is gone, not merely unused -- so it cannot be
## reached by a future edit that re-adds a caller.
chk("ACTA_TP_SAMPLE_STATUS no longer exists in this tree",
    is.null(h[["ACTA_TP_SAMPLE_STATUS"]]))

cat(if (ok) "\nSAMPLE STATUS TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
