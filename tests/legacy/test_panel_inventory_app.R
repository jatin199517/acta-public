## The master panel inventory the SHINY APP picks, and the override that makes picking one mean
## something.
##
## WHAT 3.6.14 ADDED, and why it needs its own gate. Four stacked fallbacks decide which inventory
## a run opens -- Info!master_panel_inventory_path, an inventory sitting in the run folder, a
## lab-wide default, the shipped demo -- and until now the first place that answer appeared was
## section 6 of the PDF. The app now reports it up front and can override it for one run.
##
## THE OVERRIDE IS A PRECEDENCE CHANGE, which is the part worth guarding. It is the only input
## that outranks the workbook, so every assertion in section 1 is written against a fixture where
## ALL FOUR sources are present at once: a fixture with only one source could not tell "the
## override won" from "there was nothing else to win against".
##
## AND IT MUST FAIL TO NOTHING, NOT TO SOMETHING ELSE. A chosen path that does not exist, or names
## a folder, resolves to NA -- it does not quietly fall back to the workbook. That is section 2,
## and it is the same rule the Info branch has followed since 2026-11-19: substituting a different
## inventory for the one somebody chose is how the shipped demo stood in for the lab's while the
## report named neither.
##
## NOTHING HERE MAY BE A `fail`. The inventory is provenance, not QC -- a mismatch changes no gate
## and no number -- so a pre-flight record that blocked the Run button would punish the wrong
## thing. Section 5 asserts that directly, on the worst input it can build.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
## AN ASSERTION THAT ERRORS IS A FAILURE, NOT THE END OF THE RUN -- the same reporter
## test_panel_inventory_source.R uses, and for the same reason: without it the first throwing
## predicate aborts the script and every later assertion goes silently unreported.
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- if (!is.null(r$d)) r$d else tryCatch(paste(as.character(detail), collapse = " "),
                                            error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

## THIS CHECKOUT's sources, not the installed namespace -- the whole point is to test the code
## about to be pushed, and an installed 3.6.13 has none of it.
fns   <- actaTestFunctions()
ppath <- fns$actaPanelInventoryPath
precs <- fns$actaPanelInventoryRecords
runA  <- fns$run_acta

EXAMPLE <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                     "TEMPLATE_Master_Panel_Inventory.xlsx")
if (!file.exists(EXAMPLE)) {
  cat("  [skip] the shipped example inventory is missing; nothing to copy as a fixture\n")
  cat("\nPANEL INVENTORY APP TESTS SKIPPED\n"); quit(status = 0L)
}

## ---- the fixture: ALL FOUR SOURCES PRESENT AT ONCE --------------------------------------------
## Four copies of one real inventory, distinguishable only by path. That is deliberate: the
## question under test is WHICH FILE WAS CHOSEN, and giving the copies different contents would
## let a wrong answer still look plausible.
tmp <- file.path(tempdir(), sprintf("acta_inv_%s", as.integer(Sys.time())))
runDir <- file.path(tmp, "run"); dir.create(runDir, recursive = TRUE, showWarnings = FALSE)
cp <- function(nm, dir = tmp) { p <- file.path(dir, nm); file.copy(EXAMPLE, p, overwrite = TRUE); p }
F_RUN  <- cp("Master_Panel_Inventory.xlsx", runDir)   # the run-folder branch
F_INFO <- cp("from_info.xlsx")                        # Info!master_panel_inventory_path
F_LAB  <- cp("lab_default.xlsx")                      # ACTA_MASTER_PANEL_INVENTORY
F_PICK <- cp("picked_in_the_app.xlsx")                # the Browse override
infoWB <- data.frame(master_panel_inventory_path = F_INFO, stringsAsFactors = FALSE)
Sys.setenv(ACTA_MASTER_PANEL_INVENTORY = F_LAB)
CODE <- file.path(ACTA_REPO_ROOT, "inst", "pipeline")

base <- function(x) if (is.null(x) || !length(x) || is.na(x)) NA_character_ else basename(as.character(x))
src  <- function(x) { s <- attr(x, "acta_inventory_source"); if (is.null(s)) NA_character_ else s }

cat("\n-- 0. the fixture is hostile: every source really is present --\n")
chk("the run folder holds an inventory", file.exists(F_RUN))
chk("the Info sheet names a different one", file.exists(F_INFO))
chk("a lab default is set", nzchar(Sys.getenv("ACTA_MASTER_PANEL_INVENTORY")))
chk("and the shipped example exists as a last resort", file.exists(EXAMPLE))

## ---- 1. precedence ----------------------------------------------------------------------------
cat("\n-- 1. the override outranks everything; nothing else moved --\n")
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE, override = F_PICK)
chk("with all four present, the override wins", identical(base(p), "picked_in_the_app.xlsx"), base(p))
chk("...and is tagged 'override', not 'info'", identical(src(p), "override"), src(p))

## THE CONTROLS. Each re-runs the same call with one thing removed, so a bug that returned
## F_PICK unconditionally -- or that broke the branches below it -- shows up here rather than
## passing silently because the override happened to be right.
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE)
chk("CONTROL: with no override the Info sheet wins again",
    identical(base(p), "from_info.xlsx") && identical(src(p), "info"), paste(base(p), src(p)))
p <- ppath(NULL, version_dir = runDir, code_dir = CODE)
chk("CONTROL: with no override and no Info, the run folder wins",
    identical(base(p), "Master_Panel_Inventory.xlsx") && identical(src(p), "run_folder"),
    paste(base(p), src(p)))
p <- ppath(NULL, version_dir = tmp, code_dir = CODE)
chk("CONTROL: with no run-folder inventory either, the lab default wins",
    identical(base(p), "lab_default.xlsx") && identical(src(p), "lab_default"),
    paste(base(p), src(p)))
Sys.unsetenv("ACTA_MASTER_PANEL_INVENTORY")
p <- ppath(NULL, version_dir = tmp, code_dir = CODE)
chk("CONTROL: and with nothing set at all, the shipped example is the fallback",
    identical(src(p), "example"), paste(base(p), src(p)))
Sys.setenv(ACTA_MASTER_PANEL_INVENTORY = F_LAB)
p <- ppath(NULL, version_dir = tmp, code_dir = CODE, override = F_PICK)
chk("the override also outranks the lab default", identical(base(p), "picked_in_the_app.xlsx"),
    base(p))

## A RELATIVE OVERRIDE resolves against the run folder, which is how every other path in the
## workbook behaves. Typed by hand into the app's field, a relative path is the likely spelling.
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE,
           override = "Master_Panel_Inventory.xlsx")
chk("a relative override resolves against the run folder",
    identical(base(p), "Master_Panel_Inventory.xlsx") && identical(src(p), "override"),
    paste(base(p), src(p)))

## ---- 2. a bad override fails to NOTHING ---------------------------------------------------------
cat("\n-- 2. a chosen path that is wrong does NOT fall through --\n")
w <- NULL
p <- withCallingHandlers(ppath(infoWB, version_dir = runDir, code_dir = CODE, override = tmp),
                         warning = function(x) { w <<- conditionMessage(x)
                                                 invokeRestart("muffleWarning") })
chk("an override naming a FOLDER resolves to nothing", is.na(p), base(p))
chk("...and is NOT silently replaced by the workbook's inventory", !identical(base(p), "from_info.xlsx"),
    base(p))
chk("...and says it is a folder, not that it does not exist",
    !is.null(w) && grepl("a folder, not a workbook", w, fixed = TRUE), w)
w <- NULL
p <- withCallingHandlers(ppath(infoWB, version_dir = runDir, code_dir = CODE,
                               override = file.path(tmp, "no_such_file.xlsx")),
                         warning = function(x) { w <<- conditionMessage(x)
                                                 invokeRestart("muffleWarning") })
chk("an override naming a missing file resolves to nothing", is.na(p), base(p))
chk("...and quotes the path that was wrong",
    !is.null(w) && grepl("no_such_file.xlsx", w, fixed = TRUE), w)

## A BLANK OVERRIDE IS NOT AN OVERRIDE. The app's text field is empty on launch and holds "" --
## not NULL -- the moment anyone clicks into it and back out again.
for (blank in list(NULL, "", "   ", NA_character_)) {
  p <- ppath(infoWB, version_dir = runDir, code_dir = CODE, override = blank)
  chk(sprintf("a %s override changes nothing", if (is.null(blank)) "NULL" else
                                               sprintf("'%s'", blank)),
      identical(base(p), "from_info.xlsx") && identical(src(p), "info"), paste(base(p), src(p)))
}

## ---- 3. the option is the mechanism -------------------------------------------------------------
## There are TWO call sites that resolve the inventory -- the pipeline script's panel table and the
## TiterPubs mapper -- so the override travels as an option rather than as an argument threaded
## through both. A parameter passed at one and not the other is a bundle that disagrees with the
## report shipped beside it.
cat("\n-- 3. options(acta.panel_inventory_override) reaches the resolver --\n")
old <- options(acta.panel_inventory_override = F_PICK)
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE)
chk("the option alone overrides the workbook", identical(base(p), "picked_in_the_app.xlsx"), base(p))
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE, override = F_LAB)
chk("an explicit argument beats the option", identical(base(p), "lab_default.xlsx"), base(p))
options(old)
p <- ppath(infoWB, version_dir = runDir, code_dir = CODE)
chk("CONTROL: clearing the option restores the workbook's inventory",
    identical(base(p), "from_info.xlsx"), base(p))

## ---- 4. run_acta(panel_inventory=) ---------------------------------------------------------------
## DRIVEN FOR REAL, not asserted structurally. A test that only read formals(run_acta) would pass
## against a parameter that is accepted and ignored, which is the defect worth catching. The stub
## script records what the PIPELINE sees, because that is the only reader that matters.
cat("\n-- 4. run_acta() hands it to the script, and puts it back afterwards --\n")
stubDir <- file.path(tmp, "code"); dir.create(stubDir, showWarnings = FALSE)
seen <- file.path(tmp, "seen.txt")
writeLines(c('writeLines(as.character(getOption("acta.panel_inventory_override", "<unset>")),',
             sprintf('           %s)', deparse(seen)),
             'ACTA_ANALYSIS_COMPLETE <- TRUE'),
           file.path(stubDir, "ACTA_Script_stub.R"))
options(acta.panel_inventory_override = NULL)
before <- getOption("acta.panel_inventory_override", "<unset>")
invisible(tryCatch(suppressMessages(suppressWarnings(
  runA(runDir, report = FALSE, plots = FALSE, quiet = TRUE, code_dir = stubDir,
       panel_inventory = F_PICK))), error = function(e) NULL))
chk("the stub script ran at all", file.exists(seen))
chk("...and saw the chosen inventory",
    file.exists(seen) && identical(readLines(seen, warn = FALSE)[1], F_PICK),
    if (file.exists(seen)) readLines(seen, warn = FALSE)[1] else "no file")
chk("...and the option is RESTORED when run_acta returns, even on error",
    identical(getOption("acta.panel_inventory_override", "<unset>"), before),
    getOption("acta.panel_inventory_override", "<unset>"))
## CONTROL: the same call with no panel_inventory must leave the script seeing nothing, or the
## assertion above is satisfied by an option somebody else set.
unlink(seen)
invisible(tryCatch(suppressMessages(suppressWarnings(
  runA(runDir, report = FALSE, plots = FALSE, quiet = TRUE, code_dir = stubDir))),
  error = function(e) NULL))
chk("CONTROL: without panel_inventory the script sees no override",
    file.exists(seen) && identical(readLines(seen, warn = FALSE)[1], "<unset>"),
    if (file.exists(seen)) readLines(seen, warn = FALSE)[1] else "no file")

## ---- 5. what the app actually shows --------------------------------------------------------------
cat("\n-- 5. the pre-flight records --\n")
st <- function(r) vapply(r, function(z) z$status, "")
id <- function(r) vapply(r, function(z) z$id, "")

r <- precs(infoWB, version_dir = runDir, code_dir = CODE, override = F_PICK,
           iterations = c("example_tcell_panel", "example_tcell_panel"))
chk("the resolved file rides on the records, so the app resolves ONCE",
    identical(base(attr(r, "acta_inventory_path")), "picked_in_the_app.xlsx"),
    base(attr(r, "acta_inventory_path")))
chk("...with its provenance tag", identical(attr(r, "acta_inventory_source"), "override"))
chk("...and a human label the app can print without a second export",
    identical(attr(r, "acta_inventory_label"), "chosen for this run"),
    attr(r, "acta_inventory_label"))
chk("a repeated iteration is reported once, not twice", length(r) == 2L,
    paste(id(r), collapse = ", "))
chk("the resolution row passes when a chosen file resolves and reads",
    identical(st(r)[1], "pass"), paste(st(r), collapse = ", "))
chk("...and names the file and counts its rows",
    grepl("picked_in_the_app.xlsx", r[[1]]$detail, fixed = TRUE) &&
      grepl("14 reagent row", r[[1]]$detail, fixed = TRUE), r[[1]]$detail)
chk("a declared panel that IS in the inventory passes", identical(st(r)[2], "pass"),
    r[[2]]$detail)

## An iteration the inventory does not hold, with an AUTHORITATIVE file: a real discrepancy.
r <- precs(infoWB, version_dir = runDir, code_dir = CODE, override = F_PICK,
           iterations = "Panel_The_Lab_Renamed")
chk("a declared panel that is NOT in a chosen inventory warns", identical(st(r)[2], "warn"),
    paste(st(r), collapse = ", "))
chk("...using the pipeline's own wording, so pre-flight and the PDF cannot disagree",
    grepl("is not in", r[[2]]$detail, fixed = TRUE) &&
      grepl("picked_in_the_app.xlsx", r[[2]]$detail, fixed = TRUE), r[[2]]$detail)

## The same miss against the SHIPPED EXAMPLE says nothing about the lab's inventory, and must not
## be reported as though it did. This is the example_only / unknown_iteration split.
Sys.unsetenv("ACTA_MASTER_PANEL_INVENTORY")
r <- precs(NULL, version_dir = tmp, code_dir = CODE, iterations = "Panel_The_Lab_Renamed")
chk("the shipped example is amber once a panel is declared, never a pass",
    identical(st(r)[1], "warn"), paste(st(r), collapse = ", "))
chk("...and says plainly that it is the demonstration inventory",
    grepl("demonstration inventory", r[[1]]$detail, fixed = TRUE), r[[1]]$detail)
chk("...and the panel row does NOT claim the lab's inventory lacks the panel",
    !grepl("is not in", r[[2]]$detail, fixed = TRUE), r[[2]]$detail)
## ...but it is NOT amber on a plate with no co-stain panel and no bundle, or the check goes
## yellow on every run in a lab that keeps no inventory, which is how a panel stops being read.
r <- precs(NULL, version_dir = tmp, code_dir = CODE, iterations = character(0))
chk("CONTROL: with nothing declared and no bundle, the example is not an alarm",
    identical(st(r)[1], "pass") && length(r) == 1L, paste(st(r), collapse = ", "))
## ...and IS an alarm again as soon as a bundle will be published, because the bundle carries the
## reagent facts this file supplies.
r <- precs(data.frame(titerpubs_export = "TRUE", stringsAsFactors = FALSE),
           version_dir = tmp, code_dir = CODE, iterations = character(0))
chk("...but it IS an alarm when a TiterPubs bundle will be built",
    identical(st(r)[1], "warn"), paste(st(r), collapse = ", "))
Sys.setenv(ACTA_MASTER_PANEL_INVENTORY = F_LAB)

## A FILE THAT RESOLVES AND CANNOT BE READ is the silent case: the path row would show it, the run
## would skip it, and nothing would say why.
bad <- file.path(tmp, "not_really_a_workbook.xlsx")
writeLines("this is not a zip, let alone an xlsx", bad)
r <- precs(infoWB, version_dir = runDir, code_dir = CODE, override = bad,
           iterations = "example_tcell_panel")
chk("an inventory that resolves but cannot be read warns", identical(st(r)[1], "warn"),
    paste(st(r), collapse = ", "))
chk("...and says so rather than reporting it as configured",
    grepl("no readable reagent rows", r[[1]]$detail, fixed = TRUE), r[[1]]$detail)
chk("...and emits no per-panel rows, which would all repeat that one fact", length(r) == 1L,
    length(r))

## A BAD PATH TYPED INTO THE FIELD. The warning actaPanelInventoryPath() raises is the single most
## useful sentence this panel can show, and left to propagate it would surface in the R console
## behind the browser where nobody is looking.
r <- precs(infoWB, version_dir = runDir, code_dir = CODE,
           override = file.path(tmp, "typo.xlsx"), iterations = "example_tcell_panel")
chk("a typo'd override is reported in the panel, not thrown", identical(st(r)[1], "warn"),
    paste(st(r), collapse = ", "))
chk("...quoting the resolver's own warning", grepl("typo.xlsx", r[[1]]$detail, fixed = TRUE),
    r[[1]]$detail)

cat("\n-- 5b. nothing here may ever block the Run button --\n")
## The app disables Run on any record with status "fail". Built over every shape above at once.
worst <- c(precs(infoWB, version_dir = runDir, code_dir = CODE,
                 override = file.path(tmp, "typo.xlsx"), iterations = "Panel_The_Lab_Renamed"),
           precs(infoWB, version_dir = runDir, code_dir = CODE, override = bad,
                 iterations = "Panel_The_Lab_Renamed"),
           precs(NULL, version_dir = tmp, code_dir = CODE, iterations = "Panel_The_Lab_Renamed"))
chk("no record is ever a fail", !any(st(worst) == "fail"), paste(st(worst), collapse = ", "))
chk("CONTROL: and the worst of them really is amber, not a vacuous pass",
    any(st(worst) == "warn"), paste(st(worst), collapse = ", "))
chk("every record carries the four fields the app renders",
    all(vapply(worst, function(z) all(c("id", "label", "status", "detail") %in% names(z)),
               logical(1))))

## ---- 6. every provenance tag has a label ---------------------------------------------------------
## `%||%` in actaInfoResolvedInventory() is base's, which substitutes for NULL and NOT for NA -- so
## a tag with no row in ACTA_INFO_SOURCE_LABEL prints the literal "NA" in the Source column of the
## report's Info table, which reads as a value rather than as a missing label. "override" was a new
## tag; this is what stops the next one shipping without its label.
cat("\n-- 6. the report can label every tag the resolver can set --\n")
lbl  <- fns$ACTA_INFO_SOURCE_LABEL
## The tag is always .piSrc()'s LAST argument, and the first argument is often a nested call --
## `.piSrc(normalizePath(loc[1], winslash = "/"), "run_folder")` -- so a `[^)]*` scan stops at the
## inner paren and silently finds two of the five. Matched on the trailing `, "tag")` instead, with
## comment lines dropped so prose mentioning .piSrc() cannot invent a tag.
src_lines <- grep("^\\s*##", readLines(file.path(ACTA_REPO_ROOT, "R", "panel_inventory.R"),
                                       warn = FALSE), value = TRUE, invert = TRUE)
src_lines <- grep(".piSrc(", src_lines, fixed = TRUE, value = TRUE)
tags <- unique(gsub('.*,\\s*"(\\w+)"\\).*', "\\1",
                    grep(',\\s*"\\w+"\\)', src_lines, value = TRUE)))
chk("the scan found the tags .piSrc() is called with", length(tags) >= 5L,
    paste(tags, collapse = ", "))
chk("every one of them has a label", all(tags %in% names(lbl)),
    paste(setdiff(tags, names(lbl)), collapse = ", "))
chk("'override' specifically -- the tag 3.6.14 introduced", "override" %in% names(lbl))
chk("CONTROL: a tag that does not exist is NOT in the list", !("made_up" %in% names(lbl)))
## And the label really is what the report would print.
riv <- fns$actaInfoResolvedInventory
got <- riv(fns$.piSrc(F_PICK, "override"))
chk("actaInfoResolvedInventory() prints the label, not NA",
    identical(got$source, "chosen for this run"), got$source)

## ---- 7. the override actually reaches the CHILD PROCESS -------------------------------------------
## The app runs the analysis in a separate `--vanilla` Rscript so Abort can kill it, and the call is
## assembled as TEXT. Everything above could be right and this line still drop the parameter, in
## which case the app would show the chosen inventory in its panel and run without it -- the worst
## available outcome, because the panel would be lying.
##
## EXECUTED, NOT GREPPED. The format string is lifted out of the app's parse tree, filled in with a
## path chosen to break naive quoting, and the result is PARSED AS R and walked. That proves three
## things at once: the parameter is there, the generated code is syntactically valid, and the path
## survives a single quote -- which is a legal character in a folder name on macOS and Windows and
## would otherwise close the string literal early.
cat("\n-- 7. the child process is told which inventory to use --\n")
appFile <- actaTestAppFile()
call0 <- NULL
walk <- function(e) {
  if (is.call(e)) {
    if (identical(e[[1]], quote(sprintf)) && length(e) >= 2L) {
      d <- paste(deparse(e[[2]]), collapse = " ")
      if (grepl("run_acta(", d, fixed = TRUE) && grepl("actaAppChildResult", d, fixed = TRUE))
        call0 <<- e
    }
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  } else if (is.pairlist(e) || is.list(e)) {
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])
  }
}
for (e in parse(appFile)) walk(e)
chk("the child-process command was found in the app", !is.null(call0))
if (!is.null(call0)) {
  ## paste0(.childLoad, "<the template>") -- the literal half is the part under test; .childLoad is
  ## the helper loader and belongs to test_app_package.R.
  lit <- Filter(is.character, as.list(call0[[2]]))
  lit <- if (length(lit)) lit[[length(lit)]] else NA_character_
  chk("...and carries a literal run_acta() template",
      !is.na(lit) && grepl("run_acta(", lit, fixed = TRUE), lit)
  n <- length(gregexpr("%s", lit, fixed = TRUE)[[1]])
  chk("the template takes the six values the app fills in", n == 6L, n)

  ## THE APP'S OWN EXPRESSION, EVALUATED -- not a literal retyped here. The first version of this
  ## section built the no-override case by writing "NULL" into sprintf() itself, which tested
  ## sprintf and nothing else: replacing the app's conditional with .rq(invOv() %||% "") left the
  ## suite green. Found by mutation, which is the only reason it is written this way.
  args  <- as.list(call0)[-(1:2)]
  slot  <- Filter(function(z) grepl("invOv", paste(deparse(z), collapse = ""), fixed = TRUE), args)
  chk("the panel_inventory slot is filled from the app's inventory override", length(slot) == 1L,
      length(slot))
  rq <- function(x) encodeString(as.character(x), quote = "'")   # the app's .rq, see test_arg_safety.R
  nasty <- "/tmp/Jo's Lab/Master_Panel_Inventory.xlsx"
  env <- new.env(parent = globalenv())
  env$.rq <- rq
  if (length(slot) == 1L) {
    env$invOv <- function() NULL
    v0 <- tryCatch(eval(slot[[1]], env), error = function(e) e)
    chk("with no override the app emits the literal NULL, not an empty string",
        identical(v0, "NULL"), if (inherits(v0, "error")) conditionMessage(v0) else v0)
    env$invOv <- function() nasty
    v1 <- tryCatch(eval(slot[[1]], env), error = function(e) e)
    chk("with an override it emits the path, escaped", identical(v1, rq(nasty)),
        if (inherits(v1, "error")) conditionMessage(v1) else v1)
  } else { v0 <- "NULL"; v1 <- rq(nasty) }

  ## ...and the whole generated line PARSES AS R and carries what it should. A single quote is a
  ## legal character in a folder name on macOS and Windows; interpolated raw it would close the
  ## string literal early and the child would die on a syntax error, or worse, evaluate a fragment.
  callOf <- function(txt) {
    got <- tryCatch(parse(text = txt), error = function(e) e)
    if (inherits(got, "error")) return(got)
    hit <- NULL
    f <- function(e) { if (is.call(e)) {
      if (grepl("run_acta$", paste(deparse(e[[1]]), collapse = ""))) hit <<- e
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) f(e[[i]]) } }
    for (e in got) f(e)
    hit
  }
  cc <- callOf(sprintf(lit, rq("/tmp/wd"), "TRUE", "TRUE", rq("/tmp/cd"), v1, rq("/tmp/r.rds")))
  chk("the generated child code is valid R and calls run_acta()",
      !is.null(cc) && !inherits(cc, "error"),
      if (inherits(cc, "error")) conditionMessage(cc) else "")
  chk("...with a panel_inventory argument",
      is.call(cc) && "panel_inventory" %in% names(as.list(cc)),
      if (is.call(cc)) paste(names(as.list(cc)), collapse = ",") else "")
  chk("...carrying the chosen path, single quote and all",
      is.call(cc) && identical(as.list(cc)$panel_inventory, nasty),
      if (is.call(cc)) as.list(cc)$panel_inventory else "")
  c0 <- callOf(sprintf(lit, rq("/tmp/wd"), "TRUE", "TRUE", rq("/tmp/cd"), v0, rq("/tmp/r.rds")))
  chk("and with no override the child really receives NULL",
      is.call(c0) && "panel_inventory" %in% names(as.list(c0)) &&
        is.null(as.list(c0)$panel_inventory),
      if (is.call(c0)) paste(deparse(as.list(c0)$panel_inventory), collapse = "") else "")
}

## ---- 8. every Browse button fills the field it is named for --------------------------------------
## A native file dialog cannot be driven headlessly, so the one thing about Browse that CAN be
## checked is its wiring -- and it is worth checking: pointing browse_invPath at the compensation
## field would leave both the smoke test and the unit gate green while the button silently
## overwrote an unrelated path. Found by mutation (A6).
##
## WRITTEN OVER ALL SIX ROWS, not just the new one. The invariant is the app's own naming rule --
## pathRow() builds its button as paste0("browse_", inputId) -- so a gate for one row would be a
## gate for a coincidence.
cat("\n-- 8. the Browse buttons are wired to their own fields --\n")
pairs <- list(); btns <- character(0)
w2 <- function(e) {
  if (is.call(e)) {
    f <- paste(deparse(e[[1]]), collapse = "")
    a <- as.list(e)[-1]
    if (identical(f, "browseInto") && length(a) >= 2L &&
        is.character(a[[1]]) && is.character(a[[2]]))
      pairs[[length(pairs) + 1L]] <<- c(a[[1]], a[[2]])
    if (identical(f, "actionButton") && length(a) >= 1L && is.character(a[[1]]) &&
        startsWith(a[[1]], "browse_"))
      btns <<- c(btns, a[[1]])
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) w2(e[[i]])
  } else if (is.pairlist(e) || is.list(e)) {
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) w2(e[[i]])
  }
}
for (e in parse(appFile)) w2(e)
chk("the scan found the app's Browse handlers", length(pairs) >= 6L, length(pairs))
bad <- Filter(function(z) !identical(z[1], paste0("browse_", z[2])), pairs)
chk("every handler fills the field its button is named for", !length(bad),
    paste(vapply(bad, function(z) sprintf("%s -> %s", z[1], z[2]), ""), collapse = ", "))
chk("the inventory row specifically",
    any(vapply(pairs, function(z) identical(z, c("browse_invPath", "invPath")), logical(1))))
## pathRow() generates its button id, so only the hand-written ones appear as literals -- but
## every one of THOSE must have a handler, or it is a button that does nothing.
dead <- setdiff(unique(btns), vapply(pairs, function(z) z[1], ""))
chk("no Browse button is left without a handler", !length(dead), paste(dead, collapse = ", "))
chk("CONTROL: the scan really did see literal Browse buttons", length(btns) >= 2L,
    paste(unique(btns), collapse = ", "))

Sys.unsetenv("ACTA_MASTER_PANEL_INVENTORY")
unlink(tmp, recursive = TRUE)
cat(sprintf("\n%s\n", if (ok) "PANEL INVENTORY APP: all checks passed"
                      else "PANEL INVENTORY APP: FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
