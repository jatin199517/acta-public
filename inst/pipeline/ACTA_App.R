## =============================================================================================
## ACTA desktop app
##
## Local, single-user. Launch with the .command / .bat beside it, or:
##     shiny::runApp("ACTA_App.R", launch.browser = TRUE)
##
## DRIVES ITS OWN FOLDER. Every version file -- script, functions, dashboard generator, report,
## layout workbook -- is discovered in the app's own directory by pattern, exactly as the script
## already resolves its helpers. Copy this file into any version folder and it drives that version
## with no edit.
##
## THIN BY DESIGN. All non-UI behaviour lives in ACTA_Functions.R (actaVersionFiles,
## actaValidateDashboardTemplate, actaScanExports, actaTiterOptions, actaArchiveThenCopy,
## actaRunLog*, run_acta, actaRunArtefacts, actaReadyMessage) where it is exercised by
## tests/test_app_support.R without a browser. Reimplementing any of it here would fork logic
## that is already verified.
##
## THE RUN IS A SEPARATE PROCESS. Not a future/promise: aborting must be able to interrupt a
## running C-level Bioconductor call, and only killing a process does that reliably.
## =============================================================================================

## TWO DIRECTORIES, not one. The app used to assume its own folder held everything -- the scripts,
## Template/, the instructions workbook, Titration_FCS and the outputs. That is true of a version
## folder and false of an installed package, where inst/pipeline/ is code-only and read-only, so the
## app could not be launched in place at all.
##
##   CODE_DIR  where this file lives: the scripts, the .Rmd and Template/ are its siblings.
##   WORK_DIR  the user's run folder: the instructions workbook, Titration_FCS, exports, run log.
##
## They are the SAME directory in a version folder, so nothing changes there. The split mirrors
## `run_acta(version_dir, code_dir)` and the dashboard's ACTA_LAYOUT_DIR, which already work this way.
APP_DIR  <- tryCatch(this.path::this.dir(), error = function(e) getwd())
CODE_DIR <- APP_DIR

## WORK_DIR resolution, in precedence order:
##   1. ACTA_WORK_DIR, if set -- how a launcher or acta_app() points the app at a project folder.
##   2. the CURRENT directory, when the app is running from inside an installed package. Defaulting
##      to CODE_DIR there would put a run log and a results file inside the R library, which on a
##      managed machine is not even writable.
##   3. the CLONE ROOT when the code sits at <root>/inst/pipeline -- a package-layout checkout's
##      working folder is the root, not the code directory.
##   4. CODE_DIR -- the flat version-folder case, where code and work really are one directory.
WORK_DIR <- local({
  e <- Sys.getenv("ACTA_WORK_DIR", unset = "")
  if (nzchar(e)) return(normalizePath(e, mustWork = TRUE))
  lib <- tryCatch(normalizePath(system.file(package = "ACTA"), mustWork = FALSE),
                  error = function(e) "")
  inLib <- function(p) nzchar(lib) && startsWith(normalizePath(p, mustWork = FALSE), lib)
  if (!inLib(APP_DIR)) {
    ## A PACKAGE-LAYOUT CLONE's work folder is the CLONE ROOT, not inst/pipeline. Case 3 returned
    ## APP_DIR, which was right while the code and the run folder were one directory -- in a 3.0
    ## clone APP_DIR is <clone>/inst/pipeline, so the app took the CODE folder as the working one,
    ## looked for the workbook there and would have written run artefacts into the source tree.
    ## It fails closed (no workbook found) rather than corrupting anything, but it fails for a
    ## reason that tells the operator nothing. The launchers already set ACTA_WORK_DIR to the
    ## folder they sit in, which is this same root; this makes a bare runApp() agree with them.
    if (identical(basename(APP_DIR), "pipeline") &&
        identical(basename(dirname(APP_DIR)), "inst"))
      return(normalizePath(dirname(dirname(APP_DIR)), mustWork = TRUE))
    return(APP_DIR)
  }
  w <- normalizePath(getwd())
  ## getwd() IS NOT ENOUGH ON ITS OWN. shiny::runApp() setwd()s to the app directory before it
  ## sources this file, so calling runApp() on the INSTALLED app leaves getwd() pointing at the
  ## library -- exactly what case 2 above exists to avoid, and every run artefact, staged
  ## diagnostic case and log would land there. acta_app() is the supported entry point because it
  ## records the caller's folder in ACTA_WORK_DIR first. Refuse rather than write into a library.
  if (inLib(w))
    stop("ACTA app: this would use the R library as its working folder.\n",
         "  Start R in the folder holding your workbook and Titration_FCS, then:\n",
         "      library(ACTA); acta_app()\n",
         "  or set ACTA_WORK_DIR to that folder before launching.", call. = FALSE)
  w
})
if (!identical(WORK_DIR, CODE_DIR))
  message("ACTA app: code in ", CODE_DIR, "\n            work in ", WORK_DIR)


local({
  need <- c("shiny", "processx", "this.path", "readxl", "openxlsx")
  miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
  if (length(miss)) stop("ACTA_App needs: ", paste(miss, collapse = ", "),
                         "\nInstall with install.packages(c(\"", paste(miss, collapse = "\",\""), "\"))")
})
library(shiny)

## Helpers only -- loading them does not run any analysis. Same two-source rule as the pipeline
## script (see the loader in ACTA_Script_*.R): a sibling ACTA_Functions.R in a version folder, or the
## installed ACTA namespace when the app is running out of a package's inst/pipeline/, where there is
## deliberately no sibling to source. More than one sibling is still fatal -- picking up an archived
## version's helpers is a real failure mode.
FN_FILE <- list.files(CODE_DIR, pattern = "^ACTA_Function.*\\.R$", full.names = TRUE)
if (length(FN_FILE) > 1)
  stop(sprintf("Expected at most one ACTA_Function*.R beside the app in '%s'; found %d.",
               CODE_DIR, length(FN_FILE)))
## THREE SOURCES, not two. The third is a PACKAGE-LAYOUT CLONE with nothing installed -- the
## route the README says provides the Shiny app and the launchers. inst/pipeline/ deliberately
## carries no ACTA_Function*.R, Setup.R installs the DEPENDENCIES and not the package itself, and
## nothing told the user to R CMD INSTALL, so double-clicking a launcher in a fresh clone stopped
## on "the ACTA package is not installed". The helpers are right there in <root>/R/, which is how
## acta_test_paths.R has always driven the same code.
R_DIR <- if (identical(basename(CODE_DIR), "pipeline") &&
             identical(basename(dirname(CODE_DIR)), "inst"))
           file.path(dirname(dirname(CODE_DIR)), "R") else NA_character_
R_FILES <- if (!is.na(R_DIR) && dir.exists(R_DIR))
             sort(list.files(R_DIR, pattern = "[.]R$", full.names = TRUE)) else character(0)

if (length(FN_FILE) == 1) {
  suppressWarnings(source(FN_FILE[[1]], local = FALSE))
} else if (requireNamespace("ACTA", quietly = TRUE)) {
  suppressWarnings(library(ACTA))
} else if (length(R_FILES)) {
  ## SAY WHAT THIS DOES NOT BUY. Loading the helpers from R/ gets the app up so it can report on
  ## the setup, but the pipeline script has its own two-source loader and still needs the package,
  ## so a RUN will stop. Better the operator learns that here than after filling in the form.
  message("ACTA app: the package is not installed; loading the helpers from ", R_DIR,
          "\n  The app will start, but a RUN needs the package itself: R CMD INSTALL . ",
          "from the folder you cloned into.")
  ## ATTACH THE FLOW STACK FIRST, matching the four wholesale import()s in NAMESPACE. Sourcing
  ## the helpers into the global environment does NOT reproduce what the namespace gives them:
  ## a bare S4 generic whose name also exists in base -- colnames() is the one that bit before --
  ## resolves to the base function and never dispatches, so colnames(flowSet) returns 0 channels
  ## instead of 13 and the run is SILENTLY wrong rather than broken. MEASURED on this path before
  ## adding these: 0 vs 13. The flat releases got away with it only because the pipeline script
  ## attaches everything in listOfLibrary before it touches a flowSet; the app reaches helpers
  ## earlier than that, so it cannot rely on the script's own library() calls.
  for (.p in c("flowCore", "flowWorkspace", "ggcyto", "openCyto")) {
    if (!requireNamespace(.p, quietly = TRUE))
      stop(sprintf(paste0("The ACTA package is not installed, so the helpers load from %s -- but ",
                          "that needs '%s' attached and it is not available. Run Setup.R first."),
                   R_DIR, .p), call. = FALSE)
    suppressPackageStartupMessages(library(.p, character.only = TRUE))
  }
  for (.f in R_FILES) suppressWarnings(source(.f, local = FALSE))
} else {
  stop(sprintf(paste0("No ACTA_Function*.R beside the app in '%s', no R/ beside inst/, and the ",
                      "ACTA package is not installed -- the helper library cannot be loaded from ",
                      "any source."), CODE_DIR), call. = FALSE)
}
## Both long jobs run in a `--vanilla` child (so Abort can kill them), which means the child has to
## load the helper library itself -- and it faces the same two-source choice as everything else.
## `FN_FILE[[1]]` used to be spliced in unconditionally; from a package FN_FILE is empty and that
## subscript is an error before the run even starts.
## A broken PATH is inherited by every session, so repair it HERE too -- the app used to diagnose
## the fault (via actaTlmgrStatus in the Check section) and then leave it in place, which meant the
## PDF failed on every run while the message explained why each time. See actaRepairPath().
invisible(actaRepairPath())

## EVERY path spliced into the child's `-e` code goes through this, never through sprintf("'%s'").
## Security review of 2_94, finding M-2: WORK_DIR / CODE_DIR are user-chosen folder paths, and a
## single quote is a legal character in a folder name on both macOS and Windows. Interpolated raw,
## it closes the R string literal early -- so the child either dies with a syntax error or, worse,
## evaluates a fragment that was never meant to be code. processx passes args as an ARRAY and
## invokes no shell, so this was never shell injection; it is a correctness bug in generated R,
## and it feeds regulated output, which is reason enough.
##
## encodeString(quote = "'") is R's own escaper for exactly this: it escapes quotes and backslashes
## so any character in a folder name round-trips into the generated literal. shQuote() would be
## wrong here -- that quotes for a SHELL, and there is no shell in this path.
.rq <- function(x) encodeString(as.character(x), quote = "'")

.childLoad <- {
  if (length(FN_FILE) == 1) sprintf("h<-new.env();sys.source(%s,envir=h);", .rq(FN_FILE[[1]]))
  else if (requireNamespace("ACTA", quietly = TRUE))
    "suppressMessages(library(ACTA));h<-asNamespace('ACTA');"
  ## Same third source as the parent. Without this the parent loaded fine from <root>/R/ and the
  ## child still died on library(ACTA), so a clone user reached the run button and no further.
  ## Same reasoning as the parent: attach the stack before the helpers, or a bare S4 generic in
  ## the child resolves to base and the analysis is quietly wrong.
  else sprintf("%sh<-new.env();%s",
               paste0(sprintf("suppressPackageStartupMessages(library(%s));",
                              c("flowCore", "flowWorkspace", "ggcyto", "openCyto")),
                      collapse = ""),
               paste0(sprintf("sys.source(%s,envir=h);", .rq(R_FILES)), collapse = ""))
}

## ---- global persistence ---------------------------------------------------------------------
## Remembers last-used paths and checkbox state ACROSS launches, globally (not per workbook).
## PRECEDENCE, and this order matters: a populated workbook field always wins, then persistence,
## then blank. A Browse override is session-only -- there is deliberately no write-back to the
## workbook, because it is an Excel file on OneDrive and a lock or a sync conflict would either
## fail the save or silently lose it.
PREF_FILE <- file.path(path.expand("~"), ".acta_app_prefs.rds")
prefsLoad <- function() tryCatch(if (file.exists(PREF_FILE)) readRDS(PREF_FILE) else list(),
                                 error = function(e) list())
prefsSave <- function(p) try(saveRDS(p, PREF_FILE), silent = TRUE)
PREFS <- prefsLoad()
pref <- function(k, default = NULL) if (!is.null(PREFS[[k]])) PREFS[[k]] else default

## Workbook Info value, or NA. Header annotations are stripped the same way the script does.
infoVal <- function(layout, field) {
  if (is.na(layout) || !file.exists(layout)) return(NA_character_)
  v <- tryCatch({
    d <- suppressMessages(readxl::read_excel(layout, sheet = "Info", .name_repair = "minimal"))
    names(d) <- actaStripHeaderAnnotation(names(d))
    if (!field %in% names(d)) return(NA_character_)
    x <- trimws(as.character(d[[field]][1]))
    if (!length(x) || is.na(x) || !nzchar(x) || toupper(x) == "NA") NA_character_ else x
  }, error = function(e) NA_character_)
  v
}
## workbook -> persistence -> blank
resolvePath <- function(layout, field, prefKey) {
  wb <- infoVal(layout, field)
  if (!is.na(wb)) return(list(value = wb, source = "workbook"))
  p <- pref(prefKey)
  if (!is.null(p) && nzchar(p)) return(list(value = p, source = "remembered"))
  list(value = "", source = "unset")
}

## ---- path pickers ----------------------------------------------------------------------------
## Native per platform, so no extra dependency: osascript on macOS (a real Finder dialog, and it
## needs no XQuartz), choose.dir/choose.files on Windows, tcltk elsewhere. The TEXT FIELD stays
## authoritative -- Browse only fills it in, so a pasted or workbook-supplied path is never
## second-guessed, and the app still works if every dialog is unavailable.
pickPath <- function(type = c("dir", "file"), start = WORK_DIR) {
  type <- match.arg(type)
  start <- if (!is.null(start) && nzchar(start) && dir.exists(start)) start else path.expand("~")
  out <- tryCatch({
    if (Sys.info()[["sysname"]] == "Darwin") {
      kind <- if (type == "dir") "choose folder" else "choose file"
      ## The start path is handed over as an ARGUMENT, never interpolated into the script. A path is
      ## workbook-supplied (Info!titration_export_dir reaches here through Browse), and a directory
      ## may legally contain a double quote -- interpolating one would close the AppleScript string
      ## literal and let the remainder of the name run as AppleScript. shQuote() cannot help: it
      ## quotes for the SHELL, and the injection happens one level further in, inside osascript.
      scr <- sprintf('on run argv
  set p to POSIX path of (%s with prompt "Select %s" default location POSIX file (item 1 of argv))
  return p
end run', kind, if (type == "dir") "folder" else "file")
      sub("\n$", "", system2("osascript", c("-e", shQuote(scr), shQuote(start)),
                             stdout = TRUE, stderr = FALSE))
    } else if (.Platform$OS.type == "windows") {
      if (type == "dir") utils::choose.dir(default = start) else utils::choose.files(default = start, multi = FALSE)
    } else if (requireNamespace("tcltk", quietly = TRUE)) {
      if (type == "dir") tcltk::tk_choose.dir(default = start) else tcltk::tk_choose.files(multi = FALSE)
    } else NA_character_
  }, error = function(e) NA_character_)
  if (length(out) != 1 || is.na(out) || !nzchar(out)) NA_character_ else normalizePath(out, mustWork = FALSE)
}

## A blank path field MEANS the app's own folder -- stated in the UI next to Browse. Resolving it
## here rather than pre-filling the box keeps "unset" visually distinct from "deliberately set to
## this folder", while everything downstream still receives a real directory.
effPath <- function(v) { v <- trimws(v %||% ""); if (!nzchar(v)) WORK_DIR else v }
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

## ---- status rendering -------------------------------------------------------------------------
## Colour is never the only signal: every row carries a glyph too. Warn and skip share an amber,
## so the glyph is what separates "ran, and here is a caveat" from "did not run" -- and a skip is
## never shown as a pass.
STATUS_CSS <- "
 body{font:13px/1.5 -apple-system,'Segoe UI',Roboto,Helvetica,Arial,sans-serif;color:#14181d}
 .facet{border:1px solid #d5dae0;padding:14px 16px;margin-bottom:14px;background:#fff}
 .facet h4{margin:0 0 10px;font-size:12px;letter-spacing:.13em;text-transform:uppercase;color:#5b6570}
 .chk{display:grid;grid-template-columns:18px 1fr auto;gap:9px;padding:4px 0;
      border-bottom:1px solid #eef1f4;align-items:baseline}
 .chk:last-child{border-bottom:0}
 .g{font-weight:700;text-align:center}
 .pass .g,.pass .s{color:#0b6b57} .fail .g,.fail .s{color:#b3261e}
 .warn .g,.warn .s{color:#8a5a00} .skip .g,.skip .s{color:#8a5a00}
 .s{font-size:10px;letter-spacing:.1em;text-transform:uppercase;font-weight:700}
 .d{color:#5b6570;font-size:11.5px}
 .src{font-size:10px;letter-spacing:.08em;text-transform:uppercase;color:#8a919a;margin-left:6px}
 .log{font-family:ui-monospace,Menlo,Consolas,monospace;font-size:11px;white-space:pre-wrap;
      background:#f7f8fa;border:1px solid #e3e7ec;padding:9px;height:230px;overflow:auto}
 .ready{padding:9px 12px;border-left:3px solid #0b6b57;background:#f2f8f6;font-weight:600}
 .bad{padding:9px 12px;border-left:3px solid #b3261e;background:#fdf3f2}
 .muted{color:#8a919a}
 /* Progress. Driven by knitr's OWN percentage, scraped from the child's output -- not a guessed
    animation, so it cannot claim progress the run has not made. Indeterminate stripes until the
    render starts reporting, because before that there is genuinely no number to show. */
 .pbar{height:6px;background:#e8ebee;margin:8px 0 4px;overflow:hidden}
 .pbar>i{display:block;height:100%;background:#0e7c86;width:0;transition:width .4s linear}
 .pbar.indet>i{width:100%;background:repeating-linear-gradient(90deg,#0e7c86 0 10px,#9ecdd2 10px 20px);
               animation:sl 1s linear infinite}
 @keyframes sl{from{background-position:0 0}to{background-position:20px 0}}
 .pstage{font-size:11px;color:#5b6570}
 /* The chain control. Emphasis is STRUCTURAL -- a rule and weight -- not a colour wash: amber is
    the warn/skip status colour on every check row, so a yellow band here would read as
    needs-attention on every launch. Teal is the dashboard family accent and collides with none
    of the four status colours. */
 .chain{border-left:3px solid #0e7c86;background:#f4f9fa;padding:10px 12px;margin:12px 0}
 .chain .checkbox label{font-weight:700}
 .chain .form-group{margin-bottom:0}
 .chainnote{font-size:11px;color:#5b6570;margin:2px 0 0 22px}
 .grp{border:1px solid #e3e7ec;padding:8px 10px;background:#fbfcfd}
 .grp+.grp{margin-top:8px}
 .grpsum{cursor:pointer;list-style:none;display:grid;grid-template-columns:18px 1fr auto auto;
         gap:10px;align-items:baseline}
 .grpsum::-webkit-details-marker{display:none}
 .grpsum::after{content:'▾';color:#8a919a;font-size:12px;margin-left:8px}
 .grp[open] .grpsum::after{content:'▴'}
 .grptitle{font-weight:700;font-size:12px;letter-spacing:.11em;text-transform:uppercase}
 .grpcount{font-size:11px;color:#8a919a}
 .grpsum.pass .g,.grpsum.pass .s{color:#0b6b57} .grpsum.fail .g,.grpsum.fail .s{color:#b3261e}
 .grpsum.warn .g,.grpsum.warn .s{color:#8a5a00}
"
## A dependency that cannot be installed from R carries the URL to fetch it, so it has to be
## clickable rather than a string the operator retypes.
linkify <- function(s) {
  m <- regmatches(s, regexpr("https?://[^ ,)]+", s))
  if (!length(m)) return(s)
  parts <- strsplit(s, m[[1]], fixed = TRUE)[[1]]
  tagList(parts[1], tags$a(href = m[[1]], target = "_blank", rel = "noopener", m[[1]]),
          if (length(parts) > 1) parts[2])
}
renderChecks <- function(recs) {
  if (!length(recs)) return(tags$p(class = "muted", "Nothing to check yet."))
  tagList(lapply(recs, function(r) {
    div(class = paste("chk", r$status),
        span(class = "g", ACTA_GLYPH[[r$status]]),
        div(strong(r$label),
            if (!is.na(r$detail)) div(class = "d", linkify(r$detail))),
        span(class = "s", r$status))
  }))
}
blocking <- function(recs) any(vapply(recs, function(r) r$status == "fail", logical(1)))

## Roll a group of checks up to one verdict. A SKIP does not roll up to a pass -- the same rule
## the QC roll-up follows -- so a group containing something that never ran reads "warn", never
## green. Green here means every check in the group actually ran and passed.
overallStatus <- function(recs) {
  if (!length(recs)) return("skip")
  st <- vapply(recs, function(r) r$status, character(1))
  if (any(st == "fail")) "fail" else if (any(st %in% c("warn", "skip"))) "warn" else "pass"
}
## Header carries the verdict and a count; the individual rows live behind a <details> toggle, so
## the default view is one line per group rather than twenty rows of green ticks. No JS needed --
## <details> is native, and it keeps working if anything about the Shiny session is odd.
checkPanel <- function(title, recs, open = FALSE) {
  ov <- overallStatus(recs)
  st <- vapply(recs, function(r) r$status, character(1))
  bad <- sum(st == "fail"); soft <- sum(st %in% c("warn", "skip"))
  summaryTxt <- if (!length(recs)) "nothing to check"
                else if (bad)  sprintf("%d of %d failing", bad, length(recs))
                else if (soft) sprintf("%d of %d need a look", soft, length(recs))
                else           sprintf("all %d passed", length(recs))
  tags$details(open = if (isTRUE(open) || ov != "pass") NA else NULL, class = "grp",
    tags$summary(class = paste("grpsum", ov),
      span(class = "g", ACTA_GLYPH[[ov]]),
      span(class = "grptitle", title),
      span(class = "s", ov),
      span(class = "grpcount", summaryTxt)),
    div(style = "margin-top:6px", renderChecks(recs)))
}

## =============================================================================================
## UI
## =============================================================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(STATUS_CSS)), tags$title("ACTA")),
  tags$div(style = "border-top:2px solid #14181d;padding:12px 0 10px;margin-bottom:6px",
           tags$span(style = "font-size:20px;font-weight:700;letter-spacing:.28em", "ACTA"),
           tags$span(style = "letter-spacing:.16em;text-transform:uppercase;color:#5b6570;margin-left:10px",
                     "Desktop"),
           tags$span(style = "float:right;font-family:ui-monospace,Menlo,monospace;font-size:11px;color:#5b6570",
                     textOutput("verline", inline = TRUE))),

  ## All three groups together, so preflight is one place to look rather than three.
  div(class = "facet",
      h4("Checks"),
      uiOutput("depChecks"),
      uiOutput("versionChecks"),
      uiOutput("actaChecks"),
      uiOutput("dashChecks")),

  fluidRow(
    ## ---------------- ACTA facet ----------------
    column(6, div(class = "facet",
      h4("Run ACTA"),
      uiOutput("actaPaths"),
      checkboxInput("mkReport", "Generate report (PDF)", value = isTRUE(pref("mkReport", TRUE))),
      checkboxInput("mkPlots",  "Generate plots (PNG)",  value = isTRUE(pref("mkPlots",  TRUE))),
      ## Sits with the artefact toggles because it is about the report, not a separate operation.
      checkboxInput("openReport", "Open report after generation", value = isTRUE(pref("openReport", TRUE))),
      ## Titer entry is a manual, interrupting step -- it stops the run on a modal waiting for
      ## typed values -- so it is behind a collapsed toggle rather than sitting inline where it
      ## reads like just another output switch. Native <details>: no JS, no Shiny round-trip.
      tags$details(style = "margin:2px 0 10px",
        tags$summary(style = "cursor:pointer;font-size:12px;color:var(--muted,#667)", "Advanced"),
        tags$div(style = "padding:6px 0 0 14px",
          checkboxInput("addTiter", "Add titer and titer status after report generation (Requires manual input of values)",
                        value = isTRUE(pref("addTiter", FALSE))),
          ## Also here, below titer entry. Both are chained follow-on steps rather than artefact
          ## switches. NOTE: this one retitles the Run button, so with the toggle collapsed the
          ## button text is the only visible sign it is on -- that is why the chainnote stays.
          div(class = "chain",
            ## NOT remembered between sessions, deliberately -- unlike the other toggles. This one
            ## WRITES INTO A DESTINATION FOLDER and retitles the Run button, and with the section
            ## collapsed that button text is the only visible sign it is armed. A sticky ON meant
            ## opening the app and finding it already checked from a previous session. Starts
            ## unchecked every time; see the matching omission from prefsSave() below.
            checkboxInput("copyGen", "Copy ACTA output to the dashboard folder and generate dashboard",
                          value = FALSE),
            tags$div(class = "chainnote",
                     "Runs the dashboard straight after ACTA, so this run is included in it."),
            conditionalPanel("input.copyGen == true", uiOutput("copyPathUI"))))),
      tags$hr(),
      tags$div(style = "margin-top:10px",
               uiOutput("runBtn", inline = TRUE), " ",
               actionButton("abort", "Abort", class = "btn-danger"), " ",
               ## The button cannot be the mechanism on its own -- if R is dead or the app is
               ## wedged, no button fires, which is why P1-P3 write unconditionally. This is for
               ## the case where the run failed but the app is still up, which is most of them.
               actionButton("saveDiag", "Save diagnostics", class = "btn-default",
                            title = "Write one pasteable text file: first error, child output, sessionInfo, TeX and dependency state. Paths and usernames are scrubbed; the workbook is never included.")),
      uiOutput("progress"),
      uiOutput("actaResult"))),

    ## ---------------- Dashboard facet ----------------
    column(6, div(class = "facet",
      h4("Generate dashboard"),
      uiOutput("dashPaths"),
      checkboxInput("openDash", "Open dashboard after generation", value = isTRUE(pref("openDash", TRUE))),
      tags$hr(),
      tags$div(style = "margin-top:10px",
               actionButton("runDash", "Generate dashboard", class = "btn-primary")),
      uiOutput("dashResult")))
  ),

  div(class = "facet", h4("Log"), div(class = "log", textOutput("log", container = pre))),

  ## Collapsed by default: this is a maintenance tool, not part of the daily workflow.
  div(class = "facet",
    tags$details(class = "grp",
      tags$summary(class = "grpsum", style = "grid-template-columns:1fr auto",
                   span(class = "grptitle", "Diagnostics"),
                   span(class = "grpcount", "operational qualification tests")),
      div(style = "margin-top:10px",
        tags$p(class = "muted",
               paste("Each OQ test is built on a different cytometer\u2019s data and confirms that the",
                     "installation works and that the outputs are as expected. Outputs and log.txt",
                     "are collected into the folder\u2019s \"Outputs/\", which can be deleted once the",
                     "result is recorded. These tests also write the dashboard files.")),
        uiOutput("oqButtons"),
        uiOutput("progressOq"),
        uiOutput("oqResult"))))
)

## =============================================================================================
## SERVER
## =============================================================================================
server <- function(input, output, session) {

  vf      <- actaVersionFiles(WORK_DIR, code_dir = CODE_DIR)
  layout  <- actaVersionFile(vf, "layout")
  rv <- reactiveValues(
    log = sprintf("[%s] ACTA app started in %s", format(Sys.time(), "%H:%M:%S"), WORK_DIR),
    proc = NULL, running = FALSE, result = NULL, dashResult = NULL,
    tplOverride = NULL, expOverride = NULL, dashOverride = NULL, copyOverride = NULL, oq = NULL, oqProc = NULL, oqRunning = FALSE, oqName = NULL, pct = NA, stage = NULL, t0 = NULL, compOverride = NULL, unstage = NULL,
    ## Path to THIS run's diagnostic log. NA when the folder could not be written -- callers must
    ## tolerate that rather than assume, which is why actaDiagAppend() no-ops on NA.
    diagLog = NA_character_,
    titer = NULL
  )
  say <- function(...) {
    rv$log <- paste(rv$log, sprintf("[%s] %s", format(Sys.time(), "%H:%M:%S"), paste0(...)), sep = "\n")
  }
  output$log <- renderText(rv$log)
  output$verline <- renderText({
    s <- actaVersionFile(vf, "script")
    if (is.na(s)) "no script resolved" else basename(s)
  })
  ## Computed once per session: requireNamespace() on ~35 packages is not free, and the answer
  ## cannot change while the app is open.
  ## The package scan is cached (it is the slow part). The LaTeX row is appended OUTSIDE the cache
  ## on purpose: its severity follows the Report checkbox, and a cached value would freeze at
  ## whatever the checkbox happened to be on first render.
  ## actaDependencyChecks() already carries a LaTeX row (Sys.which("xelatex") || is_tinytex()) and a
  ## pandoc row. A second engine row was redundant, and worse, it implied the panel had been silent
  ## about LaTeX when it had not -- so a green panel plus a failed render means the cause is NOT a
  ## missing engine, and an extra row saying the same thing would send the next person down the
  ## wrong path. The panel is left as it was; the diagnosis now comes from the render itself.
  depRecs <- local({ cached <- NULL; function() { if (is.null(cached)) cached <<- actaDependencyChecks(CODE_DIR); cached } })
  output$depChecks     <- renderUI(checkPanel("All dependencies installed", depRecs()))
  output$versionChecks <- renderUI(checkPanel("Files check", vf))

  logFile <- file.path(WORK_DIR, "ACTA_app_run_log.tsv")

  ## Scrape progress out of the child's stream. knitr prints its own "|=====   |  40%" bar while
  ## rendering, so the percentage is real rather than invented; the stage names come from lines the
  ## pipeline already emits. Nothing new has to be instrumented, which is what makes this cheap.
  readProgress <- function(lines) {
    for (l in lines) {
      m <- regmatches(l, regexpr("([0-9]{1,3})%", l))
      if (length(m)) rv$pct <- suppressWarnings(as.numeric(sub("%", "", m)))
      if (grepl("pre-flight|Prevalidat", l, ignore.case = TRUE)) rv$stage <- "checking the workbook"
      else if (grepl("biexp maxValue|compensation:", l))         rv$stage <- "reading FCS"
      else if (grepl("populations:", l))                          rv$stage <- sub(".*\\[ACTA\\] ", "gating ", l)
      else if (grepl("Calculation & Plotting completed", l))      rv$stage <- "building plots"
      else if (grepl("StatsExport|TitrationExport", l))           rv$stage <- "writing the export"
      else if (grepl("processing file:", l))                      rv$stage <- "rendering the report"
      else if (grepl("output file:|Output created", l))           rv$stage <- "finishing the PDF"
    }
  }
  progressUI <- function(active) {
    if (!isTRUE(active)) return(NULL)
    pct <- rv$pct; indet <- is.na(pct) || !is.finite(pct)
    el <- if (!is.null(rv$t0)) sprintf("  %ds elapsed", as.integer(difftime(Sys.time(), rv$t0, units = "secs"))) else ""
    tagList(
      div(class = paste("pbar", if (indet) "indet" else ""),
          tags$i(style = if (indet) NULL else sprintf("width:%.0f%%", max(0, min(100, pct))))),
      div(class = "pstage",
          sprintf("%s%s%s", if (is.null(rv$stage)) "starting" else rv$stage,
                  if (indet) "" else sprintf("  %.0f%%", pct), el)))
  }
  ## invalidateLater keeps the ELAPSED COUNTER ticking. progressUI() reads Sys.time(), which is not
  ## reactive, so renderUI re-ran only when rv$pct / rv$stage / rv$t0 changed -- and those change only
  ## when the poller scrapes new knitr output. Through the ~40 s gating phase there is no knitr
  ## output at all, so the text froze at "starting  0s elapsed" while the CSS animation kept moving:
  ## it looked alive and reported nothing. The invalidation is registered HERE rather than inside
  ## progressUI() so that function stays callable outside a reactive context, which the smoke test
  ## relies on.
  output$progress   <- renderUI({ if (isTRUE(rv$running))   invalidateLater(1000, session)
                                  progressUI(rv$running) })
  output$progressOq <- renderUI({ if (isTRUE(rv$oqRunning)) invalidateLater(1000, session)
                                  progressUI(rv$oqRunning) })

  ## ---- resolved paths, with provenance shown so "where did this come from" is never a guess --
  dashDirR <- reactive(if (!is.null(rv$dashOverride)) list(value = rv$dashOverride, source = "browsed")
                       else resolvePath(layout, "dashboard_dir", "dashboard_dir"))
  expDirR  <- reactive(if (!is.null(rv$expOverride)) list(value = rv$expOverride, source = "browsed")
                       else resolvePath(layout, "titration_export_dir", "titration_export_dir"))
  copyDirR <- reactive(if (!is.null(rv$copyOverride)) list(value = rv$copyOverride, source = "browsed")
                       else resolvePath(layout, "titration_export_dir", "copy_dir"))
  tplR     <- reactive({
    if (!is.null(rv$tplOverride) && nzchar(trimws(rv$tplOverride))) return(rv$tplOverride)
    p <- pref("template")
    if (!is.null(p) && file.exists(p)) return(p)
    t <- list.files(file.path(CODE_DIR, "Template"), pattern = "dashboard_template.*\\.html?$",
                    full.names = TRUE)
    if (length(t) == 1) t[[1]] else NA_character_
  })

  pathRow <- function(label, res, inputId, kind = "dir") {
    tagList(tags$div(style = "margin-bottom:10px",
      tags$strong(label),
      tags$span(class = "src", res$source),
      tags$div(class = "d", if (nzchar(res$value)) res$value else sprintf("— blank: using %s —", WORK_DIR)),
      tags$div(style = "display:flex;gap:8px;align-items:center",
               div(style = "flex:1", textInput(inputId, NULL, value = res$value, width = "100%")),
               actionButton(paste0("browse_", inputId), "Browse…", class = "btn-default")),
      ## The provenance chip above already names the source; this spells it out in words on the
      ## hint line, because "Info!dashboard_dir" means nothing to someone who has not read the
      ## workbook. Only when the value actually CAME from the layout: a browsed path did not, and a
      ## blank one came from nowhere, so claiming a layout default in either case would be a lie.
      tags$div(class = "muted", style = "margin-top:-6px;font-size:11px",
               paste0("(Blank means app\u2019s current directory)",
                      if (nzchar(res$value) && !identical(res$source, "browsed"))
                        " (Default value retrieved from the instructions file)" else ""))))
  }
  output$actaPaths <- renderUI(tagList(
    tags$div(class = "d", style = "margin-bottom:8px",
             tags$strong("Instructions: "), if (is.na(layout)) "not resolved" else basename(layout)),
    uiOutput("compUI")))

  ## Compensation is declared in the workbook, not chosen here: the line reports what Info asks for
  ## and only offers a file when that value is 'csv'. Greying out for 'self' is the honest state --
  ## there is no file to choose, the matrix comes from the acquisition.
  compModeRaw <- reactive({
    v <- infoVal(layout, "compensation")
    if (is.na(v)) "" else tolower(trimws(v))
  })
  output$compUI <- renderUI({
    m <- compModeRaw(); raw <- infoVal(layout, "compensation")
    hits <- actaCompCsvCandidates(WORK_DIR)
    head <- tags$div(tags$strong("Compensation: "),
                     tags$code(if (is.na(raw) || !nzchar(raw)) "(blank)" else raw))
    if (identical(m, "self"))
      return(tags$div(style = "margin-bottom:8px", head,
        tags$div(class = "d", "Workbook selected \u201cself\u201d \u2014 the acquisition-defined matrix will be applied."),
        tags$div(style = "display:flex;gap:8px;align-items:center;opacity:.5",
                 div(style = "flex:1", textInput("compPath", NULL, value = "", width = "100%")),
                 tags$button(class = "btn btn-default", disabled = NA, "Browse\u2026"))))
    if (!identical(m, "csv"))
      return(tags$div(style = "margin-bottom:8px", head,
        tags$div(class = "d", if (is.na(raw) || !nzchar(raw))
          "Identity matrix will be applied. Set the value to \"csv\" or \"self\" if compensation is required."
          else "Not a recognised value. Set it to blank, self or csv \u2014 see the Checks panel.")))
    ## csv: show what the script would pick up, and allow overriding it
    tags$div(style = "margin-bottom:8px", head,
      tags$div(class = "d", if (!length(hits)) "No *CompMatrix*.csv in the app folder \u2014 browse to one."
               else if (length(hits) > 1) sprintf("%d candidates present: %s \u2014 browse to pick one.",
                                                  length(hits), paste(basename(hits), collapse = ", "))
               else sprintf("Found %s", basename(hits[1]))),
      tags$div(style = "display:flex;gap:8px;align-items:center",
               div(style = "flex:1",
                   textInput("compPath", NULL, width = "100%",
                             value = if (!is.null(rv$compOverride)) rv$compOverride
                                     else if (length(hits) == 1) hits[1] else "")),
               actionButton("browse_compPath", "Browse\u2026", class = "btn-default")),
      tags$div(class = "muted", style = "margin-top:-6px;font-size:11px",
               "(Blank uses the *CompMatrix*.csv in the app\u2019s folder)"))
  })
  observeEvent(input$compPath, rv$compOverride <- input$compPath, ignoreInit = TRUE)
  output$copyPathUI <- renderUI(
    div(style = "margin:-4px 0 8px 22px", pathRow("Copy destination", copyDirR(), "copyDir", "dir")))
  output$dashPaths <- renderUI({
    t <- tplR()
    tagList(
      pathRow("Scan folder for Titration exports", expDirR(), "expDir", "dir"),
      pathRow("Write Dashboard HTML to", dashDirR(), "dashDir", "dir"),
      tags$div(style = "margin-bottom:10px", tags$strong("Dashboard HTML template"),
               tags$div(class = "d", if (is.na(t)) "— none found —" else basename(t)),
               tags$div(style = "display:flex;gap:8px;align-items:center",
                        div(style = "flex:1", textInput("tplPath", NULL,
                                                       value = if (is.na(t)) "" else t, width = "100%")),
                        actionButton("browse_tplPath", "Browse\u2026", class = "btn-default")),
               tags$div(class = "muted", style = "margin-top:-6px;font-size:11px",
                        "(Blank means app\u2019s current directory — its Template/ folder)")))
  })
  observeEvent(input$expDir,  rv$expOverride  <- input$expDir,  ignoreInit = TRUE)
  observeEvent(input$dashDir, rv$dashOverride <- input$dashDir, ignoreInit = TRUE)
  observeEvent(input$tplPath, rv$tplOverride  <- input$tplPath, ignoreInit = TRUE)
  ## Browse fills the field; the field is what everything downstream reads. A cancelled dialog
  ## returns NA and must leave the current value alone rather than blanking it.
  browseInto <- function(btnId, fieldId, kind, start) {
    observeEvent(input[[btnId]], {
      pick <- pickPath(kind, start())
      if (is.na(pick)) { say("Browse cancelled \u2014 ", fieldId, " unchanged."); return() }
      updateTextInput(session, fieldId, value = pick)
      say("Selected ", pick)
    }, ignoreInit = TRUE)
  }
  browseInto("browse_expDir",  "expDir",  "dir",  function() effPath(expDirR()$value))
  browseInto("browse_dashDir", "dashDir", "dir",  function() effPath(dashDirR()$value))
  browseInto("browse_tplPath", "tplPath", "file", function() file.path(CODE_DIR, "Template"))
  browseInto("browse_copyDir", "copyDir", "dir",  function() effPath(copyDirR()$value))
  browseInto("browse_compPath", "compPath", "file", function() WORK_DIR)
  observeEvent(input$copyDir, rv$copyOverride <- input$copyDir, ignoreInit = TRUE)

  ## ---- preflight ------------------------------------------------------------------------------
  ## Layout checks ONLY. This used to start from `vf`, so every version-file row was rendered
  ## twice -- once in the Files check panel and again at the top of this one.
  actaRecs <- reactive({
    recs <- list()
    if (is.na(layout))
      return(list(list(id = "layout", label = "Instructions workbook resolved", status = "fail",
                       detail = "no Titration_Instructions*.xlsx found -- see Files check", where = WORK_DIR)))
    out <- tryCatch({
      md  <- suppressMessages(readxl::read_excel(layout, sheet = "Layout_Plate", .name_repair = "minimal"))
      names(md) <- actaStripHeaderAnnotation(names(md))
      gt  <- suppressMessages(readxl::read_excel(layout, sheet = "gating_template", .name_repair = "minimal"))
      inf <- suppressMessages(readxl::read_excel(layout, sheet = "Info", .name_repair = "minimal"))
      names(inf) <- actaStripHeaderAnnotation(names(inf))
      mk  <- if ("Markername" %in% names(md)) "Markername" else NA_character_
      ch  <- if ("Channel" %in% names(md)) "Channel" else NA_character_
      ## The same unit of work the script loops over -- one row per TITRATED MARKER, not per folder.
      ## A bare vector of Dirnames pooled co-titrated markers, so a combinatorial sheet was checked
      ## as though each folder were one series: their stain quantities read as duplicates of each
      ## other, and a folder with one Costain per series read as one too many. NOT wrapped in
      ## tryCatch: the errors this raises -- two markers on one channel, a colliding key -- are
      ## layout faults the panel exists to show, and the outer handler already renders a stop() as a
      ## visible failure record.
      grp <- actaTitrationGroups(md, channel_col = ch)
      r <- actaPrevalidateRecords(md, gt, inf, grp, file.path(WORK_DIR, "Titration_FCS"),
                                  marker_col = mk, channel_col = ch)
      ## gate_manual is documented inactive as of v2_86; refuse it here rather than mid-run.
      gm <- if ("gating_method" %in% names(gt))
              any(grepl("gate_manual", gt$gating_method[!startsWith(trimws(as.character(gt$alias)), "#")],
                        fixed = TRUE), na.rm = TRUE) else FALSE
      ## Compensation pre-flight, in the layout check so it BLOCKS before minutes of gating rather
      ## than dying mid-run. Validates the effective matrix, so a browsed override counts.
      cmp <- actaCompensationRecords(inf, WORK_DIR, file.path(WORK_DIR, "Titration_FCS"),
                                     override = if (!is.null(rv$compOverride) && nzchar(rv$compOverride))
                                                  rv$compOverride else NA_character_,
                                     ## FOLDERS: this one globs Titration_FCS/<ab>/ for the `self`
                                     ## mode, so it wants Dirnames, not titration keys.
                                     antibody = unique(grp$dirname))
      c(r, cmp, list(if (gm)
        list(id = "gate_manual", label = "No gate_manual in the gating template", status = "fail",
             detail = "gate_manual is NOT ACTIVE as of v2_86 -- use an automated method", where = layout)
        else list(id = "gate_manual", label = "No gate_manual in the gating template",
                  status = "pass", detail = NA_character_, where = layout)))
    }, error = function(e)
      list(list(id = "workbook", label = "Instructions workbook readable", status = "fail",
                detail = conditionMessage(e), where = layout)))
    c(recs, out)
  })
  dashRecs <- reactive({
    dd <- effPath(dashDirR()$value)
    c(actaValidateDashboardTemplate(tplR()),
      ## Advisory: warns when the copy destination is somewhere the scan folder will never index,
      ## which otherwise surfaces only as a dashboard quietly missing the run just copied.
      list(actaCopyDestReachable(effPath(copyDirR()$value), effPath(expDirR()$value))),
      actaScanExports(effPath(expDirR()$value),
                      if (nzchar(dd)) file.path(dd, "ACTA_Titration_Dashboard.html") else NA_character_)$recs,
      list(if (nzchar(dd) && dir.exists(dd))
             list(id = "dash_out", label = "Dashboard output folder exists", status = "pass",
                  detail = dd, where = dd)
           else list(id = "dash_out", label = "Dashboard output folder exists", status = "fail",
                     detail = if (nzchar(dd)) "folder does not exist" else "not set", where = dd)))
  })
  output$actaChecks <- renderUI(checkPanel("ACTA instructions check", actaRecs()))
  output$dashChecks <- renderUI(checkPanel("Dashboard check", dashRecs()))

  ## The button title tracks the checkbox, as agreed; both buttons are disabled while a run is in
  ## flight and while preflight is failing.
  output$runBtn <- renderUI({
    lbl <- if (isTRUE(input$copyGen)) "Run ACTA and generate dashboard" else "Run ACTA"
    if (rv$running || blocking(depRecs()) || blocking(vf) || blocking(actaRecs()))
      tags$button(class = "btn btn-primary", disabled = NA, lbl)
    else actionButton("runActa", lbl, class = "btn-primary")
  })

  ## ---- the run: a separate process, so Abort can actually stop it -----------------------------
  observeEvent(input$runActa, {
    rv$result <- NULL; rv$titer <- NULL; rv$dashResult <- NULL
    args <- c("--vanilla", "-e",
              sprintf(paste0(.childLoad, "r<-h$run_acta(%s,report=%s,plots=%s,quiet=FALSE,code_dir=%s);saveRDS(list(report_ok=r$report_ok,report_error=r$report_error,wrote_plots=r$wrote_plots,elapsed_s=r$elapsed_s,script=r$script,script_version=r$script_version,seed=r$seed,export_file=r$export_file,plots_dir=r$plots_dir,version_dir=r$version_dir,ok=r$ok,analysis_error=r$analysis_error),%s)"),
                      .rq(WORK_DIR), isTRUE(input$mkReport), isTRUE(input$mkPlots), .rq(CODE_DIR),
                      .rq(file.path(WORK_DIR, ".acta_run_result.rds"))))
    unlink(file.path(WORK_DIR, ".acta_run_result.rds"))
    rv$pct <- NA; rv$stage <- NULL; rv$t0 <- Sys.time()
    ## actaCompensate() discovers the matrix inside the working directory, so an override has to be
    ## put there. The incumbent is MOVED to a temp folder and restored when the run ends -- copying
    ## alongside it would leave two candidates, which is fatal by design.
    ov <- if (!is.null(rv$compOverride) && nzchar(rv$compOverride)) rv$compOverride else NA_character_
    rv$unstage <- actaStageCompCsv(WORK_DIR, ov)
    if (!is.na(ov)) say("Compensation: using ", basename(ov), " (staged into the run folder)")
    say("Starting ACTA (report=", isTRUE(input$mkReport), ", plots=", isTRUE(input$mkPlots), ") ...")
    ## P1 -- WRITTEN BEFORE THE CHILD EXISTS. This is the only moment guaranteed to happen: if the
    ## child is OOM-killed or segfaults inside flowCore, no terminal row is ever written, and a
    ## `started` row with no matching outcome row IS the crash signal. Cheap, and it was the whole
    ## diagnosis of the 2026-08-27 incident. See actaDiagLogPath() for why the path can move.
    rv$diagLog <- actaDiagLogPath(WORK_DIR, "run")
    actaDiagAppend(rv$diagLog, c(
      sprintf("== ACTA run started %s ==", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("version_dir : %s", basename(WORK_DIR)),
      sprintf("layout      : %s", if (!is.na(layout)) basename(layout) else "(none)"),
      sprintf("report=%s plots=%s", isTRUE(input$mkReport), isTRUE(input$mkPlots)), ""))
    actaRunLogAppend(logFile, actaRunLogEntry("run_acta", "started", WORK_DIR, layout = layout,
                                              detail = "child not yet launched"))
    if (!is.na(rv$diagLog)) say("Diagnostics: ", rv$diagLog)
    rv$proc <- processx::process$new(file.path(R.home("bin"), "Rscript"), args,
                                     stdout = "|", stderr = "2>&1", cleanup = TRUE)
    rv$running <- TRUE
  })

  ## Poll the child: stream its output into the log, and pick up the result when it exits.
  observe({
    if (!rv$running || is.null(rv$proc)) return()
    invalidateLater(700, session)
    p <- rv$proc
    out <- tryCatch(p$read_output_lines(), error = function(e) character(0))
    if (length(out)) { readProgress(out)
                       for (l in out) if (nzchar(l)) rv$log <- paste(rv$log, l, sep = "\n")
                       ## P2 -- teed line by line, not at the end, so a hard kill still leaves
                       ## everything up to the last line on disk. Until now these lines existed
                       ## ONLY in rv$log, on screen, with no way to save them.
                       actaDiagAppend(rv$diagLog, out[nzchar(out)]) }
    if (!p$is_alive()) {
      rv$running <- FALSE; rv$pct <- NA; rv$stage <- NULL
      if (is.function(rv$unstage)) { rv$unstage(); rv$unstage <- NULL }
      rf <- file.path(WORK_DIR, ".acta_run_result.rds")
      if (file.exists(rf)) {
        res <- readRDS(rf); unlink(rf)
        rv$result <- res
        say(actaReadyMessage(res, dashboard_ok = FALSE))
        ## P3 -- an explicit closing marker. Its ABSENCE means "incomplete", which is information
        ## rather than a silence, and is the same idiom as log.txt being deliberately missing on an
        ## aborted OQ run.
        actaDiagAppend(rv$diagLog, c("", sprintf("== ACTA run ended %s (ok=%s report_ok=%s) ==",
                                                 format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                                                 isTRUE(res$ok), isTRUE(res$report_ok))))
        ## THREE outcomes, not two. `ok=TRUE` used to be hardcoded into the child's result, so a run
        ## whose ANALYSIS died inside the report render was filed as "report_failed" -- which sent
        ## every reader looking at LaTeX for a gating problem. See the sentinel in the script.
        actaRunLogAppend(logFile, actaRunLogEntry(
          "run_acta", if (isFALSE(res$ok)) "analysis_failed"
                      else if (isTRUE(res$report_ok) || is.na(res$report_ok)) "success"
                      else "report_failed",
          WORK_DIR, res = res, layout = layout, report = isTRUE(input$mkReport),
          plots = isTRUE(input$mkPlots), outputs = c(res$export_file)))
        ## Only chain the follow-up actions off a run that actually completed. reportPath() resolves
        ## by pattern, so opening it after a failed run opens the PREVIOUS run's PDF, and the
        ## dashboard would be built from an export this run never wrote.
        if (isFALSE(res$ok)) {
          say("Analysis did not complete -- skipping report open and dashboard.")
        } else if (isTRUE(input$addTiter)) {
          if (isTRUE(input$openReport)) openPath(reportPath())
          showTiterDialog()
        } else {
          if (isTRUE(input$openReport)) openPath(reportPath())
          if (isTRUE(input$copyGen)) copyThenDashboard()
        }
      } else {
        say("ACTA FAILED — no result written. See the log above.")
        ## P3 -- the detail used to read "child process produced no result", which told the reader
        ## only that something went wrong and nothing about what. The child's FIRST error is the
        ## one that explains a cascade, so quote that; the placeholder survives only when the
        ## output genuinely contains nothing error-shaped.
        .fe <- actaFirstError(strsplit(rv$log %||% "", "\n")[[1]])
        actaRunLogAppend(logFile, actaRunLogEntry("run_acta", "failed", WORK_DIR, layout = layout,
                                                  detail = if (nzchar(.fe))
                                                             gsub("\\s+", " ", .fe)
                                                           else "child process produced no result"))
        actaDiagAppend(rv$diagLog, c("", sprintf("== ACTA run FAILED (no result) %s ==",
                                                 format(Sys.time(), "%Y-%m-%d %H:%M:%S")), .fe))
      }
    }
  })

  observeEvent(input$abort, {
    if (!is.null(rv$proc) && rv$proc$is_alive()) {
      rv$proc$kill(); rv$running <- FALSE
      if (is.function(rv$unstage)) { rv$unstage(); rv$unstage <- NULL }
      ## Abort must never trigger the copy: a partially written run must not reach the dashboard.
      say("ABORTED. Partial output may remain in Plots/; the export was not copied.")
      actaDiagAppend(rv$diagLog, c("", sprintf("== ACTA run ABORTED by the user %s ==",
                                               format(Sys.time(), "%Y-%m-%d %H:%M:%S"))))
      actaRunLogAppend(logFile, actaRunLogEntry("run_acta", "aborted", WORK_DIR, layout = layout,
                                                detail = "user pressed Abort"))
    } else say("Nothing running.")
  })

  observeEvent(input$saveDiag, {
    b <- actaDiagnosticBundle(WORK_DIR, WORK_DIR, run_log = rv$diagLog, child_log = rv$diagLog,
                              layout = layout, res = rv$result, tsv = logFile)
    ## PRINT WHERE IT LANDED. A sync client can hold the working folder -- 2026-08-27, a rename
    ## into Outputs/ failed with "Directory not empty" -- so the bundle may be in tempdir(), and a
    ## file the user cannot find is a file they cannot send.
    if (isTRUE(b$ok)) {
      say("Diagnostics written (", b$lines, " lines): ", b$path)
      say("Send that file. It carries no absolute paths, no username and not the workbook.")
    } else say("Could not write the diagnostics bundle. The log above is still copyable by hand.")
  })

  observeEvent(input$oqAbort, {
    if (!is.null(rv$oqProc) && rv$oqProc$is_alive()) {
      rv$oqProc$kill()
      ## rv$oqRunning goes FALSE BEFORE the poller can next fire. The poller treats a dead child
      ## with no result file as "ERROR -- no result written", which is right for a crash and wrong
      ## for a deliberate abort; clearing the flag makes it bail instead of mislabelling this.
      rv$oqRunning <- FALSE; rv$pct <- NA; rv$stage <- NULL
      nm <- rv$oqName
      ## Drop any result file so a later run cannot pick up this one's leftovers and report a
      ## verdict that belongs to an abandoned run.
      unlink(OQ_RES)
      nmDir <- { hit <- oqDirs()[basename(oqDirs()) == nm]; if (length(hit)) hit[[1]] else WORK_DIR }
      ## Says explicitly that no log.txt was written. log.txt is only written by actaOQFinish() at
      ## the very end, so its ABSENCE is what distinguishes an aborted case from a passing one --
      ## worth stating, because partial artefacts in Outputs/ otherwise look like a finished run.
      say(nm, ": ABORTED. Partial artefacts may remain in ", file.path(nm, "Outputs"),
          "; no log.txt was written, so this run has no verdict.")
      rv$oq <- NULL
      actaRunLogAppend(logFile, actaRunLogEntry("oq_test", "aborted", nmDir,
                                               detail = "user pressed Abort OQ"))
    } else say("No OQ test running.")
  })

  reportPath <- function() {
    p <- list.files(WORK_DIR, pattern = "^ACTA_Report.*\\.pdf$", full.names = TRUE)
    if (length(p)) p[[which.max(file.mtime(p))]] else NA_character_
  }
  ## NOT browseURL(). On macOS and Linux it hands the path to a shell, and its quoting does not
  ## make an arbitrary filename safe there. The paths reaching this function are not fixed names:
  ## they are list.files()/list.dirs() results from the working folder, so whoever can write into
  ## that folder chooses them -- and titration folders live on shared drives. processx passes an
  ## argv array and builds no command line, which is the same reasoning already applied to .rq()
  ## and to the osascript call above. Guarded by test_arg_safety.R.
  openPath <- function(p) {
    if (is.na(p) || !file.exists(p)) { say("Nothing to open."); return(invisible()) }
    p <- normalizePath(p)
    opener <- if (.Platform$OS.type == "windows") NULL
              else if (identical(Sys.info()[["sysname"]], "Darwin")) "/usr/bin/open" else "xdg-open"
    try(if (is.null(opener)) shell.exec(p)
        else processx::run(opener, p, error_on_status = FALSE), silent = TRUE)
    say("Opened ", basename(p))
  }

  ## ---- titer entry ----------------------------------------------------------------------------
  showTiterDialog <- function() {
    ex <- rv$result$export_file
    if (is.na(ex) || !file.exists(ex)) { say("No export to annotate."); return(invisible()) }
    d <- suppressMessages(readxl::read_excel(ex, .name_repair = "minimal"))
    li <- if (all(c("MM_R2","MM_90pct_Vmax") %in% names(d)))
            data.frame(Dirname = d$Dirname, MM_R2 = d$MM_R2, MM_90pct_Vmax = d$MM_90pct_Vmax,
                       MM_note = if ("MM_note" %in% names(d)) d$MM_note else NA) else NULL
    thr <- suppressWarnings(as.numeric(infoVal(layout, "MM_R2_threshold"))); if (!is.finite(thr)) thr <- 0.9
    opts <- if (!is.null(li)) actaTiterOptions(li, d, thr) else list()
    rv$titerOpts <- opts
    showModal(modalDialog(
      title = "Titer and titer status", size = "l", easyClose = FALSE,
      tags$p(class = "muted", "The report is open for review. Values are written into the titration export."),
      lapply(seq_along(opts), function(i) {
        o <- opts[[i]]
        row <- d[d$Dirname == o$dirname, , drop = FALSE][1, ]
        choices <- c("Validated","Confirmed","Pending","Re-run needed","Undetermined","Custom…")
        tagList(tags$hr(),
          tags$strong(o$dirname),
          tags$div(class = "d", sprintf("max SI at %s µL · well volume %s µL · %s e6 cells/well",
                                        row$maxSI_StainQty %||% "?",
                                        (row$Well_volume_uL %||% "?"), (row$e6_cells_per_well %||% "?"))),
          radioButtons(paste0("src_", i), NULL,
                       choiceNames = list(o$maxsi$label, o$vmax90$label, "Enter manually"),
                       choiceValues = list("maxsi", "vmax90", "manual"),
                       selected = if (o$maxsi$enabled) "maxsi" else "manual"),
          if (!o$vmax90$enabled) tags$div(class = "d warn", style = "color:#8a5a00",
                                          paste("90%Vmax unavailable:", o$vmax90$reason)),
          numericInput(paste0("titer_", i), "Titer (µL)",
                       value = if (o$maxsi$enabled) o$maxsi$value else NA, width = "160px"),
          selectInput(paste0("stat_", i), "Titer status", choices = choices,
                      selected = "Validated", width = "220px"),
          conditionalPanel(sprintf("input.stat_%d == 'Custom…'", i),
                           textInput(paste0("statx_", i), "Custom status", width = "220px")))
      }),
      footer = tagList(modalButton("Skip"), actionButton("titerSave", "Save to export", class = "btn-primary"))))
  }

  observeEvent(input$titerSave, {
    ex <- rv$result$export_file
    d  <- suppressMessages(readxl::read_excel(ex, .name_repair = "minimal"))
    opts <- rv$titerOpts
    for (i in seq_along(opts)) {
      o <- opts[[i]]; k <- d$Dirname == o$dirname
      src <- input[[paste0("src_", i)]]
      val <- switch(src, maxsi = o$maxsi$value, vmax90 = o$vmax90$value,
                    suppressWarnings(as.numeric(input[[paste0("titer_", i)]])))
      st  <- input[[paste0("stat_", i)]]
      if (identical(st, "Custom…")) st <- input[[paste0("statx_", i)]]
      d$Titer[k] <- val; d$Titration_Status[k] <- st
    }
    ## Re-emit through the same writer rather than patching cells: the export is heavily styled
    ## and a targeted openxlsx write would risk the cell formatting.
    writeTitrationExport(as.data.frame(d), ex)
    say("Titer written for ", length(opts), " group(s) -> ", basename(ex))
    actaRunLogAppend(logFile, actaRunLogEntry("titer_entry", "success", WORK_DIR, res = rv$result,
                                              layout = layout, titer_edited = TRUE, outputs = ex,
                                              detail = paste(vapply(opts, function(o) o$dirname, ""),
                                                             collapse = "; ")))
    removeModal()
    if (isTRUE(input$copyGen)) copyThenDashboard()
  })

  ## ---- copy + dashboard --------------------------------------------------------------------
  copyThenDashboard <- function() {
    dest <- effPath(copyDirR()$value)  # its own field; blank means this folder
    if (!dir.exists(dest)) { say("Export folder does not exist: ", dest); return(invisible()) }
    src <- c(rv$result$export_file, reportPath(),
             if (!is.na(rv$result$plots_dir)) list.files(rv$result$plots_dir, full.names = TRUE))
    ## SANITISED HERE TOO: this is a workbook cell on its way to becoming a directory name.
    eln <- infoVal(layout, "ELN_ID"); if (is.na(eln)) eln <- "run"
    eln <- actaElnLabel(eln, for_file = TRUE)
    r <- actaArchiveThenCopy(src, dest, eln)
    ## The FULL path, not basename(): a traversal payload's last segment looks innocuous.
    if (length(r$archived)) say("Archived ", length(r$archived), " existing file(s) -> ", r$archive_dir)
    ## A blank copy path resolves to the folder ACTA just wrote to, so there is nothing to copy and
    ## nothing to archive. Say that plainly rather than "Copied 0 file(s)", which reads like a fault.
    if (length(r$in_place) && !length(r$copied))
      say("Output is already in ", dest, " -- nothing to copy. Generating the dashboard.")
    else say("Copied ", length(r$copied), " file(s) -> ", dest)
    actaRunLogAppend(logFile, actaRunLogEntry("copy", if (r$ok) "success" else "partial", WORK_DIR,
                                              res = rv$result, layout = layout,
                                              outputs = r$copied, detail = r$archive_dir))
    runDashboard()
  }

  runDashboard <- function() {
    gen <- actaVersionFile(vf, "dashboard")
    if (is.na(gen)) { say("No dashboard generator resolved."); return(invisible()) }
    ## Hand the generator the paths the UI is showing. Without this the field was decorative: the
    ## generator read Info!dashboard_dir (blank) and wrote beside itself, so browsing elsewhere
    ## changed nothing -- and we then looked for the html in the wrong folder.
    outDir <- effPath(dashDirR()$value)
    scanDir <- effPath(expDirR()$value)
    say("Generating dashboard -> ", outDir)
    out <- tryCatch({
      ## Sys.setenv, NOT system2(env=): that argument prepends VAR=value to the COMMAND LINE, which
      ## the shell splits on whitespace -- and this repo path contains spaces and an "&", so the
      ## generator was invoked with fragments of its own path as commands.
      old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR"), unset = NA)
      Sys.setenv(ACTA_DASHBOARD_DIR = outDir, ACTA_EXPORT_DIR = scanDir)
      on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
                  do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
      system2(file.path(R.home("bin"), "Rscript"), c("--vanilla", shQuote(gen)),
              stdout = TRUE, stderr = TRUE)
    }, error = function(e) paste("ERROR:", conditionMessage(e)))
    for (l in out) if (nzchar(l)) rv$log <- paste(rv$log, l, sep = "\n")
    html <- list.files(outDir, pattern = "Dashboard.*\\.html$", full.names = TRUE)
    okd  <- length(html) > 0
    rv$dashResult <- list(ok = okd, path = if (okd) html[[which.max(file.mtime(html))]] else NA_character_)
    say(actaReadyMessage(rv$result %||% list(report_ok = NA), dashboard_ok = okd))
    actaRunLogAppend(logFile, actaRunLogEntry("dashboard", if (okd) "success" else "failed", WORK_DIR,
                                              layout = layout, outputs = rv$dashResult$path))
    if (okd && isTRUE(input$openDash)) openPath(rv$dashResult$path)
  }
  observeEvent(input$runDash, {
    if (blocking(depRecs())) { say("A required dependency is missing — see the Checks panel."); return() }
    if (blocking(dashRecs())) { say("Dashboard preflight is failing — fix the red rows first."); return() }
    runDashboard()
  })

  ## ---- results panels ---------------------------------------------------------------------
  output$actaResult <- renderUI({
    r <- rv$result; if (is.null(r)) return(NULL)
    arts <- actaRunArtefacts(r)
    div(style = "margin-top:10px",
        div(class = if (isFALSE(r$ok) || isFALSE(r$report_ok)) "bad" else "ready",
            actaReadyMessage(r, FALSE)),
        tags$ul(lapply(arts, function(a) tags$li(
          if (!is.na(a$path)) sprintf("%s — %s", a$label, basename(a$path))
          else sprintf("%s — %s", a$label, a$skipped)))),
        ## The analysis error takes precedence: when the analysis died inside the render, the caught
        ## message is the analysis's, and labelling it a render failure points at the wrong stage.
        if (isFALSE(r$ok) && !is.na(r$analysis_error %||% NA))
          tags$div(class = "d", paste("Analysis failed:", r$analysis_error))
        else if (isFALSE(r$report_ok)) tags$div(class = "d", r$report_error))
  })
  output$dashResult <- renderUI({
    d <- rv$dashResult; if (is.null(d)) return(NULL)
    div(style = "margin-top:10px", class = if (d$ok) "ready" else "bad",
        if (d$ok) paste("Dashboard is ready —", basename(d$path)) else "Dashboard generation failed")
  })

  ## ---- Diagnostics / OQ ------------------------------------------------------------------
  ## Discovered, not hardcoded: drop an OQ_Test2 folder beside this one and its button appears.
  ## Cases live in Diagnostics/, one folder each. Still discovered rather than hardcoded: drop
  ## Diagnostics/OQ_Test2 beside OQ_Test1 and its button appears. The app's own folder is also
  ## searched so a case left at the top level from before is still found.
  DIAG_DIR <- file.path(WORK_DIR, "Diagnostics")
  ## The SHIPPED cases are part of the tool, so they are searched for wherever the tool is: beside the
  ## code in a version folder, and under inst/extdata/oq_small when installed. Without this the
  ## Diagnostics section is empty for every package user -- and those cases are the whole point of
  ## being able to verify an install.
  PKG_CASES <- if (requireNamespace("ACTA", quietly = TRUE))
                 system.file("extdata", "oq_small", package = "ACTA") else ""
  ## ONE BUTTON PER CASE NAME, nearest root wins. The dedup used to be unique() over full PATHS,
  ## which does nothing when two roots hold cases of the SAME NAME -- and two of them routinely do:
  ## the version folder's Diagnostics/ and, for anyone who has installed the package, its
  ## extdata/oq_small/. That rendered FOUR identical-looking buttons for two cases, and the copy
  ## inside an installed package carries THAT package's workbook -- 2_90 while this folder is 3_0 --
  ## so half of them ran the current code against a version-locked workbook it must not accept.
  ##
  ## `roots` is in PREFERENCE order and must not be re-sorted across roots: the previous sort() was
  ## alphabetical over absolute paths, which puts /Library/Frameworks (the installed package) ahead
  ## of /Users, so the two leftmost buttons were the stale ones and the correct pair sat third and
  ## fourth. Sorting happens WITHIN each root only. Reported by the user on 2026-08-27.
  ##
  ## This also disambiguates the two `oqDirs()[basename(oqDirs()) == nm][[1]]` lookups below: with
  ## duplicates present those silently took the first match, i.e. the package copy.
  oqDirs <- reactive({
    roots <- c(DIAG_DIR, WORK_DIR, file.path(CODE_DIR, "Diagnostics"), PKG_CASES)
    roots <- unique(roots[nzchar(roots) & dir.exists(roots)])
    d <- unlist(lapply(roots, function(r) {
      x <- list.dirs(r, recursive = FALSE, full.names = TRUE)
      sort(x[grepl("^OQ[_ ]?Test", basename(x), ignore.case = TRUE)])
    }), use.names = FALSE)
    d[!duplicated(basename(d))]
  })
  ## What the line above HID. Named in the UI rather than dropped quietly: somebody who knows there
  ## are two copies on this machine should be able to see which one the button will run.
  oqShadowed <- reactive({
    roots <- c(DIAG_DIR, WORK_DIR, file.path(CODE_DIR, "Diagnostics"), PKG_CASES)
    roots <- unique(roots[nzchar(roots) & dir.exists(roots)])
    d <- unlist(lapply(roots, function(r) {
      x <- list.dirs(r, recursive = FALSE, full.names = TRUE)
      sort(x[grepl("^OQ[_ ]?Test", basename(x), ignore.case = TRUE)])
    }), use.names = FALSE)
    d[duplicated(basename(d))]
  })
  ## While a case is running its own button reads "Running <name> ..." and is disabled, and the
  ## others are disabled too. Without this the button looked like a no-op for 45 s -- the run was
  ## inline, so Shiny could not repaint until it finished, and the first visible sign of life was
  ## the log file opening at the end. Hence the separate process below.
  output$oqButtons <- renderUI({
    d <- oqDirs()
    if (!length(d)) return(tags$p(class = "muted", "No OQ_Test* folders found beside the app."))
    tagList(lapply(seq_along(d), function(i) {
      nm <- basename(d[i])
      if (rv$oqRunning && identical(rv$oqName, nm))
        tags$button(class = "btn btn-primary", disabled = NA, style = "margin-right:8px",
                    sprintf("Running %s \u2026", nm))
      else if (rv$oqRunning)
        tags$button(class = "btn btn-default", disabled = NA, style = "margin-right:8px",
                    sprintf("Run %s", nm))
      else
        actionButton(paste0("oq_", i), sprintf("Run %s", nm), class = "btn-default",
                     style = "margin-right:8px")
    }),
    ## Only while something is running: a permanently visible Abort next to idle Run buttons
    ## invites a click that would report "nothing running".
    if (rv$oqRunning) actionButton("oqAbort", "Abort OQ", class = "btn-danger"),
    ## Says WHICH copy runs when the same case exists twice on this machine -- silence here reads
    ## as "there is only one", which is what made the duplicate buttons confusing.
    if (length(oqShadowed()))
      tags$p(class = "muted", style = "margin-top:8px",
             sprintf("Running the copy in %s. Also present, not used: %s.",
                     dirname(d[[1]]),
                     paste(unique(dirname(oqShadowed())), collapse = ", "))))
  })
  OQ_RES <- file.path(WORK_DIR, ".acta_oq_result.rds")
  observe({
    d <- oqDirs()
    lapply(seq_along(d), function(i) observeEvent(input[[paste0("oq_", i)]], {
      if (rv$oqRunning) return()
      dir <- d[i]; nm <- basename(dir)
      ## NEVER RUN A CASE THAT LIVES INSIDE THE INSTALLED LIBRARY. actaOQRun() starts by doing
      ## unlink(<case>/Outputs, recursive = TRUE) and then writes the export, report and plots
      ## back at the case root -- and for a package-only user that root is
      ## system.file("extdata", "oq_small"), i.e. inside the R library. Libraries are routinely
      ## group-writable (this machine's is drwxrwxr-x root:admin), so on a shared host one
      ## analyst's artefacts and machine name would land where every other user of that library
      ## can read them, and a concurrent run would have its Outputs deleted underneath it. On a
      ## managed read-only library the button just fails. Stage the case into the working folder
      ## and run the copy instead.
      ##
      ## REUSE an existing staged copy, never copy over it. file.copy(recursive = TRUE) defaults
      ## overwrite to TRUE, so a second click would merge the package original back over the
      ## staged case and silently revert anything edited there -- and the run would then report on
      ## inputs the operator did not supply. It is reachable within one session: oqDirs() is a
      ## reactive with no reactive dependencies, so it is evaluated once and keeps handing back
      ## the library path even after the copy exists.
      ## ONE IMPLEMENTATION, in actaOQStageOutOfLibrary(). The app used to carry its own copy of
      ## this logic, and the two then disagreed: this one forgot to bring validated_packages.tsv
      ## along, so the dependency-drift check silently became a skip from 3.0.4 while the verdict
      ## still read "PASS (with notes)". The helper copies the baseline and reports whether it
      ## managed to. An existing staged copy is still reused rather than overwritten.
      staged <- file.path(DIAG_DIR, nm)
      if (dir.exists(staged)) {
        say("Using the copy of ", nm, " already in ", DIAG_DIR, ".")
        ## AND BRING THE BASELINE IF IT IS NOT ALREADY THERE. DIAG_DIR persists across sessions,
        ## so a case staged by 3.0.4 -- which forgot validated_packages.tsv -- is reused here and
        ## the package-version check would silently report skip again, on exactly the upgrade path
        ## this change exists to close. The helper is bypassed on this branch by design (the copy
        ## must not be overwritten), so the one thing it does that matters here is done here.
        ## FROM THE SOURCE ROOTS, not dirname(dir). On a fresh session oqDirs() lists DIAG_DIR
        ## first and dedupes by basename, so `dir` already IS DIAG_DIR/<case> and dirname(dir) is
        ## the DESTINATION -- the first version of this looked for the baseline in the folder it
        ## was trying to populate and so never found it. The roots below are where the file
        ## actually ships.
        .dest <- file.path(DIAG_DIR, "validated_packages.tsv")
        if (!file.exists(.dest)) {
          .src <- Filter(file.exists,
                         file.path(c(PKG_CASES, file.path(CODE_DIR, "Diagnostics")),
                                   "validated_packages.tsv"))
          if (length(.src) && isTRUE(file.copy(.src[[1]], .dest)))
            say("Brought validated_packages.tsv alongside it, so the package-version check runs.")
          else
            say("Note: no validated_packages.tsv for ", nm,
                " -- the package-version check will report skip, not compare.")
        }
        dir <- staged
      } else {
        st <- tryCatch(actaOQStageOutOfLibrary(dir, dest_parent = DIAG_DIR),
                       error = function(e) e)
        if (inherits(st, "error")) {
          say("Could not copy ", nm, " out of the installed package: ", conditionMessage(st))
          return(invisible())
        }
        if (isTRUE(st$staged)) {
          say("Copied ", nm, " out of the installed package to ", st$dir, ".")
          if (!isTRUE(st$baseline))
            say("Note: validated_packages.tsv did not come with it, so the package-version ",
                "check will report skip.")
        }
        dir <- st$dir
      }
      rv$oq <- NULL; rv$oqName <- nm; rv$oqRunning <- TRUE
      rv$pct <- NA; rv$stage <- NULL; rv$t0 <- Sys.time()
      unlink(OQ_RES)
      say("--- ", nm, ": starting (about a minute) ---")
      ## Same P1 as the run: written before the child exists, so an OQ case that takes R down with
      ## it still leaves evidence that it started. actaOQFinish() writes log.txt only at the very
      ## end, so until now a crashed case left nothing at all.
      rv$diagLog <- actaDiagLogPath(WORK_DIR, paste0("oq_", nm))
      actaDiagAppend(rv$diagLog, c(sprintf("== %s started %s ==", nm,
                                           format(Sys.time(), "%Y-%m-%d %H:%M:%S")), ""))
      actaRunLogAppend(logFile, actaRunLogEntry("oq_test", "started", WORK_DIR,
                                               detail = paste(nm, "-- child not yet launched")))
      ## Separate process for the same reason the ACTA run uses one: an inline call blocks the
      ## session, so the button state and the log cannot update until it returns.
      rv$oqProc <- processx::process$new(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", "-e",
          sprintf(paste0(.childLoad, "r<-h$actaOQRun(%s,quiet=TRUE,code_dir=%s,progress=function(...) message('  ',...));saveRDS(r,%s)"),
                  .rq(dir), .rq(CODE_DIR), .rq(OQ_RES))),
        stdout = "|", stderr = "2>&1", cleanup = TRUE)
    }, ignoreInit = TRUE))
  })

  ## Poll the OQ child: stream its progress, then read the verdict when it exits.
  observe({
    if (!rv$oqRunning || is.null(rv$oqProc)) return()
    invalidateLater(600, session)
    pr <- rv$oqProc
    out <- tryCatch(pr$read_output_lines(), error = function(e) character(0))
    readProgress(out)
    for (l in out) if (nzchar(l)) rv$log <- paste(rv$log, l, sep = "\n")
    if (!pr$is_alive()) {
      rv$oqRunning <- FALSE; rv$pct <- NA; rv$stage <- NULL
      nm <- rv$oqName
      nmDir <- { hit <- oqDirs()[basename(oqDirs()) == nm]; if (length(hit)) hit[[1]] else WORK_DIR }
      if (file.exists(OQ_RES)) {
        r <- readRDS(OQ_RES); unlink(OQ_RES)
        for (x in r$recs)
          say(sprintf("  %-4s %-38s %s", toupper(x$status), x$label,
                      if (is.na(x$detail)) "" else x$detail))
        say(nm, ": ", r$verdict, sprintf("  (%.0f s)  log -> %s", r$elapsed_s,
                                         file.path(nm, "Outputs", "log.txt")))
        rv$oq <- c(r, list(name = nm))
        actaRunLogAppend(logFile, actaRunLogEntry("oq_test", if (isTRUE(r$ok)) "pass" else "fail",
                                                 nmDir, outputs = r$log, detail = r$verdict))
        openPath(r$log)
      } else {
        say(nm, ": ERROR -- no result written; see the output above.")
        rv$oq <- list(name = nm, ok = FALSE, verdict = "ERROR", recs = list(), log = NA_character_)
        actaRunLogAppend(logFile, actaRunLogEntry("oq_test", "error", nmDir,
                                                 detail = "child produced no result"))
      }
    }
  })
  output$oqResult <- renderUI({
    r <- rv$oq; if (is.null(r)) return(NULL)
    tagList(div(style = "margin-top:10px", class = if (isTRUE(r$ok)) "ready" else "bad",
                sprintf("%s — %s", r$name, r$verdict)),
            if (length(r$recs)) renderChecks(r$recs))
  })

  ## Persist the things worth remembering, globally.
  session$onSessionEnded(function() {
    prefsSave(list(mkReport = isolate(input$mkReport), mkPlots = isolate(input$mkPlots),
                   addTiter = isolate(input$addTiter),
                   ## copyGen is NOT persisted -- see the checkbox. An already-saved TRUE from before
                   ## this change is simply never read again.
                   openReport = isolate(input$openReport), openDash = isolate(input$openDash),
                   template = isolate(rv$tplOverride), dashboard_dir = isolate(rv$dashOverride),
                   titration_export_dir = isolate(rv$expOverride),
                   comp_csv = isolate(rv$compOverride),
                   copy_dir = isolate(rv$copyOverride)))
  })
}

shinyApp(ui, server)
