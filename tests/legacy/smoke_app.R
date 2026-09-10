## Headless smoke test: runs the SERVER logic via shiny::testServer -- no browser, no run.
suppressMessages({library(shiny); library(readxl)})
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
setwd(actaTestAppDir())
app <- shiny::shinyAppFile("ACTA_App.R")
ok <- TRUE; chk <- function(w, c) { if (!c) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (c) "ok" else "FAIL", w)) }
testServer(app, {
  cat("=== version files ===\n")
  st <- vapply(vf, function(r) r$status, "")
  for (r in vf) cat(sprintf("  %-16s %-5s %s\n", r$label, r$status, if (is.na(r$detail)) "" else substr(r$detail,1,48)))
  ## Same split as test_app_support.R: the shipped files must resolve, the user's workbook may be
  ## absent in a fresh clone (SKIP). `layout` is NA when it is absent, and the app must still come
  ## up -- the pre-flight panel is what refuses to RUN without one, which is asserted below.
  .id <- vapply(vf, function(r) r$id, "")
  chk("the four shipped files resolve", all(st[.id != "layout"] == "pass"))
  chk("the workbook is pass or skip, never fail", st[.id == "layout"] %in% c("pass", "skip"))
  chk("the app comes up either way", TRUE)

  cat("\n=== ACTA preflight ===\n")
  ar <- actaRecs()
  for (r in ar) if (r$status != "pass") cat(sprintf("  %-5s %s : %s\n", r$status, r$label, r$detail))
  cat(sprintf("  %d checks, %d pass, %d fail, %d warn/skip\n", length(ar),
      sum(vapply(ar,function(r) r$status=="pass",TRUE)),
      sum(vapply(ar,function(r) r$status=="fail",TRUE)),
      sum(vapply(ar,function(r) r$status %in% c("warn","skip"),TRUE))))
  ## NOT "nothing fails". A fresh clone has no working DATA -- Titration_FCS ships with only its
  ## README -- and the pre-flight is RIGHT to block on that, so a blanket assertion turns a healthy
  ## install into a red suite for every new user. The assertion is instead "nothing fails EXCEPT the
  ## missing working data", which still fails the gate on any NEW kind of failure. Listing the ids
  ## explicitly, rather than skipping the check, is what preserves that.
  ##
  ## "No working data" has TWO shapes and both are legitimate: the folder ABSENT (before it shipped),
  ## or present and EMPTY (what a clone gets now). Keying on "are there any .fcs at all" covers both --
  ## and note the failing id DIFFERS between them: absent gives fcs_root, empty gives fcs_per_dirname,
  ## because the root check passes as soon as the directory exists.
  noFcs <- !length(list.files("Titration_FCS", pattern = "[.]fcs$", recursive = TRUE))
  allowedFail <- if (noFcs) c("fcs_root", "fcs_per_dirname", "fcs_row_count") else character(0)
  badIds <- vapply(Filter(function(r) r$status == "fail", ar), function(r) r$id, "")
  chk(sprintf("ACTA preflight blocks only on %s",
              if (length(allowedFail)) "the missing working data" else "nothing"),
      all(badIds %in% allowedFail))
  chk("gate_manual check present", any(vapply(ar, function(r) r$id=="gate_manual", TRUE)))

  cat("\n=== dashboard preflight ===\n")
  dr <- dashRecs()
  for (r in dr) cat(sprintf("  %-5s %-46s %s\n", r$status, r$label, if (is.na(r$detail)) "" else substr(r$detail,1,40)))
  chk("template contract evaluated", any(vapply(dr, function(r) r$id=="tpl_tokens", TRUE)))

  cat("\n=== button label tracks the checkbox ===\n")
  session$setInputs(copyGen = FALSE); l1 <- paste(as.character(output$runBtn), collapse=" ")
  session$setInputs(copyGen = TRUE);  l2 <- paste(as.character(output$runBtn), collapse=" ")
  chk("unchecked -> 'Run ACTA'", grepl(">Run ACTA<", l1))
  chk("checked -> 'Run ACTA and generate dashboard'", grepl("Run ACTA and generate dashboard", l2))

  cat("\n=== outputs render ===\n")
  for (o in c("versionChecks","actaChecks","dashChecks","verline","log"))
    chk(paste("output:", o), { x <- try(output[[o]], silent = TRUE); !inherits(x, "try-error") && !is.null(x) })


  cat("\n=== Browse buttons + blank-means-APP_DIR ===\n")
  ui <- paste(as.character(output$dashPaths), collapse = " ")
  chk("Browse button on export folder",    grepl("browse_expDir",  ui))
  chk("Browse button on dashboard folder", grepl("browse_dashDir", ui))
  chk("Browse button on template",         grepl("browse_tplPath", ui))
  chk("note present under each field",     length(gregexpr("Blank means app", ui)[[1]]) >= 2)
  ## Blank must resolve to the app folder, not fail as "not set"
  session$setInputs(expDir = "", dashDir = "")
  dr <- dashRecs()
  eo <- Filter(function(r) r$id == "exp_dir",  dr)[[1]]
  do <- Filter(function(r) r$id == "dash_out", dr)[[1]]
  cat(sprintf("  exp_dir  -> %s : %s\n", eo$status, substr(eo$detail, 1, 60)))
  cat(sprintf("  dash_out -> %s : %s\n", do$status, substr(do$detail, 1, 60)))
  chk("blank export folder resolves to the app folder", eo$status == "pass")
  chk("blank dashboard folder resolves to the app folder", do$status == "pass")
  ## Must NEVER block, and must agree with the generator's depth rule rather than beat it.
  ## This asserted "pass" back when actaScanExports() globbed each run folder recursively. It does
  ## not any more, on purpose: the generator scans a run's root plus Outputs/ and nothing deeper, so
  ## a recursive pre-flight reported exports the dashboard would then fail to index -- a green check
  ## followed by an empty dashboard. Pointed at the app folder the OQ exports sit two levels down
  ## (Diagnostics/OQ_TestN/Outputs/), so finding nothing is the CORRECT answer here; what matters is
  ## that it warns instead of failing, since a first run legitimately has nothing to index yet.
  ef <- Filter(function(r) r$id == "exp_found", dr)[[1]]
  cat(sprintf("  exp_found -> %s : %s\n", ef$status, substr(ef$detail, 1, 70)))
  chk("blank export folder never blocks the run", ef$status %in% c("pass", "warn"))

  cat("\n=== no duplication; grouped panels ===\n")
  ar <- actaRecs()
  fileIds <- vapply(vf, function(r) r$id, "")
  chk("ACTA instructions check no longer repeats the file checks",
      !any(vapply(ar, function(r) r$id, "") %in% fileIds))
  vc <- paste(as.character(output$versionChecks), collapse=" ")
  ac <- paste(as.character(output$actaChecks),    collapse=" ")
  dc <- paste(as.character(output$dashChecks),    collapse=" ")
  chk("Files check header",        grepl("Files check", vc))
  chk("ACTA instructions check header", grepl("ACTA instructions check", ac))
  chk("Dashboard check header",    grepl("Dashboard check", dc))
  chk("collapsible <details> used",grepl("<details", vc))
  chk("overall verdict shown",     grepl("all 5 passed", vc))
  chk("passing group is collapsed", !grepl("<details open", vc))
  chk("failing group opens by default", grepl("<details open", dc) || !grepl("fail", dc))

  cat("\n=== copy destination field + empty-export warn ===\n")
  session$setInputs(copyGen = TRUE)
  cp <- paste(as.character(output$copyPathUI), collapse=" ")
  chk("copy destination field renders", grepl("copyDir", cp))
  chk("copy destination has Browse",    grepl("browse_copyDir", cp))
  chk("copy destination has the note",  grepl("Blank means app", cp))
  ## An empty folder must WARN, not block -- it is the normal first-run state.
  empt <- file.path(tempdir(), "emptyexports"); dir.create(empt, showWarnings = FALSE)
  sc <- actaScanExports(empt)
  ef <- Filter(function(r) r$id == "exp_found", sc$recs)[[1]]
  cat(sprintf("  empty export dir -> %s : %s\n", ef$status, substr(ef$detail, 1, 56)))
  chk("empty export folder warns rather than fails", ef$status == "warn")
  chk("a warn does not block the dashboard button",
      !any(vapply(sc$recs, function(r) r$status == "fail", TRUE)))

  cat("\n=== renamed labels, pooled checks, chain emphasis ===\n")
  session$setInputs(copyGen = TRUE)
  dp <- paste(as.character(output$dashPaths), collapse=" ")
  chk("label: Scan folder for Titration exports", grepl("Scan folder for Titration exports", dp))
  chk("label: Write Dashboard HTML to",           grepl("Write Dashboard HTML to", dp))
  ## all three groups render, and they now live in one panel
  for (o in c("versionChecks","actaChecks","dashChecks"))
    chk(paste("group renders:", o), nzchar(paste(as.character(output[[o]]), collapse="")))

  cat("\n=== dependency checks ===\n")
  dep <- depRecs()
  for (r in dep) cat(sprintf("  %-5s %-42s %s\n", r$status, r$label,
                             if (is.na(r$detail)) "" else substr(r$detail, 1, 44)))
  chk("dependency group has R + non-R rows", length(dep) >= 4)
  chk("script package list was parsed (not empty)",
      !any(vapply(dep, function(r) r$id == "pkgs_script" && r$status == "skip", TRUE)))
  dc <- paste(as.character(output$depChecks), collapse=" ")
  chk("dependency panel renders with its header", grepl("All dependencies installed", dc))

  cat("\n=== Diagnostics section ===\n")
  d <- oqDirs()
  cat("  OQ folders discovered:", paste(basename(d), collapse=", "), "\n")
  chk("OQ_Test1 discovered", any(grepl("OQ_Test1", basename(d))))
  ob <- paste(as.character(output$oqButtons), collapse=" ")
  chk("a Run button per OQ folder", grepl("Run OQ_Test1", ob))
  ## ONE button per case NAME, counted -- not grepped. The old assertion was
  ## grepl("Run OQ_Test1", ...), which is exactly as true with four buttons as with two, so it sat
  ## green through the duplicate-button bug the user reported on 2026-08-27: the dedup was over
  ## full PATHS, and both the version folder's Diagnostics/ and an installed package's
  ## extdata/oq_small/ hold cases of the same name. A substring match cannot see a duplicate.
  ##
  ## This machine only reproduces it with the ACTA package installed, so the count is asserted
  ## against the discovered NAMES rather than a literal -- the check has to hold either way.
  chk("no duplicate case names survive discovery", !any(duplicated(basename(d))))
  nBtn <- length(gregexpr("Run OQ[_ ]?Test", ob)[[1]])
  chk(sprintf("one Run button per distinct case (%d button(s), %d case(s))",
              nBtn, length(unique(basename(d)))),
      nBtn == length(unique(basename(d))))
  ## Nearest root wins. Alphabetical sorting over absolute paths put /Library/Frameworks (an
  ## installed package, carrying ITS version of the workbook) ahead of the version folder, so the
  ## leftmost buttons were the stale ones.
  chk("the case that wins is this folder's, not an installed package's",
      all(!grepl("/library/ACTA/|/ACTA/extdata/", d, ignore.case = TRUE)) ||
        !dir.exists(file.path(getwd(), "Diagnostics", "OQ_Test1")))
  chk("expectations file present",
      file.exists(file.path(d[1], "expected.json")))

  cat("\n=== OQ button states ===\n")
  idle <- paste(as.character(output$oqButtons), collapse=" ")
  chk("idle: clickable Run button", grepl("Run OQ_Test1", idle) && !grepl("disabled", idle))
  rv$oqRunning <- TRUE; rv$oqName <- "OQ_Test1"; session$flushReact()
  busy <- paste(as.character(output$oqButtons), collapse=" ")
  chk("running: label becomes 'Running OQ_Test1'", grepl("Running OQ_Test1", busy))
  chk("running: button disabled", grepl("disabled", busy))
  ## Abort is offered only while a case is running. Visible at idle it would invite a click that
  ## can only answer "nothing running".
  chk("running: Abort OQ offered", grepl("oqAbort", busy))
  chk("idle: no Abort OQ button", !grepl("oqAbort", idle))
  ## Pressing it with no live process must not throw and must SAY so. Asserting the message rather
  ## than a state flag: with no process to kill the handler takes its else branch and deliberately
  ## leaves rv$oqRunning alone, so any state assertion here would be either vacuous or wrong.
  session$setInputs(oqAbort = 1)
  chk("abort with no live OQ process reports it", grepl("No OQ test running", rv$log))
  rv$oqRunning <- FALSE; session$flushReact()
  chk("back to idle after the run", grepl("Run OQ_Test1", paste(as.character(output$oqButtons), collapse=" ")))
  cat("  OQ folder contents:", paste(list.files("Diagnostics/OQ_Test1"), collapse=", "), "\n")
  chk("OQ folder carries no code copies",
      !any(grepl("^ACTA_", list.files("Diagnostics/OQ_Test1"))))
  chk("OQ folder has no Template/", !dir.exists("OQ_Test1/Template"))

  cat("\n=== progress bar ===\n")
  chk("hidden when idle", is.null(progressUI(FALSE)))
  rv$t0 <- Sys.time(); rv$pct <- NA; rv$stage <- NULL
  ind <- paste(as.character(progressUI(TRUE)), collapse=" ")
  chk("indeterminate before any percentage", grepl("indet", ind))
  readProgress(c("processing file: ACTA_Report_2.87.Rmd", "  |=====        |  40%"))
  det <- paste(as.character(progressUI(TRUE)), collapse=" ")
  chk("real knitr percentage drives the width", grepl("width:40%", det))
  chk("stage text shown", grepl("rendering the report", det))
  readProgress("[ACTA] Strep_PE -- populations: 3 | Layout 3")
  chk("gating stage recognised", grepl("gating", paste(as.character(progressUI(TRUE)), collapse=" ")))

  cat("\n=== compensation line ===\n")
  cu <- paste(as.character(output$compUI), collapse=" ")
  cat("  workbook compensation =", if (nzchar(compModeRaw())) compModeRaw() else "(blank)", "\n")
  chk("Compensation line renders", grepl("Compensation", cu))
  chk("blank -> states identity + how to enable",
      ## Matches the QUOTED form ("csv" / "self"). The hint gained quotes so the two accepted
      ## values read as literals rather than as prose; asserting the bare words would pass either
      ## way and stop testing the thing that changed.
      grepl("Identity matrix will be applied", cu) &&
        grepl('&quot;csv&quot; or &quot;self&quot;|"csv" or "self"', cu))
  chk("blank -> no file picker offered", !grepl("browse_compPath", cu))
  ar <- actaRecs()
  cr <- Filter(function(r) r$id == "compensation", ar)
  chk("compensation record is in the ACTA instructions check", length(cr) == 1)
  cat("  ", cr[[1]]$status, "--", substr(cr[[1]]$detail, 1, 62), "\n")
  cat("\n=== abort with nothing running is safe ===\n")
  session$setInputs(abort = 1)
  chk("abort handled", grepl("Nothing running", rv$log))
})
cat(if (ok) "\nSMOKE TEST PASS\n" else "\nSMOKE TEST FAILURES\n"); quit(status = if (ok) 0 else 1)
