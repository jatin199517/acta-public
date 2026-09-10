## Guards the diagnostic capture added 2026-09-03: the three write points and actaDiagnosticBundle().
##
## The gap this exists for: on a failed run the app recorded `detail = "child process produced no
## result"` and the child's actual error lived only in `rv$log`, on screen, with no download handler
## anywhere in the app. So the most useful artefact in a failure was the one thing that could not be
## sent. See Plans/260827_error-capture-and-diagnostic-bundle.md.
##
## EVERY CHECK HERE ASSERTS CONTENT, NOT EXISTENCE. The defect being fixed is precisely a file that
## exists and says nothing, so "the bundle was written" proves nothing on its own -- cf. the
## `shared_upstream` assertion that passed on zero comparisons (2026-08-27).
## Rscript <this> [version_dir]   -- defaults to 3_0.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

wd <- file.path(tempdir(), paste0("acta_diag_", as.integer(Sys.time())))
dir.create(wd, recursive = TRUE, showWarnings = FALSE)

cat("=== actaFirstError takes the FIRST error, not the last ===\n")
## The real cascade, 2026-08-27: one missing PNG produced four downstream failures and only the
## first line named the cause, so a tail-only report actively misleads.
cascade <- c("[ACTA] gating Cells", "[ACTA] gating Live",
             "! xdvipdfmx:fatal: Image inclusion failed. Could not find file: lp1-Live-1-1.png",
             "Error in render(): pandoc document conversion failed",
             "Error: could not find the report PDF")
fe <- h$actaFirstError(cascade)
chk("quotes the xdvipdfmx line", grepl("xdvipdfmx", fe, fixed = TRUE))
chk("does NOT lead with the last, derivative error",
    !grepl("^Error: could not find the report PDF", fe))
chk("empty text yields empty, rather than a guess", identical(h$actaFirstError(character(0)), ""))
chk("no error-shaped line yields empty",
    identical(h$actaFirstError(c("[ACTA] all good", "done!")), ""))

cat("=== actaDiagAppend tolerates an unwritable destination ===\n")
chk("NA path is a silent no-op, not an error", isFALSE(h$actaDiagAppend(NA_character_, "x")))
lg <- file.path(wd, "kid.log")
h$actaDiagAppend(lg, cascade)
h$actaDiagAppend(lg, "== closing marker ==")
chk("appends rather than overwrites", length(readLines(lg, warn = FALSE)) == length(cascade) + 1L)

cat("=== actaDiagLogPath falls back when the work dir cannot be written ===\n")
p1 <- h$actaDiagLogPath(wd, "run")
chk("lands under the work dir when writable", !is.na(p1) && startsWith(p1, wd))
p2 <- h$actaDiagLogPath(file.path("/proc-does-not-exist", "nope"), "run")
chk("falls back to tempdir() rather than returning NA", !is.na(p2) && !startsWith(p2, "/proc"))

cat("=== the bundle CONTAINS the error, and is scrubbed ===\n")
leaky <- c(cascade, "reading /Users/someone.name/OneDrive - Company/Flow/EXP1_data.fcs",
           "contact someone.name@company.com for the layout")
h$actaDiagAppend(lg, leaky)
b <- h$actaDiagnosticBundle(wd, vd, run_log = lg, child_log = lg, layout = NA_character_,
                            res = NULL, tsv = NA_character_, tlmgr_timeout = 5)
chk("bundle was written", isTRUE(b$ok) && file.exists(b$path))
txt <- if (isTRUE(b$ok)) readLines(b$path, warn = FALSE) else character(0)
chk("carries the FIRST ERROR section", any(grepl("^== FIRST ERROR ==", txt)))
chk("and the actual error text in it", any(grepl("xdvipdfmx", txt, fixed = TRUE)))
chk("carries sessionInfo output", any(grepl("R version|platform", txt)))
chk("carries the collector manifest", any(grepl("COLLECTOR MANIFEST", txt)))
chk("carries the closing sentinel", any(grepl("END OF BUNDLE", txt)))
chk("keeps the failing file's BASENAME (losing it defeats the purpose)",
    any(grepl("EXP1_data.fcs", txt, fixed = TRUE)))
chk("no username survives", !any(grepl("someone\\.name(?!@)", txt, perl = TRUE)))
chk("no email survives", !any(grepl("@company\\.com", txt)))
chk("no absolute home path survives", !any(grepl("/Users/someone", txt, fixed = TRUE)))

cat("=== MUTATION SELF-CHECK: the leak assertions must be able to fail ===\n")
## A scrubber that is never exercised is indistinguishable from one that does nothing, so run the
## same assertions against UNSCRUBBED text and require them to fail.
raw <- leaky
chk("username IS present before scrubbing", any(grepl("someone\\.name", raw, perl = TRUE)))
chk("email IS present before scrubbing", any(grepl("@company\\.com", raw)))
chk("scrubbing changes the text", !identical(h$actaDiagScrub(raw), raw))

cat("=== a `started` row with no terminal row is detectable ===\n")
tsv <- file.path(wd, "run_log.tsv")
h$actaRunLogAppend(tsv, h$actaRunLogEntry("run_acta", "started", vd, detail = "child not yet launched"))
d <- read.delim(tsv, stringsAsFactors = FALSE)
chk("the started row is on disk before any outcome", any(d$outcome == "started"))
chk("and no terminal row accompanies it",
    !any(d$outcome %in% c("success", "failed", "analysis_failed", "report_failed", "aborted")))

cat(if (ok) "\nALL DIAGNOSTIC CHECKS PASSED\n" else "\nSOME DIAGNOSTIC CHECKS FAILED\n")
quit(status = if (ok) 0L else 1L)
