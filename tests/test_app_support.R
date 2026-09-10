suppressMessages({library(readxl); library(flowCore)})
## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestAppDir()
h <- actaTestFunctions(vd)
G <- h$ACTA_GLYPH
show <- function(recs) for (r in recs)
  cat(sprintf("  %s %-5s %-52s %s\n", G[[r$status]], toupper(r$status), r$label,
              if (is.na(r$detail)) "" else substr(r$detail, 1, 60)))
ok <- TRUE; chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

cat("=== 1. version files (exactly-one rule) ===\n"); vf <- h$actaVersionFiles(vd); show(vf)
## The four SHIPPED files must resolve. The user-supplied workbook may legitimately be ABSENT --
## that is what a fresh clone looks like, since the workbook carries real data and is gitignored --
## in which case it is SKIP, not FAIL. Asserting "all five pass" is what made a newly cloned public
## repo fail 2 of 11 test scripts on the first thing a new user does. Ambiguity (two workbooks) is
## still fatal and is asserted separately below.
.st <- setNames(vapply(vf, function(r) r$status, ""), vapply(vf, function(r) r$id, ""))
chk("the four shipped files resolve", all(.st[c("script","functions","dashboard","report")] == "pass"))
chk("the workbook is pass when present, skip when not",
    .st[["layout"]] %in% if (length(list.files(vd, pattern = "Titration_Instructions.*xlsx$")))
                           "pass" else "skip")
dec <- file.path(tempdir(), "decoy"); dir.create(dec, showWarnings = FALSE)
## Resolved by PATTERN, not by name. This hardcoded "ACTA_Script_2_87.R", so the 2_87 -> 2_88 bump
## left it copying a file that no longer existed: file.copy() returns FALSE quietly, the decoy folder
## never got its two scripts, and the assertion failed for a reason that had nothing to do with the
## rule under test. It would have broken at every future bump the same way.
.src <- list.files(vd, pattern = "^ACTA_Script.*[.]R$", full.names = TRUE)[1]
stopifnot(!is.na(.src), file.exists(.src))
invisible(file.copy(.src, file.path(dec, basename(.src)), overwrite = TRUE))
invisible(file.copy(.src, file.path(dec, sub("[.]R$", "_backup.R", basename(.src))), overwrite = TRUE))
d <- Filter(function(r) r$id == "script", h$actaVersionFiles(dec))[[1]]
chk("two scripts -> fail, not silent pick", d$status == "fail" && grepl("exactly one", d$detail))

cat("\n=== 2. dashboard template contract ===\n")
## By PATTERN, exactly as ACTA_Dashboard_*.R resolves it -- so this follows whichever template is
## the default instead of naming one, and asserts the generator's "exactly one" rule on the way.
tpl <- list.files(file.path(vd, "Template"), pattern = "dashboard_template", ignore.case = TRUE,
                  full.names = TRUE)
tpl <- tpl[grepl("[.]html?$", tpl, ignore.case = TRUE)]
chk("exactly one default dashboard template in Template/", length(tpl) == 1L)
tpl <- tpl[[1]]
show(h$actaValidateDashboardTemplate(tpl))
chk("good template passes tokens", Filter(function(r) r$id=="tpl_tokens", h$actaValidateDashboardTemplate(tpl))[[1]]$status == "pass")
## The unit guard has TWO halves and the check used to test only one. Three templates in
## Other_Templates -- including `ACTA_dashboard_template_navy-chrome` -- carried `th.unit{text-transform:none}` and never
## applied the class, so COLS' `unit` flag was ignored and a "(uL)" header would have rendered as
## "(ML)". They passed validation the whole time. Both halves are now required.
.styledOnly <- file.path(tempdir(), "styled-only.html")
writeLines(gsub("c\\.unit", "c.NOT_unit", paste(readLines(tpl, warn = FALSE), collapse = "\n")),
           .styledOnly)
.g <- function(f) Filter(function(r) r$id == "tpl_unit_guard",
                         h$actaValidateDashboardTemplate(f))[[1]]
chk("a template that styles the guard but never applies it FAILS", .g(.styledOnly)$status == "fail")
chk("and says why -- the script does not read the unit flag",
    grepl("nothing applies the class", .g(.styledOnly)$detail, fixed = TRUE))
chk("the default template passes both halves", .g(tpl)$status == "pass")
chk("every alternative in Other_Templates passes both halves too",
    all(vapply(list.files(file.path(vd, "Template", "Other_Templates"), pattern = "[.]html$",
                          full.names = TRUE),
               function(f) .g(f)$status == "pass", logical(1))))

bad <- file.path(tempdir(), "dup.html")
writeLines(sub("__META_JSON__", "__META_JSON__ <!-- __META_JSON__ -->",
               paste(readLines(tpl, warn=FALSE), collapse="\n")), bad)
r <- Filter(function(x) x$id=="tpl_tokens", h$actaValidateDashboardTemplate(bad))[[1]]
chk("duplicated token -> fail", r$status == "fail" && grepl("META_JSON__ x2", r$detail))
r2 <- Filter(function(x) x$id=="tpl_file", h$actaValidateDashboardTemplate(NA_character_))[[1]]
chk("missing template -> fail", r2$status == "fail")

cat("\n=== 3. export scan ===\n")
## Builds its OWN fixture. This used to scan the version folder and rely on a *TitrationExport*.xlsx
## happening to be left there by an earlier run -- a gitignored artefact, so the assertion passed on
## the author's machine and would FAIL on any fresh clone, which is precisely the environment this
## suite exists to validate. It broke the moment the stray artefacts were cleaned up.
.fx <- file.path(tempdir(), "scan_fixture"); unlink(.fx, recursive = TRUE)
dir.create(file.path(.fx, "run_A"), recursive = TRUE, showWarnings = FALSE)
writeLines("fixture", file.path(.fx, "run_A", "260101_EXP00000000_TitrationExport_2_87.xlsx"))
sc <- h$actaScanExports(.fx); show(sc$recs)
chk("finds the export", any(vapply(sc$recs, function(r) r$id=="exp_found" && r$status=="pass", TRUE)))
## WARN, not fail. An empty scan folder is the normal state before the first run is copied in, and
## failing there would block the very run that fixes it. This assertion said "fail" and was simply
## never reached, because the section below died first on a missing snapshot.
chk("empty dir -> warn", Filter(function(r) r$id=="exp_found",
      h$actaScanExports(tempdir())$recs)[[1]]$status == "warn")

cat("\n=== 3b. copy destination reachability ===\n")
## The copy destination and the scan folder are independent fields. When they disagree the copy
## succeeds and the dashboard silently shows a run from BEFORE it, which is what "the report has
## the titer but the dashboard does not" looks like. WARN, never fail -- an unindexed drop folder
## is unusual but legitimate.
.sc <- file.path(tempdir(), "cdr_scan")
dir.create(file.path(.sc, "run1", "Outputs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(tempdir(), "cdr_other"), showWarnings = FALSE)
.st <- function(d) h$actaCopyDestReachable(d, .sc)$status
chk("scan folder itself -> pass",        .st(.sc) == "pass")
chk("immediate subfolder -> pass",       .st(file.path(.sc, "run1")) == "pass")
chk("subfolder Outputs/ -> pass",        .st(file.path(.sc, "run1", "Outputs")) == "pass")
chk("unrelated folder -> warn",          .st(file.path(tempdir(), "cdr_other")) == "warn")
chk("two levels deep -> warn",           .st(file.path(.sc, "run1", "deeper")) == "warn")
chk("unset -> skip, never fail",         .st("") == "skip")

cat("\n=== 4. titer options (90%Vmax guard) ===\n")
## This section needs a captured run snapshot, which lives in a SESSION scratch dir named by $S.
## Unset (a fresh shell, CI, anyone but the author) the path collapsed to "/regress/final_default.rds"
## and readRDS aborted the whole file -- so every check BELOW here silently never ran, and the two
## sections above only appeared to be the whole test. Skip loudly instead of dying.
snapDir <- Sys.getenv("S", unset = NA)
snap <- if (!is.na(snapDir) && nzchar(snapDir)) file.path(snapDir, "regress", "final_default.rds") else NA
if (is.na(snap) || !file.exists(snap)) {
  cat("  SKIPPED -- no run snapshot available.\n")
  cat("  Set S=<scratch dir> holding regress/final_default.rds (capture it with\n")
  cat("  tests/regression_capture.R) to exercise the titer-option guards.\n")
} else {
res <- readRDS(snap)
li <- res$mmFits; st <- res$stats
thr <- suppressMessages(as.numeric(read_excel(file.path(vd, list.files(vd, pattern="Titration_Instructions.*xlsx$")[1]),
                                              sheet="Info", .name_repair="minimal")$MM_R2_threshold[1]))
cat(sprintf("  MM_R2_threshold from Info = %s\n", thr))
for (o in h$actaTiterOptions(li, st, thr)) {
  cat(sprintf("  %-12s %s\n", o$dirname, o$maxsi$label))
  cat(sprintf("  %-12s %s\n", "", o$vmax90$label))
  chk(sprintf("%s: 90%%Vmax guard gives a reason when disabled", o$dirname),
      o$vmax90$enabled || !is.na(o$vmax90$reason))
}
}  ## end of the snapshot-dependent section

cat("\n=== 5. archive-then-copy ===\n")
src <- file.path(tempdir(), "srcA"); dst <- file.path(tempdir(), "dstA")
dir.create(src, showWarnings=FALSE); dir.create(dst, showWarnings=FALSE)
f1 <- file.path(src, "260816_EXP1_TitrationExport_2_87.xlsx"); writeLines("v1", f1)
r1 <- h$actaArchiveThenCopy(f1, dst, "EXP1", stamp="T1")
chk("first copy, nothing archived", r1$ok && !length(r1$archived))
writeLines("v2", f1)
r2 <- h$actaArchiveThenCopy(f1, dst, "EXP1", stamp="T2")
chk("second copy archives the old one", r2$ok && length(r2$archived) == 1)
chk("archive dir carries a discriminator", grepl("T2_EXP1$", r2$archive_dir))
chk("live file is the new content", identical(readLines(file.path(dst, basename(f1))), "v2"))
chk("archived file is the old content",
    identical(readLines(file.path(r2$archive_dir, basename(f1))), "v1"))
writeLines("v3", f1); r3 <- h$actaArchiveThenCopy(f1, dst, "EXP1", stamp="T3")
chk("third run does NOT clobber the T2 archive",
    identical(readLines(file.path(r2$archive_dir, basename(f1))), "v1") &&
    dir.exists(r3$archive_dir) && r3$archive_dir != r2$archive_dir)
chk("archive path is excluded by the generator's rule",
    grepl("archive", r2$archive_dir, ignore.case = TRUE))

cat("\n=== 6. run log ===\n")
lg <- file.path(tempdir(), "acta_run_log.tsv"); unlink(lg)
## A workbook to hash. In a fresh clone there is none in the version folder, so fall back to the
## shipped TEMPLATE -- the point of this check is that a resolved workbook's md5 REACHES the ledger,
## not which workbook it was.
.wb <- c(list.files(vd, pattern = "Titration_Instructions.*xlsx$", full.names = TRUE),
         list.files(file.path(vd, "Template"), pattern = "Instructions.*xlsx$", full.names = TRUE))
h$actaRunLogAppend(lg, h$actaRunLogEntry("run_acta","success", vd, layout=.wb[[1]],
                                         report=TRUE, plots=TRUE, outputs=c("a.pdf","b.xlsx")))
h$actaRunLogAppend(lg, h$actaRunLogEntry("run_acta","aborted", vd, detail="user pressed Abort"))
lines <- readLines(lg)
chk("header + 2 rows", length(lines) == 3)
chk("aborted run recorded", any(grepl("aborted", lines)))
chk("workbook md5 captured", grepl("[0-9a-f]{32}", lines[2]))
cat("  log row 2: ", substr(lines[2], 1, 110), "...\n")


cat("=== LaTeX packages the report needs ===\n")
## Pinned because the obvious implementation is wrong in a way that always PASSES: kpsewhich returns
## character(0) for a package it cannot find, [1] of that is NA, `%||%` does not replace NA, and
## nzchar(NA) is TRUE. The first version of actaLatexMissing() reported every missing package as
## present, which is worse than having no check -- a colleague's render had already failed on a
## missing placeins.sty with nothing warning about it.
.tp <- h$actaLatexPackages(vd)
chk(sprintf("reads the packages out of the .Rmd (%d found)", length(.tp)),
    ## The list is short ON PURPOSE. tocloft went when the doubled \part entries turned out to be the
    ## .Rmd's own duplicate dividers rather than tocloft's fault; fancyhdr, lastpage, placeins and
    ## titlesec went on 2026-08-20 because they were exactly the four a colleague's minimal TeX was
    ## missing, with a broken tlmgr and so no way to install them. Each was replaced by plain LaTeX.
    all(c("multicol", "longtable", "booktabs") %in% .tp))
## Asserted as ABSENT, not merely "not required": re-adding any of them re-creates a failure mode we
## have already paid for, so it should have to be a deliberate act that breaks a test.
chk("the four dropped packages stay dropped",
    !any(c("fancyhdr", "lastpage", "placeins", "titlesec") %in% .tp))
## The list has to cover what reaches the ENGINE, not just what is written in the .Rmd. kableExtra
## injects its own packages at knit time and they are exactly the ones a minimal TeX tends to lack, so
## a check that reads only the .Rmd verified 6 of 16 -- and, because header-includes are emitted
## FIRST, a missing package of ours masks every one of theirs.
## `tabu` IS VERSION-DEPENDENT and must not be asserted as a constant -- the comment a few
## lines below has always said so (1.4.0 injects `tabu`; the release that replaced it injects
## `xltabular`), and the fallback check right after this one already requires the UNION. This
## assertion hardcoded `tabu` alone, so it passed on a machine with kableExtra 1.4.0 and failed
## on every CI runner, which installs the current release. That is what kept the public badge
## red from v2.98 through v2.99.2. Require the version-INDEPENDENT injections, and accept
## either spelling of the tabular package.
chk("kableExtra's knit-time injections are included",
    all(c("threeparttablex", "pdflscape", "makecell", "multirow", "wrapfig") %in% .tp) &&
      any(c("tabu", "xltabular") %in% .tp))
## The FALLBACK matters as much as the live read: it is what runs when kableExtra's internal list
## cannot be read, i.e. exactly when there is no way to tell which variant is installed. 1.4.0 injects
## `tabu`; the release that replaced it injects `xltabular`, which is what a colleague was missing. A
## fallback naming only one of them sends the other machine straight back into the failure.
chk("the fallback is a UNION across kableExtra versions, not one machine's snapshot",
    all(c("tabu", "xltabular") %in% h$ACTA_KABLEEXTRA_TEX))
chk("and they are read from kableExtra, not restated here",
    identical(sort(unique(gsub(".*[{]|[}]", "",
                get("latex_pkg_list", asNamespace("kableExtra"))()))) |> intersect(.tp) |> length(),
              length(unique(gsub(".*[{]|[}]", "",
                get("latex_pkg_list", asNamespace("kableExtra"))())))))
## BOTH of these need a real TeX tree to mean anything, and the guard is not optional.
## actaLatexMissing() returns character(0) when there is no kpsewhich -- deliberately, so that a
## machine without TeX makes no claim either way, which is what the very next check asserts. So on
## such a machine the negative control below gets character(0) instead of the fake package and
## FAILS, and "no false positives" passes VACUOUSLY because everything comes back empty. That is
## exactly the CI gates job, which installs no TeX (only the oq job gets TinyTeX), and it is why the
## public badge was red from v2.98 onwards while the tool itself was fine.
## Reproduce with:  env PATH=/usr/bin:/bin Rscript tests/test_app_support.R
if (nzchar(Sys.which("kpsewhich"))) {
  chk("a package that cannot exist is reported missing",
      identical(h$actaLatexMissing(c("array", "acta_no_such_pkg")), "acta_no_such_pkg"))
  chk("no false positives on the real list", length(h$actaLatexMissing(.tp)) == 0L)
} else {
  cat("  [skip] no kpsewhich here, so actaLatexMissing() cannot be exercised against a TeX tree\n")
}
chk("no kpsewhich -> no claim either way", {
  op <- Sys.getenv("PATH"); on.exit(Sys.setenv(PATH = op))
  Sys.setenv(PATH = "/nonexistent")
  z <- h$actaLatexMissing(c("array")); Sys.setenv(PATH = op); length(z) == 0L })

cat("=== can tlmgr run? (actaTlmgrStatus) ===\n")
## From a colleague's machine, 2026-08-20: the LaTeX check correctly named four missing packages and
## then offered `tinytex::tlmgr_install(...)`, which answered "env: perl: No such file or directory".
## tlmgr is a Perl script; the PATH said "/user/bin" where it meant "/usr/bin", so perl -- present at
## /usr/bin/perl -- was invisible to R. Advice that cannot run is worse than no advice, so the state
## is now probed. Her PATH is reproduced here exactly.
withPath <- function(pth, expr) {
  old <- Sys.getenv("PATH"); on.exit(Sys.setenv(PATH = old), add = TRUE)
  Sys.setenv(PATH = pth); force(expr)
}
if (nzchar(Sys.which("tlmgr"))) {
  chk("a working PATH reports tlmgr as usable", isTRUE(h$actaTlmgrStatus()$ok))
  broken <- withPath("/usr/local/bin:/user/bin:/bin", h$actaTlmgrStatus())
  chk("that PATH (the /user/bin typo) is reported as NOT usable", isFALSE(broken$ok))
  chk("and it says WHY -- a Perl script with no perl on PATH",
      grepl("Perl script", broken$why, fixed = TRUE) && grepl("perl is not on", broken$why))
  chk("and it quotes the PATH, which is where the typo is visible",
      grepl("/user/bin", broken$why, fixed = TRUE))
  chk("the fix names the directory to add, not a doomed tlmgr command",
      grepl("/usr/bin", broken$fix, fixed = TRUE) && !grepl("tlmgr_install", broken$fix))
} else {
  cat("  [skip] tlmgr not on PATH here, so its status cannot be exercised\n")
}
chk("no tlmgr at all -> reported, with a distribution as the fix",
    { r <- withPath(file.path(tempdir(), "definitely-empty"), h$actaTlmgrStatus())
      isFALSE(r$ok) && grepl("not on PATH", r$why) && grepl("TeX distribution", r$fix) })

cat("=== CI declares every package the pipeline loads ===\n")
## ggplate was added to listOfLibrary for the plate map in 2_97 and NOT to the CI workflow, so a
## fresh public runner would have failed test_plate_map.R (it builds a real plate) and every OQ case
## (the report loads ggplate) on the first push -- a red badge on the repo's opening day. Nothing
## compared the two lists, so nothing said so. This does.
##
## The workflow lives at Publish/github/workflows/ in the development repo and at
## .github/workflows/ in the released one; whichever is present is checked, and if neither is
## (someone running these tests from a bare copy) it is a SKIP rather than a failure.
.wf <- Filter(file.exists, c(file.path(dirname(vd), "Publish", "github", "workflows", "tests.yml"),
                             file.path(vd, ".github", "workflows", "tests.yml"),
                             file.path(dirname(vd), ".github", "workflows", "tests.yml")))
if (!length(.wf)) cat("  [skip] no CI workflow file found from here\n") else {
  .scr  <- Sys.glob(file.path(vd, "ACTA_Script*.R"))[1]
  .line <- grep("^listOfLibrary", readLines(.scr, warn = FALSE), value = TRUE)[1]
  .need <- eval(parse(text = sub("^listOfLibrary *<-? *", "", .line)))
  ## Shipped with R itself or only used interactively -- r-lib/actions does not install these.
  .base <- c("BiocManager", "grDevices", "rstudioapi")
  .y    <- readLines(.wf[1], warn = FALSE)
  .have <- unique(sub(".*any::", "", grep("any::", .y, value = TRUE)))
  .miss <- setdiff(setdiff(.need, .base), .have)
  chk(sprintf("every listOfLibrary package is declared in CI (%d checked, %d declared)",
              length(setdiff(.need, .base)), length(.have)),
      length(.miss) == 0L)
  if (length(.miss)) cat("          missing from the workflow:", paste(.miss, collapse = ", "), "\n")
  ## And the reverse, as a note only: a workflow may legitimately declare extras the script does not
  ## load directly (shiny, rmarkdown, magick, jsonlite, purrr are used by the app, the render and the
  ## gates), so this is information rather than a rule.
  .extra <- setdiff(.have, .need)
  if (length(.extra))
    cat("          note: declared in CI but not in listOfLibrary:", paste(.extra, collapse = ", "), "\n")
}

cat(if (ok) "\nALL APP-SUPPORT TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
