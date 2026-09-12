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
## ANCHORED ON THE REAL TREE, NOT ON `vd`. `vd` is actaTestAppDir(), which returns a TEMP STAGE
## whenever the folder has no working workbook -- which is every fresh clone, so both the flat
## public repo and the package layout. dirname(<temp stage>) never holds a workflow, so this
## check printed "[skip] no CI workflow file found from here" on EVERY CI run and only ever
## executed in a development folder that happened to have a workbook beside it. The gate that
## exists to catch a dependency missing from CI was itself inert in CI -- confirmed in run #9.
.wf <- Filter(file.exists, c(
  file.path(ACTA_REPO_ROOT,          ".github", "workflows", "tests.yml"),
  file.path(ACTA_VERSION_DIR,        ".github", "workflows", "tests.yml"),
  file.path(ACTA_REPO_ROOT,          "Publish", "github", "workflows", "tests.yml"),
  file.path(dirname(ACTA_REPO_ROOT), "Publish", "github", "workflows", "tests.yml")))
if (!length(.wf)) cat("  [skip] no CI workflow file found from here\n") else {
  ## BOTH MANIFESTS. There are two: the script's, and the REPORT's, which carries its own
  ## install-then-library walk() and adds names the script's list does not have -- `ragg` and
  ## `sessioninfo`. This check only ever read the script, so those two were undeclared in either
  ## CI job and in no DESCRIPTION field, and the weekly oq run installed them into the session
  ## doing the analysis. A one-manifest audit reported full coverage the whole time.
  .mf   <- c(Sys.glob(file.path(vd, "ACTA_Script*.R"))[1],
             Sys.glob(file.path(vd, "ACTA_Report*.Rmd"))[1])
  .mf   <- .mf[!is.na(.mf) & nzchar(.mf)]
  ## EACH FILE MUST YIELD A LIST, not merely exist. Asserting only that both files were FOUND was
  ## the same fault one level up: rename the report's `listOfLibrary` and the audit quietly shrank
  ## from 22 names to 20 and still reported full coverage. Caught by breaking it -- which is the
  ## step that keeps finding these.
  .per  <- lapply(.mf, function(f) {
    .line <- grep("^listOfLibrary", readLines(f, warn = FALSE), value = TRUE)[1]
    if (is.na(.line)) character(0) else eval(parse(text = sub("^listOfLibrary *<-? *", "", .line)))
  })
  names(.per) <- basename(.mf)
  chk(sprintf("both install manifests were found AND parsed (%s)",
              paste(sprintf("%s:%d", names(.per), lengths(.per)), collapse = ", ")),
      length(.mf) == 2L && all(lengths(.per) > 0L))
  .need <- unique(unlist(.per))
  ## Shipped with R itself -- r-lib/actions does not install these. BiocManager and rstudioapi
  ## used to be excused here too, which is exactly how they came to be missing on the runners
  ## while the pipeline installed them itself. They are ordinary declarations now.
  .base <- c("grDevices")
  .y    <- readLines(.wf[1], warn = FALSE)
  ## PER JOB, from the PARSED yaml. Two reasons this is not a union over the whole file.
  ## (1) A ref only counts as a declaration for the job that installs it. `suite` is the job
  ##     that runs `R CMD INSTALL .`, so an Import declared only in `oq` would satisfy a
  ##     whole-file check while the install still failed -- the exact failure class this
  ##     section exists to catch.
  ## (2) Reading the PARSED block means a comment that merely MENTIONS `any::something` cannot
  ##     enter the list. Both of today's near-misses came from grepping the file as text.
  .byjob <- lapply(yaml::read_yaml(.wf[1])$jobs, function(j)
    unlist(lapply(j$steps, function(st)
      if (!is.null(st$uses) && grepl("setup-r-dependencies", st$uses))
        strsplit(trimws(st$with$packages), "[[:space:],]+")[[1]])))
  .byjob <- Filter(length, .byjob)
  .refs  <- lapply(.byjob, function(t) sub("^any::", "", grep("^any::", t, value = TRUE)))
  ## `suite` is the installing job; fall back to the union if it is ever renamed.
  .inst  <- if (!is.null(.refs$suite)) .refs$suite else unique(unlist(.refs))
  .have <- unique(.inst)
  ## EVERY JOB THAT DECLARES PACKAGES, not just `suite`. listOfLibrary is the list the PIPELINE
  ## attaches, and `oq` is the job that actually runs the pipeline -- so a name missing there is
  ## resolved at run time against the runner's default repo, unpinned, and library()d into the
  ## session performing the analysis. That is the precise fault this section was written for after
  ## BiocManager and rstudioapi went missing, and fixing it in `suite` alone left the weekly run
  ## still doing it. Checking the union of both jobs would have hidden it just as thoroughly:
  ## per job is the only shape that can see a per-job gap.
  .want <- setdiff(.need, .base)
  .gaps <- lapply(names(.refs), function(j) setdiff(.want, .refs[[j]]))
  names(.gaps) <- names(.refs)
  chk(sprintf("every listOfLibrary package is declared in EVERY CI job that installs (%d checked, jobs: %s)",
              length(.want), paste(names(.refs), collapse = ", ")),
      all(vapply(.gaps, length, integer(1)) == 0L))
  for (j in names(.gaps))
    if (length(.gaps[[j]]))
      cat(sprintf("          job '%s' is missing: %s\n", j, paste(.gaps[[j]], collapse = ", ")))
  ## Two jobs, or the per-job check above is a single-job check wearing a loop. `oq` is
  ## conditional on schedule/dispatch, so it is easy to delete without noticing.
  chk("the workflow still has both the gates job and the OQ job declaring packages",
      length(.refs) >= 2L && !is.null(.refs$suite) && !is.null(.refs$oq))
  .miss <- setdiff(.want, .have)
  ## And the reverse, as a note only: a workflow may legitimately declare extras the script does not
  ## load directly (shiny, rmarkdown, magick, jsonlite are used by the app, the render and the
  ## gates), so this is information rather than a rule.
  .extra <- setdiff(.have, .need)
  if (length(.extra))
    cat("          note: declared in CI but not in listOfLibrary:", paste(.extra, collapse = ", "), "\n")

  ## AND EVERY TOKEN IN THAT BLOCK MUST BE A PACKAGE REF. `packages: |` is a YAML block scalar,
  ## so a `#` line inside it is literal text, not a comment, and the action splits the whole
  ## string on whitespace. Two explanatory lines placed in there became 29 bogus refs and pak
  ## aborted on `#` -- breaking CI on both runners while the check below still passed green,
  ## because it only ever grepped for `any::`. A list that cannot install anything is not a
  ## declaration, so assert the SHAPE of the block, not just its contents.
  .pkgblk <- unlist(.byjob, use.names = FALSE)
  .odd <- grep("^any::[A-Za-z]", .pkgblk, value = TRUE, invert = TRUE)
  chk(sprintf("every token in the CI packages block is a package ref (%d checked)", length(.pkgblk)),
      length(.odd) == 0L)
  if (length(.odd))
    cat("          not a ref:", paste(utils::head(.odd, 8), collapse = " "), "\n")
  ## AND THE PACKAGE'S OWN Imports, which are a DIFFERENT list. listOfLibrary is what the
  ## pipeline script attaches at run time; DESCRIPTION Imports is what `R CMD INSTALL` must
  ## resolve. The check above compares only the first, so when the suite job gained an
  ## `R CMD INSTALL .` step it could not see that six Imports were undeclared -- they arrived
  ## transitively on ubuntu, did not on macOS, and the v3.0 run failed there while passing on
  ## ubuntu. Two lists, two checks.
  .desc <- file.path(ACTA_REPO_ROOT, "DESCRIPTION")
  if (file.exists(.desc)) {
    .imp <- trimws(strsplit(read.dcf(.desc)[1, "Imports"], ",")[[1]])
    .imp <- .imp[nzchar(.imp)]
    .bse <- rownames(installed.packages(priority = "base"))
    .want <- sort(setdiff(.imp, .bse))
    .gap  <- setdiff(.want, .inst)
    chk(sprintf("every DESCRIPTION Import is declared in CI (%d checked)", length(.want)),
        length(.gap) == 0L)
    if (length(.gap)) cat("          missing from the workflow:", paste(.gap, collapse = ", "), "\n")
  }
}

## ---------------------------------------------------------------------------------------------
## A MISSING PANDOC MUST COST THE PDF AND NOTHING ELSE. The report .Rmd sources the pipeline
## script, so with report = TRUE the analysis happens inside the knit -- and rmarkdown::render()
## tests pandoc BEFORE knitting. On a machine without pandoc a full run therefore returned
## ok = FALSE, wrote_plots = FALSE, export_file = NA and left nothing on disk but the inputs: the
## whole analysis lost for want of a PDF, and the README's contract ("ok = TRUE with
## report_ok = FALSE means the export and plots are on disk and usable") false in that very case.
## run_acta() now checks pandoc up front and downgrades the request instead.
##
## Asserted on the SOURCE, because proving it end to end needs a machine without pandoc and this
## one has it -- the behaviour was measured by hand on a stripped PATH (ok = TRUE, export written,
## 11 stats rows, report_ok = FALSE naming pandoc). What is cheap to check every time is that the
## pre-check is still there, still ahead of the work, and still sets report_ok rather than NA.
cat("\n=== a missing pandoc costs the PDF, not the run ===\n")
local({
  src <- unlist(lapply(actaTestFunctionsFiles(vd), readLines, warn = FALSE))
  i0  <- which(grepl("^run_acta <- function", src))[1]
  i1  <- i0 - 1L + which(grepl("^  out <- list\\(", src[i0:length(src)]))[1]
  code <- src[i0:i1]
  code <- code[!grepl("^\\s*##", code)]
  iChk <- which(grepl("pandoc_available", code))[1]
  iRun <- which(grepl("rmarkdown::render|sys[.]source\\(script", code))[1]
  chk("run_acta checks pandoc before it renders or sources anything",
      !is.na(iChk) && !is.na(iRun) && iChk < iRun)
  chk("...and a missing pandoc downgrades the request rather than failing the run",
      any(grepl("^\\s*report <- FALSE\\s*$", code)))
  chk("...recording report_ok = FALSE, not NA, so a caller can tell it was asked for",
      any(grepl("report_ok <- FALSE", code, fixed = TRUE)))
  chk("...and naming the tool that is missing",
      any(grepl("pandoc was not found", code, fixed = TRUE)))
  ## The other half: with no engine at all LaTeX never runs, so there is no log to quote and the
  ## message stayed the bare "error in running command" -- which this codebase's own comment says
  ## identifies nothing. actaPdfEngine() is the thing that knows.
  chk("a failed render names a missing LaTeX engine as the cause",
      any(grepl("actaPdfEngine()$ok", code, fixed = TRUE)) &&
        any(grepl("no LaTeX engine found", code, fixed = TRUE)))
  eng <- tryCatch({
    .oldPath <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = .oldPath), add = TRUE)
    if (requireNamespace("tinytex", quietly = TRUE))
      try(assignInNamespace("is_tinytex", function(...) FALSE, ns = "tinytex"), silent = TRUE)
    Sys.setenv(PATH = "/nonexistent")
    h$actaPdfEngine()$ok
  }, error = function(e) NA)
  chk("actaPdfEngine() reports FALSE when no engine and no TinyTeX are reachable",
      identical(eng, FALSE))
})

## ---------------------------------------------------------------------------------------------
## THE PER-RUN VERSION RECORD MUST NAME EVERY PACKAGE THE RUN COULD HAVE USED.
## It parsed listOfLibrary out of the SCRIPT alone, while the dependency panel has always read
## the script AND the .Rmd -- and the .Rmd carries two names the script does not, ragg and
## sessioninfo, which are exactly the report's. So the panel reported on 23 packages while the
## traceability artefact for the same run listed 21, and the two it omitted belonged to the PDF
## an auditor is holding. DESCRIPTION's Imports are in now as well: those are what the namespace
## RESOLVES, and rmarkdown, tinytex, jsonlite, processx, xml2, magrittr, tibble and tidyselect are
## all used by code that computes or writes a number.
cat("\n=== the per-run version record covers the whole manifest ===\n")
local({
  pv <- h$actaPackageVersions(ACTA_VERSION_DIR)
  chk(sprintf("actaPackageVersions() returns a package/version frame (%d rows)", nrow(pv)),
      is.data.frame(pv) && all(c("package", "version") %in% names(pv)) && nrow(pv) > 0L)
  ## BOTH HALVES, EACH PROVED BY SOMETHING ONLY THAT HALF CAN SUPPLY.
  ## The first draft of this asserted ragg and sessioninfo -- the .Rmd's two -- and passed with
  ## the manifest parse REMOVED, because both are also DESCRIPTION Imports and arrived by the
  ## other route. Tenth unfalsifiable assertion in this codebase, same shape as the other nine:
  ## a string that is true for a reason other than the one under test.
  ##
  ## The manifest half needs a name that is in NO DESCRIPTION, so it is staged in: a sentinel
  ## added to the REPORT's listOfLibrary only. If the record is built from the script alone, or
  ## from Imports alone, the sentinel cannot appear.
  local({
    st <- file.path(tempdir(), "acta_manifest_probe"); unlink(st, recursive = TRUE)
    dir.create(st, recursive = TRUE, showWarnings = FALSE)
    for (f in c(list.files(ACTA_VERSION_DIR, pattern = "^ACTA_Script.*[.]R$", full.names = TRUE),
                list.files(ACTA_VERSION_DIR, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE)))
      file.copy(f, st)
    rmd <- list.files(st, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE)[1]
    ln  <- readLines(rmd, warn = FALSE)
    i   <- grep("^listOfLibrary[[:space:]]*<-", ln)[1]
    ln[i] <- sub('\\)$', ',"acta.sentinel.only.in.report")', ln[i])
    writeLines(ln, rmd)
    p2 <- h$actaPackageVersions(st)
    chk("the record reads the REPORT's manifest, not only the script's",
        "acta.sentinel.only.in.report" %in% p2$package)
    if (!("acta.sentinel.only.in.report" %in% p2$package))
      cat("         the sentinel in the .Rmd's listOfLibrary never reached the record\n")
    ## An uninstallable name must still be RECORDED, with NA for its version -- "absent" is
    ## itself a fact about the run, and dropping the row would hide it.
    ## PRESENT **AND** NA. Matching on version alone passes when the row is absent entirely --
    ## match() returns NA, and indexing by NA yields NA -- so this said "ok" under the very
    ## mutation the assertion above was written to catch.
    .j <- match("acta.sentinel.only.in.report", p2$package)
    chk("...and records a package it cannot find as NA rather than dropping the row",
        !is.na(.j) && is.na(p2$version[.j]))
    unlink(st, recursive = TRUE)
  })
  ## The Imports half needs a name in NO listOfLibrary. rmarkdown renders the report and appears
  ## in neither manifest.
  lol <- h$actaScriptPackages(ACTA_VERSION_DIR)
  descOnly <- setdiff(c("rmarkdown", "tinytex", "processx", "magrittr"), lol)
  chk(sprintf("...and DESCRIPTION Imports that no manifest mentions (%s)",
              paste(descOnly, collapse = ", ")),
      length(descOnly) > 0L && all(descOnly %in% pv$package))
  ## The two names that motivated this, asserted for the record even though each is now reachable
  ## by both routes -- BiocManager and rstudioapi moved into Imports in the same release.
  chk("...so ragg and sessioninfo, the report's own two, are in the record",
      all(c("ragg", "sessioninfo") %in% pv$package))
  chk("...and ACTA itself", "ACTA" %in% pv$package)
  ## Versions, not just names: a row whose version is NA for an installed package means the
  ## lookup silently failed and the record is decoration.
  inst <- pv$package[pv$package %in% rownames(utils::installed.packages())]
  chk(sprintf("every installed package in the record carries a version (%d checked)", length(inst)),
      length(inst) > 0L && !any(is.na(pv$version[match(inst, pv$package)])))
  ## purrr must NOT appear: it was removed as a dependency rather than declared.
  chk("purrr is absent, because it is no longer a dependency", !("purrr" %in% pv$package))
  ## AND WIDENING MUST NOT INVENT DRIFT. actaVersionDrift() merges on the BASELINE, so a package
  ## the old validated_packages.tsv never had cannot be reported as having moved -- asserted,
  ## because the alternative is every existing baseline suddenly reporting 19 changes.
  base <- data.frame(package = c("dplyr", "ggplot2"),
                     version = pv$version[match(c("dplyr", "ggplot2"), pv$package)],
                     stringsAsFactors = FALSE)
  chk("a baseline that predates the wider record reports no drift",
      length(h$actaVersionDrift(pv, base)) == 0L)
  ## ...and a real move is still caught, or the line above passes by being blind.
  base2 <- base; base2$version[1] <- "0.0.1"
  d <- h$actaVersionDrift(pv, base2)
  chk("...while a package that really moved still is", length(d) == 1L && grepl("^dplyr ", d))
})

## ---------------------------------------------------------------------------------------------
## A REPORT THAT WAS NEVER ATTEMPTED IS NOT A REPORT THAT FAILED.
## run_acta() checks pandoc before doing any work and downgrades the request, which leaves
## report_ok = FALSE with nothing having been tried -- and actaRunArtefacts() then told the
## operator "render failed", pointing them at LaTeX on a machine whose LaTeX was fine. Behavioural
## on both branches: the same helper, two inputs differing only in report_skipped.
cat("\n=== a skipped report and a failed render are different events ===\n")
local({
  base <- list(ok = TRUE, report_ok = FALSE, version_dir = tempdir(), wrote_plots = FALSE,
               plots_dir = NA_character_, export_file = NA_character_)
  sk <- h$actaRunArtefacts(c(base, list(report_skipped = "pandoc not found")))
  chk("a report skipped for a missing pandoc says it was skipped, and names pandoc",
      grepl("skipped", sk$report$skipped, fixed = TRUE) &&
        grepl("pandoc", sk$report$skipped, fixed = TRUE))
  chk("...and does not say the render failed",
      !grepl("render failed", sk$report$skipped, fixed = TRUE))
  fl <- h$actaRunArtefacts(c(base, list(report_skipped = NA_character_)))
  chk("a render that was attempted and failed still says render failed",
      identical(fl$report$skipped, "render failed"))
  ## The two earlier branches must be untouched: an incomplete analysis blames the analysis, and
  ## a report nobody asked for is not a failure of any kind.
  an <- h$actaRunArtefacts(c(base[setdiff(names(base), "ok")],
                             list(ok = FALSE, report_skipped = NA_character_)))
  chk("an analysis that never completed still blames the analysis",
      identical(an$report$skipped, "analysis did not complete"))
  nr <- h$actaRunArtefacts(c(base[setdiff(names(base), "report_ok")],
                             list(report_ok = NA, report_skipped = NA_character_)))
  chk("a report that was not requested is still 'not requested'",
      identical(nr$report$skipped, "not requested"))
  ## A res list from an OLDER run_acta has no report_skipped field at all. It must not error.
  old <- h$actaRunArtefacts(base)
  chk("a result list without the new field degrades to 'render failed', not an error",
      identical(old$report$skipped, "render failed"))
})

## ---------------------------------------------------------------------------------------------
## THE PANDOC CHECK MUST BE ASKED OF THE PATH THE RENDER WILL INHERIT.
## actaRepairPath() puts the standard system directories back when a machine's PATH has lost them
## -- the fault that prompted it was a PATH reading `/usr/local/bin:/user/bin`, with /usr/bin
## typo'd away. The pipeline script and the app both call it, but the SCRIPT runs after run_acta()
## has already decided about pandoc, and `library(ACTA); run_acta()` from a console never goes
## through the app at all. So the pre-check ran against the broken PATH, skipped the report as
## unavailable, and the run then repaired the PATH and could have rendered perfectly well.
cat("\n=== the pandoc pre-check sees the repaired PATH ===\n")
local({
  if (.Platform$OS.type == "windows") {
    cat("  [skip] PATH repair is a POSIX-only concern\n")
  } else {
    ## BEHAVIOURAL: break PATH down to nothing, then let the real helper repair it and see
    ## whether a tool living in a standard directory becomes findable again.
    .old <- Sys.getenv("PATH")
    on.exit(Sys.setenv(PATH = .old), add = TRUE)
    probe <- Sys.which("sh")                      # in /bin on every POSIX machine
    Sys.setenv(PATH = "/nonexistent")
    chk("with a broken PATH a standard tool is invisible", !nzchar(Sys.which("sh")))
    added <- h$actaRepairPath(quiet = TRUE)
    chk("actaRepairPath() puts the standard directories back",
        length(added) > 0L && nzchar(Sys.which("sh")))
    ## AND IN THE RIGHT ORDER, FROM A PATH THAT ALREADY HAS /usr/bin. /usr/local/bin and
    ## /opt/homebrew/bin are routinely user- or admin-group-writable -- on the machine this was
    ## written on both are owned by the logged-in user -- so ahead of /usr/bin they let any local
    ## admin shadow every tool a run shells out to: pandoc, kpsewhich, pdflatex, and the git that
    ## stamps the provenance record. They are appended for that reason.
    ##
    ## THE STARTING PATH IS THE WHOLE TEST. Asserted from the wiped PATH above, this could not
    ## fail: with /usr/bin absent it goes into the PREPENDED group either way, so it is first
    ## whichever end the opt dirs land on, and reverting the fix passed. The order is only
    ## observable from a PATH that already holds /usr/bin and lacks the other two -- which is the
    ## real machine state, not the contrived one.
    Sys.setenv(PATH = "/usr/bin:/bin")
    invisible(h$actaRepairPath(quiet = TRUE))
    .p  <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
    .ub <- match("/usr/bin", .p)
    .chkd <- 0L
    for (.d in c("/usr/local/bin", "/opt/homebrew/bin")) {
      .i <- match(.d, .p)
      if (!is.na(.i) && !is.na(.ub)) {
        .chkd <- .chkd + 1L
        chk(sprintf("...with %s after /usr/bin, never able to shadow it (%d vs %d)", .d, .i, .ub),
            .i > .ub)
      }
    }
    ## Neither directory exists on this machine -> nothing was asserted. Say so rather than
    ## reporting a silent all-clear for a check that did not run.
    if (!.chkd) note("neither /usr/local/bin nor /opt/homebrew/bin is present, so the ordering was not checked")
    Sys.setenv(PATH = .old)
  }
  ## AND THE ORDER, which is the actual defect: the repair has to happen BEFORE the pre-check
  ## reads PATH, inside run_acta itself. Matched on the two calls' positions in the source rather
  ## than on their presence, because both were already present -- in the wrong order, in
  ## different files.
  src  <- unlist(lapply(actaTestFunctionsFiles(vd), readLines, warn = FALSE))
  i0   <- which(grepl("^run_acta <- function", src))[1]
  i1   <- i0 - 1L + which(grepl("^  out <- list\\(", src[i0:length(src)]))[1]
  code <- src[i0:i1]; code <- code[!grepl("^\\s*##", code)]
  iRep <- which(grepl("actaRepairPath", code))[1]
  iPan <- which(grepl("Sys.which\\(\"pandoc\"\\)", code))[1]
  chk("run_acta() repairs PATH itself, rather than trusting its caller to have done it",
      !is.na(iRep))
  chk("...before it asks whether pandoc is present, not after",
      !is.na(iRep) && !is.na(iPan) && iRep < iPan)
})

## ---------------------------------------------------------------------------------------------
## THE NO-INSTALL CLONE PATH MUST REPRODUCE WHAT THE NAMESPACE WOULD HAVE IMPORTED.
## Sourcing the helpers into the global environment gives them none of NAMESPACE's imports. The
## four wholesale import()s were already handled -- a bare S4 generic that also exists in base
## resolves to the base one and never dispatches, which is how colnames(flowSet) returned 0
## channels instead of 13, silently. The 23 importFrom names were not: `%>%`, mutate, aes and the
## rest are not in base, so they fail loudly instead of quietly, but they still fail, and they
## fail wherever the app first reaches a helper that uses one.
## The app derives the list by PARSING NAMESPACE, so adding an importFrom cannot leave this path
## behind. This test EXECUTES the app's own block rather than re-implementing its regexes.
cat("\n=== the app's clone path attaches what NAMESPACE imports ===\n")
local({
  app <- file.path(ACTA_VERSION_DIR, "ACTA_App.R")
  ln  <- readLines(app, warn = FALSE)
  i0  <- grep("<<ACTA_NS_IMPORTS>>", ln, fixed = TRUE)
  i1  <- grep("<</ACTA_NS_IMPORTS>>", ln, fixed = TRUE)
  chk("the app marks the block that derives its attach list", length(i0) == 1L && length(i1) == 1L)
  if (length(i0) == 1L && length(i1) == 1L) {
    e <- new.env()
    assign("R_DIR", file.path(ACTA_REPO_ROOT, "R"), envir = e)
    res <- try(eval(parse(text = paste(ln[(i0 + 1L):(i1 - 1L)], collapse = "\n")), envir = e),
               silent = TRUE)
    got <- if (inherits(res, "try-error")) character(0) else get(".nsPkgs", envir = e)
    if (inherits(res, "try-error"))
      cat("         the block did not run:", conditionMessage(attr(res, "condition")), "\n")
    ## Read NAMESPACE independently -- with R's own parser, not the app's regexes, so agreement
    ## between them means something.
    nsd  <- tryCatch(parse(file.path(ACTA_REPO_ROOT, "NAMESPACE")), error = function(e) NULL)
    dcls <- if (is.null(nsd)) list() else lapply(as.list(nsd), as.list)
    wholesale <- unlist(lapply(dcls, function(d)
      if (identical(as.character(d[[1]]), "import")) as.character(d[[2]]) else NULL))
    fromPkgs  <- unlist(lapply(dcls, function(d)
      if (identical(as.character(d[[1]]), "importFrom")) as.character(d[[2]]) else NULL))
    shipped <- setdiff(unique(c(wholesale, fromPkgs)),
                       c("stats", "utils", "methods", "grDevices", "graphics", "datasets",
                         "base", "grid", "tools"))
    chk(sprintf("the app's list is exactly what NAMESPACE imports (%d packages)", length(shipped)),
        setequal(got, shipped) && length(shipped) > 0L)
    if (!setequal(got, shipped))
      cat("         app says:", paste(sort(got), collapse = ", "),
          "\n         NAMESPACE says:", paste(sort(shipped), collapse = ", "), "\n")
    ## The four generics packages specifically -- the silent-wrongness half.
    chk("...covering the four wholesale import()s, without which colnames(flowSet) returns 0",
        all(c("flowCore", "flowWorkspace", "ggcyto", "openCyto") %in% got))
    ## ...and the importFrom half, which is what this release added.
    chk("...and the importFrom packages, which it used to leave out",
        all(c("dplyr", "ggplot2", "magrittr", "tibble", "tidyr", "tidyselect") %in% got))
    ## flowMeans must stay OUT: it pulls tcltk, which needs XQuartz, and attaching it here would
    ## reintroduce the headless-load failure on the one path that has no package to protect it.
    chk("...but never flowMeans, which would need XQuartz", !("flowMeans" %in% got))
  }
})

## ---------------------------------------------------------------------------------------------
## actaOQRun() MUST FIND THE PIPELINE WHERE run_acta() FINDS IT.
## It walked UP from the case looking for an ACTA_Script*.R, which in the package layout never
## succeeds: the cases ship at inst/extdata/oq_small/OQ_TestN and the pipeline at inst/pipeline,
## so they are SIBLINGS. Every caller had to pass code_dir by hand -- this project's own CI does,
## with a comment explaining why -- and anyone driving the OQ from an install without it got
## "no folder above ... contains an ACTA_Script*.R" for a perfectly sound setup.
cat("\n=== the OQ finds the pipeline in the installed package ===\n")
local({
  ## A case folder with no ACTA_Script*.R anywhere above it, so only the new fallback can answer.
  cs <- file.path(tempdir(), "acta_oq_resolve", "OQ_TestX")
  unlink(dirname(cs), recursive = TRUE); dir.create(cs, recursive = TRUE, showWarnings = FALSE)
  seen <- character(0)
  ## ABORT FROM THE PROGRESS CALLBACK once the resolution has been announced. The alternative is
  ## letting actaOQRun go on to run an analysis against an empty folder, which costs the gate
  ## minutes to learn something it already knows.
  res <- suppressWarnings(try(h$actaOQRun(cs, quiet = TRUE, progress = function(...) {
    seen <<- c(seen, paste0(..., collapse = " "))
    if (any(grepl("installed package", seen))) stop("ACTA_TEST_ABORT")
  }), silent = TRUE))
  if (requireNamespace("ACTA", quietly = TRUE) &&
      nzchar(system.file("pipeline", package = "ACTA"))) {
    chk("with no script above the case, the OQ falls back to the installed package",
        any(grepl("using the pipeline from the installed package", seen)))
    if (!any(grepl("installed package", seen)))
      cat("         it said:", paste(utils::head(seen, 4), collapse = " | "),
          if (inherits(res, "try-error")) paste("| error:", conditionMessage(attr(res, "condition"))) else "",
          "\n")
  } else {
    ## No install to fall back TO, so the other half of the contract applies: the error has to
    ## name BOTH places it looked. "no folder above" alone read as a case-layout problem on a
    ## machine where the real answer was "install ACTA".
    chk("...and with nothing installed either, the error names both places it looked",
        inherits(res, "try-error") &&
          grepl("none in the installed package", conditionMessage(attr(res, "condition")),
                fixed = TRUE))
  }
  ## The OQ stamp's code_dir line is asserted in the provenance section below, in the form it
  ## takes NOW -- .oqAbbrev(.cd). It was pinned here against the bare .cd form, which the
  ## abbreviation fix then made stale and turned into a failing gate on a correct tree. Two
  ## assertions over one line is one too many, and the stricter one is the one that survives.
  unlink(dirname(cs), recursive = TRUE)
})

## ---------------------------------------------------------------------------------------------
## EVERY RUN WRITES ITS PROVENANCE -- ASSERTED BY RUNNING THE WRITE, NOT BY GREPPING FOR IT.
## The first gate over this searched ACTA_Functions.R for "acta_version.tsv" and was satisfied by
## the OQ's own three copies of that string, so it reported ok with the entire new block deleted
## from run_acta(), and with wrote_provenance hardcoded TRUE. Thirteenth assertion of that shape
## in this codebase, and the one guarding the release's headline change.
##
## The block is marked in the source and EXECUTED here against a temp folder. Calling run_acta()
## outright would mean a full analysis -- a minute of FCS gating to test six writeLines -- so the
## block is driven directly with the three things it reads: code_dir, version_dir and pick().
cat("\n=== every run records its own provenance ===\n")
local({
  src <- unlist(lapply(actaTestFunctionsFiles(vd), readLines, warn = FALSE))
  i0  <- grep("<<ACTA_PROVENANCE>>", src, fixed = TRUE)
  i1  <- grep("<</ACTA_PROVENANCE>>", src, fixed = TRUE)
  chk("run_acta() marks the block that writes the run record",
      length(i0) == 1L && length(i1) == 1L && i1 > i0)
  if (!(length(i0) == 1L && length(i1) == 1L && i1 > i0)) return(invisible(NULL))

  dest <- file.path(tempdir(), "acta_prov_gate"); unlink(dest, recursive = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  e <- new.env(parent = environment(h$run_acta))   # so actaPackageVersions resolves as it does live
  ## code_dir is deliberately UNDER THE HOME, so the abbreviation has something to do and the
  ## written value proves it ran. Stubbing .abbrev here, as the first version did, let the
  ## .abbrev() call be dropped from the code_dir line with this gate still green.
  .home <- normalizePath("~", mustWork = FALSE)
  assign("code_dir",    file.path(.home, "clone", "inst", "pipeline"), envir = e)
  assign("version_dir", dest,             envir = e)
  assign("analysis_ok", TRUE,             envir = e)
  assign("pick", function(nm) switch(nm, ScriptVersion = "3_0", ACTA_SEED = 123L, NULL), envir = e)
  eval(parse(text = paste(src[(i0 + 1L):(i1 - 1L)], collapse = "\n")), envir = e)

  chk("...and it reports having written them", isTRUE(get(".prov", envir = e)))
  av <- file.path(dest, "acta_version.tsv"); pvf <- file.path(dest, "package_versions.tsv")
  chk("acta_version.tsv is written beside the run's outputs", file.exists(av))
  chk("package_versions.tsv too", file.exists(pvf))
  if (file.exists(av)) {
    kv <- do.call(rbind, strsplit(readLines(av), "\t", fixed = TRUE))
    keys <- kv[, 1]
    ## THE THREE THE README PROMISES, each by name. "Every run records its own package versions,
    ## seed, and script version" was false for two of the three before 3.0.12, so each is asserted
    ## rather than trusting that a file exists.
    for (k in c("seed", "acta_script_version", "code_dir", "acta_package_version",
                "analysis_complete", "r_version"))
      chk(sprintf("...recording %s", k), k %in% keys)
    chk("...and the seed is the value the script actually set, not a placeholder",
        identical(kv[match("seed", keys), 2], "123"))
    chk("...and the script version it ran", identical(kv[match("acta_script_version", keys), 2], "3_0"))
    ## THE VALUE AS WRITTEN. This file travels -- attached to a validation package, emailed to a
    ## CRO -- and on a managed machine the home directory names the operator's account, which is
    ## their email address. Asserted on the recorded line, not on the helper in isolation.
    chk("...with the operator's home abbreviated out of the recorded path",
        identical(kv[match("code_dir", keys), 2], file.path("~", "clone", "inst", "pipeline")))
  }
  if (file.exists(pvf)) {
    t <- utils::read.delim(pvf, stringsAsFactors = FALSE)
    chk(sprintf("...and every package the run could have used (%d rows)", nrow(t)),
        nrow(t) > 30L && all(c("package", "version") %in% names(t)))
    chk("...including the report's own two", all(c("ragg", "sessioninfo") %in% t$package))
  }
  unlink(dest, recursive = TRUE)

  ## And the converse: a path OUTSIDE the home -- an installed package under the R library, which
  ## is the common case -- must be recorded whole, or the field stops identifying the code.
  chk("...while a path outside it, such as an R library, is left alone",
      identical(get(".abbrev", envir = e)("/Library/Frameworks/R.framework/Resources/library/ACTA/pipeline"),
                "/Library/Frameworks/R.framework/Resources/library/ACTA/pipeline"))
})

## ---------------------------------------------------------------------------------------------
## THE TWO SIDES THE CONSUMER TEST ABOVE CANNOT REACH. actaRunArtefacts() is asserted
## behaviourally further up, but nothing proved that run_acta() ever SETS report_skipped, or that
## the OQ collects the provenance pair out of the case root -- both survived deletion. Source
## assertions, scoped to the function that must contain them and matched whole, so deleting or
## rewriting the line fails them. Flagged as the weaker kind: they prove the code is present, not
## that it fires.
local({
  src  <- unlist(lapply(actaTestFunctionsFiles(vd), readLines, warn = FALSE))
  ## THE WHOLE FUNCTION, to the next top-level definition. The pandoc assertions further up stop
  ## at `out <- list(` because what they check precedes it -- but analysis_error is a FIELD of
  ## that list, so the same bound excluded the line under test and the assertion failed for a
  ## correct tree. Bounded by the next top-level `name <- function`, which is what actually ends
  ## run_acta.
  i0   <- which(grepl("^run_acta <- function", src))[1]
  .nxt <- which(grepl("^[A-Za-z._][A-Za-z0-9._]* <- function", src))
  i1   <- { k <- .nxt[.nxt > i0]; if (length(k)) k[1] - 1L else length(src) }
  ra   <- src[i0:i1]
  ## wrote_provenance is a README-documented assurance field, so it must carry the WRITE's own
  ## result. Hardcoding it TRUE passed the block-level gate above, which reads .prov inside the
  ## block and never sees the returned list.
  chk("wrote_provenance reports what the write actually did, not a constant",
      any(grepl("wrote_provenance = isTRUE(.prov),", ra, fixed = TRUE)))
  chk("run_acta() sets report_skipped when the pandoc pre-check downgrades the request",
      any(grepl('report_skipped <- "pandoc not found"', ra, fixed = TRUE)))
  chk("...and keeps that text out of analysis_error",
      any(grepl("analysis_error = if (!analysis_ok && is.na(report_skipped) && !is.na(report_error))",
                ra, fixed = TRUE)))
  j0 <- which(grepl("^actaOQRun <- function", src))[1]
  j1 <- which(grepl("^actaOQFinish <- function", src))[1]
  oq <- if (!is.na(j0) && !is.na(j1) && j1 > j0) src[j0:j1] else src
  chk("the OQ collects the provenance pair into Outputs/, leaving the case root clean",
      any(grepl('for (f in file.path(oq_dir, c("acta_version.tsv", "package_versions.tsv")))',
                oq, fixed = TRUE)))
  chk("...and clears an earlier run's copies before it starts",
      any(grepl('Filter(file.exists, file.path(oq_dir, c("acta_version.tsv", "package_versions.tsv")))',
                oq, fixed = TRUE)))
  ## The OQ's own stamp OVERWRITES the collected copy, so it has to carry the same fields. It was
  ## dropping the seed -- which the README promises every run records -- and writing code_dir
  ## unabbreviated a few lines after run_acta had abbreviated it, in the one folder a validation
  ## package is actually made of.
  chk("the OQ stamp records the seed, like run_acta's does",
      any(grepl('sprintf("seed\\t%s", .seed)', oq, fixed = TRUE)))
  chk("...and abbreviates the home out of its code_dir too",
      any(grepl('sprintf("code_dir\\t%s", .oqAbbrev(.cd))', oq, fixed = TRUE)))
})

cat(if (ok) "\nALL APP-SUPPORT TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
