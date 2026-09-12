## What a user who ran ONLY `remotes::install_github(...)` can reach -- no clone, no working folder.
##
## WHY THIS GATE EXISTS. v3.0.2 shipped an install path that did not work, and every gate we had
## reported clean, because every gate runs on a machine where the flow stack is ALREADY installed.
## The defect: `remotes` consults the Bioconductor repositories only when the DESCRIPTION declares
## `biocViews`. The whole test, in remotes 2.5.0, is
##
##     is_bioconductor <- function(x) !is.null(x$biocviews)
##
## and 3.0.2 had no such field. So remotes resolved the 82 CRAN dependencies, could not find
## flowCore, flowWorkspace, openCyto, ggcyto or flowMeans, MESSAGED "Skipping 5 packages not
## available:" rather than erroring, and `R CMD INSTALL` then printed `* DONE`. The failure landed
## one step later, at `library(ACTA)`, as "package 'flowCore' required by 'ACTA' could not be
## found" -- pointing at load, with nothing pointing at the step that caused it.
##
## The second half of the same problem is documentation-shaped: the README tells people to copy the
## blank workbook "out of Template/", which is a clone-relative path. It works only because the
## template also ships INSIDE the package. That is a property of the built tree, so it is asserted
## here rather than trusted.
##
## Rscript <this> [version_dir]   -- defaults to the version folder this file lives in.
## ANCHOR ON THE PACKAGE ROOT, not on actaTestVersionDir(). That helper returns the CODE directory,
## which in the package layout is inst/pipeline/ -- so the first version of this gate looked for
## inst/pipeline/DESCRIPTION and reported "version_dir: pipeline". Everything asserted below is a
## property of the package ROOT, so ACTA_REPO_ROOT is the subject.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- ACTA_REPO_ROOT
cat("package_root:", basename(vd), "\n")

## This gate is about a PACKAGE install, so it has no subject in the flat release layout. Say so
## through the whole-file banner run_all.R keys on, rather than failing a healthy checkout.
if (!file.exists(file.path(vd, "DESCRIPTION"))) {
  ## basename, not the full path: package_root is already printed above, and on a dev machine the
  ## absolute path carries the OneDrive library and the maintainer's corporate email.
  cat("\nINSTALL PATH TESTS SKIPPED -- no DESCRIPTION in",
      basename(vd), "(flat layout, not a package)\n")
  quit(status = 0L)
}
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }
note <- function(what) cat(sprintf("  [--] %s\n", what))

## ---- 1. the DESCRIPTION field that decides whether remotes can see Bioconductor at all ---------
dfile <- file.path(vd, "DESCRIPTION")
d <- read.dcf(dfile)
chk("DESCRIPTION declares a Version", "Version" %in% colnames(d) && nzchar(d[1, "Version"]))
bv <- if ("biocViews" %in% colnames(d)) d[1, "biocViews"] else NA_character_
chk("DESCRIPTION declares biocViews", !is.na(bv) && nzchar(trimws(bv)))
if (!is.na(bv)) cat("         biocViews:", trimws(gsub("\\s+", " ", bv)), "\n")

## Assert against remotes ITSELF where we can, rather than against our belief about it. remotes
## lowercases the field names in its own loader, so a hand-built list gives the wrong answer here --
## which is exactly the mistake that made this look broken after it had been fixed.
if (requireNamespace("remotes", quietly = TRUE)) {
  chk("remotes:::is_bioconductor() agrees, so the Bioc repos get added",
      isTRUE(tryCatch(remotes:::is_bioconductor(remotes:::load_pkg_description(vd)),
                      error = function(e) FALSE)))
} else {
  note("remotes is not installed -- using its rule directly instead of calling it")
  dl <- as.list(d[1, ]); names(dl) <- tolower(names(dl))
  chk("the field survives remotes' own lowercasing rule", !is.null(dl$biocviews))
}

## biocViews is only load-bearing while we actually depend on Bioconductor. If these leave Imports,
## this whole gate is theatre and should be deleted rather than kept passing.
imp <- if ("Imports" %in% colnames(d))
  trimws(gsub("\\(.*?\\)", "", strsplit(d[1, "Imports"], ",")[[1]])) else character(0)
BIOC_ONLY <- c("flowCore", "flowWorkspace", "openCyto", "ggcyto", "flowMeans")
chk(sprintf("the Bioconductor-only Imports are still declared (%s)", paste(BIOC_ONLY, collapse = ", ")),
    all(BIOC_ONLY %in% imp))

## ---- 2. what has to be INSIDE the install, because there is no clone to fall back on -----------
pl <- file.path(vd, "inst", "pipeline")
chk("inst/pipeline/ exists", dir.exists(pl))
for (pat in c("^ACTA_Script_",  "^ACTA_Report_", "^ACTA_Dashboard_", "^ACTA_App[.]R$"))
  chk(sprintf("inst/pipeline/ ships %s", pat),
      length(dir(pl, pattern = pat)) == 1L)

tpl <- file.path(pl, "Template")
chk("inst/pipeline/Template/ exists", dir.exists(tpl))
## EXACTLY one, twice over: the README's copy snippet passes dir() straight to file.copy(), so a
## second TEMPLATE would silently copy both; and the dashboard generator requires exactly one
## *dashboard_template*.html in Template/ and errors on two.
chk("exactly one TEMPLATE_*.xlsx ships inside the package",
    length(dir(tpl, pattern = "^TEMPLATE.*[.]xlsx$")) == 1L)
chk("exactly one *dashboard_template*.html sits in Template/",
    length(dir(tpl, pattern = "dashboard_template.*[.]html$")) == 1L)

## The README tells a package-only user to launch the app with acta_app(). That is the ONLY
## invocation that works from an installed library -- shiny::runApp() on the app file setwd()s to
## the app directory before sourcing it, so the app would take the library as the working folder.
## If the export ever goes, the documented route breaks with "could not find function".
nsf <- file.path(vd, "NAMESPACE")
chk("NAMESPACE exports acta_app, the documented way to launch the app from an install",
    file.exists(nsf) && any(grepl('^[[:space:]]*export\\("?acta_app"?\\)', readLines(nsf, warn = FALSE))))

## THE DESKTOP LAUNCHERS MUST NAME A FILE THAT EXISTS. They are .Rbuildignore'd, so they are not
## in the package -- but they ARE in the public repo, and they are how a clone user starts the app.
## Both did `cd` to the repo root and then runApp("ACTA_App.R"), which was right until 3.0 moved
## the app to inst/pipeline/. Double-clicking printed an error and stopped, through four releases,
## because nothing here looked at them.
`%s0%` <- function(a, b) paste0(a, b)
for (lf in c("ACTA App.command", "ACTA App.bat")) {
  f <- file.path(vd, lf)
  if (!file.exists(f)) next
  ## CODE ONLY. Reading the comments too made this fail-open: a launcher whose comment mentioned
  ## inst/pipeline/ACTA_App.R while its code still named the old path would have passed.
  txt <- readLines(f, warn = FALSE)
  ## TO END OF LINE, not whole lines only. Dropping comment LINES left a trailing comment intact,
  ## and because the check is ANY-of, `APP="wrong/place/ACTA_App.R"  # real one is
  ## inst/pipeline/ACTA_App.R` passed -- the exact fail-open the previous fix claimed to close.
  ## READ THE ASSIGNED VALUE, not any mention of the filename. Comment-stripping was the wrong
  ## instrument twice over: the first version read whole files, the second stripped only whole
  ## comment LINES, and neither addressed the real weakness -- the check is ANY-of, and each
  ## launcher names ACTA_App.R on three or four lines (the assignment, an exists probe, an error
  ## message). Breaking the ONE line that matters left the others satisfying it. So parse the
  ## assignment: `APP="..."` in sh, `set "APP=..."` in cmd, including the flat-layout fallback.
  ## Nothing here depends on recognising a comment, which is why it is no longer trying to.
  asg <- c(sub('^[[:space:]]*APP=[\"\']?([^\"\'#]*)[\"\']?.*$', "\\1",
               grep('^[[:space:]]*APP=', txt, value = TRUE)),
           sub('^[[:space:]]*\\[ -f "\\$APP" \\] \\|\\| APP=[\"\']?([^\"\'#]*)[\"\']?.*$', "\\1",
               grep('\\|\\| APP=', txt, value = TRUE)),
           sub('^[[:space:]]*set[[:space:]]+"APP=([^"]*)".*$', "\\1",
               grep('^[[:space:]]*set[[:space:]]+"APP=', txt, value = TRUE, ignore.case = TRUE)))
  asg <- unique(trimws(gsub("\\\\", "/", asg[nzchar(trimws(asg))])))
  hit <- asg[file.exists(file.path(vd, asg))]
  chk(sprintf("%s assigns an app path that exists (%s)", lf,
              if (length(hit)) paste(hit, collapse = ", ")
              else paste("none of:", paste(asg, collapse = ", "))),
      length(asg) > 0L && length(hit) > 0L)
  chk(sprintf("%s points the app at the repo root via ACTA_WORK_DIR", lf),
      any(grepl("ACTA_WORK_DIR", txt, fixed = TRUE)))
}

## SETUP.R MUST FIND THE PIPELINE FROM THE REPO ROOT. It searched its own folder, the working
## directory, and one level below -- correct for the flat release, where ACTA_Script_*.R sat at
## the root. 3.0 moved it to inst/pipeline/, two levels down, so both documented clone commands
## (`Rscript Setup.R` and `Setup.bat`, which just runs it) aborted with "cannot find the ACTA
## folder" and advice pointing back at the folder the user was already in. Four releases, and no
## gate saw it because nothing here referenced Setup.R at all.
## Run in a STAGED COPY with ACTA_SETUP_RESOLVE_ONLY, so this costs a second and installs nothing.
sf <- file.path(vd, "Setup.R")
if (!file.exists(sf)) note("no Setup.R in this layout") else local({
  st <- file.path(tempdir(), "acta_setup_gate"); unlink(st, recursive = TRUE)
  dir.create(st, recursive = TRUE, showWarnings = FALSE)
  file.copy(sf, st)
  for (d in c(file.path("inst", "pipeline"), "R")) {
    dir.create(file.path(st, d), recursive = TRUE, showWarnings = FALSE)
    src <- file.path(vd, d)
    if (dir.exists(src)) file.copy(list.files(src, full.names = TRUE), file.path(st, d))
  }
  out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                                  c("--vanilla", shQuote(file.path(st, "Setup.R"))),
                                  stdout = TRUE, stderr = TRUE,
                                  env = "ACTA_SETUP_RESOLVE_ONLY=1"))
  mk  <- grep("^ACTA_SETUP_RESOLVED:", out, value = TRUE)
  chk("Setup.R resolves a pipeline folder when run from the repo root", length(mk) == 1L)
  if (length(mk) == 1L) {
    got <- normalizePath(trimws(sub("^ACTA_SETUP_RESOLVED:", "", mk)), mustWork = FALSE)
    want <- normalizePath(file.path(st, "inst", "pipeline"), mustWork = FALSE)
    chk(sprintf("...and it is inst/pipeline, not the root (%s)", basename(got)),
        identical(got, want))
  } else {
    cat("         Setup.R said:", paste(utils::head(out, 6), collapse = " | "), "\n")
  }
  unlink(st, recursive = TRUE)
})

## SETUP.R MUST INSTALL ACTA ITSELF. The pipeline loads its helper library from the installed
## package -- inst/pipeline/ carries no ACTA_Function*.R by design -- so a clone with only the
## dependencies could open the app and never finish a run. The README used to tell people to run
## `R CMD INSTALL .` by hand, which is a step nobody should have to be told about.
## Checked on the SOURCE plus the resolve-only probe: actually installing here would write into
## the developer's library, which a gate must not do.
if (file.exists(sf)) local({
  ln <- readLines(sf, warn = FALSE)
  code <- ln[!grepl("^[[:space:]]*#", ln)]
  chk("Setup.R finds the package root by looking for a DESCRIPTION that says ACTA",
      any(grepl('read.dcf(f, "Package")', code, fixed = TRUE)))
  chk("...and installs from that directory, the way R CMD INSTALL . does",
      any(grepl('install.packages(pkg_root, repos = NULL, type = "source")', code, fixed = TRUE)))
  chk("...reports whether it worked rather than assuming",
      any(grepl("pkg_ok <- have(\"ACTA\")", code, fixed = TRUE)))
  ## MATCH THE PARSER AND THE GUARD, not a string that also appears in a message. The first
  ## version of the --no-package check passed on the literal inside
  ## cat("skipping the ACTA package install (--no-package)") -- delete the flag from the parser
  ## and the gate stayed green while the flag silently stopped working. And the NOT-READY check
  ## was a shape test: rewriting the verdict as `... || isTRUE(pkg_ok)`, which makes a FAILED
  ## install print READY, still matched. Sixth and seventh unfalsifiable assertions this cycle.
  chk("...and a failed install makes Setup.R say NOT READY",
      any(grepl("acta_bad <- !is.na(pkg_root) && !(isTRUE(pkg_ok) || (skip_pkg && have(\"ACTA\")))",
                code, fixed = TRUE)) &&
        any(grepl("ok <- !length(badApp) && !length(badAna) && !acta_bad", code, fixed = TRUE)))
  ## BEHAVIOURAL, because every textual form of this check leaked. Matching "--no-package" passed
  ## on the literal inside the skip MESSAGE; matching the guard expression passed because the same
  ## expression also appears in the summary line, so disconnecting the install guard left it green.
  ## Running it is cheap and unambiguous: a staged copy, a throwaway R_LIBS_USER, --no-package and
  ## --no-tex. It writes nothing outside tempdir and nothing must land in that library.
  local({
    st <- file.path(tempdir(), "acta_nopkg_gate"); unlink(st, recursive = TRUE)
    dir.create(st, recursive = TRUE, showWarnings = FALSE)
    file.copy(sf, st)
    for (d in c(file.path("inst", "pipeline"), "R")) {
      dir.create(file.path(st, d), recursive = TRUE, showWarnings = FALSE)
      src <- file.path(vd, d)
      if (dir.exists(src)) file.copy(list.files(src, full.names = TRUE), file.path(st, d))
    }
    file.copy(file.path(vd, "DESCRIPTION"), st); file.copy(file.path(vd, "NAMESPACE"), st)
    lib <- file.path(st, "lib"); dir.create(lib, showWarnings = FALSE)
    ## PREPEND the throwaway, do not REPLACE the library path. Setting R_LIBS_USER alone removes
    ## the user library from the child, so on any machine whose packages live there -- the default
    ## on Linux, and true of this project's own CI runners -- the child sees all 31 dependencies as
    ## missing and installs them from CRAN and Bioconductor inside the gate. The workflow's own
    ## estimate for that is 20-40 minutes. The redirect only needs to catch what Setup.R WRITES.
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(file.path(st, "Setup.R")), "--no-package", "--no-tex"),
      stdout = TRUE, stderr = TRUE,
      env = paste0("R_LIBS_USER=",
                   paste(c(lib, .libPaths()), collapse = .Platform$path.sep))))
    chk("--no-package says it is skipping the install",
        any(grepl("skipping the ACTA package install", out, fixed = TRUE)))
    chk("...and really does not install ACTA",
        !dir.exists(file.path(lib, "ACTA")))
    if (dir.exists(file.path(lib, "ACTA")))
      cat("         it installed anyway, so the flag is not wired to the install\n")
    unlink(st, recursive = TRUE)
  })
  ## And the verification must be about THIS install: have("ACTA") alone says only that some ACTA
  ## is loadable from somewhere, which anyone who ran install_github already satisfies.
  ## THE WHOLE EXPRESSION. Two independent greps caught deletion but not INVERSION: rewriting
  ## pkg_ok's && as || restores the blocking defect in full and both substrings survive. The
  ## NOT-READY check a few lines up was tightened to an exact match for exactly this reason and
  ## this one was not -- the ninth assertion this cycle that matched a string rather than a claim.
  chk("...verified by version AND by the install's own warning, not just by loadability",
      any(grepl('pkg_ok <- have("ACTA") && !is.na(want) && identical(got, want) && !.ibad',
                code, fixed = TRUE)))
})

oq <- file.path(vd, "inst", "extdata", "oq_small")
chk("inst/extdata/oq_small/ ships all three diagnostic cases",
    all(dir.exists(file.path(oq, sprintf("OQ_Test%d", 1:3)))))
nfcs <- length(dir(oq, pattern = "[.]fcs$", recursive = TRUE, ignore.case = TRUE))
chk(sprintf("the diagnostic cases carry their FCS files (%d found)", nfcs), nfcs > 0L)

## ---- 3. the closure, when there is a network. Best effort by design ----------------------------
## A transient Bioconductor 404 must not block a release, so a fetch failure is reported and not
## counted. The deterministic guard above is the regression test; this is the end-to-end proof.
## WITHOUT remotes there is no way to know which repos remotes would use, and guessing CRAN-only
## would report the 5 Bioconductor packages as unresolvable on a perfectly healthy tree -- a gate
## that fails for the absence of a dev tool is worse than one that says it could not look.
res <- if (!requireNamespace("remotes", quietly = TRUE))
  simpleError("remotes is not installed, so the repo set cannot be determined") else tryCatch({
  ## Use the repos remotes would ACTUALLY use for THIS DESCRIPTION -- i.e. gate the Bioc repos on
  ## is_bioconductor(), exactly as dev_package_deps() does. Adding them unconditionally made this
  ## check pass with biocViews deleted, which is the one state it exists to catch.
  repos <- c(CRAN = "https://cloud.r-project.org")
  if (isTRUE(remotes:::is_bioconductor(remotes:::load_pkg_description(vd))))
    repos <- c(repos, remotes:::bioc_install_repos())
  ap <- available.packages(repos = repos)
  stopifnot(nrow(ap) > 1000L)
  base_pkgs <- rownames(installed.packages(lib.loc = .Library, priority = "base"))
  deps <- function(row) {
    f <- intersect(c("Depends", "Imports", "LinkingTo"), colnames(row))
    if (!length(f)) return(character(0))
    p <- trimws(gsub("\\(.*?\\)", "", strsplit(paste(row[1, f][!is.na(row[1, f])], collapse = ","), ",")[[1]]))
    setdiff(p[nzchar(p)], c("R", base_pkgs))
  }
  seen <- character(0); miss <- character(0); todo <- deps(d)
  while (length(todo)) {
    p <- todo[1]; todo <- todo[-1]
    if (p %in% seen || p %in% miss) next
    if (!p %in% rownames(ap)) { miss <- c(miss, p); next }
    seen <- c(seen, p); todo <- c(todo, deps(ap[p, , drop = FALSE]))
  }
  list(install = seen, missing = sort(miss))
}, error = function(e) e)

if (inherits(res, "error")) {
  note(paste("the closure was not checked --", conditionMessage(res)))
} else {
  cat(sprintf("         closure: %d packages to install, %d unresolvable\n",
              length(res$install), length(res$missing)))
  chk("every declared dependency resolves from the repos remotes will use",
      length(res$missing) == 0L)
  if (length(res$missing)) cat("         missing:", paste(res$missing, collapse = ", "), "\n")
  ## AND THE README'S OWN NUMBER, which is the kind of claim that goes stale in silence. The page
  ## tells a new user how many packages one install_github call pulls in; it said 139 while the
  ## closure measured 142, and nothing anywhere compared the two. A BAND, not equality: upstream
  ## adds and drops transitive dependencies on its own schedule and a gate that fails for that
  ## would be switched off. Outside the band means the page needs editing, which is a real finding.
  ## EVERY README THAT SHIPS, because the two are different documents: the public page makes this
  ## claim and the in-tree README.md does not, so keying on one file would either check nothing in
  ## the built tree or fail for a document that never made the claim. Checked wherever it appears;
  ## a page that does not state a number is noted, not failed.
  .rm <- Filter(file.exists, c(file.path(ACTA_REPO_ROOT, "README.md"),
                               file.path(dirname(ACTA_REPO_ROOT), "Publish", "README.public.md")))
  .got   <- length(res$install)
  .found <- 0L
  for (.f in .rm) {
    .hit <- unlist(regmatches(readLines(.f, warn = FALSE),
                              gregexpr("pulls in [0-9]+ packages", readLines(.f, warn = FALSE))))
    for (.h in .hit) {
      .found <- .found + 1L
      .said  <- as.integer(sub("[^0-9]*([0-9]+).*", "\\1", .h))
      chk(sprintf("%s: the package count is within 10 of the measured closure (says %d, measured %d)",
                  basename(.f), .said, .got),
          abs(.said - .got) <= 10L)
      if (abs(.said - .got) > 10L)
        cat("         that page needs updating to", .got, "\n")
    }
  }
  if (!.found)
    note(sprintf("no README states a package count; the closure measured %d", .got))
}

## ---------------------------------------------------------------------------------------------
## --no-tex MUST MEAN NOTHING TOUCHES TeX. The LaTeX-package step was gated on `latexOK() &&
## have("tinytex")` -- whether an engine EXISTS -- and not on the flag, so --no-tex skipped
## INSTALLING an engine and then went straight on to probe eleven .sty files with kpsewhich and
## tlmgr-install whatever was missing. On a machine that already has TeX that is a CTAN download
## the user explicitly declined, and it is reachable from this very suite: the --no-package gate
## above runs Setup.R, and this machine has an engine.
##
## BEHAVIOURAL, and with its own falsifiability built in. Every textual form of this check would
## have been another shape test, and this file already carries the scars of nine of those. Shims
## for pdflatex, kpsewhich and tlmgr go first on the child's PATH and append their own name to a
## sentinel file, so the assertion is "did Setup.R execute a TeX binary", answered by whether the
## file exists. pdflatex is shimmed too, so latexOK() is TRUE regardless of the machine -- without
## that the control run would find no engine on a CI runner, the block would not execute for the
## RIGHT reason, and the probe would look like it was working when it could see nothing.
if (file.exists(sf) && .Platform$OS.type != "windows") local({
  runSetup <- function(flags) {
    st <- file.path(tempdir(), "acta_tex_gate"); unlink(st, recursive = TRUE)
    dir.create(st, recursive = TRUE, showWarnings = FALSE)
    file.copy(sf, st)
    for (d in c(file.path("inst", "pipeline"), "R")) {
      dir.create(file.path(st, d), recursive = TRUE, showWarnings = FALSE)
      src <- file.path(vd, d)
      if (dir.exists(src)) file.copy(list.files(src, full.names = TRUE), file.path(st, d))
    }
    file.copy(file.path(vd, "DESCRIPTION"), st); file.copy(file.path(vd, "NAMESPACE"), st)
    lib <- file.path(st, "lib"); dir.create(lib, showWarnings = FALSE)
    shim <- file.path(st, "shim"); dir.create(shim, showWarnings = FALSE)
    sent <- file.path(st, "touched.txt")
    for (b in c("pdflatex", "kpsewhich", "tlmgr")) {
      f <- file.path(shim, b)
      ## kpsewhich must report NOT FOUND (exit 1, no stdout) so that texMiss is non-empty and
      ## Setup.R goes on to the tlmgr step -- otherwise "all present" short-circuits the very
      ## call this gate is trying to observe.
      writeLines(c("#!/bin/sh",
                   sprintf('echo "%s" >> "$ACTA_TEX_SENTINEL"', b),
                   if (b == "kpsewhich") "exit 1" else 'echo "version 2025"; exit 0'), f)
      Sys.chmod(f, "0755")
    }
    out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
      c("--vanilla", shQuote(file.path(st, "Setup.R")), "--no-package", flags),
      stdout = TRUE, stderr = TRUE,
      env = c(paste0("R_LIBS_USER=", paste(c(lib, .libPaths()), collapse = .Platform$path.sep)),
              paste0("PATH=", paste(c(shim, Sys.getenv("PATH")), collapse = .Platform$path.sep)),
              paste0("ACTA_TEX_SENTINEL=", sent))))
    list(touched = if (file.exists(sent)) unique(readLines(sent, warn = FALSE)) else character(0),
         out = out, st = st)
  }
  ## THE CONTROL FIRST. If this does not fire, the probe cannot see anything and the real
  ## assertion below would pass for the wrong reason.
  ctl <- runSetup(character(0))
  chk("the TeX probe can see a TeX binary being run (control, no --no-tex)",
      any(c("kpsewhich", "tlmgr") %in% ctl$touched))
  if (!any(c("kpsewhich", "tlmgr") %in% ctl$touched))
    cat("         touched:", paste(ctl$touched, collapse = ", "), "| Setup.R said:",
        paste(utils::head(grep("LaTeX|TeX", ctl$out, value = TRUE), 4), collapse = " | "), "\n")
  unlink(ctl$st, recursive = TRUE)

  got <- runSetup("--no-tex")
  chk("--no-tex runs no kpsewhich and no tlmgr at all",
      !any(c("kpsewhich", "tlmgr") %in% got$touched))
  if (any(c("kpsewhich", "tlmgr") %in% got$touched))
    cat("         it ran:", paste(got$touched, collapse = ", "), "\n")
  ## And says so in the summary rather than printing a green "0 present" line for a check that
  ## did not run -- the reporting half of the same defect.
  chk("...and the summary says the LaTeX package check was skipped",
      any(grepl("skipped (--no-tex)", got$out, fixed = TRUE)))
  unlink(got$st, recursive = TRUE)
})

## ---------------------------------------------------------------------------------------------
## THE DECLARED DEPENDENCIES MUST MATCH THE ONES THE PIPELINE ACTUALLY ATTACHES.
## Two gaps, both found by reading the manifests against each other rather than against the code:
##   * BiocManager and rstudioapi sat in Suggests while listOfLibrary library()s them
##     UNCONDITIONALLY, so remotes::install_github() did not install them and the pipeline
##     reached the network mid-run to fetch them, unpinned, into the session doing the analysis.
##   * purrr was attached and self-installed by the script for ONE call -- walk() over the
##     install manifest -- and was declared nowhere at all: not in listOfLibrary, not in
##     DESCRIPTION, and so not in the per-run version record that loop exists to make
##     reproducible. Replaced with a base for loop; a manifest should not need a package to read
##     itself.
local({
  d <- read.dcf(file.path(vd, "DESCRIPTION"))
  imp <- trimws(strsplit(d[1, "Imports"], ",")[[1]])
  sug <- if ("Suggests" %in% colnames(d)) trimws(strsplit(d[1, "Suggests"], ",")[[1]]) else character(0)
  scr <- list.files(file.path(vd, "inst", "pipeline"), pattern = "^ACTA_Script.*[.]R$",
                    full.names = TRUE)
  rmd <- list.files(file.path(vd, "inst", "pipeline"), pattern = "^ACTA_Report.*[.]Rmd$",
                    full.names = TRUE)
  txt <- unlist(lapply(c(scr, rmd), readLines, warn = FALSE))
  lol <- unlist(lapply(grep("^listOfLibrary[[:space:]]*<-", txt, value = TRUE), function(one)
           trimws(gsub('["\']', "", strsplit(sub(".*c\\(", "", sub("\\).*$", "", one)), ",")[[1]]))))
  lol <- unique(lol[nzchar(lol)])
  ## grDevices is shipped with R and is not installable, so it is the one name that may stay out.
  gap <- setdiff(setdiff(lol, "grDevices"), imp)
  chk(sprintf("every package the pipeline attaches is a DESCRIPTION Import (%d attached)", length(lol)),
      length(gap) == 0L)
  if (length(gap)) cat("         attached but only Suggested or undeclared:",
                       paste(gap, collapse = ", "), "\n")
  chk("...so nothing the pipeline attaches is left in Suggests",
      !length(intersect(lol, sug)))
  ## purrr is GONE, not merely declared. Checked across every file that ships, because the point
  ## was to remove the dependency rather than to write it down.
  files <- c(scr, rmd, file.path(vd, "DESCRIPTION"), file.path(vd, "NAMESPACE"))
  hits <- unlist(lapply(files[file.exists(files)], function(f) {
    ln <- readLines(f, warn = FALSE)
    ln <- ln[!grepl("^[[:space:]]*##", ln)]            # the comment explaining its removal stays
    grep("purrr", ln, value = TRUE)
  }))
  chk("purrr is not a dependency of anything that ships", !length(hits))
  if (length(hits)) cat("         still references purrr:", paste(utils::head(hits, 3),
                                                                  collapse = " | "), "\n")
  ## The loop it was there for must still install and attach every name, or removing purrr
  ## traded a dependency for a broken installer.
  loop <- grep("^for \\(\\.pkg in listOfLibrary\\)", txt, value = TRUE)
  chk("the install manifest is walked by a base for loop over listOfLibrary", length(loop) >= 2L)
  chk("...and still installs from CRAN, falls back to Bioconductor, and attaches",
      any(grepl("install.packages(.pkg)", txt, fixed = TRUE)) &&
        any(grepl("BiocManager::install(.pkg", txt, fixed = TRUE)) &&
        any(grepl("library(.pkg, character.only = TRUE)", txt, fixed = TRUE)))
})

cat(if (ok) "\nINSTALL PATH: all checks passed\n" else "\nINSTALL PATH: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
