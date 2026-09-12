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
}

cat(if (ok) "\nINSTALL PATH: all checks passed\n" else "\nINSTALL PATH: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
