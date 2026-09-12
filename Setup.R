## ACTA -- ONE-STEP SETUP.  Run this once per machine, any of these ways:
##
##     Rscript Setup.R                 # from this folder, or with a full path from anywhere
##     source("Setup.R")               # or RStudio's Source button
##     Rscript Setup.R --no-tex        # skip TeX: everything except the PDF report
##     Rscript Setup.R --no-package    # skip installing ACTA itself, dependencies only
##
## It installs the app's packages, the analysis packages, a TeX engine if there is none, and the
## LaTeX packages the PDF report needs -- then prints one summary saying what is present and what is
## not.
##
## WHY IT IS ONE FILE. This was two: Setup.R wrapped Installation.R, because Installation.R took its
## TeX opt-in from a COMMAND-LINE FLAG and `commandArgs(trailingOnly = TRUE)` carries nothing under
## source() -- so an RStudio Source-button user silently got no LaTeX engine and no PDF. Wrapping it
## fixed that and created two new problems: two files to choose between, and TWO copies of the
## "where am I?" logic, of which the one doing the real work was the weaker (it read
## `sys.frame(1)$ofile`, which misses whenever source() is not the outermost call -- measured).
## Merging removes the flag-passing problem by construction and leaves ONE resolver.
##
## MINIMUM R VERSION, ENFORCED HERE. Security review of 2_94, finding L-2: R's native serialisation
## had CVE-2024-27322 -- a crafted .rds could execute code on readRDS() through a lazy-evaluation
## promise -- fixed in R 4.4.0. ACTA calls readRDS() on its preferences file and on every child
## process result, so the finding's "not currently exploitable" rested entirely on a sentence in the
## README saying we run 4.5.x/4.6.x. Nothing checked. A documented minimum that nothing enforces is
## a hope; this makes it a precondition. First statement in the file, before any install work.
if (getRversion() < "4.4.0")
  stop(sprintf(paste0("ACTA requires R >= 4.4.0; this is R %s.\n",
                      "R below 4.4.0 is vulnerable to CVE-2024-27322 (code execution via ",
                      "readRDS), and ACTA reads .rds files for its preferences and for every ",
                      "child-process result. Upgrade R, then re-run Setup."),
              getRversion()), call. = FALSE)

## BASE R ONLY, deliberately: this runs before any package is installed, so it cannot depend on one.
## In particular it does NOT use this.path to find itself. (The optional rstudioapi probe below is
## guarded and skipped when absent.)
##
## The package lists are READ FROM THE SOURCE FILES rather than restated here, so this script cannot
## drift out of step with them. The script and the .Rmd carry DIFFERENT lists -- the report also needs
## ragg (dev = "ragg_png") and sessioninfo -- so both are read and unioned.
##
## The app cannot install its own prerequisites: "ACTA App.command" / "ACTA App.bat" start with
## Rscript -e 'shiny::runApp(...)', so a machine without shiny fails on the first line and never
## reaches the dependency panel that would have said what was missing. That is a bootstrap problem no
## check inside the app can fix -- it happened on a fresh Windows install ("there is no package
## called 'shiny'"), which is why this file exists at all.

args      <- commandArgs(trailingOnly = TRUE)
skip_tex  <- any(args %in% c("--no-tex", "--notex")) ||
             isTRUE(get0("ACTA_SKIP_TEX", ifnotfound = FALSE))
## ACTA ITSELF is a package now, and the pipeline loads its helper library from the installed one:
## inst/pipeline/ carries no ACTA_Function*.R, by design. So a clone whose dependencies were
## installed but whose package was not could open the app and never complete a run -- the README
## had to tell people to run `R CMD INSTALL .` by hand, which is a step nobody should have to be
## told. Setup.R installs it. --no-package is for the case where you are deliberately testing an
## already-installed build against a checkout.
skip_pkg  <- any(args %in% c("--no-package", "--nopackage")) ||
             isTRUE(get0("ACTA_SKIP_PACKAGE", ifnotfound = FALSE))

## ---- find this file's folder, so the pipeline files can be read from anywhere -------------------
## FIVE routes, because no single one covers every entry point. Reported 2026-08-21: a colleague got
## "cannot find the ACTA folder", and the reason was that two common entry points both fall through to
## getwd(), which is whatever RStudio last had open:
##
##   1. ACTA_DIR              -- explicit, and the only route that ALWAYS works. Set it and stop
##                               guessing:  ACTA_DIR <- "<folder with ACTA_Script_*.R>"
##                               then       source(file.path(ACTA_DIR, "Setup.R"))
##   2. ofile, ANY FRAME      -- set by source(). Reading sys.frame(1) only was the bug: source()
##                               called from inside a function or local() puts ofile at frame 2 or 7,
##                               not 1, so a REAL source() still missed and fell through. Measured.
##   3. --file=               -- set by Rscript. Rscript ENCODES each space as "~+~" (see
##                               ?commandArgs), so a raw sub() returns a path that exists on no
##                               machine whose project lives under a folder with a space in its name.
##   4. RStudio editor path   -- covers Run-Selected-Lines and console paste, which produce NO ofile
##                               and no --file= at all. Optional: rstudioapi may not be installed yet
##                               on a fresh machine, which is the whole premise of this script.
##   5. getwd(), then ONE level down from it -- so being in the repo root rather than the version
##                               folder still works, as long as the answer is unambiguous.
##
## Still base R only (bar the optional rstudioapi probe): this runs before any package is installed.
.setupSelfDir <- function() {
  ## 1. explicit override, variable or environment
  d <- get0("ACTA_DIR", ifnotfound = Sys.getenv("ACTA_DIR", unset = ""))
  if (is.character(d) && length(d) == 1L && nzchar(d) && dir.exists(d)) return(normalizePath(d))
  ## 2. ofile in ANY frame, outermost first
  for (i in seq_len(sys.nframe())) {
    o <- tryCatch(get("ofile", envir = sys.frame(i), inherits = FALSE), error = function(e) NULL)
    if (is.character(o) && length(o) == 1L && nzchar(o) && file.exists(o))
      return(dirname(normalizePath(o)))
  }
  ## 3. Rscript
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) {
    pth <- gsub("~+~", " ", sub("^--file=", "", f[[1]]), fixed = TRUE)
    if (file.exists(pth)) return(dirname(normalizePath(pth)))
  }
  ## 4. the file open in RStudio's editor
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      isTRUE(tryCatch(rstudioapi::isAvailable(), error = function(e) FALSE))) {
    pth <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (is.character(pth) && length(pth) == 1L && nzchar(pth) && file.exists(pth))
      return(dirname(normalizePath(pth)))
  }
  NA_character_
}

## What makes a folder "the ACTA folder": exactly one pipeline script. The same "exactly one
## ACTA_Script*.R" rule run_acta() and actaOQRun() use, so all three agree by construction. (This
## used to look for Installation.R, which was the right anchor when that was a separate file.)
.hasPipeline <- function(d)
  !is.na(d) && dir.exists(d) && length(list.files(d, pattern = "^ACTA_Script.*[.]R$")) == 1L

## THE PACKAGE LAYOUT PUTS THE PIPELINE TWO LEVELS DOWN, at inst/pipeline/. Everything below was
## written for the flat release, where ACTA_Script_*.R sat at the root beside this file, and it
## searched its own folder, the working directory, and ONE level under it -- so from 3.0 onward
## both documented clone commands (`Rscript Setup.R`, `Setup.bat`) aborted with "cannot find the
## ACTA folder", and the error's advice sent the user back to the folder they were already in.
## Four releases. The same 3.0 restructure that stranded the README's run_acta() example and the
## desktop launchers stranded this too, and nothing here referenced Setup.R so no gate saw it.
.pipelineUnder <- function(d) {
  if (is.na(d) || !dir.exists(d)) return(NA_character_)
  p <- file.path(d, "inst", "pipeline")
  if (.hasPipeline(p)) normalizePath(p) else NA_character_
}

here <- .setupSelfDir()
if (!.hasPipeline(here)) {
  ## Beside this file first: that is the clone root, and it is where a user is told to stand.
  .p <- .pipelineUnder(.setupSelfDir())
  if (!is.na(.p)) here <- .p
}
if (!.hasPipeline(here)) here <- getwd()
if (!.hasPipeline(here)) {
  .p <- .pipelineUnder(getwd())
  if (!is.na(.p)) here <- .p
}
if (!.hasPipeline(here)) {
  ## One level down from the working directory: the repo root holds the version folders, so a user
  ## sitting in it is one step away. Only accepted when there is exactly ONE candidate -- with
  ## several version folders present, guessing would install against the wrong one silently.
  cand <- list.dirs(getwd(), recursive = FALSE, full.names = TRUE)
  cand <- cand[vapply(cand, .hasPipeline, logical(1))]
  ## Several candidates is the NORMAL state of the repo root -- every archived version from 2_87 on
  ## carries an ACTA_Script. The convention breaks the tie: exactly one folder is suffixed _WIP and
  ## it is the only one meant to be re-run (see the root README). Two _WIP folders means a version
  ## bump is half-done, and then guessing is exactly the wrong thing to do.
  if (length(cand) > 1L) {
    wip <- cand[grepl("_WIP$", basename(cand))]
    if (length(wip) == 1L) cand <- wip
  }
  if (length(cand) == 1L) {
    here <- normalizePath(cand)
    cat("Setup.R: found the ACTA folder one level down:", here, "\n")
  } else {
    stop("Setup.R: cannot find the ACTA folder (the one holding ACTA_Script_*.R).\n",
         "  looked in : ", getwd(),
         if (length(cand) > 1L)
           paste0("\n  ambiguous : ", length(cand), " candidate folders below it -- ",
                  paste(basename(cand), collapse = ", ")) else "",
         "\n\nThis happens when Setup.R is run in a way that hides its own path -- pasting it into\n",
         "the console, or RStudio's Run-Selected-Lines, both give no file path at all.\n",
         ## The placeholder is DESCRIBED, not named: this message ships in two layouts. The
         ## development repo has version folders (3_0), the public repo is FLAT and has none,
         ## so naming a version folder sent a public user to a directory that does not exist.
         "Point it at the folder holding ACTA_Script_*.R, or at the folder holding inst/pipeline/.\n",
         "In a release that is the folder you cloned into. Two one-line fixes, either is fine:\n",
         '  ACTA_DIR <- "<that folder>"; source(file.path(ACTA_DIR, "Setup.R"))\n',
         '  setwd("<that folder>"); source("Setup.R")\n',
         "Or from a terminal, where the path is never ambiguous:\n",
         "  Rscript <that folder>/Setup.R\n", call. = FALSE)
  }
}

## A WAY TO TEST THE DISCOVERY WITHOUT INSTALLING ANYTHING. The folder search above was wrong for
## four releases and no gate noticed, because nothing could exercise Setup.R cheaply: every route
## through it installs packages and a TeX engine. With this set, it reports what it resolved and
## stops. test_install_path.R uses it; it changes nothing for a real run.
if (nzchar(Sys.getenv("ACTA_SETUP_RESOLVE_ONLY"))) {
  cat("ACTA_SETUP_RESOLVED:", here, "\n")
  quit(save = "no", status = 0L)
}

cat("ACTA one-step setup\n")
cat("folder :", here, "\n\n")

## ---- a CRAN mirror, before anything is installed ----------------------------------------------
## A bare R started by Rscript often has none, in which case install.packages() stops to ask, which
## hangs a non-interactive run. Set one only when it is genuinely absent.
repo <- getOption("repos")
if (is.null(repo) || !("CRAN" %in% names(repo)) || !nzchar(repo[["CRAN"]]) ||
    identical(repo[["CRAN"]], "@CRAN@"))
  options(repos = c(CRAN = "https://cloud.r-project.org"))

## ---- 1. guarantee a TeX engine ----------------------------------------------------------------
## ANY engine counts. Somebody with MacTeX or MiKTeX already installed must not be given a second
## distribution, and re-running this must be a no-op rather than a 100 MB download.
hasTeX <- function()
  any(nzchar(Sys.which(c("xelatex", "pdflatex", "lualatex")))) ||
  (requireNamespace("tinytex", quietly = TRUE) &&
     isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))

if (skip_tex) {
  cat("TeX: skipped at your request. Everything except the PDF report will work.\n\n")
} else if (hasTeX()) {
  cat("TeX: an engine is already installed -- nothing to download.\n\n")
} else {
  cat("TeX: no engine found. Installing TinyTeX (about 100 MB, once per machine) ...\n")
  if (!requireNamespace("tinytex", quietly = TRUE)) try(install.packages("tinytex"), silent = TRUE)
  if (requireNamespace("tinytex", quietly = TRUE)) try(tinytex::install_tinytex(), silent = TRUE)
  cat("TeX: ", if (hasTeX()) "installed." else
      "still not found -- the summary below will say so, and the analysis runs without a PDF.",
      "\n\n", sep = "")
}

## ---- 2. hand over ------------------------------------------------------------------------------
## ---- and now the installation itself -----------------------------------------------------------
## `here` and `args` are resolved above; `want_tt` is the inverse of the skip flag so the TeX steps
## below read the same way they always did.
want_tt <- !skip_tex

cat("ACTA installation\n")
cat("R      :", R.version.string, "\n")
cat("platform:", R.version$platform, "\n")
cat("library :", .libPaths()[1], "\n")
cat("folder  :", here, "\n\n")

## ---- 1. what is needed -----------------------------------------------------------------------
readList <- function(path) {
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  m <- regmatches(txt, gregexpr("listOfLibrary\\s*<-\\s*c\\(([^)]*)\\)", txt))[[1]]
  if (!length(m)) return(character(0))
  unlist(lapply(m, function(one)
    trimws(gsub('["\']', "", strsplit(sub(".*c\\(", "", sub("\\)$", "", one)), ",")[[1]]))))
}
srcFiles <- c(list.files(here, pattern = "^ACTA_Script.*[.]R$",   full.names = TRUE),
              list.files(here, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE))
if (!length(srcFiles))
  stop("Run this from the ACTA version folder (the one holding ACTA_Script_*.R).", call. = FALSE)
analysis <- unique(unlist(lapply(srcFiles, readList)))
analysis <- analysis[nzchar(analysis)]

## The app's own dependencies. NOT added to the script's listOfLibrary: that list is what the
## ANALYSIS loads, and the analysis runs perfectly well in plain R with no app installed. Keeping
## them apart is also what lets the dependency panel report the two groups separately.
## tinytex is here rather than behind --tinytex, and it is NOT the TeX distribution -- it is the
## small R package that MANAGES one. Without it the LaTeX-package step below is skipped entirely,
## because that step is gated on have("tinytex"). Somebody who already has MacTeX or MiKTeX and runs
## a plain run therefore had a working engine, no report packages installed, and
## a suggested fix (tinytex::tlmgr_install) that needed a package they did not have. The 100 MB
## distribution is still opt-in; only this ~1 MB manager is unconditional.
app <- c("shiny", "processx", "this.path", "rmarkdown", "openxlsx", "jsonlite", "readxl", "tinytex")

cat(sprintf("analysis packages: %d\napp packages     : %d\n\n", length(analysis), length(app)))

## ---- 2. install what is missing --------------------------------------------------------------
have    <- function(p) requireNamespace(p, quietly = TRUE)
missing <- function(ps) ps[!vapply(ps, have, logical(1))]

## A bare R started by Rscript often has no CRAN mirror set, in which case install.packages() stops
## to ask -- which hangs a non-interactive run. Set one only when it is genuinely absent.
repo <- getOption("repos")
if (is.null(repo) || !("CRAN" %in% names(repo)) || !nzchar(repo[["CRAN"]]) ||
    identical(repo[["CRAN"]], "@CRAN@"))
  options(repos = c(CRAN = "https://cloud.r-project.org"))

need <- missing(c(app, analysis))
if (!length(need)) {
  cat("All packages already present -- nothing to install.\n\n")
} else {
  cat("Installing", length(need), "package(s):", paste(need, collapse = ", "), "\n\n")
  ## CRAN first. install.packages() warns rather than errors on a package it cannot find, which is
  ## why the result is re-checked afterwards instead of trusted.
  try(install.packages(need), silent = TRUE)

  ## Then Bioconductor for whatever is still absent -- flowCore, flowStats, ggcyto, openCyto,
  ## flowWorkspace and flowMeans are all Bioconductor, and install.packages() will never find them.
  still <- missing(need)
  if (length(still)) {
    cat("\nStill missing after CRAN:", paste(still, collapse = ", "), "\n")
    cat("Trying Bioconductor ...\n")
    if (!have("BiocManager")) try(install.packages("BiocManager"), silent = TRUE)
    if (have("BiocManager")) try(BiocManager::install(still, ask = FALSE, update = FALSE), silent = TRUE)
  }
}

## ---- 3. TinyTeX, only when asked -------------------------------------------------------------
## Opt-in because it is a ~100 MB download and only the PDF report needs it. Everything else --
## plots, the titration export, the dashboard and every QC number -- is produced without it.
latexOK <- function() {
  any(nzchar(Sys.which(c("pdflatex", "xelatex", "lualatex")))) ||
    (have("tinytex") && isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE)))
}

## PANDOC. rmarkdown shells out to it, so no pandoc means no report AT ALL -- not even a failed
## LaTeX pass. Setup.R cannot install it (it is not an R package and there is no official installer
## to drive), so this is a CHECK plus a link, the same contract as the XQuartz check below.
## REPORTED FROM A WINDOWS TEST RUN, 2026-09-09: Setup.R said READY and the report then failed,
## because pandoc had to be installed by hand afterwards. It never showed up in development because
## RSTUDIO BUNDLES ITS OWN PANDOC and puts it where rmarkdown finds it -- so every RStudio user is
## covered by accident and a plain `Rscript` install on Windows is not. Deliberately the SAME
## expression actaPreflight() uses (ACTA_Functions.R, the "pandoc" record), so the installer and the
## run-time check can never disagree about whether pandoc is present.
pandocOK <- function() {
  nzchar(Sys.which("pandoc")) ||
    (have("rmarkdown") && isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)))
}
if (want_tt && !latexOK()) {
  cat("\nInstalling TinyTeX (for the PDF report) ...\n")
  if (!have("tinytex")) try(install.packages("tinytex"), silent = TRUE)
  if (have("tinytex")) try(tinytex::install_tinytex(), silent = TRUE)
}

## PATH. A TinyTeX installed a moment ago is NOT on this session's PATH: tinytex's own tweak_path()
## adds its bin directory for the duration of one tinytex:: call and restores it on exit (it registers
## the undo with on.exit in the CALLING frame), so nothing it does survives into the code below. A raw
## kpsewhich would therefore not be found, every package would look missing, and the script would try
## to install all ten of them. Add the bin directory ourselves, for the rest of this run only.
texBin <- function() {
  if (!have("tinytex")) return("")
  root <- tryCatch(tinytex::tinytex_root(), error = function(e) "")
  if (!nzchar(root)) return("")
  b <- list.dirs(file.path(root, "bin"), recursive = FALSE)
  if (length(b)) b[[1]] else ""
}
if (!nzchar(Sys.which("kpsewhich")) && nzchar(bin <- texBin()))
  Sys.setenv(PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep))

## And repair a PATH that is missing the system directories.
##
## DUPLICATED ON PURPOSE. ACTA_Functions.R has actaRepairPath(), which the app and the pipeline both
## call and which covers more directories -- but this script runs BEFORE any package is installed and
## must not depend on anything, including its own sibling. Keep the two in step; if you widen the list
## there, widen it here.
##
## Seen on a colleague's Mac on
## 2026-08-20: the PATH read "/user/bin" where it meant "/usr/bin", so perl -- present, at
## /usr/bin/perl -- was invisible to R, and tlmgr (a Perl script) died with
## "env: perl: No such file or directory" before it could install anything. Everything else in
## /usr/bin was invisible to that session too. Prepending a standard system directory that is
## genuinely absent is safe, and saying so is better than silently working around it.
if (.Platform$OS.type != "windows") {
  onPath <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
  addBack <- setdiff(c("/usr/bin", "/bin"), onPath)
  addBack <- addBack[dir.exists(addBack)]
  if (length(addBack)) {
    cat("\nNOTE: PATH is missing", paste(addBack, collapse = ", "),
        "-- adding for this run.\n     Fix it permanently in ~/.Renviron or your shell profile.\n")
    Sys.setenv(PATH = paste(paste(addBack, collapse = .Platform$path.sep), Sys.getenv("PATH"),
                            sep = .Platform$path.sep))
  }
}

## The report's own LaTeX packages, whenever an engine is present -- not only when we just installed
## one. A colleague's run on 2026-08-19 produced every correct NUMBER and then failed the PDF with
## "! LaTeX Error: File `placeins.sty' not found": their TeX install simply lacked a package the
## report asks for. There are eleven of them and a minimal distribution ships few, so without this
## they get discovered one failed render at a time.
##
## Read out of the .Rmd, so this cannot drift from what the report actually loads. Best-effort: a
## failure here leaves the analysis, export, plots and dashboard working -- only the PDF needs them.
texPkgs <- character(0); texMiss <- character(0)
if (latexOK() && have("tinytex")) {
  rmd <- list.files(here, pattern = "^ACTA_Report.*[.]Rmd$", full.names = TRUE)
  if (length(rmd) == 1L) {
    txt  <- tryCatch(readLines(rmd[[1]], warn = FALSE), error = function(e) character(0))
    hits <- regmatches(txt, gregexpr("usepackage(\\[[^]]*\\])?\\{[A-Za-z0-9,]+\\}", txt))
    texPkgs <- sort(unique(unlist(strsplit(gsub(".*\\{|\\}", "", unlist(hits)), ","))))
    ## PLUS what kableExtra injects at knit time. Those never appear in the .Rmd, so reading only the
    ## .Rmd meant this installed six packages while the render needs sixteen -- and the one that was
    ## actually missing was therefore never installed.
    ##
    ## Read from the INSTALLED kableExtra rather than restated, because the set depends on its VERSION:
    ## 1.4.0 injects `tabu`, and a newer release replaced that unmaintained package with `xltabular`. A
    ## colleague's 2_89 run failed on exactly that -- xltabular missing on that machine and absent from
    ## mine, so a list copied from mine could never have named it.
    ## The fallback is the UNION across kableExtra versions -- `tabu` from 1.4.0 and `xltabular` from
    ## the release that replaced it -- because it only runs when latex_pkg_list() cannot be read, which
    ## is exactly when there is no way to tell which variant is installed. Returning character(0) here,
    ## as this did at first, would silently drop back to the six packages named in the .Rmd and skip
    ## the one that stops the render.
    if (any(grepl("kableExtra", txt, fixed = TRUE)) && have("kableExtra")) {
      keFallback <- c("booktabs", "longtable", "array", "multirow", "wrapfig", "float", "colortbl",
                      "pdflscape", "tabu", "xltabular", "threeparttable", "threeparttablex",
                      "ulem", "inputenc", "makecell", "xcolor")
      inj <- tryCatch({
        v <- gsub(".*[{]|[}]", "", get("latex_pkg_list", asNamespace("kableExtra"))())
        if (length(v)) v else keFallback
      }, error = function(e) keFallback)
      texPkgs <- sort(unique(c(texPkgs, inj)))
    }
    ## The right question is "can LaTeX FIND this .sty", which is what kpsewhich answers.
    ## tinytex::check_installed() answers a different one -- "is there a TeX Live package by this
    ## name" -- and gets multicol wrong, because multicol.sty ships inside the base `latex` bundle
    ## and there is no tlmgr package called multicol (tlmgr: "package multicol not locally!").
    found <- function(k) {
      out <- suppressWarnings(system2("kpsewhich", shQuote(paste0(k, ".sty")),
                                      stdout = TRUE, stderr = FALSE))
      length(out) >= 1L && !is.na(out[[1]]) && nzchar(out[[1]])
    }
    if (length(texPkgs)) {
      cat("\nChecking the ", length(texPkgs), " LaTeX package(s) the report needs ...\n", sep = "")
      texMiss <- texPkgs[!vapply(texPkgs, found, logical(1))]
      if (!length(texMiss)) cat("  all present\n")
      else {
        cat("  installing:", paste(texMiss, collapse = ", "), "\n")
        ## Can tlmgr run at all? It is a Perl script, so a session without perl on PATH cannot
        ## install anything -- and reporting that beats a silent failure followed by a puzzling
        ## "STILL MISSING". Windows is not exposed: TeX Live and TinyTeX ship their own perl there.
        tlv <- suppressWarnings(tryCatch(system2("tlmgr", "--version", stdout = TRUE, stderr = TRUE),
                                         error = function(e) NA_character_))
        tlok <- !any(is.na(tlv)) &&
                (is.null(attr(tlv, "status")) || identical(as.integer(attr(tlv, "status")), 0L))
        if (!tlok) {
          cat("  tlmgr WILL NOT RUN -- cannot install them.\n")
          if (.Platform$OS.type != "windows" && !nzchar(Sys.which("perl")))
            cat("  tlmgr is a Perl script and perl is not on PATH.",
                if (file.exists("/usr/bin/perl")) "It exists at /usr/bin/perl -- PATH is wrong."
                else "Install perl, or use a full TeX distribution.", "\n")
          cat("  A full distribution (MacTeX / TeX Live / MiKTeX) ships these packages already.\n")
        } else {
          ## RETRY ON A DIFFERENT MIRROR. Reported 2026-08-21:
          ##   TLPDB::_install_data: downloading did not succeed (check_file_and_remove failed)
          ##   for https://latex.us/systems/texlive/tlnet/archive/xltabular.tar.xz
          ## That is not a TeX problem and not an ACTA one -- it is one CTAN mirror serving a
          ## truncated file. tlmgr picks a mirror once and keeps using it, so the same download keeps
          ## failing and the package "cannot be installed" on a machine where nothing is wrong.
          ##
          ## `mirror.ctan.org` is CTAN's own redirector: it hands out a WORKING mirror per request, so
          ## pointing tlmgr at it is both the fix and the more robust steady state. Tried only on
          ## failure, so a machine whose mirror is fine is left completely alone.
          att <- suppressWarnings(tryCatch(
            system2("tlmgr", c("install", texMiss), stdout = TRUE, stderr = TRUE),
            error = function(e) character(0)))
          ok <- all(vapply(texMiss, found, logical(1)))

          ## RELEASE SKEW, and it is not a mirror problem -- diagnosed 2026-08-21 from a colleague's
          ## run. TeX Live repositories are YEAR-VERSIONED:
          ##   tlmgr: Local TeX Live (2025) is older than remote repository (2026).
          ## Once CTAN rolls to the next release, a distribution from the previous year cannot
          ## install from the current repository at all -- and switching mirrors cannot help, because
          ## every current mirror carries the new release. (This is very likely what the earlier
          ## "check_file_and_remove failed" was too: a next-release file landing in a last-release
          ## tree.) TeX Live keeps each past release frozen under historic/, so pointing at THAT
          ## fixes the install without downloading a whole new distribution.
          if (!ok && length(att) && any(grepl("older than remote repository|[Cc]ross release", att))) {
            yr <- suppressWarnings(as.integer(sub(".*version[[:space:]]+([0-9]{4}).*", "\\1",
              grep("version[[:space:]]+[0-9]{4}", suppressWarnings(tryCatch(
                system2("tlmgr", "--version", stdout = TRUE, stderr = TRUE),
                error = function(e) character(0))), value = TRUE)[1])))
            if (!is.na(yr)) {
              frozen <- sprintf(
                "https://ftp.tu-chemnitz.de/pub/tug/historic/systems/texlive/%d/tlnet-final", yr)
              cat("  this TeX Live is release", yr, "and the live repository has moved on --\n",
                  "   retrying against the frozen", yr, "archive instead of downloading a new\n",
                  "   distribution.\n")
              if ("tlmgr_repo" %in% getNamespaceExports("tinytex"))
                   try(tinytex::tlmgr_repo(frozen), silent = TRUE)
              else try(tinytex::tlmgr(c("option", "repository", frozen)), silent = TRUE)
              try(tinytex::tlmgr_install(texMiss), silent = TRUE)
              ok <- all(vapply(texMiss, found, logical(1)))
              if (!ok)
                cat("  still not installed. The durable fix is to upgrade the distribution:\n",
                    "   tinytex::reinstall_tinytex()   -- one command, and it keeps working next\n",
                    "   year when the repository rolls over again.\n")
            }
          } else if (!ok) {
            cat("  first attempt did not install everything -- retrying via CTAN's mirror",
                "redirector\n")
            ## tlmgr_repo() is tinytex's own wrapper for this and is the documented way to do it;
            ## the raw `tlmgr option repository` call is the fallback for a tinytex too old to have
            ## it. NOTE this setting PERSISTS on the machine, which is the point -- a mirror pinned
            ## once by install_tinytex() keeps failing on every future run until it is replaced.
            CTAN <- "https://mirror.ctan.org/systems/texlive/tlnet"
            if ("tlmgr_repo" %in% getNamespaceExports("tinytex"))
                 try(tinytex::tlmgr_repo(CTAN), silent = TRUE)
            else try(tinytex::tlmgr(c("option", "repository", CTAN)), silent = TRUE)
            try(tinytex::tlmgr_install(texMiss), silent = TRUE)
          }
        }
        ## RE-PROBED, because tlmgr_install is wrapped in try() and can fail per name -- a package
        ## that is part of a bundle rather than a package of its own cannot be installed by that
        ## name. Reporting what is still missing turns a silent failure into a named one.
        texMiss <- texMiss[!vapply(texMiss, found, logical(1))]
        cat(if (!length(texMiss)) "  installed\n" else
            paste0("  STILL MISSING: ", paste(texMiss, collapse = ", "),
                   "\n  -> the analysis, export, plots and dashboard are unaffected; only the PDF",
                   " report needs these.\n"))
      }
    }
  }
}

## ---- 4. report ------------------------------------------------------------------------------
cat("\n", strrep("-", 62), "\n", sep = "")
## ---- ACTA itself ------------------------------------------------------------------------------
## The package root is the folder holding DESCRIPTION: `here` is <root>/inst/pipeline in the
## package layout and the version folder in a flat one, so walk up until a DESCRIPTION says ACTA.
## Nothing to install in the flat layout, and saying so is not a failure.
pkg_root <- local({
  d <- here
  for (i in 1:3) {
    f <- file.path(d, "DESCRIPTION")
    if (file.exists(f) &&
        identical(unname(tryCatch(read.dcf(f, "Package")[1], error = function(e) NA)), "ACTA"))
      return(normalizePath(d))
    nd <- dirname(d); if (identical(nd, d)) break; d <- nd
  }
  NA_character_
})
pkg_ok <- NA
if (!is.na(pkg_root) && !skip_pkg) {
  cat("\ninstalling ACTA itself from ", pkg_root, "\n", sep = "")
  ## repos = NULL, type = "source" on a DIRECTORY is what `R CMD INSTALL .` does, without needing
  ## a shell or the R binaries to be on PATH -- which on Windows they are not.
  ## VERIFY THIS INSTALL, BY VERSION. have("ACTA") only asks whether SOME ACTA is loadable from
  ## somewhere, which anyone who has ever run install_github already satisfies -- so a failed
  ## install of this checkout reported the OLD version as success and printed READY. Measured:
  ## ACTA 3.0.9 on the path plus a deliberately broken source tree gave
  ## "ERROR: unable to collate and parse R files" immediately followed by
  ## "ACTA 3.0.9 installed" and exit 0. Worse than a wrong message: the clone's newer pipeline
  ## then runs against the older helper library, and this tool has no compatibility shims across
  ## versions. Compare what is loadable now against what the source says it should be.
  want <- unname(tryCatch(read.dcf(file.path(pkg_root, "DESCRIPTION"), "Version")[1],
                          error = function(e) NA_character_))
  ## WATCH THE INSTALL'S OWN VERDICT AS WELL AS THE VERSION. install.packages() reports a failed
  ## build through a WARNING ("had non-zero exit status", "'lib' is not writable"), not an error,
  ## so try() sees nothing. And the version comparison alone is not enough either: reinstalling
  ## over an identical version that is already there looks like success whether or not the build
  ## worked -- which is precisely the upgrade path most people are on. Both signals, or neither.
  .iw <- character(0)
  withCallingHandlers(
    try(install.packages(pkg_root, repos = NULL, type = "source"), silent = TRUE),
    warning = function(w) { .iw <<- c(.iw, conditionMessage(w)); invokeRestart("muffleWarning") })
  .ibad <- any(grepl("non-zero exit status|not writable|cannot open|unable to", .iw,
                     ignore.case = TRUE))
  for (.w in .iw) cat("  install said: ", .w, "\n", sep = "")
  got  <- tryCatch(as.character(utils::packageVersion("ACTA")), error = function(e) NA_character_)
  pkg_ok <- have("ACTA") && !is.na(want) && identical(got, want) && !.ibad
  cat(if (isTRUE(pkg_ok))
        sprintf("  ACTA %s installed -- a run can load its helpers from it.\n", got)
      else paste0(sprintf("  ACTA %s did NOT install (loadable now: %s).\n",
                          if (is.na(want)) "?" else want, if (is.na(got)) "none" else got),
                  "  The app will still start, but a run will use whatever ACTA is already there,",
                  "\n  or stop with \"the ACTA package is not installed\". Re-run this script and\n",
                  "  read the install errors above.\n"), sep = "")
} else if (!is.na(pkg_root)) {
  cat("\nskipping the ACTA package install (--no-package).\n")
}

badApp <- missing(app); badAna <- missing(analysis)
line <- function(lbl, bad, n)
  cat(sprintf("%-22s %s\n", lbl,
              if (!length(bad)) sprintf("OK  (%d present)", n)
              else sprintf("MISSING %d: %s", length(bad), paste(bad, collapse = ", "))))
## ALWAYS REPORT ACTA. Gating the line on "did we try" meant the two paths that skip the install
## -- --no-package, and a pkg_root that did not resolve -- said nothing about it and still printed
## READY, which is the exact state this release exists to remove.
## ONE VERDICT ABOUT ACTA, feeding both the line and the final READY. They were computed
## separately, so --no-package on a machine with no ACTA printed "MISSING 1: ACTA" and then
## "READY" and exited 0 -- a summary contradicting itself, and the same false-READY this release
## exists to remove, moved one path over.
## ACTA IS ONLY REQUIRED WHERE THE LAYOUT REQUIRES IT. pkg_root is NA in the flat layout, where
## the helpers sit beside the script and the package is irrelevant -- folding that case in with
## --no-package made a flat checkout report NOT READY for a package it never needed. Where the
## package layout IS in play, the install must have worked, or --no-package must have been given
## and a usable ACTA already be there.
acta_bad <- !is.na(pkg_root) && !(isTRUE(pkg_ok) || (skip_pkg && have("ACTA")))
line(if (!is.na(pkg_root) && !skip_pkg) "ACTA package" else "ACTA package (not installed by this run)",
     if (acta_bad) "ACTA" else character(0), 1)
line("app packages",      badApp, length(app))
line("analysis packages", badAna, length(analysis))
line("LaTeX (PDF report)", if (latexOK()) character(0) else "engine",
     if (latexOK()) 1L else 0L)
## Its own line: an engine with the report's packages missing is precisely the state that produced
## every correct NUMBER and no PDF on a colleague's machine on 2026-08-19.
if (latexOK()) line("LaTeX packages", texMiss, length(texPkgs))
if (!latexOK())
  cat("                       -> re-run as:  Rscript Setup.R\n",
      "                          (or install MacTeX / MiKTeX / TeX Live yourself)\n", sep = "")
## Same severity as the LaTeX line and reported the same way: the numbers, plots, export and
## dashboard are all unaffected, but the PDF report needs pandoc as well as an engine.
line("pandoc (PDF report)", if (pandocOK()) character(0) else "pandoc",
     if (pandocOK()) 1L else 0L)
if (!pandocOK())
  cat("                       -> install it once per machine:\n",
      "                          https://pandoc.org/installing.html\n",
      sep = "")
if (.Platform$OS.type != "windows" && Sys.info()[["sysname"]] == "Darwin" &&
    !nzchar(Sys.which("Xquartz")) && !dir.exists("/opt/X11"))
  cat("\nXQuartz not detected. flowMeans loads tcltk, which needs X11 on macOS:\n",
      "  https://www.xquartz.org\n", sep = "")

ok <- !length(badApp) && !length(badAna) && !acta_bad
cat(strrep("-", 62), "\n", sep = "")
cat(if (ok) "READY -- launch the app with \"ACTA App.command\" (macOS) or \"ACTA App.bat\" (Windows).\n"
    else "NOT READY -- see the missing packages above, then run this script again.\n")
if (!ok) quit(status = 1)
