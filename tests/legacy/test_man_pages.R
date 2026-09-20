## EVERY EXPORT IS DOCUMENTED, AND THE DOCUMENTATION STILL DESCRIBES THE CODE.
##
## WHY THIS GATE EXISTS. `man/` did not exist at all until 3.2.2, so `R CMD check` reported
## "Undocumented code objects" for all 31 exports and `help(run_acta)` found nothing -- on a
## package whose README tells people to install it from GitHub and call run_acta(). That is the
## kind of defect that cannot be caught by reading the code, because the code is fine.
##
## Writing the pages fixes it once. This gate is what stops it drifting back, and drift is the
## likely failure here rather than absence: a new export, a renamed argument or a changed default
## leaves the pages behind silently, and nothing else in the suite reads man/.
##
## THE ASSERTIONS ARE R's OWN, NOT RE-IMPLEMENTATIONS. tools::undoc(), tools::checkDocFiles(),
## tools::codoc() and tools::checkRd() are the four things `R CMD check` itself runs over
## documentation. Calling them directly means this gate cannot be more lenient than the check it
## is standing in for -- and it runs in one second rather than the several minutes a full check
## costs, which is the only reason it can sit in the fast suite.
##
## THE FIFTH ASSERTION IS THE ROXYGEN CANARY, and it is the one worth explaining.
##
## NAMESPACE is hand-written. It carries four wholesale import() directives whose absence is
## SILENTLY WRONG rather than merely broken: without import(flowCore), a bare colnames(flowSet)
## inside the namespace resolves to base::colnames, does not dispatch, returns NULL, and the run
## builds a 0 x 0 compensation matrix before dying somewhere unrelated. man/ is hand-written for
## the same reason -- there are no @export or @import tags anywhere in R/, so a single
## roxygen2::roxygenise() would regenerate NAMESPACE with NEITHER the exports nor the imports and
## reintroduce exactly that fault while appearing to be a routine documentation step.
##
## So: the export list NAMESPACE declares must equal the set of topics man/ documents, and R/ must
## contain no roxygen tags. The first catches a NAMESPACE that has been regenerated (its exports
## would no longer match the pages); the second catches the precondition, before anyone runs it.
## Both read with R's own parsers where a parser exists. The tag scan is a text scan because the
## absence of a marker is a textual property and there is nothing to execute.
##
## Rscript <this> [version_dir]
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

ok <- TRUE
chk  <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE
                               cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }
note <- function(what) cat(sprintf("  [--] %s\n", what))

## The package ROOT, not the code folder: man/, NAMESPACE and DESCRIPTION are siblings of R/, and
## in the package layout ACTA_VERSION_DIR is inst/pipeline/.
root <- ACTA_REPO_ROOT

## A version-folder checkout has no DESCRIPTION and never had man/ -- there is no package there to
## document. Skip the FILE, so the runner reports SKIP rather than a pass for assertions that
## never ran.
if (!file.exists(file.path(root, "DESCRIPTION")) || !dir.exists(file.path(root, "R"))) {
  cat("\nMAN PAGE TESTS SKIPPED -- ", basename(root),
      " is not a package layout (no DESCRIPTION + R/)\n", sep = "")
  quit(status = 0L)
}

cat("package root:", basename(root), "\n")
cat(sprintf("man/: %d topic file(s)\n",
            length(list.files(file.path(root, "man"), pattern = "[.]Rd$"))))

## --- 1-4: the four checks R CMD check runs over documentation ---------------------------------
## Each returns an object whose print method emits NOTHING when clean, so the verdict is "did it
## have anything to say", and any complaint is printed verbatim rather than summarised.
runTool <- function(what, expr) {
  out <- tryCatch(capture.output(print(expr)),
                  error = function(e) paste("the check itself failed:", conditionMessage(e)))
  chk(what, length(out) == 0L)
  if (length(out)) cat(paste0("        ", out, collapse = "\n"), "\n")
}

cat("\n=== R CMD check's own documentation checks ===\n")
runTool("tools::undoc -- every exported object has an \\alias",
        tools::undoc(dir = root))
runTool("tools::checkDocFiles -- \\usage and \\arguments agree",
        tools::checkDocFiles(dir = root))
## codoc compares \usage against the REAL formals, defaults included. It is the one that catches a
## renamed argument or a changed default, which is the drift this gate is mostly guarding against.
runTool("tools::codoc -- \\usage matches the function definitions",
        tools::codoc(dir = root))

cat("\n=== every .Rd parses and is well formed ===\n")
rds <- sort(list.files(file.path(root, "man"), pattern = "[.]Rd$", full.names = TRUE))
chk("man/ is not empty", length(rds) > 0L)
for (f in rds) {
  out <- tryCatch(capture.output(print(tools::checkRd(f))),
                  error = function(e) paste("checkRd failed:", conditionMessage(e)))
  if (length(out)) {
    chk(sprintf("checkRd: %s", basename(f)), FALSE)
    cat(paste0("        ", out, collapse = "\n"), "\n")
  }
}
if (all(vapply(rds, function(f) !length(tryCatch(capture.output(print(tools::checkRd(f))),
                                                 error = function(e) "x")), logical(1))))
  chk(sprintf("checkRd: all %d file(s) clean", length(rds)), TRUE)

## --- 5: the roxygen canary --------------------------------------------------------------------
cat("\n=== NAMESPACE and man/ describe the same package ===\n")

## R's own namespace parser, so this cannot disagree with what R will actually do at load time.
nsi <- tryCatch(parseNamespaceFile(basename(root), dirname(root)), error = function(e) e)
if (inherits(nsi, "error")) {
  chk(paste("NAMESPACE parses:", conditionMessage(nsi)), FALSE)
} else {
  exports <- sort(unique(nsi$exports))
  ## The four wholesale import()s. Named here rather than counted, because losing a SPECIFIC one
  ## is what breaks S4 dispatch, and a count would pass if one were swapped for another.
  wholesale <- vapply(nsi$imports, function(i) if (length(i) == 1L) as.character(i) else NA_character_,
                      character(1))
  wholesale <- sort(wholesale[!is.na(wholesale)])
  need <- c("flowCore", "flowWorkspace", "ggcyto", "openCyto")
  chk(sprintf("NAMESPACE still import()s %s wholesale -- S4 generics do not dispatch without them",
              paste(need, collapse = ", ")),
      all(need %in% wholesale))
  if (!all(need %in% wholesale))
    cat("        missing:", paste(setdiff(need, wholesale), collapse = ", "),
        "\n        NAMESPACE may have been regenerated by roxygen2 -- see its header\n")

  ## Aliases via R's Rd parser, not a regex over the files.
  aliasesOf <- function(f) {
    rd <- tools::parse_Rd(f)
    tg <- vapply(rd, function(x) attr(x, "Rd_tag") %||% "", character(1))
    unlist(lapply(rd[tg == "\\alias"], function(x) trimws(paste(unlist(x), collapse = ""))))
  }
  `%||%` <- function(a, b) if (is.null(a)) b else a
  al <- sort(unique(unlist(lapply(rds, aliasesOf))))

  chk(sprintf("every one of NAMESPACE's %d exports has a man/ topic", length(exports)),
      length(setdiff(exports, al)) == 0L)
  if (length(setdiff(exports, al)))
    cat("        undocumented:", paste(setdiff(exports, al), collapse = ", "), "\n")

  ## The other direction, which is the roxygen canary proper: a regenerated NAMESPACE would drop
  ## exports, and the pages documenting them would then be describing functions the package no
  ## longer exposes. "ACTA"/"ACTA-package" are the package overview and are deliberately not
  ## exported, so they are the only permitted extras.
  extra <- setdiff(al, c(exports, "ACTA", "ACTA-package"))
  chk("no man/ topic documents a name NAMESPACE does not export", length(extra) == 0L)
  if (length(extra)) cat("        documented but not exported:", paste(extra, collapse = ", "), "\n")
}

## R/ MUST STAY FREE OF ROXYGEN TAGS. This is the precondition, caught before anyone acts on it:
## with no tags there is no reason to run roxygenise(), and with tags there is -- and the run would
## silently take the imports out. Matches a tag only at the start of a `#'` line, which is the only
## place roxygen reads one, so prose mentioning "@export" cannot trip it.
cat("\n=== R/ carries no roxygen tags (NAMESPACE and man/ are hand-written) ===\n")
rsrc <- sort(list.files(file.path(root, "R"), pattern = "[.][Rr]$", full.names = TRUE))
hits <- unlist(lapply(rsrc, function(f) {
  ln <- readLines(f, warn = FALSE)
  i <- grep("^\\s*#'\\s*@[A-Za-z]", ln)
  if (length(i)) sprintf("%s:%d: %s", basename(f), i, trimws(ln[i])) else character(0)
}))
chk(sprintf("no roxygen tags in R/ (%d file(s) scanned)", length(rsrc)), length(hits) == 0L)
if (length(hits)) {
  cat(paste0("        ", head(hits, 10), collapse = "\n"), "\n")
  cat("        Adding tags makes roxygenise() look like a routine step. It is not:\n",
      "        there are no @import tags, so it would regenerate NAMESPACE without the\n",
      "        four wholesale import()s and break S4 dispatch silently.\n", sep = "")
}

cat(sprintf("\n%s\nMAN PAGE TESTS %s\n", strrep("-", 62), if (ok) "PASSED" else "FAILED"))
quit(status = if (ok) 0L else 1L)
