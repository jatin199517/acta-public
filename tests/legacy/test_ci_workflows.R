## The two CI workflows must not drift apart.
##
## THE RELEASE FLOW AGREED 2026-11-20: an update goes to `origin` (private) first, the PRIVATE
## workflow has to go green, and only then does anything reach `acta-public`. That makes the
## private workflow a gate on publication -- so a package the PUBLIC workflow installs and the
## private one does not is a green that certifies nothing, and the red arrives on the public repo
## AFTER you have published. Precisely the ordering the flow exists to prevent.
##
## Two separate files, by necessity: the published tree has the package at its ROOT, the private
## repo has it under ACTA_package/, and make_public_release.sh copies
## Publish/github/workflows/tests.yml to .github/workflows/tests.yml in the OUTPUT tree -- so the
## private repo's own .github/ cannot be the source for it. Nothing joins them but this file.
##
## SKIPS in the public release tree, which ships tests/ but neither Publish/ nor the private
## .github/. Banner says "TESTS SKIPPED" because run_all.R greps for exactly that.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}

## The git repo root is above the package root; both workflows are siblings of it.
root <- ACTA_REPO_ROOT
repeat {
  if (dir.exists(file.path(root, ".git"))) break
  up <- dirname(root); if (identical(up, root)) break
  root <- up
}
PUB <- file.path(root, "Publish", "github", "workflows", "tests.yml")
PRIV <- file.path(root, ".github", "workflows", "tests.yml")
if (!file.exists(PUB) || !file.exists(PRIV)) {
  cat(sprintf("  [skip] needs both workflows; public:%s private:%s\n",
              file.exists(PUB), file.exists(PRIV)))
  cat("\nCI WORKFLOW TESTS SKIPPED\n")
  quit(status = 0L)
}

## The `packages: |` block of a setup-r-dependencies step, as the action itself reads it: a YAML
## block scalar split on whitespace. Parsed by indentation rather than with a YAML library,
## because the repo ships no YAML reader and adding a dependency to read two files is worse than
## 15 lines -- and the action's own parsing is this crude.
pkgBlocks <- function(path) {
  L <- readLines(path, warn = FALSE)
  starts <- grep("^\\s*packages:\\s*\\|\\s*$", L)
  lapply(starts, function(s) {
    ind <- nchar(sub("^(\\s*).*$", "\\1", L[s]))
    out <- character(0)
    i <- s + 1L
    while (i <= length(L)) {
      ln <- L[i]
      if (nzchar(trimws(ln)) && nchar(sub("^(\\s*).*$", "\\1", ln)) <= ind) break
      t <- trimws(ln)
      if (nzchar(t)) out <- c(out, t)
      i <- i + 1L
    }
    out
  })
}

pb <- pkgBlocks(PUB); vb <- pkgBlocks(PRIV)
chk("the public workflow has at least one packages: block", length(pb) >= 1L, length(pb))
chk("the private workflow has exactly one packages: block", length(vb) == 1L, length(vb))
## A CONTROL: if the parser stops finding packages the comparison below is vacuously clean, which
## is the fail-open shape this repo keeps meeting.
chk("the parser actually found packages", length(vb) && length(vb[[1]]) > 20L,
    if (length(vb)) length(vb[[1]]) else 0L)
## EVERY LINE MUST BE `any::<pkg>`. The action splits the block on whitespace, so a stray comment
## or a wrapped line silently becomes bogus package refs -- that once produced 29 of them and pak
## aborted on `#`.
allLines <- unlist(c(pb, vb))
bad <- allLines[!grepl("^any::[A-Za-z][A-Za-z0-9.]*$", allLines)]
chk("every packages: line in BOTH workflows is a bare any::<pkg>", !length(bad),
    paste(bad, collapse = " | "))

## THE DRIFT CHECK ITSELF. The private `suite` list must match the PUBLIC `suite` list -- the
## first block in the public file, which is that job's. The public file's later blocks belong to
## `oq` and `report-windows`, jobs the private workflow deliberately does not have.
pubSuite <- if (length(pb)) pb[[1]] else character(0)
privSuite <- if (length(vb)) vb[[1]] else character(0)
onlyPub  <- setdiff(pubSuite, privSuite)
onlyPriv <- setdiff(privSuite, pubSuite)
chk("the private suite installs everything the public suite does", !length(onlyPub),
    paste(onlyPub, collapse = ", "))
chk("...and nothing the public suite does not", !length(onlyPriv),
    paste(onlyPriv, collapse = ", "))

## The private workflow must actually run the gates from the private layout. A workflow that
## resolves to a path that does not exist would be red for the wrong reason.
priv <- readLines(PRIV, warn = FALSE)
## CODE ONLY. Both of the checks below were written against the raw file first and BOTH passed on
## a mutation that broke them: `windows-latest` appears in a comment explaining why the shell is
## bash, and `working-directory: ACTA_package` appears three times, so `any()` survived deleting
## one. A check that its own prose can satisfy is not a check -- third time this pattern has shown
## up in this release, so it is worth stating plainly.
code <- sub("#.*$", "", priv)
code <- code[nzchar(trimws(code))]

## ALL THREE steps need it: dependency resolution reads ACTA_package/DESCRIPTION, the install runs
## there, and so do the gates. Counted, not any()-ed.
nwd <- sum(grepl("^\\s*working-directory:\\s*ACTA_package\\s*$", code))
chk("all three steps declare working-directory: ACTA_package", nwd == 3L,
    sprintf("found %d, expected 3", nwd))
chk("...and the gates are run from tests/legacy/run_all.R",
    any(grepl("tests/legacy/run_all.R", code, fixed = TRUE)))
## THE MATRIX LINE, not a mention. Windows is where this project has repeatedly shipped breakage.
chk("...and the os matrix includes a Windows runner",
    any(grepl("^\\s*os:\\s*\\[.*windows-latest.*\\]\\s*$", code)),
    paste(grep("^\\s*os:", code, value = TRUE), collapse = " | "))
chk("...and installs the package before running the gates, or test_app_package.R goes inert",
    any(grepl("R CMD INSTALL", code, fixed = TRUE)))
## NOT A COPY OF THE PUBLIC ONE: if someone pastes the public workflow over this file, the
## working-directory disappears and so does the whole reason it exists separately.
chk("the private workflow is not a verbatim copy of the public one",
    !identical(readLines(PUB, warn = FALSE), priv))

## The release script's source path is load-bearing: it copies the PUBLIC workflow out of
## Publish/, not out of .github/. If that ever changes to .github/ the published tree gets a
## workflow pointing at ACTA_package/, which does not exist there.
rel <- file.path(root, "Publish", "make_public_release.sh")
if (file.exists(rel)) {
  rl <- readLines(rel, warn = FALSE)
  chk("make_public_release.sh still stages the PUBLIC workflow from Publish/",
      any(grepl("Publish/github/workflows/tests.yml", rl, fixed = TRUE)))
  chk("...and does not stage the private one",
      !any(grepl("ROOT/.github/workflows", rl, fixed = TRUE)))
}

cat(sprintf("\n%s\n", if (ok) "CI WORKFLOWS: all checks passed" else "CI WORKFLOWS: FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
