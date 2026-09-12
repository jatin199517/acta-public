## THE README'S OWN R EXAMPLES MUST RESOLVE. Read out of the document, not retyped.
##
## WHY THIS GATE EXISTS. Both READMEs documented a run_acta() call that could not work, and had
## since 3.0 -- for four releases, until a colleague's automated run hit it. The examples were
## written when the code and the run folder were the same place; 3.0 moved the pipeline to
## inst/pipeline/ and nothing revisited the prose. `code_dir` defaults to `version_dir`, and
## run_acta() does NOT search for the script -- it requires exactly one ACTA_Script*.R in
## `code_dir` and stops otherwise. So the documented call failed with
## "expected exactly one ACTA_Script*.R in '<run folder>'; found 0".
##
## Every other gate reads the code. None read the documentation, which is why prose and behaviour
## drifted apart silently. This one checks the two things the examples assert about the layout:
## that each sys.source() path exists, and that each literal code_dir holds exactly one script.
##
## It does NOT run a full analysis -- that needs FCS and a minute. It checks the precondition that
## actually failed, which is the whole of the defect.
##
## Rscript <this> [version_dir]
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- ACTA_REPO_ROOT
ok <- TRUE
chk <- function(what, cond) { if (!isTRUE(cond)) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "ok" else "FAIL", what)) }
note <- function(what) cat(sprintf("  [--] %s\n", what))

rf <- file.path(vd, "README.md")
if (!file.exists(rf)) {
  cat("\nREADME CALL TESTS SKIPPED -- no README.md in", basename(vd), "\n")
  quit(status = 0L)
}
cat("README:", basename(vd), "/README.md\n")
md <- readLines(rf, warn = FALSE)

## Only fenced ```r blocks. Prose mentions a path in passing; an example is a promise it runs.
fence <- grep("^```", md)
blocks <- list()
if (length(fence) >= 2L)
  for (k in seq(1, length(fence) - 1L, by = 2L))
    if (grepl("^```r\\s*$", md[fence[k]]) && fence[k + 1L] > fence[k] + 1L)
      blocks[[length(blocks) + 1L]] <- md[(fence[k] + 1L):(fence[k + 1L] - 1L)]
inR2 <- rep(FALSE, length(md))
if (length(fence) >= 2L)
  for (k in seq(1, length(fence) - 1L, by = 2L))
    inR2[fence[k]:fence[k + 1L]] <- TRUE
## PARSE THE BLOCKS WITH R'S OWN PARSER. The first two versions of this gate scanned text, and
## both were fail-open in the same way the defect was: version one asked whether the BLOCK
## mentioned code_dir, so a trailing comment satisfied it; version two stripped comments with a
## regex that abstained whenever the tail held a quote -- an apostrophe in prose was enough -- and
## its paren-walk returned the rest of the document when the parens did not balance, which then
## matched code_dir from any later example. Both passed the exact bug this file exists to catch.
## A hand-rolled scanner kept losing to R syntax, so the answer now comes from R: comments and
## strings cannot be confused with code by the thing that defines what they are.
exprs <- list()
for (b in blocks) {
  e <- tryCatch(parse(text = paste(b, collapse = "\n")), error = function(err) err)
  if (inherits(e, "error")) {
    chk(sprintf("an r block parses as R (%s)", conditionMessage(e)), FALSE)
  } else {
    chk("an r block parses as R", TRUE)
    exprs <- c(exprs, as.list(e))
  }
}
chk("the README has R examples to check", length(exprs) > 0L)

## Walk the syntax tree for calls to either entry point, however they are written: run_acta(),
## h$run_acta(), ACTA::run_acta().
.fname <- function(f) {
  if (is.name(f)) return(as.character(f))
  if (is.call(f) && as.character(f[[1]]) %in% c("$", "::", ":::")) return(as.character(f[[3]]))
  NA_character_
}
.collect <- function(x, want, acc = list()) {
  if (is.call(x)) {
    if (identical(.fname(x[[1]]), want)) acc[[length(acc) + 1L]] <- x
    for (i in seq_along(x)) acc <- .collect(x[[i]], want, acc)
  }
  acc
}
## Calls, plus the source text of the block each came from -- the install/clone distinction is a
## property of the surrounding example, not of the call.
calls <- function(want) {
  out <- list(); src <- list()
  for (bi in seq_along(blocks)) {
    e <- tryCatch(parse(text = paste(blocks[[bi]], collapse = "\n")), error = function(err) NULL)
    if (is.null(e)) next
    got <- do.call(c, c(list(list()), lapply(as.list(e), .collect, want = want)))
    for (g in got) { out[[length(out) + 1L]] <- g; src[[length(src) + 1L]] <- blocks[[bi]] }
  }
  attr(out, "blk") <- src
  out
}

## PATHS RESOLVE WHERE THE READER IS TOLD TO STAND, and must land inside the tree. The published
## README says "the folder you cloned into" (= the repo root); the version-folder one writes paths
## from the DEV repo root, one level up. The parent is allowed only for a path whose first segment
## names this folder, and every hit is then required to be inside vd after normalizePath -- because
## restricting which roots are TRIED does not stop a path climbing out with "..".
resolve <- function(rel) {
  cand <- file.path(vd, rel)
  if (identical(strsplit(rel, "/", fixed = TRUE)[[1]][1], basename(vd)))
    cand <- c(cand, file.path(dirname(vd), rel))
  hit <- cand[file.exists(cand)]
  if (!length(hit)) return(NA_character_)
  root <- normalizePath(vd, mustWork = TRUE)
  inside <- vapply(hit, function(h) startsWith(normalizePath(h, mustWork = FALSE),
                                               paste0(root, .Platform$file.sep)), logical(1))
  if (any(inside)) hit[inside][[1]] else NA_character_
}
.lit <- function(x) if (is.character(x) && length(x) == 1L) x else NA_character_
## Evaluate a system.file() call from the AST, but ONLY when every argument is a literal -- so the
## path checked is the one the README prints, and nothing from the document is ever eval()'d as
## arbitrary code. Returns NA with a reason when it cannot be resolved, rather than passing.
.sysfile <- function(v) {
  a <- as.list(v)[-1]
  shown <- paste0("system.file(", paste(vapply(a, function(x)
    if (is.character(x)) sprintf('"%s"', x) else "<expr>", character(1)),
    ifelse(nzchar(names(a) %||% ""), paste0(names(a), " = "), ""), collapse = ", "), ")")
  shown <- paste0("system.file(", paste(mapply(function(nm, x)
    paste0(if (nzchar(nm)) paste0(nm, " = ") else "",
           if (is.character(x)) sprintf('"%s"', x) else "<expr>"),
    if (is.null(names(a))) rep("", length(a)) else names(a), a), collapse = ", "), ")")
  if (!all(vapply(a, is.character, logical(1))))
    return(list(path = NA_character_, shown = shown, why = "has a non-literal argument"))
  pkg <- if ("package" %in% names(a)) a[["package"]] else "base"
  if (!nzchar(system.file(package = pkg)))
    return(list(path = NA_character_, shown = shown,
                why = sprintf("package '%s' is not installed, so it cannot be checked", pkg)))
  list(path = do.call(system.file, a), shown = shown, why = NA_character_)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

## 1. every entry-point call passes code_dir, and every LITERAL code_dir holds exactly one script.
nlit <- 0L; nsysfile <- 0L
for (fn in c("run_acta", "actaOQRun")) {
  cl <- calls(fn)
  if (!length(cl)) { note(sprintf("no %s() example in this README", fn)); next }
  blk <- attr(cl, "blk")
  cat(sprintf("  -- %d %s() example(s)\n", length(cl), fn))
  for (i in seq_along(cl)) {
    a <- as.list(cl[[i]])[-1]
    has <- "code_dir" %in% names(a)
    ## WHICH CONTRACT APPLIES DEPENDS ON THE EXAMPLE. Since 3.0.5 code_dir is RESOLVED, so an
    ## install example is supposed to show the bare call and pinning code_dir there would be
    ## wrong. A clone example still has to pass it: the resolver's second branch looks in the
    ## working folder, which in a clone is the reader's data, not inst/pipeline.
    ## The first version of this check demanded code_dir on EVERY call -- it passed in the dev
    ## tree, where 3_0/README.md still pinned it, and would have failed the build on the public
    ## README the same release rewrote. A gate that encodes the previous release's contract stops
    ## the release rather than protecting it.
    ## FAILS CLOSED. An example only counts as an install example when it says so -- library(ACTA)
    ## and no sys.source/h$ -- and everything else is treated as a clone example and must pass
    ## code_dir. Detecting "clone" by its markers and defaulting the remainder to "install" had it
    ## the wrong way round: a block with neither marker would be excused.
    ## And there is NO assertion for the install case, because there is nothing to assert -- the
    ## argument is optional there. The first version wrote chk(..., TRUE), which is unfalsifiable
    ## and reports coverage it does not have. A note says so honestly instead.
    isInstall <- any(grepl("library\\(ACTA\\)", blk[[i]], fixed = FALSE)) &&
                 !any(grepl("sys[.]source|h\\$", blk[[i]]))
    if (!isInstall) chk(sprintf("%s() clone example %d passes code_dir", fn, i), has)
    else note(sprintf("%s() install example %d: code_dir is optional there%s", fn, i,
                      if (has) " (it passes one anyway, checked below)" else ""))
    if (!has) next
    v <- a[["code_dir"]]
    if (is.character(v)) {
      nlit <- nlit + 1L
      d <- .lit(v); r <- resolve(d)
      n <- if (is.na(r)) NA_integer_ else length(list.files(r, pattern = "^ACTA_Script.*[.]R$"))
      chk(sprintf('%s() example %d: code_dir = "%s" holds exactly one ACTA_Script*.R (found %s)',
                  fn, i, d, if (is.na(n)) "no such folder" else n),
          !is.na(n) && n == 1L)
    } else if (is.call(v) && identical(.fname(v[[1]]), "system.file")) {
      ## READ THE NODE'S OWN ARGUMENTS. The first version built its own
      ## system.file("pipeline", package = "ACTA") and asserted on that, so a README saying
      ## system.file("NOT_A_REAL_DIR", ...) passed -- and the label printed a string the README did
      ## not contain. Same shape as the `|| TRUE` already caught elsewhere: an assertion about
      ## something other than the artefact under test.
      nsysfile <- nsysfile + 1L
      got <- .sysfile(v)
      if (is.na(got$path))
        note(sprintf("%s() example %d: %s -- %s", fn, i, got$shown, got$why))
      else
        chk(sprintf("%s() example %d: %s resolves and holds exactly one ACTA_Script*.R",
                    fn, i, got$shown),
            nzchar(got$path) &&
              length(list.files(got$path, pattern = "^ACTA_Script.*[.]R$")) == 1L)
    } else note(sprintf("%s() example %d passes code_dir as an expression; not checkable here", fn, i))
  }
  ## FIRST POSITIONAL ARGUMENT, but only for actaOQRun. Its first argument names a case that
  ## ships, so it must exist. run_acta()'s first argument is the reader's OWN run folder, so in
  ## documentation it is necessarily a placeholder -- checking it flagged "path/to/run/folder".
  if (identical(fn, "actaOQRun")) for (i in seq_along(cl)) {
    a <- as.list(cl[[i]])[-1]
    pos <- a[!nzchar(names(a)) | is.null(names(a))]
    if (!length(pos)) next
    v1 <- pos[[1]]
    ## A system.file() CASE PATH IS CHECKED TOO. These are the install examples a package user
    ## copy-pastes, and none of their paths was verified: only `code_dir` was taught to read a
    ## system.file() node, so a wrong component in the FIRST argument -- which is what names the
    ## diagnostic case -- passed. system.file() returns "" for a path that is not there, so the
    ## example would hand actaOQRun("") to the user.
    if (is.call(v1) && identical(.fname(v1[[1]]), "system.file")) {
      got <- .sysfile(v1)
      if (is.na(got$path)) note(sprintf("%s() example %d: %s -- %s", fn, i, got$shown, got$why))
      else chk(sprintf("%s() example %d: %s resolves to a folder that exists", fn, i, got$shown),
               nzchar(got$path) && file.exists(got$path))
      next
    }
    d <- .lit(v1)
    if (is.na(d) || !grepl("/", d, fixed = TRUE)) next
    chk(sprintf('%s() example %d: "%s" exists', fn, i, d), !is.na(resolve(d)))
  }
}
chk("the README pins code_dir to something checkable at least once", nlit + nsysfile > 0L)

## 2. every sys.source() path exists
for (i in seq_along(calls("sys.source"))) {
  a <- as.list(calls("sys.source")[[i]])[-1]
  d <- .lit(if ("file" %in% names(a)) a[["file"]] else a[[1]])
  if (is.na(d)) next
  chk(sprintf('sys.source("%s") resolves', d), !is.na(resolve(d)))
}

## 3. PROSE PATHS TOO. A stale path in a sentence is as wrong as one in an example, and the
## fenced-block scan could not see them: `Diagnostics/OQ_Test3` sat in the prose for four releases
## while every code block was correct. Only backticked, slash-bearing, path-shaped tokens whose
## first segment names something really here -- otherwise every URL and column name is a candidate.
prose <- md[!inR2]
## SLASH-BEARING PATHS *AND* BARE FILENAMES. Requiring a "/" made the scan blind to
## `ACTA_App.R` sitting in a sentence telling Linux users to run it from the repo root -- the file
## moved to inst/pipeline/ in 3.0 and the instruction was wrong for four releases. A bare
## filename with a code-ish extension is a claim about the layout just as much as a path is.
## INVERTED. The first version kept a token only if its FIRST SEGMENT already existed in the
## tree -- which silently dropped every path whose top-level folder had been removed or renamed,
## i.e. the commonest way a path goes stale, and precisely the `Diagnostics/OQ_Test3` this section
## was written for. Restoring that exact string to the README left the gate reporting clean.
## So: keep anything path-shaped and report what does not resolve. The allow-list is for the few
## backticked tokens that name something produced AT RUN TIME and so cannot exist in a checkout.
## Produced at RUN TIME or created by the operator, so absent from any checkout by definition.
## Kept short and explicit: every name here is a check given up, so it should be obvious why.
## Quoted literals inside inline-code spans, kept only when they look like a path or a filename
## with a code-ish extension -- so "pipeline" and "ACTA" (system.file components) are ignored
## while "ACTA_App.R" and "inst/pipeline" are not.
.inlineStrings <- function(lines) {
  spans <- unlist(regmatches(lines, gregexpr("`[^`]+`", lines)))
  ## BOTH QUOTE STYLES. R treats them identically, and the first version matched only double
  ## quotes -- so the same defect spelled `shiny::runApp('ACTA_App.R')` walked straight through.
  ## Not a hypothetical spelling: both desktop launchers write runApp('%APP%'), so a maintainer
  ## copying from a launcher into the prose lands exactly in the blind spot.
  lits  <- c(unlist(regmatches(spans, gregexpr('"[^"]+"', spans))),
             unlist(regmatches(spans, gregexpr("'[^']+'", spans))))
  lits  <- gsub("[\"']", "", lits)
  lits[grepl("/", lits, fixed = TRUE) |
       grepl("[.](R|Rmd|xlsx|html|bat|command|tsv|pdf|png)$", lits)]
}

tok <- unique(c(
  unlist(regmatches(prose, gregexpr("`[A-Za-z0-9_][A-Za-z0-9_./-]*/[A-Za-z0-9_./-]*`", prose))),
  unlist(regmatches(prose, gregexpr("`[A-Za-z0-9_][A-Za-z0-9_.-]*[.](R|Rmd|xlsx|html|bat|command)`",
                                    prose))),
  ## AND STRINGS INSIDE BACKTICKED CODE. The defect that prompted widening this was
  ## `shiny::runApp("ACTA_App.R")` in a sentence -- a whole-token match cannot see it, because the
  ## backticks wrap an EXPRESSION and the filename is a string literal inside it. Prose that
  ## shows a call is making the same claim about the layout as prose that shows a path.
  .inlineStrings(prose)))
tok <- gsub("`", "", tok)
tok <- tok[!grepl("^https?:|^[A-Za-z]+:", tok)]

RUNTIME_OK <- c("Outputs/log.txt", "Outputs",   # written by a run
                "Titration_FCS",                # the operator's own input folder
                "Plots", "Diagnostics_log",      # written by a run
                "Archive",                       # the operator makes this to exclude runs
                "ACTA_Titration_Dashboard.html") # written by the dashboard generator
tok <- sub("/$", "", tok)
tok <- tok[!tok %in% RUNTIME_OK]
tok <- tok[!grepl("^Outputs/|/Outputs/|^Plots/|[<>*]", tok)]
if (!length(tok)) note("no path-shaped tokens in the prose") else
  ## The pipeline-relative fallback is for `Template/...` ONLY. The dashboard theme section
  ## discusses Template/ and Template/Other_Templates/, which live inside inst/pipeline/ rather
  ## than at the root. Applying that fallback to EVERY token made it a loophole: `ACTA_App.R` in
  ## a sentence telling Linux users to run it from the repo root resolved via
  ## inst/pipeline/ACTA_App.R and reported ok -- rescuing the exact instruction that was wrong.
  ## A token is judged where the prose says the reader is standing, and only Template/ is
  ## documented as relative to the code folder.
  for (t in tok) {
    hit <- resolve(t)
    if (is.na(hit) && identical(strsplit(t, "/", fixed = TRUE)[[1]][1], "Template"))
      hit <- resolve(file.path("inst", "pipeline", t))
    chk(sprintf("prose path `%s` exists", t), !is.na(hit))
  }

## 4. the programmatic entry point must still be documented at all. It was nearly concluded to
## have been withdrawn in 3.0 because a reader could not find it.
chk("the README documents run_acta()", any(grepl("run_acta", md, fixed = TRUE)))

## AND IT MUST NOT TELL ANYONE TO RUN `R CMD INSTALL .`. Both documented routes install the
## package themselves now -- install_github by definition, and Setup.R since 3.0.10 -- so any
## mention on the page can only be advice that has gone stale, which is how the run_acta example
## and the Linux launch line got wrong in the first place. A NEGATIVE mention counts too: saying
## "you do not need to" is still the page talking about it. Decided 2026-09-12.
## PROSE ONLY, not fenced blocks. The risk is stale ADVICE, and all of it is prose. Matching the
## whole file would also block a future troubleshooting section that quoted the app's own message
## -- the page already quotes its sibling, "the ACTA package is not installed" -- which would make
## this an obstacle rather than a guard. `prose` is the fenced-block mask the file already builds.
chk("the README never tells anyone to run R CMD INSTALL",
    !any(grepl("R CMD INSTALL", prose, fixed = TRUE)))

cat(if (ok) "\nREADME CALLS: all checks passed\n" else "\nREADME CALLS: FAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
