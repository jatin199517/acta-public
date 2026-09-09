## Sanitisation gate for the publicly shareable OQ cases.
##
## Run from anywhere:  Rscript 2_99/tests/verify_oq_sanitised.R [case_dir ...]
## With no arguments it scans EVERY Diagnostics/OQ_Test* case. That default is deliberate: this
## script used to hardcode a single folder name, and when the cases were moved under Diagnostics/
## it pointed at a path that no longer existed and exited with an error -- while still being cited
## as evidence that the data was clean. Discovering the cases removes that failure mode.
##
## Exit status is 1 on any hit, so this can gate a commit.
suppressMessages({library(flowCore); library(readxl)})

## The token list is NOT kept in this file, and that is the point. An earlier version hard-coded it
## -- the company name, two operator accounts, two cytometer serials, the programme codes and an ELN
## prefix -- so the gate PUBLISHED the very strings it exists to remove. The mechanism ships; the
## values do not.
##
## Private tokens live in a gitignored sidecar beside this script:
##     tests/.acta_scrub_tokens.R   ->   SCRUB_WORD <- c(...)   # short, matched on non-letter
##                                      SCRUB_SUB  <- c(...)   # substrings / regex
##                                      SCRUB_MARKER_MAP <- c("<real>" = "<placeholder>")
## Absent -- which is what a fresh public clone looks like -- the GENERIC patterns below still run
## and the script says loudly that it is in reduced mode, so a clean result is never mistaken for a
## complete one.
##
## SHORT tokens are matched with NON-LETTER boundaries, not as bare substrings: a three-letter token
## in the list matched the standard FCS keyword $CARRIERTYPE ("96 U-Bottom") in all 12 OQ_Test1 files
## and reported 12 leaks that were not leaks. A gate that cries wolf gets ignored, which is worse
## than no gate. The boundary form still catches TOK, TOK-T and TOK_construct (a hyphen and an
## underscore are both non-letters) while leaving a longer word containing it alone.
## Deliberately NARROW. Synced-library names (OneDrive, SharePoint) are not here even though a
## leaked path contains them: ACTA's own README and comments discuss OneDrive sync legitimately, so
## a generic rule on the name flags documentation rather than disclosure. The absolute-path shape
## and the home-directory pattern catch the actual leak; the private list adds the rest.
GENERIC_SUB <- c("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,}",  # e-mail address
                 "Users/[A-Za-z0-9._-]+")                            # home directory -> username
skipped <- character(0)          # sections that could not be checked -- see the summary

SCRUB_WORD <- character(0); SCRUB_SUB <- character(0)
.tokfile <- file.path(this.path::this.dir(), ".acta_scrub_tokens.R")
if (file.exists(.tokfile)) {
  source(.tokfile)
  cat(sprintf("token sidecar loaded: %d word token(s), %d substring(s)\n",
              length(SCRUB_WORD), length(SCRUB_SUB)))
} else {
  cat("NOTE: no .acta_scrub_tokens.R beside this script -- running in REDUCED mode.\n",
      "      Only generic patterns (e-mail, home directory, synced-library paths) are checked.\n",
      "      A CLEAN result here does NOT mean the private token list was applied.\n", sep = "")
  ## REDUCED MODE IS FATAL IN A DEVELOPMENT CHECKOUT, AND FINE IN A PUBLIC CLONE.
  ##
  ## The file is gitignored, so it NEVER ships -- a public user has no private tokens to scrub and
  ## reduced mode is the correct, complete answer for them. Failing on its absence would break every
  ## public clone's first run, which is the exact mistake just fixed for the working workbook.
  ##
  ## But in OUR checkout its absence is a silent hole: it went missing after 2_92 -- gitignored, so
  ## `cp -R` in a bump carried it while a git-based copy did not -- and the gate then reported CLEAN
  ## for THREE versions while never applying the private list. When it was finally restored on
  ## 2026-09-04 it immediately found a proprietary marker name in 2_95 that no generic pattern can
  ## see. A NOTE was not enough; nobody reads a note.
  ##
  ## The discriminator is sibling version folders: the development repo has ~26 of them next to this
  ## one, a public clone has none. Measured, not assumed. The TEST happens further down, once
  ## acta_test_paths.R has been sourced -- ACTA_VERSION_DIR does not exist this early, and reaching
  ## for it here made the gate die with "object 'ACTA_VERSION_DIR' not found" instead of reporting
  ## anything. Only the FACT is recorded here.
  .reduced <- TRUE
}
if (!exists(".reduced")) .reduced <- FALSE
## An empty word list must NOT become "(?:)", which matches the empty string everywhere and flags
## every file. Build the alternation only when there is something in it.
BAD <- paste(c(if (length(SCRUB_WORD))
                 sprintf("(?<![A-Za-z])(?:%s)(?![A-Za-z])", paste(SCRUB_WORD, collapse = "|")),
               SCRUB_SUB, GENERIC_SUB), collapse = "|")

## One cwd contract for the whole suite -- see acta_test_paths.R. Run this script from anywhere.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))

## REDUCED MODE IS FATAL IN A DEVELOPMENT CHECKOUT AND FINE IN A PUBLIC CLONE -- see the NOTE above
## for why, and note this runs HERE because it needs ACTA_VERSION_DIR.
## FAIL CLOSED. This used to infer "development checkout" from sibling folders matching `/2_[0-9]`,
## and that inference is wrong in the one tree the release is actually cut from: the staged release
## lives in `Plans/public_release_2_98`, whose siblings are `public_release_2_9x` -- `_2_98`, not
## `/2_98`. MEASURED 2026-09-08: the match count there is 0, the guard needs > 1, so it never fired
## and the gate printed CLEAN having never loaded the private list. That is the same tree whose
## workbooks were carrying a company SOP title and the SharePoint tenant hostname.
##
## So the discriminator is now EXPLICIT and the default is failure. A published tree carries a
## tracked `.acta_public` marker written by the release build, which says "reduced mode
## is expected here" -- a public user has no private tokens to scrub and must not be greeted by a
## red gate. Our own checkout and a bare copy are a FAILURE, because there the sidecar is expected
## and its absence is exactly the silent hole that let three versions report CLEAN after 2_92.
## ACTA_SCRUB_REDUCED_OK=1 is the deliberate override.
##
## AND BE HONEST ABOUT WHAT THIS CANNOT DO. An earlier version of this comment also claimed a release
## STAGING folder is a failure. It is not, and it CANNOT be: the staging tree is byte-identical to
## the published tree by design, so it carries `.acta_public` too and takes the branch above. A
## tracked file inside an artifact can never distinguish that artifact from its own staging copy.
## That is acceptable ONLY because the private-token sweep for a release does not run here -- it runs
## in the release build, in the source checkout where the sidecar exists, and since
## 2026-09-08 it covers workbook PARTS as well as text files. If that build step is ever dropped,
## nothing applies the private list to a release and this gate will not tell you so.
if (isTRUE(.reduced)) {
  .is_public   <- file.exists(file.path(ACTA_VERSION_DIR, ".acta_public"))
  .override_ok <- identical(Sys.getenv("ACTA_SCRUB_REDUCED_OK"), "1")
  if (.is_public) {
    cat("NOTE: ...and this tree carries .acta_public, so generic-only patterns are expected here.\n")
  } else if (.override_ok) {
    cat("NOTE: ...and ACTA_SCRUB_REDUCED_OK=1 is set, so reduced mode is allowed deliberately.\n")
  } else {
    skipped <- c(skipped, paste("the PRIVATE token list -- no .acta_scrub_tokens.R, and no",
                                ".acta_public marker, so this is a tree where one is EXPECTED.",
                                "Copy it forward from an earlier version folder, or set",
                                "ACTA_SCRUB_REDUCED_OK=1 if you really mean generic-only."))
    cat("NOTE: ...and no .acta_public marker, so that is a FAILURE, not a note.\n")
  }
}
args <- commandArgs(trailingOnly = TRUE)
## Arguments here are CASE folders, not a version folder, so the default scans this script's own
## version rather than the current directory.
cases <- if (length(args)) args else actaTestCaseDirs()
cases <- cases[dir.exists(cases)]
if (!length(cases)) stop(sprintf("no OQ case folders found under %s", actaTestCaseRoot()))

## read.FCSheader() returns a named CHARACTER VECTOR, so `[[` ERRORS on an absent name rather
## than returning NULL. Every lookup goes through this.
kwGet <- function(kw, k) if (k %in% names(kw)) kw[[k]] else NA_character_

bad <- 0L
for (cs in cases) {
  cat("\n########", basename(cs), "########\n")

  cat("=== filenames ===\n")
  fn <- list.files(cs, recursive = TRUE)
  fn <- fn[!grepl("^Outputs/", fn)]          # run artefacts, gitignored, not shipped
  h <- grep(BAD, fn, value = TRUE, ignore.case = TRUE, perl = TRUE)
  if (length(h)) { bad <- bad + length(h); for (x in h) cat("  LEAK:", x, "\n") } else cat("  clean\n")

  fcs <- list.files(file.path(cs, "Titration_FCS"), pattern = "[.]fcs$",
                    recursive = TRUE, full.names = TRUE)
  cat(sprintf("=== FCS keywords (%d files, every keyword VALUE and NAME) ===\n", length(fcs)))
  nHit <- 0L
  for (p in fcs) {
    kw <- read.FCSheader(p)[[1]]
    v <- as.character(kw); nm <- names(kw)
    ## Names as well as values: a keyword called e.g. PROG_BATCH leaks even with an empty value.
    hit <- grepl(BAD, v, ignore.case = TRUE, perl = TRUE) |
           grepl(BAD, nm, ignore.case = TRUE, perl = TRUE)
    ## Any value shaped like an absolute path is a leak regardless of content -- write.FCS bakes
    ## a FILENAME keyword holding the SOURCE path, which carries the username.
    hit <- hit | grepl("^(/|[A-Za-z]:\\\\)", v)
    if (any(hit)) { nHit <- nHit + sum(hit)
      for (i in which(hit)) cat(sprintf("  LEAK %s  %s=%s\n", basename(p), nm[i], substr(v[i], 1, 70))) }
  }
  bad <- bad + nHit
  if (!nHit) cat("  clean across all files\n")

  if (length(fcs)) {
    k1 <- read.FCSheader(fcs[1])[[1]]
    cat("  sample identity keywords:",
        paste(sprintf("%s=%s", c("$INST", "$OP", "$PROJ", "$CYTSN", "$SRC"),
                      vapply(c("$INST", "$OP", "$PROJ", "$CYTSN", "$SRC"),
                             function(k) as.character(kwGet(k1, k)), "")), collapse = "  "), "\n")
  }

  cat("=== layout workbook (every sheet, every cell) ===\n")
  lay <- Sys.glob(file.path(cs, "*Instructions*.xlsx"))
  lay <- lay[!grepl("backup", lay, ignore.case = TRUE)]
  nL <- 0L
  for (f in lay) for (sh in excel_sheets(f)) {
    d <- suppressMessages(read_excel(f, sheet = sh, col_types = "text"))
    cells <- c(names(d), unlist(lapply(d, as.character)))
    cells <- cells[!is.na(cells)]
    h2 <- unique(grep(BAD, cells, value = TRUE, ignore.case = TRUE, perl = TRUE))
    if (length(h2)) { nL <- nL + length(h2)
      for (x in h2) cat(sprintf("  LEAK %s[%s]: %s\n", basename(f), sh, substr(x, 1, 70))) }
  }
  bad <- bad + nL
  if (!nL) cat("  clean\n")

  cat("=== sidecar text inputs (csv / json) ===\n")
  side <- c(Sys.glob(file.path(cs, "*.csv")), Sys.glob(file.path(cs, "*.json")))
  nS <- 0L
  for (f in side) {
    ln <- readLines(f, warn = FALSE)
    h3 <- grep(BAD, ln, value = TRUE, ignore.case = TRUE, perl = TRUE)
    if (length(h3)) { nS <- nS + length(h3)
      for (x in h3) cat(sprintf("  LEAK %s: %s\n", basename(f), substr(trimws(x), 1, 70))) }
  }
  bad <- bad + nS
  if (!nS) cat(sprintf("  clean (%d file(s))\n", length(side)))
}

## --------------------------------------------------------------------------------------------
## THE VERSION FOLDER ITSELF -- code, docs, templates and the blank instructions workbook.
##
## This scope was missing, and the omission had teeth: the blank template carried an internal
## construct code for ten versions, and the gate's OWN regex matched it the whole time -- the gate
## simply never looked outside Diagnostics/. Everything that ships is checked now.
##
## Only TRACKED files are read. Untracked ones are working data by definition (the ignore rules keep
## the working workbook and Titration_FCS out), so scanning them would report leaks in files that are
## never published and drown the real signal. `git ls-files` is the same definition of "what ships"
## that the release uses.
## --------------------------------------------------------------------------------------------
## WHAT SHIPS is layout-dependent, and getting this wrong in either direction is bad:
##   version-folder repo : the CURRENT version folder ships, plus the repo-root README/.gitignore.
##                         Using the repo root here instead would sweep 2_0 ... 2_89 as well and
##                         report every identifier ever scrubbed out of an archived version -- a
##                         hundred findings in files nobody is publishing.
##   package             : the package root IS the shippable unit, README and all, so scan that and
##                         there is no separate root pass to do.
vd      <- if (identical(ACTA_TEST_LAYOUT$kind, "package")) ACTA_REPO_ROOT else ACTA_VERSION_DIR
rootAlso <- identical(ACTA_TEST_LAYOUT$kind, "version")
cat(sprintf("\n######## %s -- the shipped tree ########\n", basename(vd)))
tracked <- suppressWarnings(system2("git", c("-C", shQuote(vd), "ls-files"), stdout = TRUE, stderr = FALSE))
nV <- 0L
if (!length(tracked)) {
  ## A SKIP MUST TAINT THE SUMMARY. This printed the SKIP honestly and then let the final line read
  ## "CLEAN: 0 finding(s)" -- and on 2026-09-04 that made me briefly believe a sanitised export was
  ## clean when its code had never been swept at all (it sat under a gitignored folder, so
  ## `git ls-files` returned nothing). Same defect this project has already been bitten by twice:
  ## `shared_upstream` passing on zero comparisons, and the distinct()/Clone no-op. A check that
  ## cannot see its subject has not passed on it.
  skipped <<- c(skipped, "the shipped tree (not a git checkout -- code was NOT swept)")
  cat("  SKIP: not a git checkout, so 'what ships' cannot be determined here.\n")
} else {
  ## Binary payloads are covered by their own passes above (FCS keywords, workbook cells), so the
  ## text sweep skips them rather than grepping compressed bytes and reporting nothing useful.
  txt <- tracked[!grepl("[.](fcs|xlsx|pdf|png|rds)$", tracked, ignore.case = TRUE)]
  cat(sprintf("=== tracked text, code and templates (%d file(s)) ===\n", length(txt)))
  ## ONE FILE IS ALLOWED TO CONTAIN LEAK-SHAPED TEXT. tests/test_diagnostics.R has to build a fake
  ## identity in order to prove actaDiagScrub() removes it, so it trips this sweep by design -- the
  ## diagnostic-bundle plan predicted the collision in as many words: "our own gate would flag our
  ## own diagnostic output".
  ##
  ## It is NOT excluded from the sweep. An excluded file is a hole nobody looks in, and a real
  ## accidental path pasted into that file later would then ship unnoticed. Instead the FIXTURE
  ## IDENTITY IS DECLARED HERE and masked before grepping, so everything else in the file is still
  ## checked exactly as before. The other option -- building the strings with paste0() so a literal
  ## grep cannot see them -- was rejected: teaching ourselves to hide text from our own gate is how
  ## a gate stops meaning anything.
  FIXTURE_FILE <- "tests/test_diagnostics.R"
  FIXTURE_ID   <- "someone(\\.name)?"
  nFix <- 0L
  for (rel in txt) {
    f <- file.path(vd, rel)
    if (!file.exists(f)) next
    ln <- tryCatch(readLines(f, warn = FALSE), error = function(e) character(0))
    if (identical(rel, FIXTURE_FILE)) {
      masked <- gsub(FIXTURE_ID, "<declared-fixture>", ln, perl = TRUE)
      nFix <- sum(masked != ln)
      ln <- masked
    }
    i <- grep(BAD, ln, ignore.case = TRUE, perl = TRUE)
    for (k in i) { nV <- nV + 1L
      cat(sprintf("  LEAK %s:%d %s\n", rel, k, substr(trimws(ln[k]), 1, 70))) }
  }
  if (!nV) cat("  clean\n")
  ## Said out loud, every run: an allowance that is silent is an allowance nobody audits.
  if (nFix)
    cat(sprintf("  note: %s swept with the declared fixture identity masked on %d line(s)\n",
                FIXTURE_FILE, nFix))

  ## FILENAMES too: the blank template used to be named after a real-looking ELN, so the name alone
  ## was a finding even though every cell inside it was clean.
  cat("=== tracked filenames ===\n")
  hN <- grep(BAD, tracked, value = TRUE, ignore.case = TRUE, perl = TRUE)
  if (length(hN)) { nV <- nV + length(hN); for (x in hN) cat("  LEAK name:", x, "\n") } else cat("  clean\n")

  ## EVERY TRACKED WORKBOOK, wherever it lives. This used to read
  ##     & !grepl("^Diagnostics/", tracked)
  ## on the reasoning that the section was about "the blank template that ships as part of the tool".
  ## But three of the four workbooks we ship ARE under Diagnostics/ -- the OQ cases -- so the check
  ## inspected exactly one file and then printed "clean", and the release manifest recorded that
  ## verdict over three workbooks it had never opened. OQ_Test3's instructions carried a real
  ## dc:creator e-mail and a real cp:lastModifiedBy name through the 2_97 release gate that way.
  ## The scope of a gate must be the scope of what ships: if it is tracked, it is checked.
  ##
  ## AND WHICH BYTES, NOT ONLY WHICH FILES. Widening the file scope above was only half the fix.
  ## Line ~198 excludes .xlsx from the text sweep, so for a workbook the ONLY things ever read were
  ## sheet CELLS (via read_excel) and two docProps/core.xml fields -- and a full-tree review on
  ## 2026-09-08 found two disclosures sitting in parts neither of those can see:
  ##   * xl/worksheets/sheet3.xml <headerFooter> -- an internal controlled-document title and a
  ##     "Confidential" footer, in OQ_Test1, OQ_Test2 AND the blank Template users are told to copy.
  ##     Values NOT quoted here: this file ships, and naming them would republish what was removed.
  ##   * xl/workbook.xml <x15ac:absPath> -- the full https://<tenant>.sharepoint.com/sites/...
  ##     URL the workbook was authored at: the tenant hostname and the internal folder tree down
  ##     to a pre-release working directory. NOT reproduced here -- this file ships, and quoting
  ##     the value would republish exactly what it was written to remove. The gate flagged this
  ##     comment when it first said the URL out loud, which is the gate working.
  ## This gate had printed "clean (4 workbook(s))" over all four. So every PART of every tracked
  ## workbook is now decompressed and swept with the same BAD patterns as the text tree, plus two
  ## structural assertions, because a differently-worded internal SOP title matches no generic
  ## pattern and only the private token list would catch it.
  ##
  ## READ FROM THE GIT INDEX, NOT THE WORKING TREE. What publishes is the staged/committed blob, and
  ## in this repository they differ constantly: the OneDrive sync client re-injects customXml/* and
  ## [trash]/*.dat into every workbook seconds after any write. Auditing the worktree means auditing
  ## bytes nobody will ever receive -- and would report failures that are not in the commit.
  cat("=== shipped workbooks: every PART of every tracked .xlsx, from the git index ===\n")
  wbs <- tracked[grepl("[.]xlsx$", tracked)]
  nW <- 0L
  if (!length(wbs)) cat("  none\n")
  ## The staged bytes, or NA when this is not a git checkout (the caller already reports that).
  ## An .xlsx is BINARY, so the blob is redirected straight to a file: system2(stdout = TRUE) hands
  ## back character LINES and paste()-ing those back together corrupts the archive (it cost a run of
  ## "not staged" skips to notice). The pathspec is ":./rel" -- a bare ":rel" resolves against the
  ## REPOSITORY ROOT, not against -C, and `tracked` here is relative to the version folder.
  .staged <- function(rel) {
    tf <- tempfile(fileext = ".xlsx")
    st <- suppressWarnings(system2("git", c("-C", shQuote(vd), "show", shQuote(paste0(":./", rel))),
                                   stdout = tf, stderr = FALSE))
    if (!identical(as.integer(st), 0L) || !file.exists(tf) || file.size(tf) == 0) {
      unlink(tf); return(NA_character_)
    }
    tf
  }
  for (rel in wbs) {
    f <- file.path(vd, rel)
    for (sh in excel_sheets(f)) {
      d <- suppressMessages(read_excel(f, sheet = sh, col_types = "text"))
      cells <- c(names(d), unlist(lapply(d, as.character))); cells <- cells[!is.na(cells)]
      h <- unique(grep(BAD, cells, value = TRUE, ignore.case = TRUE, perl = TRUE))
      if (length(h)) { nW <- nW + length(h)
        for (x in h) cat(sprintf("  LEAK %s[%s]: %s\n", basename(rel), sh, substr(x, 1, 70))) }
    }
    ## docProps/core.xml carries dc:creator and cp:lastModifiedBy -- a person's NAME, invisible to a
    ## cell-level read, and untouched by re-saving the sheets. Every shipped workbook carried one
    ## until 2026-08-21, and OQ_Test3 re-acquired both when it was reopened in Excel after that
    ## sweep -- which is why this runs on every release rather than once.
    core <- suppressWarnings(system2("unzip", c("-p", shQuote(f), "docProps/core.xml"),
                                    stdout = TRUE, stderr = FALSE))
    who <- unlist(regmatches(core, gregexpr("<dc:creator>[^<]+|<cp:lastModifiedBy>[^<]+", core)))
    who <- sub("^<[^>]+>", "", who)
    who <- who[nzchar(trimws(who))]
    if (length(who)) { nW <- nW + length(who)
      for (x in who) cat(sprintf("  LEAK %s docProps author: %s\n", basename(rel), x)) }

    ## SHAREPOINT LIBRARY METADATA, reported but NOT counted as a leak. Every workbook authored or
    ## re-saved inside the synced library carries customXml/item*.xml + docProps/custom.xml holding
    ## the library's ContentTypeId and managed-metadata field GUIDs. They name no person, no company
    ## and no URL -- opaque identifiers that cannot be resolved without access to the tenant -- so
    ## they are not treated as disclosure. They are printed because they are internal in origin and
    ## a reviewer should get to make that call rather than have it made silently. Stripping them is
    ## deliberately NOT done here: it means deleting parts, their Overrides and their relationships
    ## from an Excel-authored package, which risks the very corruption this release fixed.
    parts <- suppressWarnings(utils::unzip(f, list = TRUE)$Name)
    sp <- grep("^customXml/|^docProps/custom[.]xml$", parts, value = TRUE)
    if (length(sp))
      cat(sprintf("  note: %s carries %d SharePoint library metadata part(s) (GUIDs only, no identity)\n",
                  basename(rel), length(sp)))

    ## ---- EVERY PART, swept as text. The subject is the INDEX where one exists. ----------------
    src <- .staged(rel)
    if (is.na(src)) {
      cat(sprintf("  SKIP %s: not in the git index, so the published bytes cannot be read here.\n",
                  basename(rel)))
      skipped <<- c(skipped, sprintf("workbook parts for %s (not staged)", basename(rel)))
      next
    }
    on.exit(unlink(src), add = TRUE)
    pl <- suppressWarnings(utils::unzip(src, list = TRUE)$Name)
    ## ASSERTED ON PRESENCE, BECAUSE ITS CONTENTS CANNOT BE SWEPT. Each xl/printerSettings/*.bin is a
    ## Windows DEVMODE naming the print server the workbook was last set up against -- an internal
    ## UNC path. It is invisible to the byte sweep below: the part is stored as an ASCII space-
    ## separated HEX DUMP of UTF-16LE, so grepping the decompressed part for the hostname returns
    ## NOTHING while the hostname is plainly there once decoded twice. This gate read all 4500 bytes
    ## of one of these and reported clean (found 2026-09-08). Presence is the only property a sweep
    ## can honestly check, so presence is what is checked. Removal is lossless -- Excel regenerates
    ## print settings on demand -- and Publish/ strips these parts on the way out.
    .ps <- grep("^xl/printerSettings/", pl, value = TRUE)
    if (length(.ps)) { nW <- nW + length(.ps)
      for (x in .ps)
        cat(sprintf("  LEAK %s[%s]: printerSettings names a print server (hex-encoded; no text sweep sees it)\n",
                    basename(rel), x)) }
    for (pp in pl) {
      ## Opened and CLOSED explicitly -- unz() inside readBin() leaves the connection dangling and R
      ## warns about every one of them at exit, which buries the gate's own output.
      raw <- tryCatch({
        con <- unz(src, pp, open = "rb"); on.exit(close(con), add = TRUE)
        readBin(con, "raw", n = 8e6)
      }, error = function(e) raw(0))
      if (!length(raw)) next
      txt <- rawToChar(raw[raw != as.raw(0)]); Encoding(txt) <- "UTF-8"
      h <- unique(unlist(regmatches(txt, gregexpr(BAD, txt, ignore.case = TRUE, perl = TRUE))))
      h <- h[nzchar(trimws(h))]
      if (length(h)) { nW <- nW + length(h)
        for (x in h) cat(sprintf("  LEAK %s[%s]: %s\n", basename(rel), pp, substr(x, 1, 70))) }
      ## Structural, because these two carry internal text that no generic pattern describes.
      if (grepl("<headerFooter", txt, fixed = TRUE)) { nW <- nW + 1L
        cat(sprintf("  LEAK %s[%s]: a print headerFooter is set -- %s\n", basename(rel), pp,
                    substr(gsub("\\s+", " ", regmatches(txt, regexpr("<headerFooter.*?</headerFooter>|<headerFooter[^>]*/>", txt))[1]), 1, 90))) }
      if (grepl("absPath", txt, fixed = TRUE)) { nW <- nW + 1L
        cat(sprintf("  LEAK %s[%s]: an absPath records where the file was authored\n",
                    basename(rel), pp)) }
    }
  }
  if (length(wbs) && !nW) cat(sprintf("  clean (%d workbook(s))\n", length(wbs)))
  nV <- nV + nW
}
bad <- bad + nV

## The REPO ROOT ships too -- README.md and .gitignore are both published, and both sat outside every
## earlier sweep because they live one level above the version folder. The root README carried a
## cytometer serial in a worked example until 2026-08-21; .gitignore's own comments described what
## they exclude in terms that named the thing.
## THE GIT TOP-LEVEL, not dirname(). These four files live one level above the version folder in the
## development repo and AT the root in the flat published layout, so dirname() found them in ours and
## pointed one level above the repository in theirs -- where none of them exist, so the pass read
## nothing and still printed "clean". Asking git removes the guesswork and makes the same code cover
## both layouts instead of silently covering neither.
rootDir <- suppressWarnings(system2("git", c("-C", shQuote(ACTA_VERSION_DIR), "rev-parse",
                                             "--show-toplevel"), stdout = TRUE, stderr = FALSE))
## Braces, because a bare `else` on its own line is a syntax error at top level in R.
if (!(length(rootDir) == 1L && nzchar(rootDir) && dir.exists(rootDir))) {
  rootDir <- dirname(ACTA_VERSION_DIR)
}
## LICENSE and CITATION.cff ship too, and CITATION in particular is hand-edited metadata -- exactly
## the kind of file where a real name or an internal identifier gets typed in later and never
## reviewed. Added 2026-09-04 when they were created; the root sweep had a two-file hardcoded list.
rootTxt <- if (rootAlso) c("README.md", ".gitignore", "LICENSE", "CITATION.cff") else character(0)
if (length(rootTxt)) cat("\n######## repository root ########\n")
nR <- 0L
for (rel in rootTxt) {
  f <- file.path(rootDir, rel)
  if (!file.exists(f)) next
  ln <- readLines(f, warn = FALSE)
  i <- grep(BAD, ln, ignore.case = TRUE, perl = TRUE)
  for (k in i) { nR <- nR + 1L
    cat(sprintf("  LEAK %s:%d %s\n", rel, k, substr(trimws(ln[k]), 1, 70))) }
}
## NEVER PRINT "clean" AFTER READING NOTHING. `rootDir` is dirname(ACTA_VERSION_DIR), which in the
## development layout is the repository root but in the FLAT published layout is one level ABOVE the
## repository -- so none of the four names exist, every iteration took the `next` above, and this
## line printed "clean (0 file(s))" while covering nothing. (No live leak: in the flat layout those
## four files sit at the repo root and ARE swept by the tracked-text pass. The verdict was the
## defect, and it is the same "a check that cannot see its subject has not passed on it" rule this
## file states for itself elsewhere.) A zero count now goes to `skipped`, which makes the run
## INCOMPLETE rather than CLEAN.
.nRootRead <- sum(file.exists(file.path(rootDir, rootTxt)))
if (length(rootTxt) && !nR) {
  if (.nRootRead == 0L) {
    cat("  SKIP: none of the root files are at", rootDir, "-- nothing was read here.\n")
    skipped <- c(skipped, sprintf("the repository-root pass (no root files found at %s)", rootDir))
  } else {
    cat(sprintf("  clean (%d of %d file(s) read)\n", .nRootRead, length(rootTxt)))
  }
}
bad <- bad + nR

cat(sprintf("\n==== %s: %d finding(s) across %d case(s) + the shipped tree ====\n",
            if (bad) "LEAKS FOUND" else if (length(skipped)) "INCOMPLETE" else "CLEAN",
            bad, length(cases)))
## INCOMPLETE is not CLEAN and does not exit 0. "No findings" from a sweep that never ran is not
## evidence of anything, and the exit status is what a release gate reads.
if (length(skipped)) {
  cat("     NOT CHECKED: ", paste(skipped, collapse = "; "), "\n", sep = "")
  cat("     Re-run where the subject can be seen; do not treat this as a clean result.\n")
}
if (bad || length(skipped)) quit(status = 1)
