## SCAN A FINISHED BUNDLE FOR CONFIDENTIAL DATA, FROM THE OUTSIDE.
##
## This deliberately knows nothing about how the bundle was made. It opens the zip that would
## actually be sent and reads every file in it the way a recipient would -- which is the only
## check that could have caught the defect that prompted it.
##
## WHAT PROMPTED IT. The TiterPubs maintainer found that every rewritten FCS in the bundle we
## sent carried a `FILENAME` keyword holding the local absolute path: the account name (an email
## address on a managed machine) and the OneDrive library (the company and the programme).
## flowCore synthesises that keyword on read and persists it on write. An earlier in-house check
## looked at the ORIGINAL files, correctly found no FILENAME, and concluded there was no leak --
## the wrong artefact. The pipeline reported the scrub as successful throughout.
##
## THREE RULES, learned the hard way and all of them load-bearing:
##   1. SCAN THE OUTPUT, never the input. A scrub is a claim about what was written.
##   2. DECOMPRESS FIRST. An .xlsx is a zip and a PDF's text is Flate-encoded, so grepping the
##      container bytes finds nothing whether or not the string is in there -- an earlier sweep
##      in this project came out clean for exactly that reason while the value sat in
##      sharedStrings.xml.
##   3. READ STRUCTURE, not just text. FCS keywords live in a header, not in the data, and no
##      amount of text grepping over a binary finds a keyword reliably.

## What counts as a leak. Deliberately broad: a false positive costs a glance, a false negative
## costs a disclosure to an external party.
## `extra` is FIXED STRINGS. `extra_regex` is for patterns that are deliberately patterns.
##
## FIXED IS THE DEFAULT BECAUSE OF WHAT A SITE ACTUALLY TYPES. Measured on four realistic
## entries: 'C:\Users\<account>' is a PCRE compile error, 'Acme Corp (Gen2)' reads the
## parentheses as a capture group and matches the text WITHOUT them, and 'lot 5+5' reads 5+ as
## "one or more 5s" and matches 'lot 55'. Three of the four scanned CLEAN over a file that
## contained them. The natural thing to write here is a literal copied out of a leaked file, and
## a literal must work literally.
actaBundleScanPatterns <- function(extra = character(), extra_regex = character()) {
  home <- tryCatch(path.expand("~"), error = function(e) "")
  user <- tryCatch(Sys.info()[["user"]], error = function(e) "")
  node <- tryCatch(Sys.info()[["nodename"]], error = function(e) "")
  p <- c(
    "home directory"      = if (nzchar(home)) home else NA,
    "account name"        = if (nzchar(user)) user else NA,
    "machine name"        = if (nzchar(node)) node else NA,
    ## The account also appears path-encoded, with separators flattened to '-', in scratch and
    ## sandbox paths -- which is exactly the form the FILENAME leak took.
    "account, path-encoded" = if (nzchar(user)) gsub("[^A-Za-z0-9]", "-", user) else NA,
    "OneDrive library"    = "OneDrive-SharedLibraries",
    "cloud storage path"  = "CloudStorage",
    "a POSIX home path"   = "/Users/",
    ## FIXED STRING, SO ONE BACKSLASH EACH. This read "C:\\\\Users\\\\" -- in R source the ELEVEN
    ## characters C:\\Users\\, two backslashes in every position -- and it is matched with
    ## fixed = TRUE, so it could only match text whose backslashes were already doubled. A real
    ## Windows path has one. Measured: the scanner returned 0 findings over a file containing
    ## `C:\Users\jdoe\OneDrive\run.fcs`, which left the blocking set with NO working Windows
    ## coverage at all -- every other path pattern here is the macOS CloudStorage layout. The
    ## quadrupling is the escaping a REGEX would need, and this is not a regex. Same family as the
    ## standing note that grepl("\\\\", fixed = TRUE) never matches a Windows path.
    "a Windows user path" = "C:\\Users\\",
    ## Windows syncs a shared library to `C:\Users\<u>\OneDrive - <Org>\`, which matches none of
    ## the macOS forms above, so the organisation name would have shipped unnoticed.
    "OneDrive for Windows" = "OneDrive - ",
    "an email address"    = "[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+[.][A-Za-z]{2,}")
  p <- p[!is.na(p)]
  ## Validated HERE, once, so a broken pattern stops the scan instead of quietly matching nothing.
  for (nm in names(extra_regex))
    tryCatch(grepl(extra_regex[[nm]], "probe", perl = TRUE),
             error = function(e) stop(sprintf("bundle scan regex '%s' is not valid: %s",
                                              nm, conditionMessage(e)), call. = FALSE))
  out <- c(p, extra, extra_regex)
  ## A DUPLICATED NAME SILENCES EVERY PATTERN AFTER THE FIRST. .bsHits() addresses patterns by name
  ## with `[[`, which returns only the first match, so a site that labels two constructs with one
  ## name gets the second never evaluated -- the loop runs twice over the same pattern, with no
  ## warning. That is the behaviour this file's header calls the worst available to a leak scan,
  ## reached by a different route than the one already guarded. Measured: extra = c(construct=A,
  ## construct=B) found A twice and B never.
  if (anyDuplicated(names(out)))
    stop(sprintf(paste0("bundle scan: duplicate pattern name(s): %s. A pattern addressed by a ",
                        "duplicated name is never evaluated, so the scan would be silently thinned."),
                 paste(unique(names(out)[duplicated(names(out))]), collapse = ", ")), call. = FALSE)
  ## Severity is name-based, so a site pattern reusing an advisory name would be downgraded to
  ## "reported but does not block" without anyone asking for that.
  if (any(names(c(extra, extra_regex)) %in% ACTA_BUNDLE_ADVISORY))
    stop(sprintf(paste0("bundle scan: a site pattern may not reuse the advisory name %s -- it ",
                        "would be reported but would not block."),
                 paste(sQuote(intersect(names(c(extra, extra_regex)), ACTA_BUNDLE_ADVISORY)),
                       collapse = ", ")), call. = FALSE)
  out
}

## WHICH FINDINGS STOP A BUILD, AND WHICH ONLY GET REPORTED.
##
## BLOCKING is everything that identifies the machine, the account or the organisation. None of
## it is ever part of a titration, no submission needs it, and scrub_confidential is not the
## control for it -- that switch is about LAB identifiers (operator, ELN, panel name). A home
## path has no business in a bundle whether the operator asked for scrubbing or not.
##
## ADVISORY is the email address, and it is advisory ON PURPOSE: a submission legitimately
## carries a submitter address, so failing on the shape would block every real bundle. It is
## still reported, because an UNEXPECTED address is worth a human glance.
ACTA_BUNDLE_ADVISORY <- c("an email address")

actaBundleSeverity <- function(pattern)
  ifelse(pattern %in% ACTA_BUNDLE_ADVISORY, "advisory", "blocking")

## `regex_names` are matched as PERL REGEX; everything else is matched LITERALLY, because a home
## path or a library name is full of characters a regex would read as syntax.
##
## CALLER-SUPPLIED EXTRAS ARE REGEX. They are site patterns -- the same shape as
## acta.bundle_blocked_markers and acta.test_material_rules -- and a site wants "XYZ-[0-9]+",
## not one literal lot number. It is also the safer default here: a literal passed as a regex
## over-matches slightly (a "." matching any character), and for a leak scan over-matching costs
## a glance while under-matching costs a disclosure.
.bsHits <- function(txt, pats, where, regex_names = c("an email address")) {
  out <- list()
  for (nm in names(pats)) {
    fx <- !(nm %in% regex_names)
    ## A PATTERN THAT CANNOT BE EVALUATED IS AN ERROR, NOT A PASS. This swallowed compile
    ## failures and returned FALSE, so a malformed pattern made the gate report "clean" -- the
    ## single worst behaviour available to a leak scan. Measured: 'C:\Users\<account>' is a
    ## PCRE error (\U is not supported), and the bundle containing that exact path scanned clean.
    hit <- tryCatch(grepl(pats[[nm]], txt, fixed = fx, perl = !fx),
                    error = function(e) stop(sprintf(
                      "bundle scan pattern '%s' could not be evaluated (%s). A pattern that cannot run must not be read as 'no leak'.",
                      nm, conditionMessage(e)), call. = FALSE))
    if (any(hit)) {
      v <- txt[hit][1]
      out[[length(out) + 1L]] <- data.frame(
        where = where, pattern = nm,
        sample = substr(gsub("[[:space:]]+", " ", v), 1, 160), stringsAsFactors = FALSE)
    }
  }
  out
}

## A READER THAT CANNOT READ ITS ARTEFACT IS NOT A CLEAN SCAN. All three structured readers used
## to `return(list())` when the reader was missing or the file would not parse -- the SAME value as
## "scanned, found nothing" -- so the gate printed "clean" over a file it had never opened.
## .bsHits has said exactly this about an unevaluable PATTERN since it was written; this is the
## same rule for an unreadable FILE.
##
## THE PDF CASE WAS THE LIVE ONE. pdftools is in Suggests, so install_github() does not install it,
## which made the DEFAULT PUBLIC INSTALL blind to every report PDF -- and the report is the one
## bundle artefact with no scrub pass of its own (only `instructions` and `export` are scrubbed;
## the report is file.copy'd verbatim). Measured on a report carrying a home path: 2 blocking
## findings with pdftools present, 0 without, and a raw-byte grep sees nothing either way because
## PDF text is Flate-encoded.
##
## "could not be read" is deliberately NOT in ACTA_BUNDLE_ADVISORY, so it is blocking.
.bsUnread <- function(where, why)
  list(data.frame(where = where, pattern = "could not be read", sample = why,
                  stringsAsFactors = FALSE))

## Every keyword of every FCS. Read as STRUCTURE: read.FCSheader() returns the TEXT segment's
## keywords, which is where FILENAME lives and where a text grep over a binary would miss it.
.bsFcs <- function(f, rel, pats, rx = "an email address") {
  k <- tryCatch(flowCore::read.FCSheader(f)[[1]], error = function(e) NULL)
  if (is.null(k)) return(.bsUnread(sprintf("%s [FCS]", rel),
                                   "read.FCSheader() could not parse this file"))
  v <- unlist(k); if (!length(v)) return(list())
  out <- .bsHits(paste(names(v), v, sep = " = "), pats, sprintf("%s [FCS keyword]", rel), rx)
  ## Flagged on its own terms as well as by pattern: a keyword the FCS standard does not define,
  ## written by a library rather than the instrument, is the shape this class of leak takes.
  nonstd <- setdiff(names(v)[!startsWith(names(v), "$")], c("FCSversion", "ORIGINALGUID", "GUID"))
  if ("FILENAME" %in% toupper(nonstd))
    out <- c(out, list(data.frame(where = sprintf("%s [FCS keyword]", rel),
                                  pattern = "FILENAME keyword present",
                                  sample = substr(v[[which(toupper(names(v)) == "FILENAME")[1]]], 1, 160),
                                  stringsAsFactors = FALSE)))
  out
}

## An xlsx is a zip: unpack it and read the XML, or the sweep is theatre.
.bsXlsx <- function(f, rel, pats, rx = "an email address") {
  d <- file.path(tempdir(), sprintf("bs_%s", basename(tempfile(""))))
  on.exit(unlink(d, recursive = TRUE), add = TRUE)
  ## TEST WHAT CAME OUT, NOT WHETHER IT THREW. utils::unzip() WARNS on a corrupt archive and
  ## returns character(0); it does not error, so a tryCatch(error=) never fires and the
  ## scan went on to read an empty directory and report nothing. The extracted-file list is
  ## the honest test. An xlsx that unpacks to nothing is not a readable workbook either way.
  ex <- tryCatch(suppressWarnings(utils::unzip(f, exdir = d)), error = function(e) character(0))
  if (!length(ex)) return(.bsUnread(sprintf("%s [xlsx]", rel),
                                    "the workbook could not be unzipped"))
  ## THE SAME TWO CHECKS THE CALLER GOT, ONE LEVEL DOWN. actaBundleScan() was fixed to compare
  ## extracted against declared and to enumerate with all.files = TRUE; neither was propagated
  ## here. `_rels/.rels` is DOT-NAMED, so list.files() omitted it from every workbook -- measured:
  ## parts=26 scanned=25 on the shipped instructions workbook, and 1 part short on all of them.
  ## That part carries package-level relationships, including any TargetMode="External" whose
  ## Target is an absolute file:///Users/<account>/... URL.
  dec <- tryCatch(nrow(utils::unzip(f, list = TRUE)), error = function(e) NA_integer_)
  if (is.na(dec) || length(ex) != dec)
    return(.bsUnread(sprintf("%s [xlsx]", rel), "the workbook could not be fully unpacked"))
  txt <- unlist(lapply(list.files(d, recursive = TRUE, full.names = TRUE,
                                  all.files = TRUE, no.. = TRUE),
                       function(x) tryCatch(readLines(x, warn = FALSE), error = function(e) character(0))))
  .bsHits(txt, pats, sprintf("%s [xlsx xml]", rel), rx)
}

## A PDF's text is Flate-encoded; pdf_text() is the decompressed view.
.bsPdf <- function(f, rel, pats, rx = "an email address") {
  if (!requireNamespace("pdftools", quietly = TRUE))
    return(.bsUnread(sprintf("%s [pdf]", rel),
                     "pdftools is not installed, so this PDF's text was never read"))
  txt <- tryCatch(pdftools::pdf_text(f), error = function(e) NULL)
  if (is.null(txt)) return(.bsUnread(sprintf("%s [pdf]", rel),
                                     "pdftools could not read this PDF"))
  .bsHits(txt, pats, sprintf("%s [pdf text]", rel), rx)
}

## Scan a TiterPubs bundle for confidential data
##
## @param path A bundle .zip, or a directory holding an unpacked bundle.
## @param extra Named character vector of additional patterns (fixed strings).
## @return A data frame of findings, one row per file x pattern. Zero rows means clean.
actaBundleScan <- function(path, extra = character(), extra_regex = character(), quiet = FALSE) {
  pats <- actaBundleScanPatterns(extra, extra_regex)
  ## Only the built-in email rule and explicitly-declared regexes are treated as patterns.
  rx   <- c("an email address", names(extra_regex))
  root <- path
  if (grepl("[.]zip$", path, ignore.case = TRUE)) {
    root <- file.path(tempdir(), sprintf("bscan_%s", basename(tempfile(""))))
    unlink(root, recursive = TRUE); dir.create(root, recursive = TRUE)
    ## THE EXTRACTED LIST IS THE HONEST TEST HERE TOO -- the same defect .bsXlsx was just fixed
    ## for, one level up, on the reader that actually gates the upload. utils::unzip() WARNS on a
    ## truncated or unreadable archive and returns character(0) rather than erroring, so a bundle
    ## the scanner could not open enumerated zero files and scanned "clean". Measured: a truncated
    ## bundle produced 0 findings, passed actaAssertBundleClean() and was left on disk.
    ex  <- tryCatch(suppressWarnings(utils::unzip(path, exdir = root)),
                    error = function(e) character(0))
    dec <- tryCatch(nrow(utils::unzip(path, list = TRUE)), error = function(e) NA_integer_)
    if (!length(ex) || is.na(dec) || length(ex) != dec)
      stop(sprintf(paste0("actaBundleScan(): %s could not be fully extracted (%d of %s entries). ",
                          "A bundle that cannot be read must not be reported as clean."),
                   basename(path), length(ex),
                   if (is.na(dec)) "an unknown number of" else as.character(dec)), call. = FALSE)
  }
  if (!dir.exists(root)) stop("actaBundleScan(): nothing to scan at ", path)

  ## all.files = TRUE TO MATCH .tpZip(). The zipper enumerates with all.files = TRUE and this
  ## did not, so every dot-file -- .DS_Store, AppleDouble ._* -- was PUBLISHED in the bundle
  ## and never scanned, contents or entry name. Measured on a staged Plots/: zip 3 entries,
  ## scanner 1. no.. = TRUE keeps "." and ".." out of the walk.
  files <- list.files(root, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  ## The PATH INSIDE THE ZIP is itself published -- a folder named after the operator leaks
  ## without any file having to be opened.
  found <- .bsHits(sub(paste0("^", root, "/?"), "", files), pats, "[entry name]", rx)

  for (f in files) {
    rel <- sub(paste0("^", root, "/?"), "", f)
    ext <- tolower(tools::file_ext(f))
    found <- c(found, switch(ext,
      fcs  = .bsFcs(f, rel, pats, rx),
      xlsx = .bsXlsx(f, rel, pats, rx),
      pdf  = .bsPdf(f, rel, pats, rx),
      ## Everything else that is plausibly text. Binaries are read with warn = FALSE and simply
      ## yield noise, which is cheaper than maintaining a list of what to skip.
      if (ext %in% c("png", "jpg", "jpeg", "gif", "zip", "rds")) list()
      else .bsHits(tryCatch(readLines(f, warn = FALSE), error = function(e) character(0)),
                   pats, rel, rx)))
  }
  res <- if (length(found)) do.call(rbind, found) else
    data.frame(where = character(0), pattern = character(0), sample = character(0),
               stringsAsFactors = FALSE)
  res$severity <- if (nrow(res)) actaBundleSeverity(res$pattern) else character(0)
  if (!isTRUE(quiet)) {
    nb <- sum(res$severity == "blocking")
    ## SAY WHAT WAS CHECKED, NOT "no findings". This scan looks for OPERATOR AND MACHINE
    ## identifiers -- home directories, account and machine names, the OneDrive library path, an
    ## e-mail shape. It has NO pattern for a site-confidential token: an ELN or experiment id, a
    ## programme code, an internal panel name. Those are handled by substitution
    ## (scrub_confidential), not by this gate, and substitution does not reach every carrier --
    ## an FCS file keeps the name the operator gave it, and that name is published as the zip
    ## entry. A bare "no findings" is read as "nothing confidential is in here", which is a
    ## claim this function cannot make and never could.
    if (!nrow(res))
      message(sprintf("[ACTA] bundle scan: %d file(s), no operator or machine identifiers found.",
                      length(files)))
    else message(sprintf("[ACTA] bundle scan: %d finding(s) across %d file(s) -- %d blocking, %d advisory.",
                         nrow(res), length(unique(res$where)), nb, nrow(res) - nb))
  }
  res
}

## THE GATE. Scans a finished bundle and DELETES IT if anything blocking is in it.
##
## Deleting rather than warning is the whole point: a bundle that failed its own leak check must
## not be sitting on disk where it can be uploaded by someone reading the folder rather than the
## log. Three separate leaks shipped in one day because each was caught by a scan run AFTER the
## build and by eye -- FCS keywords, the Info sheet, the export's audit sheets. The scan only
## prevents anything if the build cannot finish while it fails.
##
## Runs regardless of scrub_confidential: the blocking set is machine and organisation
## identifiers, which that switch was never about.
actaAssertBundleClean <- function(zipf, extra = character(), extra_regex = character()) {
  if (!file.exists(zipf)) return(invisible(NULL))
  res <- tryCatch(actaBundleScan(zipf, extra = extra, extra_regex = extra_regex, quiet = TRUE),
                  error = function(e) e)
  ## NOT A WARNING. .bsHits() stops rather than return "no leak" when a PATTERN cannot run; its
  ## only caller then downgraded the same condition for the WHOLE SCAN to a warning and left the
  ## zip in place, so an unverifiable bundle still reached "ready to upload". The zip goes.
  if (inherits(res, "error")) {
    unlink(zipf)
    stop(sprintf(paste0("TiterPubs bundle could not be verified (%s) and was deleted. A bundle ",
                        "that cannot be scanned must not be published."),
                 conditionMessage(res)), call. = FALSE)
  }
  adv <- res[res$severity == "advisory", , drop = FALSE]
  for (i in seq_len(nrow(adv)))
    message(sprintf("  bundle scan (advisory): %s in %s", adv$pattern[i], adv$where[i]))
  bad <- res[res$severity == "blocking", , drop = FALSE]
  ## NAMES ITS SCOPE -- see the note in actaBundleScan(). "clean" was the word an operator read
  ## just before uploading, and the one thing this gate cannot tell them is whether their own
  ## identifiers are in the bundle: it has no pattern for an ELN id or a panel name, and the FCS
  ## files carry whatever filenames the operator gave them.
  if (!nrow(bad)) {
    message("  bundle scan: no operator or machine identifiers found.")
    message("  (This gate does not check for ELN ids, programme codes or panel names -- those ",
            "rely on scrub_confidential, which does not rename FCS files.)")
    return(invisible(res))
  }
  unlink(zipf)
  stop(sprintf(paste0("TiterPubs bundle FAILED its leak scan and was deleted: %d finding(s).\n%s\n",
                      "Nothing was left on disk that could be uploaded by mistake."),
               nrow(bad),
               paste(sprintf("  - %s in %s: %s", bad$pattern, bad$where,
                             substr(bad$sample, 1, 110)), collapse = "\n")), call. = FALSE)
}
