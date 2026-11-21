## actaBundleScan() / actaAssertBundleClean(): the leak scan that GATES a TiterPubs bundle.
##
## WHY IT IS A GATE AND NOT A REPORT. Three separate leaks reached a finished bundle on one day
## -- a FILENAME keyword holding the local absolute path, a submitter address in the Info sheet,
## the same address again in the export's audit sheets. Every one was caught by a scan run after
## the build, by eye. A scan only prevents anything if the build cannot finish while it fails.
##
## The scan reads the zip the way a RECIPIENT would and knows nothing about how it was made,
## which is the only way it could have caught any of those three.

ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  r <- tryCatch(list(v = isTRUE(pass), d = NULL),
                error = function(e) list(v = FALSE, d = paste("ERROR:", conditionMessage(e))))
  d <- r$d %||% tryCatch(paste(as.character(detail), collapse = " "), error = function(e) "")
  if (!isTRUE(r$v)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
`%||%` <- function(a, b) if (is.null(a)) b else a

suppressMessages(library(ACTA))
scan_  <- ACTA:::actaBundleScan
assert <- ACTA:::actaAssertBundleClean

mkzip <- function(files) {
  d <- file.path(tempdir(), sprintf("bz_%s", basename(tempfile(""))))
  dir.create(d, recursive = TRUE)
  for (nm in names(files)) writeLines(files[[nm]], file.path(d, nm))
  z <- file.path(d, "bundle.zip")
  old <- setwd(d); on.exit(setwd(old), add = TRUE)
  utils::zip(z, files = names(files), flags = "-Xq9")
  z
}

## ---- severity split ---------------------------------------------------------------------------
chk("an email is ADVISORY -- a submission legitimately carries one",
    ACTA:::actaBundleSeverity("an email address") == "advisory")
for (p in c("home directory", "account name", "OneDrive library", "a POSIX home path",
            "FILENAME keyword present"))
  chk(sprintf("'%s' is BLOCKING", p), ACTA:::actaBundleSeverity(p) == "blocking")

## ---- a clean bundle passes and survives ---------------------------------------------------------
clean <- mkzip(list("notes.txt" = "CD4 BUV496 clone SK3, lot 5315516"))
## THE EXTRACTION ROOT IS NOT A PATTERN, and this runs on every platform on purpose.
##
## actaBundleScan() used to strip its temp root from each entry name with
## sub(paste0("^", root, "/?"), ...), i.e. as a REGEX. On Windows the root is
## C:\\Users\\RUNNER~1\\AppData\\... where \\U, \\R, \\A and \\L are escapes, so the strip
## matched nothing, the full local path became the "entry name", and it contains C:\\Users\\ --
## a BLOCKING pattern. The gate refused every bundle a Windows operator built, naming its own
## scratch directory as the leak. Invisible on macOS and Linux, where the root has no
## backslashes and no metacharacters, so only the Windows runner could ever have caught it.
##
## DRIVES THE REAL FUNCTION, not a copy of its arithmetic: actaBundleScan() uses the path it is
## given as the root when that path is a directory, and a backslash is a legal filename
## character on POSIX -- so the Windows shape is reproducible wherever this suite runs.
## NOT ON WINDOWS, and the reason is the point of the fixture. It needs a directory literally
## named `C:\\Users\\...` to reproduce the Windows temp root on a POSIX machine -- and on Windows
## itself that name CANNOT EXIST: ":" is reserved and "\\" is the separator, so dir.create()
## fails and the test errors out. Measured on the CI runner: this fixture is what turned the
## windows job red on v3.6.1 after the real defect was already fixed.
##
## Nothing is lost by skipping it there. On Windows the REAL extraction root is that shape
## already, so "a clean bundle scans with no findings" below exercises the same code path
## natively -- which is exactly how the original defect was caught.
if (.Platform$OS.type == "windows") {
  cat("  [skip] the synthetic Windows-root fixture needs a path Windows cannot name;\n",
      "        the real root has that shape here, so the clean-bundle check covers it.\n", sep = "")
} else local({
  ## The first name reproduces the Windows temp root EXACTLY, `C:\\Users\\` and all -- a colon
  ## and a backslash are both legal in a POSIX filename, so the real failing input is
  ## constructible here. An earlier version of this fixture used backslashes WITHOUT the
  ## `C:\\Users\\` prefix and passed against the broken code: the strip failed exactly as it
  ## does on Windows, but the leftover path tripped no pattern, so the gate proved nothing.
  for (nm in c("C:\\Users\\RUNNER~1\\AppData\\Local\\Temp", "meta+b(race)[1]", "plain")) {
    d <- file.path(tempdir(), paste0("rootchk_", basename(tempfile("")), "_", nm))
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    writeLines("nothing of interest here", file.path(d, "notes.txt"))
    r <- scan_(d, quiet = TRUE)
    chk(sprintf("a root containing %-24s yields no finding of its own",
                sQuote(substr(nm, 1, 24))),
        nrow(r) == 0L,
        if (nrow(r)) paste(r$pattern[1], "in", substr(r$where[1], 1, 70)) else "")
    unlink(d, recursive = TRUE)
  }
})

chk("a clean bundle scans with no findings", nrow(scan_(clean, quiet = TRUE)) == 0L)
invisible(assert(clean))
chk("...and the gate leaves it on disk", file.exists(clean))

## ---- the submitter placeholder does NOT fail the build ------------------------------------------
## This is the case that would make the gate useless if it were wrong: every real bundle carries
## a submitter address, so blocking on the shape would block everything.
mail <- mkzip(list("t.txt" = "Email: johndoe@anonymouscompany.com"))
r <- scan_(mail, quiet = TRUE)
chk("an email is found", nrow(r) >= 1L)
chk("...and classed advisory, not blocking", all(r$severity == "advisory"), unique(r$severity))
invisible(assert(mail))
chk("...so the bundle survives the gate", file.exists(mail))

## ---- a real leak fails the build AND removes the artefact ---------------------------------------
## The exact shape that shipped: an absolute path carrying the account under /Users.
leak <- mkzip(list("t.txt" = sprintf("FILENAME = %s/Library/CloudStorage/x/y.fcs",
                                     path.expand("~"))))
r2 <- scan_(leak, quiet = TRUE)
chk("a home path is found", any(r2$severity == "blocking"), unique(r2$pattern))
err <- tryCatch({ assert(leak); NA_character_ }, error = function(e) conditionMessage(e))
chk("the gate STOPS the build", !is.na(err), err)
chk("...naming the finding so it can be fixed",
    !is.na(err) && grepl("FAILED its leak scan", err, fixed = TRUE), substr(err, 1, 80))
chk("...and DELETES the bundle, so nothing uploadable is left behind", !file.exists(leak))

## ---- the OneDrive library name alone is enough --------------------------------------------------
lib <- mkzip(list("t.txt" = "path: /somewhere/OneDrive-SharedLibraries-Example/run"))
chk("the OneDrive library name blocks on its own",
    any(scan_(lib, quiet = TRUE)$severity == "blocking"))
chk("...and that bundle is deleted too",
    { tryCatch(assert(lib), error = function(e) NULL); !file.exists(lib) })

## ---- extra patterns from the caller -------------------------------------------------------------
## ---- caller patterns: FIXED by default, regex only when declared -------------------------------
## Measured, and the reason for the split: three of four realistic site entries scanned CLEAN
## over a file containing them when extras were regex -- a Windows path is a PCRE compile error,
## "(Gen2)" became a capture group, "5+5" became "one or more 5s". The natural thing to write is
## a literal copied out of a leaked file, so a literal must work literally.
## NOT `C:\\Users\\...` ANY MORE. This block tests CALLER-SUPPLIED extras, so the base text must
## not trip a built-in pattern. It used a Windows user path, which was safe only because the
## built-in Windows pattern was a dead literal; fixing that pattern made these two assertions
## fail, correctly. A D: path keeps the backslash -- the regex-syntax character that is the
## point of the fixture -- without being a home path the blocking set now catches.
site <- mkzip(list("t.txt" = "programme XYZ-0720 at D:\\Runs\\someone (Gen2) lot 5+5"))
chk("no caller pattern -> clean", nrow(scan_(site, quiet = TRUE)) == 0L)
for (lit in c("D:\\Runs\\someone", "(Gen2)", "lot 5+5"))
  chk(sprintf("a LITERAL containing regex syntax blocks: %s", lit),
      any(scan_(site, extra = setNames(lit, "site"), quiet = TRUE)$severity == "blocking"))
chk("a literal is NOT treated as a pattern",
    nrow(scan_(site, extra = c(site = "XYZ-[0-9]+"), quiet = TRUE)) == 0L)
chk("a DECLARED regex blocks",
    any(scan_(site, extra_regex = c(site = "XYZ-[0-9]+"), quiet = TRUE)$severity == "blocking"))
## The worst available behaviour for a leak gate is "the pattern broke, so nothing is wrong".
chk("an invalid regex STOPS the scan rather than reading as clean",
    inherits(tryCatch(scan_(site, extra_regex = c(bad = "XYZ-[0-9"), quiet = TRUE),
                      error = function(e) e), "error"))

## ---- fail-closed, and the platforms the pattern set actually covers ---------------------------
## All five assertions below are for defects found by the 2026-11-05 pre-publication review. Each
## one passed as "clean" before the fix, which is the only reason they are worth a gate.

cat("-- a reader that cannot read its artefact is not a clean scan --\n")
d <- file.path(tempdir(), paste0("bsfc_", basename(tempfile(""))))
dir.create(d, recursive = TRUE)
writeLines("not an archive", file.path(d, "broken.xlsx"))
writeLines("not an fcs",     file.path(d, "broken.fcs"))
r <- scan_(d, quiet = TRUE)
chk("an unreadable xlsx is a BLOCKING finding, not silence",
    any(r$pattern == "could not be read" & grepl("xlsx", r$where)) &&
    all(r$severity[r$pattern == "could not be read"] == "blocking"))
chk("an unreadable fcs is a BLOCKING finding, not silence",
    any(r$pattern == "could not be read" & grepl("FCS", r$where)))
## pdftools is in Suggests, so install_github() does not install it: the DEFAULT PUBLIC INSTALL
## was blind to every report PDF, and the report is the one bundle artefact with no scrub of its
## own. Simulated by shadowing requireNamespace in the function's own environment, because the
## package is installed here and cannot be un-installed for one assertion.
pdfd <- file.path(tempdir(), paste0("bspdf_", basename(tempfile(""))))
dir.create(pdfd, recursive = TRUE)
grDevices::pdf(file.path(pdfd, "ACTA_Report.pdf"), width = 6, height = 3)
plot.new(); text(0.5, 0.5, "a clean report"); invisible(grDevices::dev.off())
.bsPdf <- ACTA:::.bsPdf
noPdftools <- .bsPdf
environment(noPdftools) <- local({ e <- new.env(parent = environment(.bsPdf))
                                   e$requireNamespace <- function(...) FALSE; e })
res <- noPdftools(file.path(pdfd, "ACTA_Report.pdf"), "ACTA_Report.pdf",
                  ACTA:::actaBundleScanPatterns())
chk("pdftools missing is a BLOCKING finding, not an empty scan",
    length(res) == 1L && identical(res[[1]]$pattern, "could not be read") &&
    identical(unname(ACTA:::actaBundleSeverity("could not be read")), "blocking"))
chk("...and with pdftools present a clean PDF still reports nothing",
    length(.bsPdf(file.path(pdfd, "ACTA_Report.pdf"), "ACTA_Report.pdf",
                  ACTA:::actaBundleScanPatterns())) == 0L)

cat("-- the scanner must enumerate exactly what the zipper packs --\n")
## .tpZip() uses all.files = TRUE and the scanner did not, so .DS_Store and AppleDouble ._* were
## published and never read -- neither contents nor entry name.
dd <- file.path(tempdir(), paste0("bsdot_", basename(tempfile(""))))
dir.create(file.path(dd, "Plots"), recursive = TRUE)
writeLines("plain", file.path(dd, "Plots", "a.png"))
## BUILT AT RUNTIME, not written as a literal. This gate's own source is swept by
## verify_oq_sanitised.R, and a home-path literal here is a finding in a tracked file -- it
## was, on first write. Same reason the Windows fixture above is assembled from pieces.
writeLines(paste0("/", "Users", "/someone/secret"), file.path(dd, "Plots", ".DS_Store"))
chk("a dot-file is scanned, because the bundle would publish it",
    any(grepl("DS_Store", scan_(dd, quiet = TRUE)$where)))

cat("-- Windows paths, which had NO working coverage --\n")
## "C:\\\\Users\\\\" in R source is C:\\Users\\ -- two backslashes in every position -- matched
## with fixed = TRUE. A real path has one, so the only Windows pattern in the blocking set could
## never fire. Built from pieces here so the fixture cannot be "corrected" into the same bug.
winDir <- file.path(tempdir(), paste0("bswin_", basename(tempfile(""))))
dir.create(winDir, recursive = TRUE)
bs <- "\\"
writeLines(paste0("$FIL = C:", bs, "Users", bs, "jdoe", bs, "OneDrive - Acme Corp", bs, "run.fcs"),
           file.path(winDir, "t.txt"))
rw <- scan_(winDir, quiet = TRUE)
chk("a real Windows user path BLOCKS", "a Windows user path" %in% rw$pattern,
    paste(rw$pattern, collapse = ", "))
chk("the Windows OneDrive form, which matches none of the macOS patterns, BLOCKS",
    "OneDrive for Windows" %in% rw$pattern)

cat("-- a mapped network drive, the one path shape none of the above cover --\n")
## A core that keeps its FCS on a share rather than in OneDrive writes `\\server\Flow\...` into
## every FILENAME keyword. That string has no C:\Users\, no /Users/, no CloudStorage and no
## OneDrive, so it passed every blocking pattern -- while naming an internal file server, which is
## precisely what the blocking set is for. Same string reaches `_rels` as a file:// authority.
uncDir <- file.path(tempdir(), paste0("bsunc_", basename(tempfile(""))))
dir.create(uncDir, recursive = TRUE)
writeLines(paste0("$FIL = ", bs, bs, "NAS01", bs, "Flow", bs, "Titrations", bs, "run.fcs"),
           file.path(uncDir, "t.txt"))
ru <- scan_(uncDir, quiet = TRUE)
chk("a UNC path BLOCKS", "a UNC network path" %in% ru$pattern,
    paste(ru$pattern, collapse = ", "))
chk("...and nothing else in the blocking set was already catching it",
    identical(setdiff(ru$pattern, "a UNC network path"), character(0)),
    paste(ru$pattern, collapse = ", "))

relDir <- file.path(tempdir(), paste0("bsrel_", basename(tempfile(""))))
dir.create(relDir, recursive = TRUE)
writeLines('<Relationship TargetMode="External" Target="file://NAS01/Flow/map.xlsx"/>',
           file.path(relDir, "t.txt"))
rr <- scan_(relDir, quiet = TRUE)
chk("a file:// URL with a HOST blocks", "a non-local file:// URL" %in% rr$pattern,
    paste(rr$pattern, collapse = ", "))

## THE QUIET HALF, and the half that matters most here: general FQDN masking was added and
## REMOVED TWICE for over-redaction, so these two patterns must fire on a host-bearing path and
## on nothing else. file:///Users/ is already the POSIX rule's job; localhost names nothing.
quietDir <- file.path(tempdir(), paste0("bsq_", basename(tempfile(""))))
dir.create(quietDir, recursive = TRUE)
writeLines(c("file://localhost/tmp/run.fcs", "file:///C:/Temp/run.fcs",
             "CD4 BV421 clone RPA-T4", "p < 0.05", "5 / 95 split",
             "see acta.internal for the SOP", "host flowcore.corp.example.com"),
           file.path(quietDir, "t.txt"))
rq <- scan_(quietDir, quiet = TRUE)
chk("neither new pattern fires on a localhost, drive-letter or bare-hostname line",
    !any(c("a UNC network path", "a non-local file:// URL") %in% rq$pattern),
    paste(rq$pattern, collapse = ", "))

## .bsHits() picks fixed-vs-regex BY NAME. A built-in regex missing from that list is matched
## literally, matches nothing, and the reader reports CLEAN. Assert the list is the one constant.
##
## THE LOGIN IS PINNED, NOT INHERITED, and that is the whole lesson of this assertion.
##
## `account, path-encoded` is emitted ONLY when the login contains a non-alphanumeric character --
## for an alphanumeric one it is a byte-copy of `account name`, and that duplicate was removed in
## 3.6.12 (see the note in actaBundleScanPatterns()). The first version of this check called
## actaBundleScanPatterns() with NO ARGUMENTS, so it inherited whoever ran it:
##
##   an e-mail-shaped login -> flattens to a DIFFERENT string -> pattern emitted -> PASS
##   runner / runneradmin (both CI runners) -> unchanged      -> pattern absent  -> FAIL
##
## So it was green on the author's Mac and red on every ordinary account, which is the first thing
## the private CI ever caught. A machine-dependent assertion is not an assertion; `user` and `node`
## are arguments precisely so a rule keyed on this machine's particulars can be proven on another.
## `first.last` is deliberately NOT e-mail-shaped: it only needs a non-alphanumeric character to
## make the path-encoded form differ, and an address here would trip the scan's own e-mail rule
## in a file that ships publicly. The first draft of this line did exactly that.
.bsAllNames <- names(ACTA:::actaBundleScanPatterns(user = "first.last", node = "host"))
chk("every built-in regex pattern is declared in ACTA_BUNDLE_REGEX",
    all(ACTA:::ACTA_BUNDLE_REGEX %in% .bsAllNames),
    paste(setdiff(ACTA:::ACTA_BUNDLE_REGEX, .bsAllNames), collapse = ", "))
## THE OTHER DIRECTION, which is the fail-open one: a pattern that IS a regex but is NOT named in
## the constant gets matched with fixed = TRUE, finds nothing, and the reader reports clean.
.bsRegexish <- grep("(?<!\\\\)[\\\\^$.|?*+()\\[\\]]", .bsAllNames, value = TRUE, perl = TRUE)
chk("...and no undeclared pattern name looks like a regex",
    !length(setdiff(.bsRegexish, ACTA:::ACTA_BUNDLE_REGEX)),
    paste(setdiff(.bsRegexish, ACTA:::ACTA_BUNDLE_REGEX), collapse = ", "))
## A CONTROL. If someone makes the pattern unconditional to "fix" a red build, the duplicate it was
## removed for comes back and every account finding is reported twice again.
chk("...and `account, path-encoded` is still CONDITIONAL on a non-alphanumeric login",
    !("account, path-encoded" %in%
        names(ACTA:::actaBundleScanPatterns(user = "runner", node = "host"))))

cat("-- the bundled workbook must not name the operator in docProps --\n")
## docProps/core.xml and app.xml were reached by NOTHING on the bundle path: .tpCleanWorkbook only
## dropped parts, and .tpApplyRepl (which both the scrub and the e-mail strip route through)
## targets sharedStrings and the sheet XML. So Excel's <cp:lastModifiedBy> shipped into a CC BY 4.0
## archive while Layout_Plate!Operator was correctly placeholdered two parts away -- and the scan
## cannot catch it, having no pattern for a human display name.
## From the INSTALLED package, so this gate needs no path helper; working tree as a fallback.
tmplSrc <- system.file("pipeline", "Template", "TEMPLATE_Ab_Titration_Instructions.xlsx",
                       package = "ACTA")
if (!nzchar(tmplSrc))
  tmplSrc <- "inst/pipeline/Template/TEMPLATE_Ab_Titration_Instructions.xlsx"
if (!file.exists(tmplSrc)) {
  chk("TEMPLATE workbook available for the docProps check", FALSE, tmplSrc)
} else {
  wb <- file.path(tempdir(), paste0("docp_", basename(tempfile("")), ".xlsx"))
  file.copy(tmplSrc, wb, overwrite = TRUE)
  ex <- file.path(tempdir(), paste0("docx_", basename(tempfile(""))))
  dir.create(ex, recursive = TRUE); utils::unzip(wb, exdir = ex)
  cf <- file.path(ex, "docProps", "core.xml")
  x  <- paste(readLines(cf, warn = FALSE), collapse = "\n")
  x  <- sub("<cp:lastModifiedBy></cp:lastModifiedBy>",
            "<cp:lastModifiedBy>Doe, Jane</cp:lastModifiedBy>", x, fixed = TRUE)
  writeLines(x, cf, useBytes = TRUE)
  unlink(wb)
  zip::zip(wb, files = list.files(ex, recursive = TRUE, all.files = TRUE, no.. = TRUE), root = ex)
  chk("the planted author is really there before the clean",
      grepl("Doe, Jane", paste(readLines(unz(wb, "docProps/core.xml"), warn = FALSE),
                               collapse = ""), fixed = TRUE))
  invisible(ACTA:::.tpCleanWorkbook(wb))
  after <- paste(readLines(unz(wb, "docProps/core.xml"), warn = FALSE), collapse = "")
  chk("tpCleanWorkbook blanks the docProps author", !grepl("Doe, Jane", after, fixed = TRUE))
  chk("...and the workbook still opens with every sheet",
      identical(tryCatch(readxl::excel_sheets(wb), error = function(e) character(0)),
                c("Info", "gating_template", "Layout_Plate", "TiterPubs_template")))
}

## ---- the login is matched with a boundary and a length floor -----------------------------------
## WHY THESE EXIST. In 3.6.11 the login was a bare fixed substring. An operator could not build
## ANY bundle, because the shipped OQ_Test1 Info sheet carries ACTA's OWN REPOSITORY URL in
## References, and their login was a substring of the account handle in it -- so ACTA's own URL
## failed ACTA's own gate. Four artefacts carry that URL, each counted twice by a path-encoded
## rule that was a byte-copy of the plain one: 8 findings, zip deleted, every one a false positive.
##
## The names below are GENERIC on purpose. A real login written into a tracked file is a leak in
## its own right, and this repository's own scrub gate flags it -- which is how the first draft of
## this test was caught. The shipped URL itself is pinned by reading the fixture, further down,
## rather than by copying it here.
##
## None of this was visible to a reviewer or to CI, because on a managed machine the login is an
## e-mail address, which is never a substring of anything innocent. That is why the patterns are
## built from an ARGUMENT here rather than from Sys.info(): a rule keyed on this machine's
## particulars can only be falsified with somebody else's.
pats_ <- ACTA:::actaBundleScanPatterns
m_ <- function(p, s) isTRUE(grepl(p, s, perl = TRUE))

p_user <- pats_(user = "johndoe", node = "buildbox77")
chk("the login yields a boundary-anchored regex, not a bare substring",
    grepl("^\\(\\?<!\\[A-Za-z0-9\\]\\)", p_user[["account name"]]), p_user[["account name"]])
chk("'account name' is declared a regex, or it would be matched literally and find nothing",
    "account name" %in% ACTA:::ACTA_BUNDLE_REGEX)

## THE REGRESSION ITSELF.
chk("ACTA's own repo URL does NOT trip the gate for a login that is a substring of the handle",
    !m_(p_user[["account name"]], "https://github.com/johndoe199517/acta-public"))

## ...AND THE SAME THING AGAINST THE REAL SHIPPED VALUE, read from the fixture rather than
## copied here. A generic re-enactment proves the regex; only this proves that THE STRING ACTA
## ACTUALLY SHIPS is safe against a login drawn from it. The login is derived from the handle at
## run time, so nothing identifying is written down and the check follows the URL if it moves.
local({
  d <- system.file("extdata/oq_small/OQ_Test1", package = "ACTA")
  wb <- if (nzchar(d)) list.files(d, pattern = "Ab_Titration_Instructions\\.xlsx$",
                                  full.names = TRUE) else character(0)
  if (!length(wb)) { cat("  [skip] OQ_Test1 fixture not installed\n"); return(invisible(NULL)) }
  ## Info is a KEY/VALUE sheet (Category | Variable | Value | Description), so References is a
  ## ROW, not a column -- read it with the pipeline's own accessor rather than by column name.
  inf <- tryCatch(suppressMessages(
           readxl::read_excel(wb[[1]], sheet = "Info", .name_repair = "minimal")),
         error = function(e) NULL)
  ref <- if (is.null(inf)) NA_character_
         else tryCatch(ACTA:::actaInfoValue(inf, "References"), error = function(e) NA_character_)
  ## FAIL, don't skip, if the URL is not there: the whole point is that this value ships.
  chk("the shipped OQ_Test1 fixture still carries a github URL in Info!References",
      !is.na(ref) && grepl("github\\.com/[^/]+/", ref))
  if (is.na(ref) || !grepl("github\\.com/[^/]+/", ref)) return(invisible(NULL))
  handle <- sub("^.*github\\.com/([^/[:space:]]+).*$", "\\1", ref)
  ## A PREFIX of the handle is exactly the shape of the incident: login ⊂ handle.
  login  <- substr(handle, 1L, max(4L, nchar(handle) - 6L))
  chk(sprintf("a %d-char login drawn from that handle does not trip on the shipped URL",
              nchar(login)),
      !m_(pats_(user = login)[["account name"]], ref))
  ## The control: that login really is a substring, so the assertion above is not vacuous.
  chk("...and that login genuinely IS a substring of the handle, so the check is not vacuous",
      nchar(login) >= 4L && grepl(login, handle, fixed = TRUE))
})
chk("...nor does a different account that merely starts the same way",
    !m_(p_user[["account name"]], "/Users/johndoe2/run.fcs"))

## AND EVERY REAL LEAK SHAPE STILL BLOCKS -- in all of them the neighbouring character is a
## separator, never alphanumeric, which is the whole reason the boundary is safe here.
for (leak in c("/Users/johndoe/run.fcs", "C:\\Users\\johndoe\\run.fcs", "/home/johndoe/x.fcs",
               "scratch johndoe-anonymouscompany-com", "FILENAME = /Users/johndoe/OneDrive/x.fcs",
               "sent by johndoe"))
  chk(sprintf("still blocks: %s", leak), m_(p_user[["account name"]], leak))

## THE DUPLICATE.
chk("a path-encoded rule identical to the plain one is dropped, not reported twice",
    !("account, path-encoded" %in% names(p_user)))
p_mail <- pats_(user = "johndoe@anonymouscompany.com", node = "buildbox77")
chk("...but it is kept when it genuinely differs (the original FILENAME leak shape)",
    "account, path-encoded" %in% names(p_mail) &&
      m_(p_mail[["account, path-encoded"]], "/tmp/johndoe-anonymouscompany-com/run.fcs"))
chk("...and the e-mail login itself still blocks",
    m_(p_mail[["account name"]], "/Users/johndoe@anonymouscompany.com/run.fcs"))

## THE LENGTH FLOOR -- this is what lets an operator with a short login publish at all.
## Before it, a two-character login matched 18 files of a real bundle.
for (short in c("ab", "jk", "lab"))
  chk(sprintf("a %d-character login yields no bare-account pattern", nchar(short)),
      !any(c("account name", "account, path-encoded") %in% names(pats_(user = short))))
chk("a four-character login is still covered -- the floor is >= 4, not > 4",
    "account name" %in% names(pats_(user = "emma")) &&
      m_(pats_(user = "emma")[["account name"]], "/Users/emma/x.fcs"))
chk("a short login is still covered in PATH form by the home prefixes",
    any(grepl("home", names(pats_(user = "ab")))))

## THE HOST HAS THE SAME FLOOR, for the same reason, and stays FIXED so the detector cannot
## become narrower than actaDeIdentifyText(), which masks the host with fixed = TRUE.
chk("a three-character hostname is below the floor",
    !("machine name" %in% names(pats_(user = "someone", node = "Mac"))))
chk("a real hostname still blocks",
    identical(pats_(user = "someone", node = "flowbox1")[["machine name"]], "flowbox1"))

cat(if (ok) "\nBUNDLE SCAN TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
