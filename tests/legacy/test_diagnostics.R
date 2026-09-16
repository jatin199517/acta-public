## Guards the diagnostic capture added 2026-09-03: the three write points and actaDiagnosticBundle().
##
## The gap this exists for: on a failed run the app recorded `detail = "child process produced no
## result"` and the child's actual error lived only in `rv$log`, on screen, with no download handler
## anywhere in the app. So the most useful artefact in a failure was the one thing that could not be
## sent. See Plans/260827_error-capture-and-diagnostic-bundle.md.
##
## EVERY CHECK HERE ASSERTS CONTENT, NOT EXISTENCE. The defect being fixed is precisely a file that
## exists and says nothing, so "the bundle was written" proves nothing on its own -- cf. the
## `shared_upstream` assertion that passed on zero comparisons (2026-08-27).
## Rscript <this> [version_dir]   -- defaults to 3_0.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

wd <- file.path(tempdir(), paste0("acta_diag_", as.integer(Sys.time())))
dir.create(wd, recursive = TRUE, showWarnings = FALSE)

cat("=== actaFirstError takes the FIRST error, not the last ===\n")
## The real cascade, 2026-08-27: one missing PNG produced four downstream failures and only the
## first line named the cause, so a tail-only report actively misleads.
cascade <- c("[ACTA] gating Cells", "[ACTA] gating Live",
             "! xdvipdfmx:fatal: Image inclusion failed. Could not find file: lp1-Live-1-1.png",
             "Error in render(): pandoc document conversion failed",
             "Error: could not find the report PDF")
fe <- h$actaFirstError(cascade)
chk("quotes the xdvipdfmx line", grepl("xdvipdfmx", fe, fixed = TRUE))
chk("does NOT lead with the last, derivative error",
    !grepl("^Error: could not find the report PDF", fe))
chk("empty text yields empty, rather than a guess", identical(h$actaFirstError(character(0)), ""))
chk("no error-shaped line yields empty",
    identical(h$actaFirstError(c("[ACTA] all good", "done!")), ""))

cat("=== actaDiagAppend tolerates an unwritable destination ===\n")
chk("NA path is a silent no-op, not an error", isFALSE(h$actaDiagAppend(NA_character_, "x")))
lg <- file.path(wd, "kid.log")
h$actaDiagAppend(lg, cascade)
h$actaDiagAppend(lg, "== closing marker ==")
chk("appends rather than overwrites", length(readLines(lg, warn = FALSE)) == length(cascade) + 1L)

cat("=== actaDiagLogPath falls back when the work dir cannot be written ===\n")
p1 <- h$actaDiagLogPath(wd, "run")
chk("lands under the work dir when writable", !is.na(p1) && startsWith(p1, wd))
p2 <- h$actaDiagLogPath(file.path("/proc-does-not-exist", "nope"), "run")
chk("falls back to tempdir() rather than returning NA", !is.na(p2) && !startsWith(p2, "/proc"))

cat("=== the bundle CONTAINS the error, and is scrubbed ===\n")
leaky <- c(cascade, "reading /Users/someone.name/OneDrive - Company/Flow/EXP1_data.fcs",
           "contact someone.name@company.com for the layout")
h$actaDiagAppend(lg, leaky)
b <- h$actaDiagnosticBundle(wd, vd, run_log = lg, child_log = lg, layout = NA_character_,
                            res = NULL, tsv = NA_character_, tlmgr_timeout = 5)
chk("bundle was written", isTRUE(b$ok) && file.exists(b$path))
txt <- if (isTRUE(b$ok)) readLines(b$path, warn = FALSE) else character(0)
chk("carries the FIRST ERROR section", any(grepl("^== FIRST ERROR ==", txt)))
chk("and the actual error text in it", any(grepl("xdvipdfmx", txt, fixed = TRUE)))
chk("carries sessionInfo output", any(grepl("R version|platform", txt)))
chk("carries the collector manifest", any(grepl("COLLECTOR MANIFEST", txt)))
chk("carries the closing sentinel", any(grepl("END OF BUNDLE", txt)))
chk("keeps the failing file's BASENAME (losing it defeats the purpose)",
    any(grepl("EXP1_data.fcs", txt, fixed = TRUE)))
chk("no username survives", !any(grepl("someone\\.name(?!@)", txt, perl = TRUE)))
chk("no email survives", !any(grepl("@company\\.com", txt)))
chk("no absolute home path survives", !any(grepl("/Users/someone", txt, fixed = TRUE)))

cat("=== MUTATION SELF-CHECK: the leak assertions must be able to fail ===\n")
## A scrubber that is never exercised is indistinguishable from one that does nothing, so run the
## same assertions against UNSCRUBBED text and require them to fail.
raw <- leaky
chk("username IS present before scrubbing", any(grepl("someone\\.name", raw, perl = TRUE)))
chk("email IS present before scrubbing", any(grepl("@company\\.com", raw)))
chk("scrubbing changes the text", !identical(h$actaDiagScrub(raw), raw))

## AND SCRUB THIS MACHINE'S OWN IDENTITY, which is the only fixture that cannot be wrong about
## what a real leak looks like. The fixtures above use invented names ("someone.name",
## "@company.com"), and every one of them passed while the scrub was leaking the REAL thing: the
## `Users/[A-Za-z0-9._-]+` class has no `@`, so a login that IS an e-mail address -- the norm under
## managed identity -- had its local part masked and its DOMAIN published, inside a file the app
## tells the operator to send on the promise that it carries no username. A bare directory path
## survived whole as well, because the basename pass only rewrites paths ending in a filename.
## "The text changed" was the only assertion standing between that and a user, and it passes for
## any change at all.
local({
  .home <- normalizePath(path.expand("~"), mustWork = FALSE)
  .node <- Sys.info()[["nodename"]]
  .user <- Sys.info()[["user"]]
  mine <- c(file.path(.home, "Library", "TinyTeX"),            # a bare DIRECTORY, no filename
            file.path(.home, "MyLab", "book.xlsx"),            # a path ending in a file
            sprintf("running on %s as %s", .node, .user),      # host and login
            sprintf("! LaTeX Error: nothing in %s/texmf", .home))
  out <- h$actaDiagScrub(mine)
  chk("this machine's home directory does not survive the scrub",
      !any(grepl(.home, out, fixed = TRUE)))
  chk("...nor its hostname", !any(grepl(.node, out, fixed = TRUE)))
  ## Only meaningful when the login is long enough not to match by accident.
  if (nchar(.user) > 3L)
    chk("...nor the login name", !any(grepl(.user, out, fixed = TRUE)))
  ## AND WITH AN INJECTED LOGIN, because the line above passes on this machine for the wrong
  ## reason: the login here IS an e-mail address, so the e-mail pass caught it and the login rule
  ## was never exercised. On a CI runner the login is "runner", nothing masked it, and the
  ## fixture published it -- found by CI on a release that was already tagged.
  local({
    chk("a bare login in prose is masked",
        identical(h$actaDeIdentifyText("running on box as runner", user = "runner"),
                  "running on box as <redacted-user>"))
    ## ...and the prose around it is NOT, which is the other half: an unbounded mask would eat
    ## "running" and leave a diagnostic nobody can read.
    chk("...without eating a word that merely starts the same way",
        identical(h$actaDeIdentifyText("the job is running fine", user = "runner"),
                  "the job is running fine"))
    ## A login too short to tell from English is left alone deliberately.
    chk("...and a login under four characters is left alone",
        identical(h$actaDeIdentifyText("run the thing as run", user = "run"),
                  "run the thing as run"))
  })
  ## THE DOMAIN SPECIFICALLY. This is the half that leaked: masking a login's local part and
  ## leaving "@employer.com" is worse than useless, because the promise says it is gone.
  chk("...and no e-mail domain is left behind",
      !any(grepl("@[A-Za-z0-9.-]+[.][A-Za-z]{2,}", out)))
  ## A DIRECTORY IS COLLAPSED TOO, not only a path ending in a filename. Leaving directories
  ## whole is how "~/Library/CloudStorage/OneDrive-SharedLibraries-<Employer>/<Library>/Flow"
  ## travelled with only the account removed -- the organisation and its internal document
  ## library intact, inside the file the app tells the operator to send.
  chk("...and a directory path is collapsed as well as a file path",
      any(out == "TinyTeX"))
  ## ONE COMPONENT, reverted 2026-09-15. Two was chosen on 2026-09-13 as "enough to act on, not
  ## enough to say whose machine it came from", and that reasoning missed that the PENULTIMATE
  ## component of a real path is very often a home directory: /data/<acct>/run.log came out
  ## "<acct>/run.log". v3.0.12 reduced to the basename and destroyed those, so keeping two was a
  ## REGRESSION against what is already published -- under a header promising the opposite.
  chk("...reducing to the basename, which is what the bundle header promises",
      any(out == "book.xlsx"))
  ## "~/texmf" is NOT a second component: the tilde is the redaction of this machine's home, so
  ## the form is "<redacted>/<one component>" and is exactly what the promise describes. The
  ## check is for a NAMED component before the slash, which is what would identify somebody.
  chk("...and never a named component before the slash",
      !any(grepl("[^[:space:]~]/[^[:space:]]", out)))
  ## AND THE OUTPUT IS STILL READABLE. An earlier fix produced "~book.xlsx" and
  ## "/Users/<redacted-email>thing.log" -- the prefix glued to the basename. A mangled diagnostic
  ## is a diagnostic nobody can act on.
  chk("...while remaining legible rather than mangled",
      !any(grepl("[.]com[a-z]", out)) && !any(grepl("~[A-Za-z]", out)))
  if (any(grepl(.home, out, fixed = TRUE)) || any(grepl("@[A-Za-z0-9.-]+[.][A-Za-z]{2,}", out)))
    cat("         scrubbed output was:", paste(out, collapse = " | "), "\n")
})

## ---------------------------------------------------------------------------------------------
## WINDOWS PATH SHAPES, AS LITERALS, because CI runs ubuntu and macOS and this project ships a
## .bat launcher for the platform neither of them is. Four security reviews looked at these
## scrubbers; the one that found they were INERT on Windows found it by reading, not by running,
## because nothing here could run it.
##
## Two things are specifically Windows and both defeated the old code:
##   * path.expand("~") resolves to <profile>/Documents, NOT the profile root -- so a package
##     library under C:/Users/<account>/AppData never matched the prefix being replaced, and the
##     default non-admin R install puts it exactly there;
##   * R returns backslashed paths from some calls and forward-slashed from others, and every
##     former copy of this logic compared one form with fixed = TRUE.
## actaPathLabel() is pure string work, so the Windows SHAPES can be asserted on any machine.
## That is not the same as running on Windows -- see the CI item -- but it is the difference
## between a claim nothing tests and a claim this suite tests everywhere.
## ---------------------------------------------------------------------------------------------
## A PROPERTY, NOT A FIXTURE LIST. Six security reviews found six identity leaks in this scrubber
## and THREE of them were the same mechanism -- a lossy rewrite running before the structural
## redaction that would have caught the leak -- each time wearing a different disguise. Every
## round fixed the instance and added a fixture for it, and every round the next disguise walked
## past the new fixture, because a fixture list can only ever contain the cases somebody already
## thought of.
##
## The invariant is simple and it is what actually matters: FOR ANY ACCOUNT NAME, IN ANY
## CONTAINER, WITH ANY SEPARATOR, AT ANY DEPTH, THE ACCOUNT NAME MUST NOT APPEAR IN THE OUTPUT.
## Swept over a generated grid rather than a list, including the two shapes that produced
## blockers:
##   * a path TERMINATING at the home directory, where nothing follows the account and the
##     "never drop the last component" rule had nothing else to name;
##   * an account name of which THIS MACHINE'S LOGIN IS A PREFIX (jo/john, chris/christine),
##     where an unanchored substring match half-rewrote the path and destroyed the very marker
##     the redaction matches on.
## The second is why varying only the account name was not enough: the old fixtures held this
## machine's home fixed, so a collision with it could never occur.
cat("=== property: no account name survives, over a generated grid ===\n")
local({
  .login <- Sys.info()[["user"]]
  ## Accounts that collide with the local login in both directions, plus unrelated ones. The
  ## collision pair is built FROM the real login so it cannot go stale when the machine changes.
  ## NON-ASCII AND PUNCTUATION ACCOUNTS. The classes were [A-Za-z0-9._@%+-], so an apostrophe or
  ## any accented letter defeated both the redaction and the collapse, leaving the name in full
  ## and eating the separator. A global company has colleagues called O'Brien and Renee with an
  ## acute; the grid had only ASCII literals and this machine's own login.
  accounts <- unique(c("someone", "someone.name", "someone@example.com",
                       "O'Brien", "Ren\u00e9e.Dubois", "\u00e9lise", "someone(1)", "John Smith",
                       substr(.login, 1, max(2L, nchar(.login) %/% 2L)),   # a prefix of it
                       paste0(.login, "x"),                               # an extension of it
                       "a"))
  ## ANY CONTAINER, which is what the invariant above claims. The grid used to generate only
  ## the two container names the structural redaction recognises -- so it could never produce
  ## the shape that actually leaked: a home directory one level above a file in a container with
  ## an ordinary name. /data/<acct>/run.log came out "<acct>/run.log" where v3.0.12 gave
  ## "run.log", a regression against what is already published, and this grid said nothing.
  containers <- c("Users", "home", "USERS", "Home",
                  "data", "nfs", "export", "srv", "opt", "scratch", "Volumes", "projects")
  seps       <- c("/", "\\")
  tails      <- list(character(0), "work", c("work", "run.log"), c("a", "b", "c", "d"))
  ## One prefix is THIS MACHINE'S OWN HOME, so the grid contains paths that CONTAIN the home
  ## prefix as a substring -- without that the prefix collision cannot arise at all, and the
  ## property passed its own mutation test for exactly that reason. The accounts above already
  ## include one that EXTENDS the login; this is the other half of the pair.
  prefixes   <- c("", "C:", "/var/tmp", "\\\\server\\share",
                  dirname(normalizePath(path.expand("~"), mustWork = FALSE)))
  bad <- character(0); mangled <- character(0); n <- 0L
  for (acct in accounts) for (cont in containers) for (sp in seps)
    ## AN EMPTY TAIL ONLY WHERE THE CONTAINER SAYS "ACCOUNT". "/data/someone" terminates at its
    ## second component and is structurally indistinguishable from a folder called "someone";
    ## reducing it to the basename (which is what v3.0.12 did too, so this is no regression)
    ## leaves that name standing. Only Users/ and home/ declare their child to be an account, and
    ## for those the terminal case IS handled -- it returns <redacted>, asserted below. Demanding
    ## it of every container would assert something undecidable, and a gate that cannot be
    ## satisfied gets switched off rather than fixed.
    for (tl in (if (tolower(cont) %in% c("users", "home")) tails else tails[-1L]))
      for (pre in prefixes) {
      path <- paste(c(pre, cont, acct, tl), collapse = sp)
      ## NO PROSE IN THE PROBE. The sentence used to read "child log quoted %s while running", and
      ## on a machine whose login is "runner" the grid derives the short account "run" -- which
      ## appears inside the word "running". The scrub itself was correct -- it reduced the path
      ## to its basename -- and the CHECK was wrong, and it only showed up on CI. A carrier with
      ## no letters cannot
      ## collide with any account name.
      ## A LETTERLESS CARRIER. "child log quoted %s while running" put the account "run"
      ## (derived from a CI runner's login) inside the word "running"; "path=[%s]|end" then put
      ## the one-character account "a" inside "path". The redaction markers are stripped before
      ## the check, but "<redacted>" has letters of its own, so the only carrier that cannot
      ## collide with any account is one with no letters at all.
      line <- sprintf("[[%s]]", path)
      out  <- h$actaDiagScrub(line)
      n <- n + 1L
      ## The account must be gone. Checked as a SUBSTRING, because a FRAGMENT surviving is how the
      ## prefix-collision leak read: "~hn/Lab/run.log" with the separator eaten still names john.
      ##
      ## The redaction markers are stripped first. Without that a one-character account matches
      ## inside the word "<redacted>" itself and the property reports 32 leaks that are the
      ## scrubber working correctly -- a false alarm in the gate is how a gate gets switched off.
      probe <- gsub("<redacted[^>]*>", "", out)
      if (grepl(acct, probe, fixed = TRUE)) bad <- c(bad, sprintf("%s -> %s", path, out))
      ## AND THE STRUCTURAL INVARIANT, which is what the prefix collision actually violates.
      ## That leak does not leave the WHOLE account behind -- it leaves a FRAGMENT, because the
      ## home prefix was consumed from the middle of a longer name: with HOME=/Users/<short>,
      ## "/Users/<short>hn/Lab" became "~hn/Lab" with the separator eaten. Checking for the whole account misses it entirely,
      ## which is how this property passed its own first mutation test.
      ##
      ## What the damage always looks like is a `~` that is NOT followed by a separator or the
      ## end of the string: the tilde stands for a whole directory, so anything glued directly
      ## to it is a fragment of a name that should not be there, and a separator was eaten.
      if (grepl("~[^/\\\\]", out)) mangled <- c(mangled, sprintf("%s -> %s", path, out))
    }
  chk(sprintf("no account name survives any container/separator/depth (%d combinations)", n),
      !length(bad))
  chk("...and no output glues a name fragment onto ~, which is what a prefix collision leaves",
      !length(mangled))
  if (length(mangled)) {
    cat(sprintf("         %d mangled, first few:\n", length(mangled)))
    for (b in utils::head(mangled, 6)) cat("           ", b, "\n")
  }
  if (length(bad)) {
    cat(sprintf("         %d leaking, first few:\n", length(bad)))
    for (b in utils::head(bad, 6)) cat("           ", b, "\n")
  }
  ## AND THE OUTPUT IS STILL WORTH READING. A scrubber that returns "" or mangles every line into
  ## punctuation satisfies the property above and is useless -- an earlier fix produced
  ## "~book.xlsx" and "<redacted>@<employer>.combook.xlsx".
  .sample <- h$actaDiagScrub(sprintf("child log quoted %s while running",
                                     paste("Users", accounts[1], "work", "run.log", sep = "/")))
  chk("...while the line remains legible, not mangled to punctuation",
      grepl("child log quoted", .sample, fixed = TRUE) &&
        grepl("run.log", .sample, fixed = TRUE) &&
        !grepl("[.]com[a-z]|~[A-Za-z]", .sample))
  ## The local user's own home must still render as ~ or collapse -- fixing the foreign case by
  ## flattening everything would pass the property and destroy the diagnostic.
  .h <- normalizePath(path.expand("~"), mustWork = FALSE)
  chk("...and this machine's own paths still collapse rather than vanish",
      identical(h$actaDiagScrub(file.path(.h, "MyLab", "book.xlsx")), "book.xlsx") &&
        identical(h$actaDiagScrub(paste0(.h, "/texmf")), "~/texmf"))
})

cat("=== Windows path shapes ===\n")
local({
  ## THE ACCOUNT NAME IS "someone" ON PURPOSE: verify_oq_sanitised.R declares that as this file's
  ## FIXTURE_ID and masks it before sweeping, so the fixture stays visible to a reader while the
  ## scrub gate is not trained to ignore a real-looking path. Inventing a second name (my first
  ## draft used one) trips that gate for no reason -- and a gate that fires on a wanted string is
  ## the thing the token list's own comment warns about.
  cases <- list(
    c("C:/Users/someone/AppData/Local/R/win-library/4.5", "4.5"),
    c("C:\\Users\\someone\\AppData\\Local\\R\\win-library\\4.5", "4.5"),
    c("C:\\Users\\someone\\Documents\\ACTA\\inst\\pipeline", "pipeline"),
    c("C:/Users/someone/Documents/MyLab/Run_1", "Run_1"),
    ## A clone directly under the profile: the parent IS the account, and must not survive.
    c("C:\\Users\\someone\\ACTA", "ACTA"))
  for (cc in cases) {
    got <- h$actaPathLabel(cc[1])
    chk(sprintf("%s -> %s", cc[1], cc[2]), identical(got, cc[2]))
    if (!identical(got, cc[2])) cat("         got:", got, "\n")
  }
  ## THE ROUND-5/6 FIX, ASSERTED DIRECTLY. actaPathLabel() is called bypassing actaDiagScrub in
  ## three shipping places -- code_dir in acta_version.tsv, the OQ's "code from" line, and
  ## version_dir in ACTA_app_run_log.tsv -- and deleting the terminal-account rule left the whole
  ## 16-script suite green, because actaDiagScrub's own structural pass covered every route the
  ## grid generates. Those three artefacts travel into validation packages.
  for (.p in c("/Users/someone", "C:\\Users\\someone", "/home/someone"))
    chk(sprintf("actaPathLabel redacts a path that IS an account (%s)", .p),
        identical(h$actaPathLabel(.p), "<redacted>"))
  ## AND THE keep = 2 PATH, which is the mechanism the whole basename fix rests on and which no
  ## test reached: deleting the ACTA_LABEL_PENULT filter left all sixteen scripts green, because
  ## the collapse pins keep = 1L explicitly and nothing else called keep = 2L. That default is
  ## used for code_dir in acta_version.tsv, the OQ's "code from" line and version_dir in the app
  ## run log -- the three artefacts that travel into validation packages.
  ## A PATH INSIDE A SENTENCE keeps the sentence. The account class was widened to "anything but
  ## a separator or space", which swallowed the punctuation ending the path and let the collapse
  ## eat the rest of the line -- the FIRST ERROR section of the bundle reduced to "<redacted>".
  ## Every fixture here ended at a path boundary, so none of them could see it.
  for (.cc in list(c("/Users/someone: Permission denied", "Permission denied"),
                   c('Error in setwd("/Users/someone") : cannot change directory',
                     "cannot change directory"),
                   c("/Users/someone; retrying", "retrying")))
    chk(sprintf("the message survives beside the path (%s)", .cc[2]),
        grepl(.cc[2], h$actaDiagScrub(.cc[1]), fixed = TRUE) &&
          !grepl("someone", h$actaDiagScrub(.cc[1]), fixed = TRUE))
  chk("keep = 2 keeps a STRUCTURAL second component",
      identical(h$actaPathLabel("/x/library/ACTA/pipeline", keep = 2L), "ACTA/pipeline") &&
        identical(h$actaPathLabel("/x/clone/inst/pipeline", keep = 2L), "inst/pipeline"))
  chk("...and drops one that is just somebody's directory",
      identical(h$actaPathLabel("/data/someone/Run_1", keep = 2L), "Run_1") &&
        identical(h$actaPathLabel("/nfs/someone/Run_7", keep = 2L), "Run_7"))
  ## THE ORDER, which the source calls "the whole fix" and which nothing pinned. Moving the two
  ## account-redaction gsubs back BELOW the collapse -- the defect three consecutive reviews found,
  ## each in a different disguise -- left the suite green, because actaPathLabel's own redaction
  ## holds alone for the shapes the grid generates. So it is real defence-in-depth with no
  ## behavioural consequence today, and the only honest way to gate it is on the source.
  local({
    .src <- unlist(lapply(actaTestFunctionsFiles(vd), readLines, warn = FALSE))
    .i0 <- which(grepl("^actaDiagScrub <- function", .src))[1]
    .body <- .src[.i0:(.i0 + 80L)]
    .iRedact <- which(grepl('gsub(sprintf("(?i)(Users[/', .body, fixed = TRUE))[1]
    .iColl   <- which(grepl("m <- gregexpr(", .body, fixed = TRUE))[1]
    chk("actaDiagScrub redacts accounts BEFORE it collapses paths",
        !is.na(.iRedact) && !is.na(.iColl) && .iRedact < .iColl)
  })
  ## And no Windows account name survives any of them, which is the claim that matters.
  outs <- vapply(vapply(cases, `[`, character(1), 1), h$actaPathLabel, character(1),
                 USE.NAMES = FALSE)
  chk("no Windows account name survives a path label", !any(grepl("someone", outs, fixed = TRUE)))
  ## The prefix list itself must offer both slash forms, or fixed = TRUE matching can only ever
  ## catch one of them.
  pref <- h$actaHomePrefixes()
  chk("the home-prefix list carries both slash directions",
      any(grepl("/", pref, fixed = TRUE)) && any(grepl("\\", pref, fixed = TRUE)))
  ## Longest first, or on Windows <profile> would be replaced before <profile>/Documents and
  ## leave a dangling "~/Documents".
  chk("...longest first, so the most specific prefix wins",
      identical(pref, pref[order(nchar(pref), decreasing = TRUE)]))
  ## Nothing short enough to match half the filesystem.
  chk("...and nothing shorter than a real home directory", all(nchar(pref) > 4L))
})

cat("=== a `started` row with no terminal row is detectable ===\n")
tsv <- file.path(wd, "run_log.tsv")
h$actaRunLogAppend(tsv, h$actaRunLogEntry("run_acta", "started", vd, detail = "child not yet launched"))
d <- read.delim(tsv, stringsAsFactors = FALSE)
chk("the started row is on disk before any outcome", any(d$outcome == "started"))
chk("and no terminal row accompanies it",
    !any(d$outcome %in% c("success", "failed", "analysis_failed", "report_failed", "aborted")))

cat(if (ok) "\nALL DIAGNOSTIC CHECKS PASSED\n" else "\nSOME DIAGNOSTIC CHECKS FAILED\n")
quit(status = if (ok) 0L else 1L)
