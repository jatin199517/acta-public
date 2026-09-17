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
## AN UNWRITABLE DESTINATION THAT IS UNWRITABLE EVERYWHERE. This used "/proc-does-not-exist",
## which is unwritable on Linux and macOS and, on Windows, is a DRIVE-RELATIVE path -- the runner
## could create it at the root of the workspace drive, so the fallback never fired and the
## assertion failed for the platform rather than the code.
##
## A directory cannot be created underneath a regular FILE on any of the three, so the parent is
## a file here. That is the same premise, stated in something every filesystem agrees about.
.blocker <- file.path(wd, "not-a-directory")
writeLines("x", .blocker)
.bad <- file.path(.blocker, "nope")
p2 <- h$actaDiagLogPath(.bad, "run")
chk("falls back to tempdir() rather than returning NA",
    !is.na(p2) && !startsWith(p2, .blocker))

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
      ## AN EXACT-MATCH ASSERTION, not a substring search. Two CI failures came from the CHECK
      ## rather than the code, both of them collisions with the grid's OWN text: the carrier used
      ## to read "child log quoted %s while running" and a login of "runner" makes the grid derive
      ## the account "run", which is inside "running"; a letterless carrier "[[%s]]" then still
      ## collided, because the grid's own tail filename is "run.log" and that contains "run" too.
      ## The scrub was right every time.
      ##
      ## So assert what the label must BE. The contract is exact: the basename of the path, or
      ## <redacted> where the path terminates at a container that declares its child an account.
      ## An exact match cannot collide with a fixture, and it is a stronger claim than "the
      ## account does not appear" -- it also catches a label that keeps a component it should not.
      line <- sprintf("[[%s]]", path)
      out  <- h$actaDiagScrub(line)
      n <- n + 1L
      want <- if (length(tl)) tl[length(tl)] else "<redacted>"
      ## A UNC path keeps its leading backslash: a share path with an account under it collapses to
      ## "\\f", because the separator run before the first component is not part of any
      ## component. Cosmetic, and not identity, so it is normalised away here rather than asserted.
      got <- sub("^\\[\\[[\\\\/]*", "[[", out)
      if (!identical(got, sprintf("[[%s]]", want)))
        bad <- c(bad, sprintf("%s -> %s (wanted [[%s]])", path, out, want))
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

## ---------------------------------------------------------------------------------------------
## THE FOUR FOLLOW-UPS THE 3.1.0 REVIEW CARRIED, each asserted where it leaked.
cat("\n=== punctuation in a name, and hosts and addresses ===\n")
local({
  ## 1. THE COLLAPSE CLASS WAS A FINITE ALLOWLIST. A folder or account written with a hash,
  ## percent, exclamation or tilde broke the path match outside the two declared container names,
  ## so the name survived AND the line was mangled.
  ## ALL FOUR CHARACTERS ARE COVERED HERE. The first version of this gate asserted only `,` and
  ## `~` and `#`: removing `%` or `!` from the class re-leaked "100%Lab" and "Wow!Lab" with the
  ## suite green, which is the fault this file has the longest history of.
  for (cc in list(c("reading '/data/Smith, John/Lab/run.log'", "run.log"),
                  c("reading /data/Smith,John/run.log",        "run.log"),
                  c("/Volumes/Flow/O'Brien, Ryan/plate.xlsx",  "plate.xlsx"),
                  c("/data/user~1/run.log",                    "run.log"),
                  c("/nfs/lab#3/plate.xlsx",                   "plate.xlsx"),
                  c("/data/100%Lab/Nguyen/run.log",            "run.log"),
                  c("/data/Wow!Lab/Smith/run.log",             "run.log"),
                  ## THE NAME FORMS. The rule handles an initial, a middle name, a doubled
                  ## space and a non-Latin script; "van Dijk, Jan" and "Smith, John (Lab)" are
                  ## still missed, as they were in v3.0.12 -- named here so the next reader knows
                  ## which side of the line they are on.
                  c("/data/Nguyen, Thi Minh/plate.xlsx",       "plate.xlsx"),
                  c("/data/Petrov, I/run.log",                 "run.log"),
                  c("/data/Dubois,  Renee/run.log",            "run.log"))) {
    got <- h$actaDiagScrub(cc[1])
    chk(sprintf("a name with punctuation does not survive (%s)", cc[1]),
        grepl(cc[2], got, fixed = TRUE) &&
          !grepl("Smith", got, fixed = TRUE) && !grepl("O'Brien", got, fixed = TRUE) &&
          !grepl("Nguyen", got, fixed = TRUE) && !grepl("user~1", got, fixed = TRUE) &&
          !grepl("Thi Minh", got, fixed = TRUE) && !grepl("Petrov", got, fixed = TRUE) &&
          !grepl("Renee", got, fixed = TRUE) &&
          !grepl("lab#3", got, fixed = TRUE) && !grepl("100%Lab", got, fixed = TRUE) &&
          !grepl("Wow!Lab", got, fixed = TRUE))
    if (grepl("Smith|O'Brien|Nguyen|Petrov|Renee|user~1|lab#3|100%Lab|Wow!Lab", got))
      cat("         got:", got, "\n")
  }
  ## THE ACCOUNT REDACTION'S OWN CLASS CARRIES THE COMMA, and this is why. Removing it leaves
  ## "<redacted>, John" -- the given name standing beside the marker -- and the collapse cannot
  ## rescue it, because the comma is deliberately NOT in the collapse's class. Nothing asserted
  ## this, and the property grid's name alphabet has no comma in it either.
  ## "someone" is this file's declared FIXTURE_ID -- see the note in the Windows section below.
  for (cc in c("C:\\Users\\Smith, John\\plate.xlsx", "/home/Smith, John/run.log",
               "cannot read /Users/someone, Sam/Lab/plate.xlsx"))
    chk(sprintf("a comma'd account under a container name goes whole (%s)", cc),
        !grepl("John", h$actaDiagScrub(cc), fixed = TRUE) &&
          !grepl("Sam", h$actaDiagScrub(cc), fixed = TRUE))
  ## ...AND THE LINE AROUND THE PATH SURVIVES. The comma was admitted to the collapse class to
  ## close the "Surname, Given" shape above, and that traded the leak for something worse: the
  ## class has no right-hand bound, so the match ran from the path across the prose to the last
  ## slash in the line. Every shape below came out as the third column and is now the second --
  ## and the second column is what the release BEFORE this one produced, verified by running its
  ## scrubber side by side. The slash that ends the run is ordinary: ug/mL and cells/well are
  ## this tool's own units.
  ##                                                                     was
  ## wrote /data/lab/out.tsv, stain 0.25 ug/mL       out.tsv, ...         "wrote mL"
  ## layout '/data/lab/plate.xlsx', sheet, row 3/4   plate.xlsx, ...      "layout '4"
  ## cannot read /data/a/a.log, the CSV and/or TSV   a.log, ...           "cannot read or the TSV"
  for (cc in list(c("wrote /data/lab/out.tsv, stain 0.25 ug/mL",
                    "wrote out.tsv, stain 0.25 ug/mL"),
                  c("layout '/data/lab/plate.xlsx', sheet 'Info', row 3/4",
                    "layout 'plate.xlsx', sheet 'Info', row 3/4"),
                  c("cannot read /data/alpha/a.log, use the CSV and/or the TSV",
                    "cannot read a.log, use the CSV and/or the TSV"),
                  c("compare /data/a/expected.json, /data/b/observed.json",
                    "compare expected.json, observed.json"),
                  c("loaded /data/lab/plate.xlsx (3 sheets), 12 wells/plate",
                    "loaded plate.xlsx (3 sheets), 12 wells/plate"),
                  ## THE RELATIVE-PATH FAMILY. The first version of the "Surname, Given"
                  ## rule was not anchored at a path root, so it fired on any "X/Word, Word/Y" --
                  ## and then the collapse reduced what it produced. These are shapes this tool
                  ## and this repo write constantly, and every one of them lost both halves.
                  c("channels detected: FITC/PE, APC/Cy7", "channels detected: FITC/PE, APC/Cy7"),
                  c("no scatter channel among: FSC/Area, SSC/Area",
                    "no scatter channel among: FSC/Area, SSC/Area"),
                  c("swept 3_1/DESCRIPTION, Publish/README.public.md and CITATION.cff",
                    "swept 3_1/DESCRIPTION, Publish/README.public.md and CITATION.cff"),
                  c("gate path /Lymphocytes, Singlets/CD3 missing",
                    "gate path /Lymphocytes, Singlets/CD3 missing"),
                  ## ...and the trailing-separator bound, which is what keeps a capitalised word
                  ## after a comma from being read as a given name.
                  c("wrote /data/Report, Stain 0.25 ug and more", "wrote Report, Stain 0.25 ug and more"),

                  ## The units alone, with no path in the line at all: the first draft of the
                  ## "Surname, Given" rule read "mL, cells" as a name and took the whole line.
                  ## "cellss" is the pre-existing space run-on ("/well and events/s" -> "s"),
                  ## unchanged since v3.0.12 and not what this gate is about; the point is that
                  ## the text before the comma is still there.
                  c("0.25 ug/mL, cells/well and events/s", "0.25 ug/mL, cellss"))) {
    got <- h$actaDiagScrub(cc[1])
    chk(sprintf("a message keeps its words around a path (%s)", cc[2]), identical(got, cc[2]))
    if (!identical(got, cc[2])) cat("         got:", got, "\n")
  }
  ## 2. AN E-MAIL LOCAL PART WITH A NON-ASCII LETTER OR AN APOSTROPHE left a fragment beside the
  ## marker. The domain always went, which is why this survived six reviews.
  ## THE ADDRESSES ARE ASSEMBLED, NOT WRITTEN. A literal address in the source is a leak to the
  ## sanitisation gate however invented the name is -- this file is swept like any other.
  .dom <- paste0("@", "example", ".", "com")
  for (cc in list(c(paste0("contact Renée.Dubois", .dom, " for the layout"), "Ren"),
                  c(paste0("O'Brien", .dom, " wrote"), "Brien"))) {
    got <- h$actaDiagScrub(cc[1])
    chk(sprintf("no name fragment is left beside <redacted-email> (%s)", cc[2]),
        grepl("<redacted-email>", got, fixed = TRUE) && !grepl(cc[2], got, fixed = TRUE))
    if (grepl(cc[2], got, fixed = TRUE)) cat("         got:", got, "\n")
  }
  ## ...AND AN @ THAT IS NOT AN ADDRESS SURVIVES. Dropping the dotted-TLD requirement to catch an
  ## scp target made "any word@word" an address, and in a package built on S4 objects that is
  ## ordinary text: a slot in a traceback and a version pin in an install line both went.
  for (cc in c("Error in gs@pointer : invalid object", "fs@phenoData has 0 rows",
               "gate@Live, parent@Cells", "installed ACTA@3.2.0 from source",
               ## A LETTER-ONLY TLD is what keeps a version pin out of the e-mail rule, and
               ## nothing asserted it: relaxing it to [A-Za-z0-9]{2,} broke no gate while
               ## redacting this line.
               "pinned at ACTA@v3.2.10 for the validation run",
               paste0("remotes::install_github(\"jatin199517/acta-public", "@", "v3.2.0\")"))) {
    got <- h$actaDiagScrub(cc)
    chk(sprintf("an @ that is not an address is left alone (%s)", cc), identical(got, cc))
    if (!identical(got, cc)) cat("         got:", got, "\n")
  }
  ## 3. AN scp TARGET AND AN INTERNAL FQDN. A host with no dotted TLD kept both halves, and an
  ## internal suffix named a site. A BARE foreign hostname is deliberately still not masked --
  ## it cannot be told from any other word in prose, and the bundle header claims only that THIS
  ## machine's name is masked.
  ## AN scp TARGET, both halves of the rule. ONE fixture satisfied BOTH alternatives, so either
  ## could be deleted with the suite green -- so there are two: a hyphenated host with no colon,
  ## and a host that needs the colon to be recognised at all.
  g1 <- h$actaDiagScrub("scp asmith@build-node-12 /acta/out.tsv")
  chk("an scp target loses both the account and the hyphenated host",
      !grepl("asmith", g1, fixed = TRUE) && !grepl("build-node-12", g1, fixed = TRUE))
  g2 <- h$actaDiagScrub("scp asmith@buildnode:/acta/out.tsv")
  chk("...and the same with a colon and no hyphen",
      !grepl("asmith", g2, fixed = TRUE) && !grepl("buildnode", g2, fixed = TRUE))
  ## A BARE INTERNAL FQDN IS NOT MASKED, and that is the decision, not an oversight. A suffix rule
  ## (.local/.internal/.intra/.lan/.corp) was written, reviewed twice and taken back out: those are
  ## config suffixes as much as network ones, and both attempts turned real filenames into
  ## <redacted-host>, which misinforms the reader worse than leaving the host. Asserted, so that
  ## putting the rule back is a deliberate act with these cases in front of whoever does it.
  chk("a bare internal FQDN is left standing, deliberately",
      identical(h$actaDiagScrub("mount failed on host nas01.corp refused"),
                "mount failed on host nas01.corp refused"))
  ## ...and prose, and a plain FILENAME, are left alone -- or the diagnostic is unreadable. The
  ## first draft of the suffix rule ate config files: ".local" and ".internal" are R and Unix
  ## suffixes too, so "Makevars.local" and "settings.internal" became the marker, losing the one
  ## detail a support bundle about a broken TeX build needs. The gate that claimed to cover this
  ## was given an input with no filename in it.
  for (cc in c("no paths here at all", "loading .Rprofile.local",
               "cannot find package 'data.local'", "wrote settings.internal to disk",
               "sheet 'Info.corp' missing", "wrote plate_map.png and ACTA_Report_3_1.pdf",
               ## The multi-label and hyphen-or-digit shapes the second attempt still ate.
               ## Rprofile.site and Makevars.win are real R files.
               "read config.yml.local and settings.json.local", "read Rprofile.site.local",
               "read Makevars.win.local", "hook pre-commit.local failed",
               "wrote my-settings.local", "wrote plate2.local and run-1.internal")) {
    got <- h$actaDiagScrub(cc)
    chk(sprintf("ordinary prose and a plain filename are untouched (%s)", cc), identical(got, cc))
    if (!identical(got, cc)) cat("         got:", got, "\n")
  }
  chk("...and a home-relative config file keeps its name",
      identical(h$actaDiagScrub("read ~/.R/Makevars.local"), "read Makevars.local"))
})

cat("\n=== where the report is rendered, and what rmarkdown does to it ===\n")
local({
  ## A colleague's Windows OQ_Test1, 2026-09-16: the report would not compile and the surviving
  ## .tex named their home directory. One cause -- rmarkdown::pandoc_path_arg() replaces a path
  ## containing a SPACE with utils::shortPathName(), which returns BACKSLASHES, and those land
  ## inside \includegraphics{} where every \word is an undefined control sequence.
  ##
  ## THE FIRST FIX WAS INERT, and these gates are why it shipped for a day: they tested the
  ## helper in isolation and never composed it with rmarkdown's own normalisation, which UNDOES
  ## it -- render() calls normalize_path() on output_dir, and on Windows that converts a short
  ## name straight back to the long one (?normalizePath). The assertion below is the one that
  ## would have caught it, and it is first for that reason.
  ## ...and it has to be given a SPACED path to have anything to undo. My first draft of this very
  ## assertion passed tempdir(), which has no space, so the function early-returned and the gate
  ## proved nothing -- an unfalsifiable assertion written to guard against an inert fix.
  .sp <- "C:/Users/someone/Flow Cytometry/TIER 1/Run_1"
  chk("rmarkdown's own normalisation does not undo the choice of directory",
      grepl(" ", .sp, fixed = TRUE) &&
        !grepl(" ", rmarkdown:::normalize_path(h$actaRenderStage(.sp, windows = TRUE)),
               fixed = TRUE))
  ## THE CHOICE. Only Windows, only a spaced path, and only a candidate that is space-free AND
  ## writable -- each condition asserted separately, because each one silently disables the fix.
  ## The candidate must EXIST: a directory that has to be created first cannot be checked for
  ## writability, and a stage that turns out to be unwritable fails the render instead of the
  ## check. tempdir() and $PUBLIC both always exist.
  .ok <- file.path(tempdir(), "acta_stage_gate_ok"); dir.create(.ok, showWarnings = FALSE)
  .in_ok <- h$actaRenderStage(.sp, windows = TRUE, candidates = .ok)
  chk("a spaced Windows run folder renders somewhere without a space",
      startsWith(.in_ok, .ok) && dir.exists(.in_ok) && !grepl(" ", .in_ok, fixed = TRUE))
  ## THE SPACED CANDIDATE MUST EXIST, or dir.exists() refuses it before the space test runs and
  ## the gate proves nothing -- measured: with the space test deleted it still passed.
  .spc <- file.path(tempdir(), "acta stage gate spaced"); dir.create(.spc, showWarnings = FALSE)
  chk("...and a spaced candidate is refused, not used",
      identical(h$actaRenderStage(.sp, windows = TRUE, candidates = .spc), .sp))
  ## A DIRECTORY THAT EXISTS AND CANNOT BE WRITTEN. My first draft passed "/nonexistent-root/x",
  ## which dir.exists() alone refuses -- so the write check never decided anything and could be
  ## deleted with the suite green. chmod 0555 is the case that separates the two.
  .ro <- file.path(tempdir(), "acta_stage_gate_ro"); unlink(.ro, recursive = TRUE)
  dir.create(.ro, recursive = TRUE); Sys.chmod(.ro, "0555")
  chk("...and one that exists but cannot be written is refused too",
      identical(h$actaRenderStage(.sp, windows = TRUE, candidates = .ro), .sp))
  ## A CANDIDATE THAT DOES NOT EXIST IS REFUSED, not created. tempfile() plus a recursive
  ## dir.create() would otherwise build a tree in a location nobody chose.
  .gone <- file.path(tempdir(), "acta_stage_gate_absent"); unlink(.gone, recursive = TRUE)
  chk("...and one that does not exist is refused rather than created",
      identical(h$actaRenderStage(.sp, windows = TRUE, candidates = .gone), .sp) &&
        !dir.exists(.gone))
  Sys.chmod(.ro, "0755"); unlink(.ro, recursive = TRUE)
  ## TWO VALID CANDIDATES, or "first wins" and "last wins" give the same answer. Candidate order
  ## is the only thing keeping a report out of a shared location when the private one is usable.
  .c1 <- file.path(tempdir(), "acta_stage_gate_c1"); .c2 <- file.path(tempdir(), "acta_stage_gate_c2")
  dir.create(.c1, showWarnings = FALSE); dir.create(.c2, showWarnings = FALSE)
  chk("...and the FIRST space-free writable candidate wins",
      startsWith(h$actaRenderStage(.sp, windows = TRUE, candidates = c(.c1, .c2)), .c1))
  chk("...and a spaced one is skipped in favour of a later good one",
      startsWith(h$actaRenderStage(.sp, windows = TRUE, candidates = c(.spc, .c2)), .c2))
  ## A KEPT STAGE IS NEVER SWEPT BY ANYONE ELSE. When a copy fails the stage is retained on
  ## purpose -- the report is in it -- and nothing removes it afterwards, so in a shared location
  ## a complete report would sit there indefinitely, one per failure. Anything a week old is
  ## abandoned and goes; a recent one is somebody's in-flight render and must not.
  .sw <- file.path(tempdir(), "acta_stage_gate_sweep"); unlink(.sw, recursive = TRUE)
  dir.create(.sw, recursive = TRUE)
  .stale <- file.path(.sw, paste0(h$ACTA_REPORT_STAGE, "_old_1111"))
  .fresh <- file.path(.sw, paste0(h$ACTA_REPORT_STAGE, "_new_2222"))
  dir.create(.stale); dir.create(.fresh)
  ## DECOYS, because the glob and the directory test are both load-bearing: widening the glob
  ## would delete any week-old entry in a shared temp area, and dropping the dir.exists() filter
  ## would delete files.
  .other <- file.path(.sw, "somebody_elses_work"); dir.create(.other)
  writeLines("x", file.path(.other, "keep.me"))
  .filed <- file.path(.sw, paste0(h$ACTA_REPORT_STAGE, "_notadir"))
  writeLines("x", .filed)
  Sys.setFileTime(.stale, Sys.time() - as.difftime(30, units = "days"))
  Sys.setFileTime(.other, Sys.time() - as.difftime(30, units = "days"))
  Sys.setFileTime(.filed, Sys.time() - as.difftime(30, units = "days"))
  invisible(h$actaRenderStage(.sp, windows = TRUE, candidates = .sw))
  chk("an abandoned stage is swept before a new one is made", !dir.exists(.stale))
  chk("...and a recent one is left alone", dir.exists(.fresh))
  chk("...and an unrelated old directory survives with its contents",
      dir.exists(.other) && file.exists(file.path(.other, "keep.me")))
  chk("...and an old FILE of the stage name is not a stage", file.exists(.filed))
  unlink(.sw, recursive = TRUE)
  ## THE LEAF NAME IS UNPREDICTABLE, and a path that already exists is refused rather than
  ## adopted. A guessable stage in a shared location is three faults at once: two runs of the
  ## same name share it, the pre-render unlink() destroys whatever a local user left there, and
  ## anything planted there is copied into the run folder, where it travels.
  .a <- h$actaRenderStage(.sp, windows = TRUE, candidates = .c1)
  .b <- h$actaRenderStage(.sp, windows = TRUE, candidates = .c1)
  chk("two stages in the same location do not collide", !identical(.a, .b) && dir.exists(.b))
  chk("...and neither is a name that could have been guessed from the run folder",
      !any(vapply(c(.a, .b), function(z) identical(basename(z), "acta_report_stage_Run_1"),
                  logical(1))))
  unlink(c(.c1, .c2, .spc), recursive = TRUE)
  ## AND IT MUST NOT TOUCH ANYTHING ELSE, or every artefact reference on every other platform
  ## moves for no reason. macOS runs on a path whose every component has a space in it.
  chk("a Windows run folder with no space renders where it always did",
      identical(h$actaRenderStage("C:/Users/someone/ACTA/Run_1", windows = TRUE), 
                "C:/Users/someone/ACTA/Run_1"))
  chk("...and a POSIX path with spaces is untouched",
      identical(h$actaRenderStage(.sp, windows = FALSE), .sp))
  unlink(.ok, recursive = TRUE)
  .fn0 <- unlist(lapply(actaTestFunctionsFiles(), readLines, warn = FALSE))
  ## THE COLLECTION, which is the half that can lose a report. A staged render puts the PDF, the
  ## figures directory and (on failure) the .tex and .log in the stage; all of it has to arrive in
  ## the run folder, and the stage has to end up gone.
  .st <- file.path(tempdir(), "acta_stage_gate_src"); .vd <- file.path(tempdir(), "acta_stage_gate_dst")
  unlink(c(.st, .vd), recursive = TRUE)
  dir.create(file.path(.st, "ACTA_Report_3.1_files", "figure-latex"), recursive = TRUE)
  dir.create(.vd, recursive = TRUE)
  writeLines("pdf", file.path(.st, "ACTA_Report_3.1.pdf"))
  writeLines("tex", file.path(.st, "ACTA_Report_3.1.tex"))
  writeLines("png", file.path(.st, "ACTA_Report_3.1_files", "figure-latex", "GatingPlot-1.png"))
  .got <- h$actaRenderCollect(.st, .vd)
  chk("a staged render's PDF arrives in the run folder",
      file.exists(file.path(.vd, "ACTA_Report_3.1.pdf")) && length(.got$moved) == 2L)
  chk("...and so does its evidence, while the figures tree stays behind",
      file.exists(file.path(.vd, "ACTA_Report_3.1.tex")) &&
        !dir.exists(file.path(.vd, "ACTA_Report_3.1_files")))
  chk("...and the stage is left gone, not full", !dir.exists(.st))
  chk("collecting from a stage that IS the run folder does nothing",
      identical(h$actaRenderCollect(.vd, .vd)$moved, character(0)) &&
        file.exists(file.path(.vd, "ACTA_Report_3.1.pdf")))
  ## THE FAILURE PATH TAKES FILES AND LEAVES THE FIGURES TREE. Nothing sweeps a figures directory
  ## in the run folder when a render fails -- the intermediates are kept as evidence on purpose --
  ## so it sat in the case folder, which is what broke the OQ's self-containment check once
  ## before, and put a PNG per plot into a synced folder for every failure.
  unlink(c(.st, .vd), recursive = TRUE)
  dir.create(file.path(.st, "ACTA_Report_3.1_files"), recursive = TRUE); dir.create(.vd)
  writeLines("tex", file.path(.st, "ACTA_Report_3.1.tex"))
  writeLines("png", file.path(.st, "ACTA_Report_3.1_files", "GatingPlot-1.png"))
  .cf <- h$actaRenderCollect(.st, .vd)
  chk("a failed render's evidence arrives without its figures tree",
      file.exists(file.path(.vd, "ACTA_Report_3.1.tex")) &&
        !dir.exists(file.path(.vd, "ACTA_Report_3.1_files")) && !length(.cf$lost))
  ## A COPY THAT CANNOT LAND MUST NOT DESTROY THE ONLY OTHER COPY. It used to: the stage was
  ## unlinked outside the loop whatever happened, both callers discarded the return value, and an
  ## OLDER report already in the folder was then picked up as this run's.
  unlink(c(.st, .vd), recursive = TRUE)
  dir.create(.st); dir.create(.vd)
  writeLines("new", file.path(.st, "ACTA_Report_3.1.pdf"))
  writeLines("old", file.path(.vd, "ACTA_Report_3.1.pdf"))
  ## The DESTINATION FILE is what has to be unwritable. A read-only DIRECTORY does not stop
  ## file.copy() overwriting a file that is already in it -- measured -- so a gate built that way
  ## asserts nothing. This shape leaves the OLD content in place, which is the scenario itself:
  ## a stale report standing in for the one that could not land.
  Sys.chmod(file.path(.vd, "ACTA_Report_3.1.pdf"), "0444")
  .lc <- suppressWarnings(h$actaRenderCollect(.st, .vd))
  Sys.chmod(file.path(.vd, "ACTA_Report_3.1.pdf"), "0644")
  chk("a report that cannot be moved is reported lost, not silently dropped",
      identical(.lc$lost, "ACTA_Report_3.1.pdf") && identical(.lc$stage, .st))
  chk("...and the stage still holds it", file.exists(file.path(.st, "ACTA_Report_3.1.pdf")) &&
        identical(readLines(file.path(.st, "ACTA_Report_3.1.pdf")), "new"))
  ## The PREDICATE is executed above (actaStageKeep); what is left to pin is the WIRING -- that
  ## the stop is reached through it, on the success path, and that it names where the report is.
  chk("...and run_acta() turns that into a failure rather than a green run",
      any(grepl("if (actaStageKeep(.stageState, .col$lost))", .fn0, fixed = TRUE)) &&
        any(grepl("actaStageKeep(.stageState, .ec$lost)", .fn0, fixed = TRUE)) &&
        any(grepl("the report rendered but could not be moved", .fn0, fixed = TRUE)) &&
        any(grepl("It is still in '%s'", .fn0, fixed = TRUE)))
  unlink(c(.st, .vd), recursive = TRUE)
  ## AND report_ok NO LONGER IMPLIES A PDF IS HERE. which.max(file.mtime(character(0))) is
  ## integer(0) and pdfs[[integer(0)]] throws "attempt to select less than one element" -- out of
  ## the app's completion panel, on a run whose analysis succeeded. Those two facts came apart the
  ## moment a render could land somewhere else and fail to be moved back.
  .er <- file.path(tempdir(), "acta_art_gate"); unlink(.er, recursive = TRUE)
  dir.create(file.path(.er, "Outputs"), recursive = TRUE)
  .res <- list(ok = TRUE, report_ok = TRUE, wrote_plots = FALSE, plots_dir = NA_character_,
               version_dir = .er, out_dir = file.path(.er, "Outputs"), work_dir = .er)
  .art <- tryCatch(h$actaRunArtefacts(.res), error = function(e) conditionMessage(e))
  chk("a report that never arrived does not crash the completion panel",
      is.list(.art) && !is.character(.art))
  chk("...and it says so rather than naming another run's report",
      is.list(.art) && is.na(.art$report$path) && grepl("not in this folder", .art$report$skipped))
  unlink(.er, recursive = TRUE)
  ## And the render call must use both halves, or they are decoration.
  .fn <- .fn0
  chk("render() renders into the stage and the stage is collected",
      sum(grepl("output_dir = .stage, intermediates_dir = .interDir", .fn, fixed = TRUE)) == 1L &&
        ## Once on each path: the success branch and the error handler.
        sum(grepl("actaRenderCollect(.stage, version_dir)", .fn, fixed = TRUE)) == 2L &&
        sum(grepl(".stage <- actaRenderStage(version_dir, tag = .runTag)", .fn,
                  fixed = TRUE)) == 1L &&
        ## and the tag is SANITISED, or a spaced run name puts a space back into the one path
        ## that exists to be free of them.
        sum(grepl('.runTag <- gsub("[^A-Za-z0-9._-]", "_", basename(version_dir))', .fn,
                  fixed = TRUE)) == 1L)
  ## knit_root_dir must stay the REAL folder: it is the chunks' working directory, and staging it
  ## would make a chunk's relative path resolve into a temp folder. Nothing else enforces this.
  ## THE TWO DECISIONS, EXECUTED. These were inline conditions guarded by assertions over
  ## run_acta()'s parse tree, and a parse-tree assertion binds the SYMBOLS in a condition, not the
  ## predicate: eight semantic mutations passed the whole suite, including inverting the handler's
  ## polarity -- which destroys exactly the stage that must be kept, the defect the guard existed
  ## for -- and turning `length(lost)` into `> 1`, so the ordinary case of ONE lost report never
  ## raised the flag. They are functions now, so a test can just try them.
  .st <- new.env(parent = emptyenv()); .st$keep <- FALSE
  chk("nothing lost leaves the flag down", isFALSE(h$actaStageKeep(.st, character(0))) &&
        isFALSE(.st$keep))
  chk("ONE lost report raises it", isTRUE(h$actaStageKeep(.st, "ACTA_Report_3.1.pdf")) &&
        isTRUE(.st$keep))
  .d1 <- file.path(tempdir(), "acta_sweep_keep"); .d2 <- file.path(tempdir(), "acta_sweep_go")
  unlink(c(.d1, .d2), recursive = TRUE); dir.create(.d1); dir.create(.d2)
  invisible(h$actaStageSweep(.d1, "/elsewhere", keep = TRUE))
  invisible(h$actaStageSweep(.d2, "/elsewhere", keep = FALSE))
  chk("the handler keeps a stage the collect kept", dir.exists(.d1))
  chk("...and removes one it did not", !dir.exists(.d2))
  dir.create(.d1, showWarnings = FALSE)
  invisible(h$actaStageSweep(.d1, .d1, keep = FALSE))
  chk("...and never removes the run folder itself", dir.exists(.d1))
  unlink(c(.d1, .d2), recursive = TRUE)
  ## The handler must still be ADDED to the others, not replace them: a bare on.exit() clears
  ## every handler registered before it, and this function relies on two of them.
  chk("the exit handler is added, not substituted",
      any(grepl("on.exit(actaStageSweep(.stage, version_dir, .stageState$keep), add = TRUE)",
                .fn0, fixed = TRUE)))
  chk("...and knit_root_dir is still the run folder",
      sum(grepl("knit_root_dir = version_dir", .fn, fixed = TRUE)) == 1L)
})

cat("\n=== the truncated-account test in the OQ identity scan ===\n")
local({
  ## THE CLASS HAD HALF ITS BACKSLASHES. R handed PCRE `Users[/\][^/\]*~[0-9]`, where `\]` is an
  ## escaped bracket, so the class never closed where it looked like it did and the pattern
  ## degenerated to "Users" plus ONE character. It therefore MISSED the backslash spelling it was
  ## written for and matched three things that name nobody -- including this file's own public
  ## stage candidate, which would have failed a clean run on a path that identifies no one.
  ## Nothing asserted it: the whole branch could be deleted with the suite green.
  ##
  ## THE SHIPPED PATTERN IS LIFTED OUT OF THE SOURCE, not restated here. A copy would have been
  ## written correctly and passed while the shipped one stayed broken -- which is the whole shape
  ## of this defect.
  .src <- unlist(lapply(actaTestFunctionsFiles(), readLines, warn = FALSE))
  .ln  <- grep("~[0-9]", .src[grepl("grepl(\"Users[", .src, fixed = TRUE)], value = TRUE,
               fixed = TRUE)
  chk("the scan carries exactly one truncated-account pattern", length(.ln) == 1L)
  .rx <- if (length(.ln) == 1L)
    tryCatch(eval(parse(text = regmatches(.ln, regexpr('"[^"]+"', .ln)))),
             error = function(e) NA_character_) else NA_character_
  hit <- function(s) isTRUE(!is.na(.rx) && grepl(.rx, s, perl = TRUE, ignore.case = TRUE))
  ## ASSEMBLED, not written: a literal "<container>/<name>" is a leak to the sanitisation gate
  ## however invented the name is, and this file is swept like any other. .U is the container
  ## directory name Windows and macOS share.
  .U <- paste0("U", "sers")
  for (cc in list(list(paste0("C:\\", .U, "\\NAME~1\\AppData\\Local\\Temp\\x"),  TRUE),
                  list(paste0("C:/", .U, "/NAME~1/AppData/Local/Temp/x"),        TRUE),
                  list(paste0("C:/", .U, "/Public/acta_report_stage_Run_1_ab/x"), FALSE),
                  list(paste0("/", .U, "/Shared/R/library/ACTA/pipeline/logo.png"), FALSE),
                  list(paste0(.U, "2023 validation summary"),                    FALSE))) {
    chk(sprintf("%s -> %s", cc[[1]], if (cc[[2]]) "flagged" else "left alone"),
        identical(hit(cc[[1]]), cc[[2]]))
  }
})

cat("\n=== a failed render leaves no LaTeX scratch in the code folder ===\n")
local({
  ## A FAILED RENDER MUST NOT LEAVE LaTeX SCRATCH IN THE CODE FOLDER. The .log and the .tex are
  ## evidence and are moved beside the run; .aux/.out/.toc are not, and left in inst/pipeline they
  ## fail the release gate asserting that folder ships exactly one ACTA_Report_* file -- so the
  ## release's own "16 of 16" was contingent on nobody having had a render fail since the last
  ## cleanup. Measured: one failed render left the suite at 15 of 16.
  .cd <- file.path(tempdir(), "acta_codedir_gate"); unlink(.cd, recursive = TRUE); dir.create(.cd)
  for (.e in c("aux", "out", "toc", "log", "tex")) writeLines("x", file.path(.cd, paste0("ACTA_Report_3.1.", .e)))
  chk("the scratch extensions are found where a failed render leaves them",
      setequal(basename(h$actaReportIntermediates(.cd, ext = c("aux", "out", "toc"),
                                                  include_files_dir = FALSE)),
               paste0("ACTA_Report_3.1.", c("aux", "out", "toc"))))
  chk("...and the .log and .tex are NOT among them, because they are the evidence",
      !any(grepl("[.](log|tex)$", h$actaReportIntermediates(.cd, ext = c("aux", "out", "toc"),
                                                            include_files_dir = FALSE))))
  unlink(.cd, recursive = TRUE)
  .fn1 <- unlist(lapply(actaTestFunctionsFiles(), readLines, warn = FALSE))
  chk("...and the failure path unlinks them from the code folder",
      any(grepl('unlink(actaReportIntermediates(code_dir, ext = c("aux", "out", "toc")',
                .fn1, fixed = TRUE)))
})

cat("\n=== the LaTeX error names the fault, not just the line ===\n")
local({
  ## A colleague's Windows run, 2026-09-16: "LaTeX said: ! Undefined control sequence. |
  ## l.312 ...-latex/GatingPlot-1}" -- and nothing after the brace. TeX puts the error on TWO
  ## lines, and the undefined command is on the SECOND one; the quoting kept only lines matching
  ## ^! or ^l., so the token that names the cause was the one thing dropped. A whole class of
  ## LaTeX faults reported the line and not the reason.
  .log <- c("This is XeTeX, Version 3.141592653",
            "! Undefined control sequence.",
            "l.312 ...port_3.1_files/figure-latex/GatingPlot-1}",
            "                                                  \\pandocbounded",
            "?", "! ==> Fatal error occurred, no output PDF file produced!")
  got <- h$actaTexErrorLines(.log)
  chk("the continuation line that names the undefined command is quoted",
      any(grepl("pandocbounded", got, fixed = TRUE)))
  chk("...and so are the error and the line it is on",
      any(grepl("^! Undefined control sequence", got)) && any(grepl("^l[.]312", got)))
  ## The OTHER shape: an error inside a macro argument is announced by "<argument>", which no
  ## ^! / ^l. pattern matches either. Measured against xelatex: a backslash in an \includegraphics
  ## path reports exactly this.
  .log2 <- c("! Undefined control sequence.",
             "<argument> C:Users ",
             "                   x AppData Local Temp/GatingPlot-1")
  chk("an <argument> context line is quoted too",
      any(grepl("C:Users", h$actaTexErrorLines(.log2), fixed = TRUE)))
  ## And a log with no "!" at all still yields whatever it has, rather than nothing.
  chk("a log with no ! line falls back to what it does have",
      identical(h$actaTexErrorLines(c("x", "LaTeX Error: File `tabu.sty' not found.")),
                "LaTeX Error: File `tabu.sty' not found."))
  chk("...and an empty log yields nothing rather than failing",
      length(h$actaTexErrorLines(character(0))) == 0L)
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
    ## BOUNDED BY THE FUNCTION, not by a line count. A fixed +80 window meant that adding comments
    ## inside actaDiagScrub pushed the collapse out of view and this gate failed for the length of
    ## the function rather than for its order.
    .i1 <- .i0 + which(grepl("^[A-Za-z._][A-Za-z._0-9]* <- function",
                             .src[(.i0 + 1L):length(.src)]))[1]
    .body <- .src[.i0:(if (is.na(.i1)) length(.src) else .i1)]
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
