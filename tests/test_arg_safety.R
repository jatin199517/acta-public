## Guards the fix for the 2_94 security review's H-1: NO WORKBOOK CELL IS EVER EVALUATED.
##
## The review found one of three paths. `flowjo_transformation_arg` was handed to
## `eval(parse(text=...))` with the argument NAMES validated afterwards -- too late, the code has
## run. But `gating_args` and `preprocessing_args` are equivalent: openCyto's .argParser() only
## parses, then .gating_adaptor() does `do.call(gFunc, c(list(fr = fr, ...), args))` and R evaluates
## each argument as the plugin reads it. Demonstrated 2026-09-04. It is also why `cluster=neg` once
## died with `object 'neg' not found` AFTER three gates had run -- that error was evaluation.
##
## LAZINESS MATTERS TO THIS TEST. An argument the callee never touches is never forced, so a stub
## that discards its arguments proves nothing; the stub below READS the value, as every real plugin
## reads `quantile`/`K`.
## Rscript <this> [version_dir]   -- defaults to 2_99.
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
h <- actaTestFunctions(vd)
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

## Every distinct arg cell in every shipped and working workbook, surveyed 2026-09-04. The
## allowlist was derived FROM this set, so if a future workbook needs a new construct this is the
## list that has to grow -- deliberately, not by accident.
REAL <- c("cluster='neg', quantile=0.99, K=2",
          "costain_quantile=0.99",
          "costain_quantile=0.99, sat_threshold=0.90, dip_pval=0.05",
          "K=2, cluster='neg', quantile=0.99",
          "K=2, cluster='neg', quantile=0.99, ashman_D=2",
          "K=2, cluster=\"neg\", quantile=0.99",
          "K=2, quantile=0.75, min=NULL, max=NULL",
          "K=3, quantile1=0.8, quantile3=0.8, usePrior='no', prior=list(NA), trans=0",
          "K=5, quantile=0.9, min=NULL, max=NULL",
          "min=c(1.5e6,-Inf), max=c(Inf,Inf)",
          "min=c(1e6,-Inf)",
          "min=c(50000,10000), max=c(250000,250000)",
          "prediction_level=0.99, wider_gate=FALSE")
cat("=== every real arg cell still passes (no false positives) ===\n")
for (a in REAL)
  chk(sprintf("accepted: %s", substr(a, 1, 52)),
      length(h$actaArgsNotParseable(list(a), "Reagent", "gating_args")) == 0)

cat("=== code in an arg cell is refused, by name ===\n")
HOSTILE <- c("K=2, quantile=(function(){ 0.99 })()", "quantile=system('id')",
             "K=eval(parse(text='1'))", "quantile=get('system')('id')",
             "quantile=0.99, K=source('/tmp/x.R')$value", "min=c(1, unlink('~/x'))",
             "quantile=.Machine$double.eps", "K=Recall()")
for (a in HOSTILE)
  chk(sprintf("refused: %s", substr(a, 1, 52)),
      length(h$actaArgsNotParseable(list(a), "Reagent", "gating_args")) > 0)

cat("=== the allowlist is exactly what the survey justified ===\n")
chk("c, list and unary +/- only", identical(sort(h$ACTA_ARG_CALL_ALLOW), sort(c("c","list","-","+"))))
chk("a call head that is itself a call cannot dodge a NAME allowlist",
    length(h$actaDisallowedCalls(parse(text = "list(x=(function() 1)())"))) > 0)
chk("an expression object is walked, not silently skipped",
    length(h$actaDisallowedCalls(parse(text = "list(x=system('id'))"))) > 0)
chk("  and the same tree with only literals is clean",
    length(h$actaDisallowedCalls(parse(text = "list(x=1, y=c(2,-Inf), z=list(NA))"))) == 0)

cat("=== flowjo_transformation_arg: numbers only, never evaluated ===\n")
md <- function(v) data.frame(Dirname = "D1", flowjo_transformation_arg = v, stringsAsFactors = FALSE)
gr <- data.frame(dirname = "D1", alias = "D1", key = "D1", stringsAsFactors = FALSE)
chk("pos=4.5 reads as a number", identical(h$actaTransArgs(md("pos=4.5"), gr), list(pos = 4.5)))
chk("a negative literal reads as a number",
    identical(h$actaTransArgs(md("pos=4.5, widthBasis=-10"), gr), list(pos = 4.5, widthBasis = -10)))
probe <- tempfile()
chk("a call is refused and DOES NOT RUN",
    inherits(try(h$actaTransArgs(md(sprintf('pos=(function(){ writeLines("x","%s"); 4.5 })()', probe)),
                                 gr), silent = TRUE), "try-error") && !file.exists(probe))
chk("maxValue is still blocked by name",
    inherits(try(h$actaTransArgs(md("pos=4.5, maxValue=1e6"), gr), silent = TRUE), "try-error"))
chk("a quoted string is refused (these arguments are numeric)",
    inherits(try(h$actaTransArgs(md("pos='high'"), gr), silent = TRUE), "try-error"))
unlink(probe)

cat("=== MUTATION SELF-CHECK: openCyto really does evaluate these ===\n")
## If this stops being true the pre-flight is belt-and-braces rather than load-bearing -- worth
## knowing either way, so it is asserted rather than assumed.
ap <- tryCatch(get(".argParser", envir = asNamespace("openCyto")), error = function(e) NULL)
if (is.null(ap)) cat("  [skip] openCyto .argParser not available\n") else {
  pr <- tempfile()
  args <- ap(sprintf('K=2, quantile=(function(){ writeLines("ran","%s"); 0.99 })()', pr))
  chk("parsing alone executes nothing", !file.exists(pr))
  stub <- function(fr, K, quantile) { z <- quantile + 0; invisible(z) }
  do.call(stub, c(list(fr = NULL), args))
  chk("a plugin READING the argument executes it -- hence the pre-flight", file.exists(pr))
  unlink(pr)
}

cat("=== LaTeX escaping: a workbook cell cannot reach the TeX stream as code (2_97 review F3) ===\n")
## The report hands Dirname/Alias/Markername to kable(escape = FALSE) and to \textbf{%s}, so
## .actaTexEsc is the whole defence. Its three predecessors in the .Rmd escaped #$%&_{} but NOT the
## backslash, which is the one character that starts a command.
te <- h$.actaTexEsc
chk("backslash is neutralised",      identical(te("\\"), "\\textbackslash{}"))
chk("\\input is inert text",          identical(te("CD4\\input{/etc/passwd}"),
                                               "CD4\\textbackslash{}input\\{/etc/passwd\\}"))
chk("\\write18 is inert text",        !grepl("(^|[^\\\\])\\\\write18", te("\\immediate\\write18{id}")))
chk("no \\textbackslash braces get double-escaped",
    !grepl("textbackslash\\\\[{]", te("a\\b")))
chk("tilde and caret are handled",   identical(te("a~b^c"),
                                               "a\\textasciitilde{}b\\textasciicircum{}c"))
for (plain in c("CD4", "CD4_APC", "100% of CD3+", "a&b", "5$", "x#1", "CD4{Live}"))
  chk(sprintf("legitimate content unchanged in behaviour: %s", plain),
      identical(te(plain), gsub("([#$%&_{}])", "\\\\\\1", plain)))

cat("=== the REPORT's asis sinks route untrusted values through the escaper ===\n")
## WHY THIS EXISTS. The check above proves .actaTexEsc is correct; it does NOT prove the report USES
## it everywhere, and for a while it did not. Three kable/\textbf sinks were escaped while five
## others -- Info!compensation, the compensation note and matrix path, the no-MM-fit Dirname list and
## the layout workbook's filename -- were cat() straight into a results='asis' chunk, where pandoc
## hands a backslash to LaTeX. A workbook cell of  csv` \input{/etc/passwd} `  read an arbitrary
## file into the report PDF; the backtick closed the markdown code span that was mistaken for a
## defence. Found by review 2026-09-08 and reproduced end-to-end before the fix.
##
## So this asserts the WIRING, not the escaper: every asis line that prints one of these values must
## pass it through .actaTexEsc (or knitr's own escaper). A grep is the right shape here -- rendering
## the whole report needs FCS, LaTeX and minutes, which is what the OQ cases are for.
rmd <- Sys.glob(file.path(vd, "ACTA_Report_*.Rmd"))
chk("the report file is there to inspect", length(rmd) == 1L)
if (length(rmd) == 1L) {
  ln <- readLines(rmd, warn = FALSE)
  ## The untrusted values, named EXACTLY as the report writes them (matched as fixed strings, not
  ## regexes -- "basename(metadata_file_name)" as a pattern would have its parentheses read as a
  ## capture group and match nothing, which is how the first version of this check passed itself).
  SINKS <- c("ci$raw", "ci$note", "ci$source", "bad$Dirname", "bad$MM_note",
             "basename(metadata_file_name)")
  ## Every OCCURRENCE must sit INSIDE the parentheses of an escaper, or inside a guard that only
  ## tests the value and never emits it. Scope, not adjacency: the report writes
  ## `.actaTexEsc(basename(ci$source))`, so the text immediately before `ci$source` is `basename(`
  ## -- an adjacency check called that unescaped and was wrong. This walks the parens.
  SAFECALL <- c(".actaTexEsc(", "escape_latex(", "is.na(", "nzchar(")
  ## Character positions covered by any SAFECALL's argument list, for one line.
  covered <- function(l) {
    keep <- logical(nchar(l))
    for (fnm in SAFECALL) {
      at <- gregexpr(fnm, l, fixed = TRUE)[[1]]
      if (at[1] == -1L) next
      for (a in at) {
        i <- a + nchar(fnm) - 1L          # the opening "("
        depth <- 1L; j <- i
        while (depth > 0L && j < nchar(l)) {
          j <- j + 1L; ch <- substr(l, j, j)
          if (ch == "(") depth <- depth + 1L else if (ch == ")") depth <- depth - 1L
        }
        keep[i:j] <- TRUE
      }
    }
    keep
  }
  body <- grep("^\\s*##", ln, value = TRUE, invert = TRUE)   # drop comment lines
  for (sk in SINKS) {
    n_ok <- 0L; bad_ctx <- character(0)
    for (l in body) {
      at <- gregexpr(sk, l, fixed = TRUE)[[1]]
      if (at[1] == -1L) next
      cv <- covered(l)
      for (a in at) {
        if (isTRUE(cv[a])) n_ok <- n_ok + 1L
        else bad_ctx <- c(bad_ctx, trimws(substr(l, max(1L, a - 40L), a + nchar(sk) + 4L)))
      }
    }
    chk(sprintf("%-28s never printed unescaped (%d guarded/escaped use(s))", sk, n_ok),
        n_ok > 0L && length(bad_ctx) == 0L)
    for (u in bad_ctx) cat("          UNESCAPED: ...", u, "...\n")
  }
}

cat("=== end to end: a hostile cell cannot read a file into the PDF ===\n")
## The real thing: R -> pandoc -> LaTeX, in the sink shapes the report actually uses. Skipped where
## the toolchain is absent (the fast CI job installs no LaTeX), because a skip is honest and a
## silent pass is not.
have <- nzchar(Sys.which("pandoc")) && requireNamespace("rmarkdown", quietly = TRUE) &&
        (nzchar(Sys.which("xelatex")) || nzchar(Sys.which("pdflatex")))
if (!have) cat("  [skip] pandoc/LaTeX not available here, so the render cannot be exercised\n") else {
  d <- file.path(tempdir(), sprintf("texinj_%s", basename(tempfile(""))))
  dir.create(d, showWarnings = FALSE)
  writeLines("CANARY-b7f3-DO-NOT-EMBED", file.path(d, "secret.txt"))
  fn <- normalizePath(file.path(vd, "ACTA_Functions.R"), winslash = "/")
  writeLines(c("---", "title: t", "output: pdf_document", "---", "",
               "```{r, echo=FALSE, results='asis'}",
               sprintf("source(%s, local = TRUE)", encodeString(fn, quote = '"')),
               ## the two shapes that leaked, with the fix applied
               'v <- "csv` \\\\input{secret.txt} `"',
               'cat(sprintf("A: \\\\texttt{%s}\\n\\n", .actaTexEsc(v)))',
               'cat(sprintf("- \\\\texttt{%s}\\n\\n", .actaTexEsc("\\\\input{secret.txt}")))',
               "```"),
             file.path(d, "t.Rmd"))
  owd <- setwd(d)
  r <- try(suppressWarnings(rmarkdown::render("t.Rmd", quiet = TRUE)), silent = TRUE)
  setwd(owd)
  pdf <- file.path(d, "t.pdf")
  if (inherits(r, "try-error") || !file.exists(pdf)) {
    cat("  [skip] the probe report did not render here:",
        trimws(sub("\n.*", "", as.character(r))), "\n")
  } else {
    txt <- if (nzchar(Sys.which("pdftotext")))
      paste(suppressWarnings(system2("pdftotext", c("-q", shQuote(pdf), "-"), stdout = TRUE)),
            collapse = " ")
    else paste(readBin(pdf, "character", 1L), collapse = " ")
    chk("the canary file's contents are NOT in the rendered PDF",
        !grepl("CANARY-b7f3", txt, fixed = TRUE))
    chk("the payload is still SHOWN, as inert text", grepl("input", txt, fixed = TRUE))
  }
  unlink(d, recursive = TRUE)
}

cat("=== MUTATION SELF-CHECK: the old one-liner really did pass a command through ===\n")
old_esc <- function(x) gsub("([#$%&_{}])", "\\\\\\1", x)
chk("the predecessor left \\write18 live -- hence .actaTexEsc",
    grepl("(^|[^\\\\])\\\\write18", old_esc("\\immediate\\write18{id}")))

if (Sys.info()[["sysname"]] == "Darwin" && nzchar(Sys.which("osascript"))) {
  cat("=== the folder picker passes its path as data, not as script (2_97 review F1) ===\n")
  ## Info!titration_export_dir reaches pickPath() through Browse, and a directory name may legally
  ## contain a double quote. Interpolated into the AppleScript source it closes the string literal
  ## and the rest of the name RUNS. shQuote() does not help: it quotes for the shell, one level out.
  payload <- 'out" & (1+1) & "x'
  argv_scr <- 'on run argv\n  return POSIX path of (POSIX file (item 1 of argv))\nend run'
  d <- file.path(tempdir(), payload)
  dir.create(d, showWarnings = FALSE)
  got <- suppressWarnings(system2("osascript", c("-e", shQuote(argv_scr), shQuote(d)),
                                  stdout = TRUE, stderr = FALSE))
  chk("the payload comes back verbatim as a path",
      length(got) == 1 && identical(sub("/$", "", got), d))
  chk("the injected sub-expression did NOT evaluate", !any(grepl("out2x|& 2 &", got)))

  cat("=== MUTATION SELF-CHECK: interpolation really is exploitable ===\n")
  bad_scr <- sprintf('return POSIX path of (POSIX file "%s")', d)
  bad <- suppressWarnings(system2("osascript", c("-e", shQuote(bad_scr)),
                                  stdout = TRUE, stderr = TRUE))
  ## AppleScript evaluates (1+1) and reports the broken literal as a THREE-ITEM LIST whose middle
  ## element is the result: {file "...:out", 2, "x"}. That 2 is the proof -- the name stopped being
  ## data and became an expression. Matched literally: the message uses a curly apostrophe, and \\b
  ## does not match around it.
  chk("the interpolated form evaluates the payload -- hence the argv form",
      any(grepl(", 2, ", bad, fixed = TRUE)) && any(grepl("[Cc]an.t get|syntax error", bad)))
  unlink(d, recursive = TRUE)

  cat("=== the shipped app uses the argv form ===\n")
  app <- readLines(file.path(vd, "ACTA_App.R"), warn = FALSE)
  chk("pickPath builds an `on run argv` handler", any(grepl("on run argv", app, fixed = TRUE)))
  chk("pickPath reads the path from argv",
      any(grepl("item 1 of argv", app, fixed = TRUE)))
  chk("no path is interpolated into a POSIX file literal",
      !any(grepl('POSIX file \\\\?"%s', app)))
} else cat("=== folder-picker checks skipped (not macOS) ===\n")

cat(if (ok) "\nALL ARG-SAFETY CHECKS PASSED\n" else "\nSOME ARG-SAFETY CHECKS FAILED\n")
quit(status = if (ok) 0L else 1L)
