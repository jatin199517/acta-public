## THE FAILURE-MODE LAYER for the 3.6.12 workbook-convention work.
##
## Separate from test_workbook_conventions.R on purpose. That file pins the CONTRACT -- what the
## helpers promise on the inputs the template actually produces. This one asks the other question:
## for every new or touched expression, what makes it zero-length, NA, absent, duplicated,
## wrong-typed, mixed-case or out-of-order -- and does it then do something defensible.
##
## A green regression suite says the happy paths still work. It says nothing about the paths the
## change created, and every OQ fixture is a happy path.
##
## THE INPUTS ARE DELIBERATELY NOT WORKBOOK-SHAPED. Nothing here reads a fixture: a fixture can only
## show what somebody already thought to put in one.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
suppressWarnings(suppressMessages(library(readxl)))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
h <- actaTestFunctions()
bare <- h$actaBareName; dropv <- h$actaDropTrailingParen; drop0 <- h$actaDropCommentRows

cat("\n-- 1. actaBareName(): the degenerate inputs --\n")
chk("zero-length in, zero-length out -- NOT character(1)",
    identical(bare(character(0)), character(0)))
chk("NA stays NA, it does not become the string 'NA'",
    identical(bare(NA_character_), NA_character_))
chk("NA among real names leaves the others intact",
    identical(bare(c("A (N)", NA, "B")), c("A", NA, "B")))
## A HEADER THAT IS NOTHING BUT AN ANNOTATION. `(N)` strips to "" -- an empty column name. This is
## the input that would turn an Info Variable into a blank published row, so it is pinned rather
## than left to be discovered in a deposit.
chk("a name that is ONLY an annotation strips to empty, and is not silently kept",
    identical(bare("(N)"), "") && identical(bare("  (L)  "), ""))
chk("a factor comes back as its label, not its integer code",
    identical(bare(factor(c("A (N)", "B"))), c("A", "B")))
chk("a numeric name is coerced, not dropped", identical(bare(42), "42"))
chk("empty string survives", identical(bare(""), ""))
chk("whitespace-only collapses to empty", identical(bare("   "), ""))
## ONE annotation, the LAST one. Two trailing brackets is the shape a 3.6.13 edit could produce by
## pasting a hint onto a slot name.
chk("only the final annotation is stripped, never both",
    identical(bare("Markername ($PnS) (N)"), "Markername ($PnS)"))
chk("a mid-string bracket is untouched", identical(bare("IL-2 (high) count"), "IL-2 (high) count"))
chk("nested brackets are left alone rather than half-stripped",
    identical(bare("Foo (N (0-100))"), "Foo (N (0-100))"))
chk("no trailing annotation is a no-op", identical(bare("StainQty"), "StainQty"))

cat("\n-- 2. the VALUE rule is a separate rule, and stays separate --\n")
## If these two ever become one function, a widening of the header rule silently weakens the
## equipment-serial scrub. Asserted behaviourally: they agree today on the shapes they share, and
## the test that forbids a third copy lives in test_workbook_conventions.R.
chk("the value rule drops an instrument serial", identical(dropv("Aurora (SN0000)"), "Aurora"))
chk("...and leaves a bare model alone", identical(dropv("Aurora"), "Aurora"))
chk("...and is NA-safe like the header rule", identical(dropv(NA_character_), NA_character_))
chk("...and zero-length safe", identical(dropv(character(0)), character(0)))
chk("a serial containing brackets is not half-removed",
    identical(dropv("Aurora (SN(0))"), "Aurora (SN(0))"))

cat("\n-- 3. actaStripHeaderAnnotation(): collisions, and WHICH sheet it blames --\n")
chk("a collision stops rather than silently keeping one column",
    inherits(try(h$actaStripHeaderAnnotation(c("StainQty (N)", "StainQty")), silent = TRUE),
             "try-error"))
msg <- tryCatch(h$actaStripHeaderAnnotation(c("A (N)", "A"), where = "Info"),
                error = function(e) conditionMessage(e))
chk("the error names the sheet it was given, not a hard-coded default",
    grepl("^Info headers collide", msg), msg)
chk("...and names BOTH colliding spellings so it can be fixed without guessing",
    grepl("A (N)", msg, fixed = TRUE) && grepl("'A'", msg, fixed = TRUE), msg)
## TWO EMPTIES ARE NOT A COLLISION the guard should report -- `nzchar(out)` excludes them. Two
## blank headers are a different fault (an empty column), and reporting them as an annotation
## collision would send the operator to the wrong place.
chk("two blank headers do not raise an ANNOTATION collision",
    !inherits(try(h$actaStripHeaderAnnotation(c("", "")), silent = TRUE), "try-error"))
chk("zero-length headers do not error", identical(h$actaStripHeaderAnnotation(character(0)),
                                                  character(0)))

cat("\n-- 4. actaDropCommentRows(): what is a comment, and what only looks like one --\n")
mk <- function(first, n = length(first))
  data.frame(Row = first, Val = seq_len(n), stringsAsFactors = FALSE)
chk("a leading-# row is dropped", nrow(drop0(mk(c("A", "#note", "B")))) == 2L)
chk("...and the SURVIVORS keep their values, not just their count",
    identical(drop0(mk(c("A", "#note", "B")))$Val, c(1L, 3L)))
chk("leading whitespace before the # still counts as a comment",
    nrow(drop0(mk(c("A", "   # note")))) == 1L)
## NOT A COMMENT: the hash has to START the cell.
chk("a # elsewhere in the first cell is NOT a comment",
    nrow(drop0(mk(c("A", "lot #4")))) == 2L)
chk("a # in a LATER column is not a comment -- only the first column votes",
    nrow(drop0(data.frame(Row = c("A", "B"), Val = c("#x", "y"),
                          stringsAsFactors = FALSE))) == 2L)
chk("NA in the first cell is kept, not treated as a comment",
    nrow(drop0(mk(c("A", NA)))) == 2L)
chk("a blank first cell is kept", nrow(drop0(mk(c("A", "")))) == 2L)
## DEGENERATE FRAMES. Each of these reached the function in a real layout at some point: an empty
## sheet, a sheet read with no columns, a NULL from a missing sheet.
chk("NULL passes through as NULL", is.null(drop0(NULL)))
chk("a 0-row frame stays a 0-row FRAME, not NULL",
    is.data.frame(drop0(mk(character(0), 0))) && nrow(drop0(mk(character(0), 0))) == 0L)
chk("a 0-column frame passes through",
    is.data.frame(drop0(data.frame()[0, , drop = FALSE])))
chk("a non-data-frame passes through untouched", identical(drop0(1:3), 1:3))
chk("ALL rows commented yields a 0-row frame, not NULL and not an error",
    is.data.frame(drop0(mk(c("#a", "#b")))) && nrow(drop0(mk(c("#a", "#b")))) == 0L)
chk("column names and types survive the filter",
    identical(names(drop0(mk(c("A", "#x")))), c("Row", "Val")) &&
      is.integer(drop0(mk(c("A", "#x")))$Val))
chk("a numeric first column does not error and drops nothing",
    nrow(drop0(data.frame(Row = c(1, 2), Val = c("a", "b"), stringsAsFactors = FALSE))) == 2L)
chk("a factor first column is read by its label",
    nrow(drop0(data.frame(Row = factor(c("A", "#x")), Val = 1:2))) == 1L)

cat("\n-- 5. actaTemplateFlag(): literal beats bare, and neither shadows a real column --\n")
gtm <- function(nms, vals) {
  d <- data.frame(alias = c("r1", "r2"), stringsAsFactors = FALSE)
  for (i in seq_along(nms)) d[[nms[i]]] <- vals[[i]]
  d
}
f <- h$actaTemplateFlag
chk("an unannotated flag column is found",
    all(f(gtm("Stats_Export", list(c("TRUE", "FALSE"))), "Stats_Export") == c(TRUE, FALSE)))
chk("an ANNOTATED flag column is found",
    all(f(gtm("Stats_Export (L)", list(c("TRUE", "FALSE"))), "Stats_Export") == c(TRUE, FALSE)))
## BOTH PRESENT: the exact header must win, or the fallback could shadow a real column.
##
## COLUMN ORDER IS THE WHOLE TEST. With the literal column FIRST, a reader that tries the bare name
## first still lands on it -- match() returns the first hit, and both names are "Stats_Export" once
## stripped. So that arrangement passes against a broken reader, and the first draft of this check
## did exactly that: swapping the two lookups in actaTemplateFlag() was not detected.
## The ANNOTATED column therefore goes FIRST, where only a literal-first lookup can get past it.
## Same lesson as .tpInvByCatalog -- a fixture in the convenient order proves nothing.
bothAnnFirst <- gtm(c("Stats_Export (L)", "Stats_Export"),
                    list(c("FALSE", "FALSE"), c("TRUE", "TRUE")))
chk("with BOTH spellings present the LITERAL header wins -- annotated column FIRST",
    all(f(bothAnnFirst, "Stats_Export") == c(TRUE, TRUE)))
## A CONTROL on the fixture itself: if these two stop colliding, the check above is vacuous.
chk("control: the two headers really do collide once stripped",
    identical(bare(c("Stats_Export (L)", "Stats_Export")), c("Stats_Export", "Stats_Export")))
## ...and the convenient order too, so a reordering of the sheet cannot change the answer.
bothLitFirst <- gtm(c("Stats_Export", "Stats_Export (L)"),
                    list(c("TRUE", "TRUE"), c("FALSE", "FALSE")))
chk("...and the same answer with the literal column first",
    all(f(bothLitFirst, "Stats_Export") == c(TRUE, TRUE)))
chk("a missing flag column falls back to the default, and says it was not explicit",
    all(f(gtm("other", list(c("x", "y"))), "Stats_Export", default = TRUE)) &&
      !any(attr(f(gtm("other", list(c("x", "y"))), "Stats_Export"), "explicit")))
chk("...and the default is honoured when it is FALSE",
    !any(f(gtm("other", list(c("x", "y"))), "Stats_Export", default = FALSE)))
chk("matching is case-insensitive, which is what the OQ fixtures rely on",
    all(f(gtm("stats_export", list(c("TRUE", "TRUE"))), "Stats_Export")))
chk("a 0-row template yields a 0-length flag, not an error",
    length(f(data.frame(alias = character(0), Stats_Export = character(0),
                        stringsAsFactors = FALSE), "Stats_Export")) == 0L)

cat("\n-- 6. the published Info Variable is bare, including in the ugly cases --\n")
## actaInfoReport() publishes the bare name, which is what makes the whole 3.6.13 hint pass
## output-neutral. The hazard is a Variable that is ONLY an annotation, or one that collides with
## another once stripped -- both of which reach a CC BY 4.0 deposit if they get through.
ir <- h$actaInfoReport
info <- data.frame(Category = c("General", "General", "General"),
                   Variable = c("Downsample_to (N)", "miflowcyt_export (L)", "keywords"),
                   Value = c("10000", "FALSE", "a;b"),
                   stringsAsFactors = FALSE)
d <- ir(info)
chk("every published Variable is bare", !any(grepl("\\(", d$Variable)),
    paste(d$Variable, collapse = " | "))
chk("...and the VALUES are untouched by the stripping",
    all(c("10000", "FALSE", "a;b") %in% d$Value), paste(d$Value, collapse = " | "))
chk("a blank Variable row does not become a phantom published setting",
    { i2 <- rbind(info, data.frame(Category = "General", Variable = "", Value = "x",
                                   stringsAsFactors = FALSE))
      d2 <- ir(i2); !any(is.na(d2$Variable)) })

cat("\n-- 7. layoutVals(): the app's new Layout_Plate reader --\n")
## Sourced out of the app file, so this grades the shipped reader rather than a copy of it.
appSrc <- readLines(actaTestAppFile(), warn = FALSE)
appEnv <- new.env(parent = h)
.lv <- grep("^layoutVals <- function", appSrc)
if (length(.lv) == 1L) {
  depth <- 0L; end <- NA_integer_
  for (i in seq(.lv, length(appSrc))) {
    depth <- depth + lengths(regmatches(appSrc[i], gregexpr("{", appSrc[i], fixed = TRUE))) -
                     lengths(regmatches(appSrc[i], gregexpr("}", appSrc[i], fixed = TRUE)))
    if (depth <= 0L && i > .lv) { end <- i; break }
  }
  eval(parse(text = paste(appSrc[.lv:end], collapse = "\n")), envir = appEnv)
}
chk("layoutVals() was found in the app", exists("layoutVals", envir = appEnv))
lv <- if (exists("layoutVals", envir = appEnv)) get("layoutVals", envir = appEnv) else NULL

if (!is.null(lv)) {
  td <- file.path(tempdir(), "convention_edge"); dir.create(td, showWarnings = FALSE)
  wb <- function(nm, d) {
    p <- file.path(td, nm)
    openxlsx::write.xlsx(list(Layout_Plate = d), p, overwrite = TRUE)
    p
  }
  ## EVERY NON-WORKBOOK INPUT returns character(0) -- never NA, never an error, never a length-1
  ## empty string that a caller would mistake for a value.
  chk("a NA path yields character(0)", identical(lv(NA_character_, "ELN_ID"), character(0)))
  chk("a missing file yields character(0)",
      identical(lv(file.path(td, "nope.xlsx"), "ELN_ID"), character(0)))
  bad <- file.path(td, "notxlsx.xlsx"); writeLines("this is not a workbook", bad)
  chk("an unreadable file yields character(0), it does not propagate the read error",
      identical(lv(bad, "ELN_ID"), character(0)))
  nosheet <- file.path(td, "nosheet.xlsx")
  openxlsx::write.xlsx(list(Info = data.frame(a = 1)), nosheet, overwrite = TRUE)
  chk("a workbook with no Layout_Plate sheet yields character(0)",
      identical(lv(nosheet, "ELN_ID"), character(0)))

  p1 <- wb("one.xlsx", data.frame(ELN_ID = c("EXP00000001", "EXP00000001", NA, "", "NA", "  "),
                                  Other = 1:6, stringsAsFactors = FALSE))
  got <- lv(p1, "ELN_ID")
  chk("blank, NA and the literal 'NA' are all dropped", identical(got, rep("EXP00000001", 2)), got)
  chk("a column that is not there yields character(0)",
      identical(lv(p1, "No_Such_Column"), character(0)))
  chk("...and so does a column that is entirely blank",
      identical(lv(wb("blank.xlsx", data.frame(ELN_ID = c(NA, "", "NA"),
                                               stringsAsFactors = FALSE)), "ELN_ID"),
                character(0)))
  ## DUPLICATES ARE KEPT. ELN_ID is per well, so a 96-row column is 96 values; collapsing is
  ## actaElnLabel()'s job, and doing it here would hide a second experiment id from the caller.
  chk("duplicates are RETURNED, not uniqued -- that is the caller's decision",
      length(lv(p1, "ELN_ID")) == 2L)
  chk("...and actaElnLabel() is what collapses them",
      identical(h$actaElnLabel(lv(p1, "ELN_ID")), "EXP00000001"))
  chk("two different ids survive as both, and label as A+B",
      identical(h$actaElnLabel(lv(wb("two.xlsx",
                   data.frame(ELN_ID = c("EXP00000002", "EXP00000001"),
                              stringsAsFactors = FALSE)), "ELN_ID"), for_file = TRUE),
                "EXP00000002+EXP00000001"))
  ## A NUMERIC ELN_ID. Excel stores an all-digit id as a number, and as.character() on a double
  ## gives "1e+05" for some values -- so this is the shape that silently renames a folder.
  numr <- lv(wb("num.xlsx", data.frame(ELN_ID = c(100000, 26001460))), "ELN_ID")
  chk("a numeric id comes back without scientific notation",
      !any(grepl("e\\+", numr)), paste(numr, collapse = ", "))
  ## THE 3.6.13 READINESS CHECK, and the reason this reader strips at all: an annotated header must
  ## still resolve, or the hint pass breaks the app's archive naming all over again.
  chk("an ANNOTATED header still resolves -- layoutVals survives the 3.6.13 hint pass",
      identical(lv(wb("hint.xlsx", data.frame(`ELN_ID (req)` = "EXP00000001",
                                              check.names = FALSE)), "ELN_ID"),
                "EXP00000001"))
  ## ...and a COLLIDING pair must stop rather than pick one, the same as every other read site.
  chk("two headers colliding after stripping is an error, not a silent choice",
      identical(lv(wb("collide.xlsx",
                      data.frame(`ELN_ID` = "A", `ELN_ID (req)` = "B", check.names = FALSE)),
                   "ELN_ID"), character(0)))
}

cat("\n-- 8. the app's infoVal() AGREES with the package reader, on real workbooks --\n")
## THE DEFECT THIS SECTION EXISTS FOR, and it is the biggest thing the 3.6.12 pass found.
##
## The app's infoVal() matched `field` against the sheet's COLUMN NAMES. That is correct for a WIDE
## Info sheet. The sheet went LONG in 3.6.0 -- Category | Variable | Value | Description -- and this
## reader was never taught to pivot, so from 3.6.0 through 3.6.11 it returned NA for EVERY setting
## on every workbook ACTA ships. Silently, because NA is also the right answer for a setting nobody
## filled in. The analysis was never affected (run_acta() reads through the package); what was
## affected is everything the APP mirrors from the workbook.
##
## DIFFERENTIAL, NOT A FIXED EXPECTATION. Asserting "scrub_confidential is FALSE here" would pass
## against a reader that returns NA if someone later wrote the expectation from the broken output.
## Two readers, same workbook, same answer -- that cannot be satisfied by a reader that reads
## nothing.
appEnv2 <- new.env(parent = h)
.iv <- grep("^infoVal <- function", appSrc)
if (length(.iv) == 1L) {
  depth <- 0L; end2 <- NA_integer_
  for (i in seq(.iv, length(appSrc))) {
    depth <- depth + lengths(regmatches(appSrc[i], gregexpr("{", appSrc[i], fixed = TRUE))) -
                     lengths(regmatches(appSrc[i], gregexpr("}", appSrc[i], fixed = TRUE)))
    if (depth <= 0L && i > .iv) { end2 <- i; break }
  }
  eval(parse(text = paste(appSrc[.iv:end2], collapse = "\n")), envir = appEnv2)
}
chk("the app's infoVal() was found", exists("infoVal", envir = appEnv2))
if (exists("infoVal", envir = appEnv2)) {
  aiv <- get("infoVal", envir = appEnv2)
  fx  <- list.files(file.path(ACTA_REPO_ROOT, "inst", "extdata", "oq_small"),
                    pattern = "Instructions[.]xlsx$", recursive = TRUE, full.names = TRUE)
  ## Every setting the app actually asks for, plus Template_Version as a value that is never blank.
  KEYS <- c("titerpubs_export", "scrub_confidential", "compensation", "MM_R2_threshold",
            "dashboard_dir", "titration_export_dir", "Template_Version")
  disagree <- character(0); nonNA <- 0L
  for (f in fx) {
    d <- suppressMessages(as.data.frame(readxl::read_excel(f, sheet = "Info", col_types = "text",
                                                           .name_repair = "minimal")))
    for (k in KEYS) {
      a <- aiv(f, k); b <- h$actaInfoValue(d, k)
      if (!identical(a, b))
        disagree <- c(disagree, sprintf("%s/%s: app=%s pkg=%s", basename(dirname(f)), k,
                                        if (is.na(a)) "NA" else a, if (is.na(b)) "NA" else b))
      if (!is.na(b)) nonNA <- nonNA + 1L
    }
  }
  chk("the two readers agree on every setting, in every fixture", !length(disagree),
      paste(disagree, collapse = " | "))
  ## A CONTROL. If the fixtures stopped carrying values, the agreement above would be NA == NA for
  ## everything -- which is what the broken reader produced, and would pass.
  chk("control: the fixtures really do carry values for the reader to find", nonNA >= 9L,
      sprintf("%d non-NA settings across %d fixture(s)", nonNA, length(fx)))
  ## THE SHAPE THAT CAUSED IT: a long sheet must not read as empty.
  chk("a LONG Info sheet is read, not skipped",
      !is.na(aiv(fx[[1]], "Template_Version")), aiv(fx[[1]], "Template_Version"))
  ## ...and the wide shape still works, so the fix did not trade one layout for the other.
  wide <- file.path(tempdir(), "wide_info.xlsx")
  openxlsx::write.xlsx(list(Info = data.frame(Template_Version = "3.6.8",
                                              `miflowcyt_export (L)` = "TRUE",
                                              check.names = FALSE)), wide, overwrite = TRUE)
  chk("a WIDE Info sheet still reads, annotation and all",
      identical(aiv(wide, "Template_Version"), "3.6.8") &&
        identical(aiv(wide, "miflowcyt_export"), "TRUE"))
  chk("a missing setting is still NA, not an error", is.na(aiv(fx[[1]], "no_such_setting")))
  chk("a NA path and a missing file are still NA",
      is.na(aiv(NA_character_, "Template_Version")) &&
        is.na(aiv(file.path(tempdir(), "nope.xlsx"), "Template_Version")))
}

cat("\n-- 9. the DASHBOARD's infoVal() agrees with the package reader too --\n")
## THE SIBLING OF SECTION 8, and the one I did not write the first time.
##
## ACTA_Dashboard.R carries its OWN infoVal() because the file is deliberately standalone -- it
## never sources ACTA_Functions.R, so it cannot delegate the way the app now does. It had the same
## defect: it matched the wanted name against COLUMN HEADERS, which is right only for the WIDE Info
## sheet, and the sheet went LONG in 3.6.0. A standalone dashboard run therefore ignored
## dashboard_dir, titration_export_dir and dashboard_tabbed_by from 3.6.0 onward.
##
## REPORTED BY THE METRDY MAINTAINERS, from a reading of the 3.6.11 source -- not by anything here.
## Section 8 compared the APP's reader against the package reader and passed, because I tested the
## reader I had just fixed and not the one I had not touched. Two readers of one cell, again:
## [[acta-leak-gates-fail-open]] reached from the third direction.
dashSrc <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Dashboard.R")
if (!file.exists(dashSrc)) {
  chk("ACTA_Dashboard.R is present", FALSE, dashSrc)
} else {
  dL <- readLines(dashSrc, warn = FALSE)
  dEnv <- new.env(parent = h)
  for (fn in c("stripAnn", "normKey", "infoVal")) {
    i <- grep(sprintf("^%s <- function", fn), dL)
    if (length(i) != 1L) next
    depth <- 0L; end <- NA_integer_
    for (k in seq(i, length(dL))) {
      depth <- depth + lengths(regmatches(dL[k], gregexpr("{", dL[k], fixed = TRUE))) -
                       lengths(regmatches(dL[k], gregexpr("}", dL[k], fixed = TRUE)))
      if (depth <= 0L && k > i) { end <- k; break }
    }
    if (!is.na(end)) eval(parse(text = paste(dL[i:end], collapse = "\n")), envir = dEnv)
  }
  chk("the dashboard's infoVal() was found and sourced", exists("infoVal", envir = dEnv))
  if (exists("infoVal", envir = dEnv)) {
    div <- get("infoVal", envir = dEnv)
    KEYS <- c("dashboard_dir", "titration_export_dir", "dashboard_tabbed_by",
              "MM_R2_threshold", "compensation", "Template_Version", "titerpubs_export")
    fx <- list.files(file.path(ACTA_REPO_ROOT, "inst", "extdata", "oq_small"),
                     pattern = "Instructions[.]xlsx$", recursive = TRUE, full.names = TRUE)
    disagree <- character(0); nonNA <- 0L
    for (f in fx) {
      d <- suppressMessages(readxl::read_excel(f, sheet = "Info", .name_repair = "minimal"))
      for (k in KEYS) {
        a <- div(d, k); b <- h$actaInfoValue(as.data.frame(d), k)
        if (!identical(a, b))
          disagree <- c(disagree, sprintf("%s/%s: dash=%s pkg=%s", basename(dirname(f)), k,
                                          if (is.na(a)) "NA" else a, if (is.na(b)) "NA" else b))
        if (!is.na(b)) nonNA <- nonNA + 1L
      }
    }
    chk("the dashboard reader and the package reader agree on every setting",
        !length(disagree), paste(disagree, collapse = " | "))
    ## THE CONTROL. Without it, "they agree" is satisfied by both returning NA for everything --
    ## which is precisely what the broken reader did.
    chk("control: the fixtures carry non-blank settings for them to agree ABOUT", nonNA >= 6L,
        sprintf("%d non-NA across %d fixture(s)", nonNA, length(fx)))
    ## The LONG shape specifically, since that is the one that was unreadable.
    d1 <- suppressMessages(readxl::read_excel(fx[[1]], sheet = "Info", .name_repair = "minimal"))
    chk("a LONG Info sheet is readable by the dashboard", !is.na(div(d1, "Template_Version")),
        div(d1, "Template_Version"))
    ## ...and the WIDE shape still is, so the fix did not trade one layout for the other. Every
    ## pre-3.6.0 export is wide and a dashboard routinely pools runs from both sides.
    wide <- data.frame(Template_Version = "3.6.8", `miflowcyt_export (L)` = "TRUE",
                       check.names = FALSE, stringsAsFactors = FALSE)
    chk("...and a WIDE Info sheet still is, annotation and all",
        identical(div(wide, "Template_Version"), "3.6.8") &&
          identical(div(wide, "miflowcyt_export"), "TRUE"))
    chk("a setting absent from a long sheet is NA, not an error",
        is.na(div(d1, "no_such_setting")))
  }
}

cat(sprintf("\n%s\n", if (ok) "ALL PASS" else "FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
