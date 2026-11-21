## The workbook column registry, held against the three things that can disagree with it: the
## SHIPPED TEMPLATE, the OQ FIXTURES, and the SOURCE.
##
## WHAT THIS GATE IS FOR. The instructions workbook is ACTA's entire input surface, and four
## separate readers consume it -- actaInfoValue() in the package, infoVal()/resolvePath() in the
## Shiny app, infoVal() in the standalone dashboard, and bare `metadata_file$X` in the pipeline.
## Every one of them is written to return NA for a name it cannot find, because an Info setting is
## legitimately optional. So a column that is renamed in the template, read by code and missing from
## the template, or read under one spelling by one reader and another by the next, produces NO
## ERROR ANYWHERE. It produces a default.
##
## That is the same shape as the ten findings in the 3.6.0 review: a reader that cannot read reports
## clean. This file is what makes those loud, and it is the gate that has to hold while 3.6.13
## annotates the template.
##
## IT FOUND TWO ON ITS FIRST RUN, both invisible to the other 41 scripts:
##   * ACTA_App.R named the archive folder from `Info!ELN_ID` -- a row that moved to Layout_Plate in
##     3.6.8 and that NO template has carried since. The lookup always returned NA, so every
##     app-driven copy archived into `Archived/<stamp>_run` instead of the experiment id. Section 5.
##   * `Info!scrub_eln_placeholder` is read, validated, and can stop a run, and appears in no
##     template. Section 5 forces it to be declared; ACTA_INFO_SITE_ONLY records that the absence
##     is deliberate rather than letting the check quietly pass.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
suppressWarnings(suppressMessages(library(readxl)))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, never the installed namespace.
h   <- actaTestFunctions()
reg <- h$actaColumnRegistry()
bare <- h$actaBareName

TEMPLATE <- file.path(ACTA_REPO_ROOT, "inst", "pipeline", "Template",
                      "TEMPLATE_Ab_Titration_Instructions.xlsx")
FIXTURES <- list.files(file.path(ACTA_REPO_ROOT, "inst", "extdata", "oq_small"),
                       pattern = "Instructions[.]xlsx$", recursive = TRUE, full.names = TRUE)

rd <- function(path, sheet)
  suppressMessages(as.data.frame(readxl::read_excel(path, sheet = sheet, col_types = "text",
                                                    .name_repair = "minimal")))
## The Info sheet is LONG: its names live in the `Variable` column, not in the header.
infoVars <- function(path) {
  d <- rd(path, "Info")
  v <- trimws(as.character(d[[2]]))
  v[!is.na(v) & nzchar(v)]
}
regFor <- function(s) reg[reg$sheet == s, , drop = FALSE]

cat("\n-- 0. the registry is well formed --\n")
chk("every row has a sheet and a name",
    all(nzchar(reg$sheet)) && all(nzchar(reg$name)), sum(!nzchar(reg$name)))
chk("no duplicate sheet+name", !any(duplicated(paste(reg$sheet, reg$name))),
    paste(reg$name[duplicated(paste(reg$sheet, reg$name))], collapse = ", "))
chk("names are already BARE -- the registry never stores an annotation",
    identical(reg$name, bare(reg$name)),
    paste(reg$name[reg$name != bare(reg$name)], collapse = ", "))
chk("every row carries a note for the operator", all(nzchar(reg$note)),
    paste(reg$name[!nzchar(reg$note)], collapse = ", "))
chk("sheets are the four real ones",
    setequal(unique(reg$sheet),
             c("Info", "Layout_Plate", "gating_template", "TiterPubs_template")),
    paste(unique(reg$sheet), collapse = ", "))
chk("every status is one of the three", all(reg$status %in% h$ACTA_COLUMN_STATUS),
    paste(unique(reg$status[!reg$status %in% h$ACTA_COLUMN_STATUS]), collapse = ", "))
## Only Info can hold a non-current row: the other three sheets are read positionally or by exact
## name, so there is nowhere for a superseded column to hide on them.
chk("only Info carries site_only or legacy rows",
    all(reg$sheet[reg$status != "current"] == "Info"),
    paste(reg$name[reg$status != "current" & reg$sheet != "Info"], collapse = ", "))
chk("a legacy row names its replacement", all(grepl("Superseded by", reg$note[reg$status == "legacy"])),
    paste(reg$name[reg$status == "legacy" & !grepl("Superseded by", reg$note)], collapse = ", "))

cat("\n-- 1. the annotation vocabulary is CLOSED --\n")
## Every hint the registry declares, and every annotation the shipped template actually writes, has
## to come from one list. Without this, 3.6.13 drifts into (num)/(number)/(numeric) one column at a
## time and nothing notices, because actaBareName() strips them all equally well.
used <- unique(reg$hint[nzchar(reg$hint)])
isChoices <- function(x) grepl("|", x, fixed = TRUE)
bad <- used[!(toupper(used) %in% toupper(h$ACTA_COLUMN_HINTS) | vapply(used, isChoices, TRUE))]
chk("every registry hint is in ACTA_COLUMN_HINTS (or is a choices list)",
    !length(bad), paste(bad, collapse = ", "))

## CASE. `(N)` and `(n)` behave identically, so only an explicit pin can keep the template tidy.
offcase <- reg$name[nzchar(reg$hint) & reg$hint != "" &
                    !(reg$hint %in% h$ACTA_COLUMN_HINTS) & toupper(reg$hint) %in% h$ACTA_COLUMN_HINTS]
chk("the ONLY non-canonical-case hint is the one pinned in ACTA_COLUMN_HINT_CASE_DEVIATIONS",
    setequal(offcase, h$ACTA_COLUMN_HINT_CASE_DEVIATIONS),
    sprintf("found [%s], pinned [%s]", paste(offcase, collapse = ", "),
            paste(h$ACTA_COLUMN_HINT_CASE_DEVIATIONS, collapse = ", ")))

cat("\n-- 2. Layout_Plate: the template and the registry agree, exactly --\n")
lpHdr  <- names(rd(TEMPLATE, "Layout_Plate"))
lpBare <- bare(lpHdr)
lpReg  <- regFor("Layout_Plate")
chk("every template header is declared", setequal(setdiff(lpBare, lpReg$name), character(0)),
    paste(setdiff(lpBare, lpReg$name), collapse = ", "))
chk("every declared column is in the template", setequal(setdiff(lpReg$name, lpBare), character(0)),
    paste(setdiff(lpReg$name, lpBare), collapse = ", "))
chk("the template header count matches the registry", length(lpHdr) == nrow(lpReg),
    sprintf("template %d, registry %d", length(lpHdr), nrow(lpReg)))

## THE ANNOTATION ITSELF, not just the bare name: the registry records what the workbook SAYS.
tplHint <- function(full, b) {
  s <- trimws(sub(paste0("^", gsub("([.|()\\^{}+$*?\\[\\]])", "\\\\\\1", b)), "", trimws(full)))
  if (!nzchar(s)) return("")
  sub("^\\(\\s*(.*?)\\s*\\)$", "\\1", s)
}
hintMismatch <- character(0)
for (i in seq_along(lpHdr)) {
  b <- lpBare[i]; j <- match(b, lpReg$name); if (is.na(j)) next
  got <- tplHint(lpHdr[i], b)
  if (!identical(got, lpReg$hint[j]))
    hintMismatch <- c(hintMismatch, sprintf("%s: template '%s' vs registry '%s'", b, got,
                                            lpReg$hint[j]))
}
chk("every Layout_Plate annotation matches the registry CHARACTER FOR CHARACTER",
    !length(hintMismatch), paste(hintMismatch, collapse = " | "))

cat("\n-- 3. gating_template: raw, and the openCyto ten are untouchable --\n")
gtHdr <- names(rd(TEMPLATE, "gating_template"))
gtReg <- regFor("gating_template")
chk("every gating_template header is declared", !length(setdiff(gtHdr, gtReg$name)),
    paste(setdiff(gtHdr, gtReg$name), collapse = ", "))
chk("every declared gating_template column is in the template", !length(setdiff(gtReg$name, gtHdr)),
    paste(setdiff(gtReg$name, gtHdr), collapse = ", "))
## NO ANNOTATION, EVER, on this sheet -- openCyto matches by exact name and would not find
## `gating_method (list)`. This is why ACTA_Script.R does not pass these names through the stripper.
chk("NO gating_template header carries an annotation", identical(gtHdr, bare(gtHdr)),
    paste(gtHdr[gtHdr != bare(gtHdr)], collapse = ", "))
chk("the registry declares no hint for any gating_template column", !any(nzchar(gtReg$hint)),
    paste(gtReg$name[nzchar(gtReg$hint)], collapse = ", "))
chk("the ten openCyto columns are all present and all declared",
    all(h$ACTA_OPENCYTO_COLUMNS %in% gtHdr) && all(h$ACTA_OPENCYTO_COLUMNS %in% gtReg$name),
    paste(setdiff(h$ACTA_OPENCYTO_COLUMNS, gtHdr), collapse = ", "))
## ...and the three ACTA flags ARE hintable, which is what actaTemplateFlag()'s bare fallback buys.
## Proven by asking for them under an annotated header rather than by reading the function.
gtFake <- rd(TEMPLATE, "gating_template")[0, , drop = FALSE]
names(gtFake)[match("Stats_Export", names(gtFake))] <- "Stats_Export (L)"
chk("actaTemplateFlag() finds an ANNOTATED ACTA flag column",
    !is.null(h$actaTemplateFlag(gtFake, "Stats_Export")))
chk("...and still finds the unannotated one",
    !is.null(h$actaTemplateFlag(rd(TEMPLATE, "gating_template"), "Stats_Export")))

cat("\n-- 4. Info and TiterPubs_template: the template and the registry agree --\n")
tplInfo <- bare(infoVars(TEMPLATE))
infReg  <- regFor("Info")
notCurrent <- infReg$name[infReg$status != "current"]
chk("every Info variable in the template is declared", !length(setdiff(tplInfo, infReg$name)),
    paste(setdiff(tplInfo, infReg$name), collapse = ", "))
## THE EXACT CORRESPONDENCE: template Info == the `current` rows, no more and no less. Stating it
## as an equality is the point -- "every current row is in the template" alone would let a row be
## demoted to legacy to silence a failure.
chk("the template's Info variables are EXACTLY the `current` rows",
    setequal(tplInfo, infReg$name[infReg$status == "current"]),
    sprintf("in template not current [%s], current not in template [%s]",
            paste(setdiff(tplInfo, infReg$name[infReg$status == "current"]), collapse = ", "),
            paste(setdiff(infReg$name[infReg$status == "current"], tplInfo), collapse = ", ")))
chk("a site_only or legacy setting is genuinely absent from the template",
    !any(notCurrent %in% tplInfo), paste(intersect(notCurrent, tplInfo), collapse = ", "))
tpHdr <- bare(names(rd(TEMPLATE, "TiterPubs_template")))
tpReg <- regFor("TiterPubs_template")
chk("TiterPubs_template agrees with the registry both ways", setequal(tpHdr, tpReg$name),
    paste(union(setdiff(tpHdr, tpReg$name), setdiff(tpReg$name, tpHdr)), collapse = ", "))

cat("\n-- 5. SOURCE: no reader may consume a key the registry has not declared --\n")
## FOUR READERS, scanned as text. This is the clause that caught Info!ELN_ID and
## scrub_eln_placeholder, and it is deliberately a scan of the LITERAL ARGUMENTS rather than a
## behavioural probe: the failure being hunted is a key nobody ever exercises.
srcFiles <- c(list.files(file.path(ACTA_REPO_ROOT, "R"), pattern = "[.]R$", full.names = TRUE),
              file.path(ACTA_REPO_ROOT, "inst", "pipeline",
                        c("ACTA_Script.R", "ACTA_App.R", "ACTA_Dashboard.R")))
srcFiles <- srcFiles[file.exists(srcFiles)]
src <- unlist(lapply(srcFiles, readLines, warn = FALSE))
src <- src[!grepl("^\\s*#", src)]                       # whole-line comments are prose, not calls
grab <- function(pat) {
  m <- regmatches(src, gregexpr(pat, src, perl = TRUE))
  v <- unlist(m)
  unique(sub('.*"([^"]*)".*', "\\1", v))
}
keys <- unique(c(grab('actaInfoValue\\([A-Za-z_.][A-Za-z0-9_.]*,\\s*"[^"]*"'),
                 grab('\\binfoVal\\([A-Za-z_.][A-Za-z0-9_.]*,\\s*"[^"]*"'),
                 grab('resolvePath\\([A-Za-z_.][A-Za-z0-9_.]*,\\s*"[^"]*"'),
                 grab('(?<![A-Za-z0-9_.])iv\\("[^"]*"')))
## A CONTROL. If the scan stops finding keys it is reporting "no undeclared key" vacuously -- the
## exact fail-open shape this file exists to catch. These four are read by four DIFFERENT readers.
chk("the source scan is alive and reaches all four readers",
    all(c("Template_Version", "Submitter_Name", "MM_R2_threshold", "compensation") %in% keys),
    sprintf("%d keys found: %s", length(keys), paste(sort(keys), collapse = ", ")))
undeclared <- setdiff(keys, infReg$name)
chk("every Info key read by any reader is declared in the registry",
    !length(undeclared), paste(undeclared, collapse = ", "))
## THE OTHER DIRECTION is deliberately NOT asserted: `flowjo_transformation_arg` and
## `Equipment_Make` reach actaInfoValue() through a FORMAL's default rather than a literal, so a
## declared-but-unscanned setting is normal and an "unused" check here would be noise.

cat("\n-- 6. col_req is asserted by RUNNING the pre-flight, not by reading it --\n")
## Drop each column in turn from a real fixture layout and see whether the real pre-flight objects.
## Grepping for `need <- c(...)` would pass against a pre-flight that had been deleted.
lay <- rd(FIXTURES[[1]], "Layout_Plate"); names(lay) <- bare(names(lay))
gt  <- rd(FIXTURES[[1]], "gating_template")
inf <- rd(FIXTURES[[1]], "Info");         names(inf) <- bare(names(inf))
fcsRoot <- file.path(tempdir(), "no_such_fcs_root")
preflightFails <- function(md) {
  grp <- tryCatch(h$actaTitrationGroups(md), error = function(e) NULL)
  if (is.null(grp)) return(TRUE)        # it could not even group the layout: that is a failure
  mk <- if ("Markername" %in% names(md)) "Markername" else NA_character_
  ch <- if ("Channel" %in% names(md)) "Channel" else NA_character_
  recs <- tryCatch(h$actaPrevalidateRecords(md, gt, inf, grp, fcsRoot,
                                            marker_col = mk, channel_col = ch),
                   error = function(e) NULL)
  if (is.null(recs)) return(TRUE)
  ids <- c("layout_columns", "marker_column", "fix_perm_columns", "eln_columns",
           "dirname_populated")
  any(vapply(recs, function(x) isTRUE(x$id %in% ids) && identical(x$status, "fail"), logical(1)))
}
## A CONTROL FIRST: the untouched fixture must NOT trip any of those records, or every drop below
## "fails" for a reason that has nothing to do with the dropped column.
chk("control: the intact fixture layout passes all the column checks", !preflightFails(lay))

lpWant <- lpReg$name[lpReg$col_req]
missedReq <- character(0); falseReq <- character(0)
for (nm in lpReg$name) {
  if (!nm %in% names(lay)) next
  if (nm %in% h$ACTA_MARKER_OR_CHANNEL) next          # either/or, asserted on its own below
  fails <- preflightFails(lay[, setdiff(names(lay), nm), drop = FALSE])
  if (nm %in% lpWant && !fails)  missedReq <- c(missedReq, nm)
  if (!(nm %in% lpWant) && fails) falseReq <- c(falseReq, nm)
}
chk("dropping a col_req column really does fail pre-flight", !length(missedReq),
    paste(missedReq, collapse = ", "))
chk("dropping a non-col_req column really does NOT fail pre-flight", !length(falseReq),
    paste(falseReq, collapse = ", "))
## The either/or: neither alone is required, but losing BOTH is fatal.
chk("Layout_Plate survives losing Markername OR Channel, but not both",
    !preflightFails(lay[, setdiff(names(lay), "Markername"), drop = FALSE]) &&
      preflightFails(lay[, setdiff(names(lay), h$ACTA_MARKER_OR_CHANNEL), drop = FALSE]))
gtWant <- regFor("gating_template")$name[regFor("gating_template")$col_req]
chk("the gating_template col_req set is openCyto's own five",
    setequal(gtWant, c("alias", "pop", "parent", "dims", "gating_method")),
    paste(gtWant, collapse = ", "))
chk("no Info setting is col_req -- the whole sheet is optional",
    !any(infReg$col_req), paste(infReg$name[infReg$col_req], collapse = ", "))

cat("\n-- 7. the OQ fixtures do not drift from the template --\n")
## Three shipped fixtures, three chances for a workbook to quietly stop representing the template
## it is supposed to exercise.
for (f in FIXTURES) {
  tag <- basename(dirname(f))
  fh  <- bare(names(rd(f, "Layout_Plate")))
  chk(sprintf("%s: Layout_Plate headers match the template", tag), setequal(fh, lpBare),
      paste(union(setdiff(fh, lpBare), setdiff(lpBare, fh)), collapse = ", "))
  fi <- bare(infoVars(f))
  ## A fixture may OMIT an optional Info setting; it may not invent one.
  chk(sprintf("%s: every Info variable is declared", tag), !length(setdiff(fi, infReg$name)),
      paste(setdiff(fi, infReg$name), collapse = ", "))
  fg <- names(rd(f, "gating_template"))
  chk(sprintf("%s: no gating_template header carries an annotation", tag),
      identical(fg, bare(fg)), paste(fg[fg != bare(fg)], collapse = ", "))
  ## CASE-INSENSITIVE, and that is a finding rather than a concession: all three fixtures write
  ## `stats_export` where the template writes `Stats_Export`. actaTemplateFlag() lowercases both
  ## sides, so they work -- and the fixtures are therefore the only thing exercising that. The
  ## openCyto ten get no such latitude; they are checked for EXACT case just below.
  chk(sprintf("%s: every gating_template header is declared (case-insensitively)", tag),
      !length(setdiff(tolower(fg), tolower(gtReg$name))),
      paste(setdiff(tolower(fg), tolower(gtReg$name)), collapse = ", "))
  chk(sprintf("%s: openCyto's ten appear with EXACT case", tag),
      all(h$ACTA_OPENCYTO_COLUMNS %in% fg),
      paste(setdiff(h$ACTA_OPENCYTO_COLUMNS, fg), collapse = ", "))
  ## The flags really do resolve from this fixture's own spelling -- asked of the real helper, not
  ## asserted from the header list.
  gtf <- rd(f, "gating_template")
  chk(sprintf("%s: all three ACTA flag columns resolve through actaTemplateFlag()", tag),
      all(vapply(h$ACTA_GATING_FLAG_COLUMNS,
                 function(cl) !is.null(h$actaTemplateFlag(gtf, cl)) &&
                              any(attr(h$actaTemplateFlag(gtf, cl), "explicit")), logical(1))))
}

cat("\n-- 8. req means the cell is FILLED, in every fixture --\n")
## col_req is about the column existing; req is about the operator having answered. The three
## fixtures are the only workbooks ACTA ships that are meant to be complete.
for (f in FIXTURES) {
  tag <- basename(dirname(f))
  d <- rd(f, "Layout_Plate"); names(d) <- bare(names(d))
  ## Only the wells that are actually in play: a layout legitimately carries unused wells.
  live <- if ("Dirname" %in% names(d)) !is.na(d$Dirname) & nzchar(trimws(d$Dirname)) else rep(TRUE, nrow(d))
  blanks <- character(0)
  for (nm in lpReg$name[lpReg$req & lpReg$sheet == "Layout_Plate"]) {
    if (!nm %in% names(d)) { blanks <- c(blanks, paste0(nm, " (absent)")); next }
    v <- trimws(as.character(d[[nm]]))[live]
    if (any(is.na(v) | !nzchar(v))) blanks <- c(blanks, sprintf("%s (%d blank)", nm,
                                                                sum(is.na(v) | !nzchar(v))))
  }
  chk(sprintf("%s: every req Layout_Plate cell is filled on live wells", tag), !length(blanks),
      paste(blanks, collapse = ", "))
  iv <- rd(f, "Info")
  vars <- bare(trimws(as.character(iv[[2]])))
  vals <- trimws(as.character(iv[[3]]))
  miss <- character(0)
  for (nm in infReg$name[infReg$req]) {
    j <- match(nm, vars)
    if (is.na(j) || is.na(vals[j]) || !nzchar(vals[j])) miss <- c(miss, nm)
  }
  chk(sprintf("%s: every req Info setting is filled", tag), !length(miss),
      paste(miss, collapse = ", "))
}

cat("\n-- 9. the app reads ELN_ID from the LAYOUT, and really gets the id --\n")
## THE DEFECT THIS FILE FOUND. `infoVal(layout, "ELN_ID")` returned NA on every workbook ACTA has
## shipped since 3.6.8, so the archive folder was always named "run". Nothing failed; a default was
## taken. The fix reads Layout_Plate, where the pipeline has always read it.
##
## EXERCISED, NOT GREPPED. layoutVals() is sourced out of the app file and run against a real
## fixture workbook, so a regression that reinstates the Info lookup fails here even if the comment
## above it still says "layout".
appSrc <- readLines(actaTestAppFile(), warn = FALSE)
## CODE ONLY. The first version of this assertion matched the comment ABOVE the fix, which quotes
## the old call to explain it -- a check that forbids naming the defect you repaired is a check
## that pushes the explanation out of the file.
appCode <- appSrc[!grepl("^\\s*#", appSrc)]
chk("the app no longer reads ELN_ID from the Info sheet",
    !any(grepl('infoVal\\(layout,\\s*"ELN_(ID|Name)"', appCode)),
    paste(grep('infoVal\\(layout,\\s*"ELN_', appCode, value = TRUE), collapse = " | "))
## ...and the replacement is really wired in, not merely defined. Deleting the call site alone
## would satisfy the assertion above.
chk("the archive/copy path reads the layout column",
    any(grepl('layoutVals\\(layout,\\s*"ELN_ID"\\)', appCode)))

## Source just the helper, with the package functions it needs in scope.
appEnv <- new.env(parent = h)
.lv <- grep("^layoutVals <- function", appSrc)
if (length(.lv) == 1L) {
  ## To the closing brace of the function, found by brace balance rather than by counting lines.
  depth <- 0L; end <- NA_integer_
  for (i in seq(.lv, length(appSrc))) {
    depth <- depth + lengths(regmatches(appSrc[i], gregexpr("{", appSrc[i], fixed = TRUE))) -
                     lengths(regmatches(appSrc[i], gregexpr("}", appSrc[i], fixed = TRUE)))
    if (depth <= 0L && i > .lv) { end <- i; break }
  }
  eval(parse(text = paste(appSrc[.lv:end], collapse = "\n")), envir = appEnv)
}
chk("layoutVals() was found and sourced out of the app", exists("layoutVals", envir = appEnv))
if (exists("layoutVals", envir = appEnv)) {
  lvf <- get("layoutVals", envir = appEnv)
  got <- lvf(FIXTURES[[1]], "ELN_ID")
  chk("layoutVals() returns the fixture's real ELN_ID values", length(got) > 0 && all(nzchar(got)),
      sprintf("%d value(s): %s", length(got), paste(unique(got), collapse = ", ")))
  ## The whole point: the id survives into the folder name instead of collapsing to "run".
  lbl <- h$actaElnLabel(got, for_file = TRUE)
  chk("...and actaElnLabel() turns them into a usable folder name that is NOT 'run'",
      nzchar(lbl) && !identical(lbl, "run") && !identical(lbl, "NA") &&
        !grepl("[/\\\\]", lbl), lbl)
  ## A CONTROL: the OLD expression, verbatim, on the same workbook. If this ever stops returning NA
  ## the defect was never real and the assertions above are decoration.
  oldWay <- tryCatch({
    d <- suppressMessages(readxl::read_excel(FIXTURES[[1]], sheet = "Info", .name_repair = "minimal"))
    names(d) <- bare(names(d))
    if (!"ELN_ID" %in% names(d)) NA_character_ else trimws(as.character(d[["ELN_ID"]][1]))
  }, error = function(e) NA_character_)
  chk("control: the OLD Info lookup really does come back empty on a shipped workbook",
      is.na(oldWay), oldWay)
  chk("layoutVals() returns character(0), not NA, for a column that is not there",
      identical(lvf(FIXTURES[[1]], "No_Such_Column"), character(0)))
}

cat(sprintf("\n%s\n", if (ok) "ALL PASS" else "FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
