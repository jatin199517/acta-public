## The workbook's self-documenting conventions: type/slot annotations, and `#` comment rows.
##
## WHY THIS GATE EXISTS. ACTA's headers document themselves -- `StainQty (N)`, `Fix (L)`,
## `Markername ($PnS)` -- and the whole scheme rests on ONE regex stripping them at read time. That
## regex had been retyped at five sites and TWO OF THEM DISAGREED: four used the anchored
## `\s*\([^()]*\)\s*$`, while the OQ check used a greedy `[[:space:]]*\(.*$` that eats from the
## FIRST bracket to the end. Same input, different answers, and the greedy one contradicted the
## documented contract. Section 5 pins the unification at source level so it cannot drift apart
## again -- a behavioural test alone would not have caught it, because no shipped header has a
## mid-string bracket yet.
##
## AND `#` COMMENT ROWS WERE HALF-ADOPTED. .actaTemplateLive() has excluded `#` aliases from
## gating_template for releases; Layout_Plate and Info had no such rule, so the same `#` an analyst
## learned in one sheet became a phantom well in another -- and, in Info, a published row, because
## actaInfoReport() emits every row it does not read as blank into the report and the deposited
## export. Section 3 pins both implementations against each other.

source(file.path(this.path::this.dir(), "acta_test_paths.R"))
ok <- TRUE
chk <- function(label, pass, detail = NULL) {
  d <- paste(as.character(detail), collapse = " ")
  if (!isTRUE(pass)) { ok <<- FALSE
    cat(sprintf("  [FAIL] %s%s\n", label, if (nzchar(d)) paste0(" -- ", d) else "")) }
  else cat(sprintf("  [ok]   %s\n", label))
}
## THIS CHECKOUT's sources, not the installed namespace.
h  <- actaTestFunctions()
bn <- h$actaBareName

cat("\n-- 1. actaBareName(): the anchored contract --\n")
chk("a type hint is removed",        identical(bn("StainQty (N)"), "StainQty"))
chk("a slot hint is removed",        identical(bn("Markername ($PnS)"), "Markername"))
chk("an unannotated name is untouched", identical(bn("Dirname"), "Dirname"))
chk("trailing whitespace goes too",  identical(bn("Fix (L)   "), "Fix"))
chk("it is vectorised",              identical(bn(c("Fix (L)", "Perm (L)")), c("Fix", "Perm")))
chk("NA stays NA",                   is.na(bn(NA_character_)))
chk("an empty vector survives",      identical(bn(character(0)), character(0)))
## THE 3.6.13 FORM ALREADY WORKS. The required marker goes INSIDE the brackets precisely because
## of this: `StainQty* (N)` would not strip, and every lookup keyed on `StainQty` would miss.
chk("a combined hint `(N, req)` strips",   identical(bn("StainQty (N, req)"), "StainQty"))
chk("a bare `(req)` strips",               identical(bn("Dirname (req)"), "Dirname"))
chk("an allowed-value list strips",        identical(bn("compensation (a | b | c)"), "compensation"))
## ONLY A TRAILING BRACKET, which is the whole difference from the greedy form that used to sit in
## the OQ check: that one would have returned "Rate" here and "Dose" below.
chk("a mid-string bracket SURVIVES",       identical(bn("Rate (per well) (N)"), "Rate (per well)"))
chk("...and with no trailing hint, nothing is stripped at all",
    identical(bn("Rate (per well)"), "Rate"), bn("Rate (per well)"))
## NESTED BRACKETS ARE NOT HANDLED, BY CONSTRUCTION. Asserted so the limit is a decision on the
## record rather than a surprise: keep annotations flat.
chk("a NESTED hint is left alone (documented limit)",
    identical(bn("Dose (N (0-100))"), "Dose (N (0-100))"), bn("Dose (N (0-100))"))

cat("\n-- 2. actaStripHeaderAnnotation(): the collision guard, and which sheet it names --\n")
sh <- h$actaStripHeaderAnnotation
chk("clean headers pass through",
    identical(sh(c("Dirname", "StainQty (N)")), c("Dirname", "StainQty")))
e <- tryCatch({ sh(c("StainQty (N)", "StainQty (uL)")); NULL }, error = conditionMessage)
chk("a collision is a hard error", !is.null(e))
chk("...naming BOTH original spellings",
    !is.null(e) && grepl("StainQty (N)", e, fixed = TRUE) && grepl("StainQty (uL)", e, fixed = TRUE))
chk("...and the sheet defaults to Layout_Plate", !is.null(e) && grepl("Layout_Plate", e, fixed = TRUE))
e2 <- tryCatch({ sh(c("a (N)", "a (L)"), where = "Info"); NULL }, error = conditionMessage)
## The Info sheet goes through the same stripper and used to report its collisions as Layout_Plate
## ones -- an error message pointing at the wrong sheet is worse than a vague one.
chk("...or the sheet it was actually given",
    !is.null(e2) && grepl("Info", e2, fixed = TRUE) && !grepl("Layout_Plate", e2, fixed = TRUE), e2)
chk("two DIFFERENT names that both annotate do not collide",
    identical(sh(c("Fix (L)", "Perm (L)")), c("Fix", "Perm")))

cat("\n-- 3. actaDropCommentRows(), and its agreement with .actaTemplateLive() --\n")
dc <- h$actaDropCommentRows
df <- data.frame(Dirname = c("A", "# a note", "B", NA, "C#D", " # spaced"),
                 Alias   = c("x", "y", "z", "w", "v", "u"), stringsAsFactors = FALSE)
r <- dc(df)
chk("a `#` first cell is dropped",            !("# a note" %in% r$Dirname))
chk("...and a LEADING-SPACE `#` too",         !(" # spaced" %in% r$Dirname))
chk("an NA first cell is KEPT",               any(is.na(r$Dirname)))
chk("a `#` MID-STRING is kept",               "C#D" %in% r$Dirname)
chk("ordinary rows survive",                  all(c("A", "B") %in% r$Dirname))
chk("exactly two rows were dropped",          nrow(r) == nrow(df) - 2L, nrow(r))
chk("a `#` in a LATER column is not a comment",
    nrow(dc(data.frame(a = "x", b = "# no", stringsAsFactors = FALSE))) == 1L)
chk("an empty frame is returned unchanged",   nrow(dc(df[0, , drop = FALSE])) == 0L)
chk("NULL is returned unchanged",             is.null(dc(NULL)))
chk("a zero-column frame does not error",     identical(ncol(dc(data.frame())), 0L))
## TWO IMPLEMENTATIONS OF ONE RULE, pinned against each other. gating_template keeps its own
## (.actaTemplateLive, which also drops blank aliases); if either drifts, the convention stops
## meaning one thing and the workbook's `#` becomes sheet-dependent again.
live <- h$.actaTemplateLive
cases <- c("Reagent", "# commented out", " # spaced", "C#D")
chk("gating_template agrees on every case",
    identical(unname(live(cases)),
              cases %in% dc(data.frame(alias = cases, stringsAsFactors = FALSE))$alias),
    paste(live(cases), collapse = "/"))

cat("\n-- 4. actaTemplateFlag(): ACTA's own flag columns become hintable --\n")
## actaTemplateFlag() returns a NAMED logical carrying an `explicit` attribute, so compare values
## only -- identical() against a bare c(TRUE, FALSE) fails on the attributes, not on the answer.
tf0 <- h$actaTemplateFlag
tf  <- function(...) { v <- tf0(...); attributes(v) <- NULL; v }
mk <- function(nm) { d <- data.frame(alias = c("Live", "Single"), stringsAsFactors = FALSE)
                     d[[nm]] <- c("TRUE", "FALSE"); d }
chk("an unannotated column still resolves",
    identical(unname(tf(mk("Stats_Export"), "stats_export", default = FALSE)), c(TRUE, FALSE)))
chk("an ANNOTATED column now resolves too",
    identical(unname(tf(mk("Stats_Export (L)"), "stats_export", default = FALSE)), c(TRUE, FALSE)))
chk("...case-insensitively, as before",
    identical(unname(tf(mk("STATS_EXPORT (L)"), "stats_export", default = FALSE)), c(TRUE, FALSE)))
chk("an absent column still falls back to the caller's default",
    all(tf(mk("Something_Else"), "stats_export", default = TRUE)))
## LITERAL BEATS BARE, so the fallback can never shadow a real column.
both <- data.frame(alias = c("Live", "Single"), Stats_Export = c("FALSE", "FALSE"),
                   `Stats_Export (L)` = c("TRUE", "TRUE"), check.names = FALSE,
                   stringsAsFactors = FALSE)
chk("with both spellings present, the EXACT header wins",
    identical(unname(tf(both, "stats_export", default = NA)), c(FALSE, FALSE)))

cat("\n-- 5. SOURCE INVARIANT: one regex, one place --\n")
rfiles <- list.files(file.path(ACTA_REPO_ROOT, "R"), pattern = "[.]R$", full.names = TRUE)
src    <- unlist(lapply(rfiles, readLines, warn = FALSE))
## The anchored pattern may appear exactly TWICE, and only as the two named constants:
##
##   ACTA_HEADER_ANNOTATION -> actaBareName()          headers and variable NAMES
##   ACTA_VALUE_ANNOTATION  -> actaDropTrailingParen()  VALUES, e.g. "Aurora (SN0000)"
##
## Two rules of identical shape, kept apart on purpose: the header rule is a parsing convenience
## somebody will widen one day, while the value rule is what keeps an instrument serial out of a
## CC BY 4.0 deposit. If they shared one constant, widening the first would silently weaken the
## second. Every OTHER site must call one of the two helpers.
##
## Written as a split literal so this line does not count as an occurrence of itself.
pat  <- paste0("\\\\s*", "\\\\([^()]*\\\\)", "\\\\s*$")
hits <- src[vapply(src, function(l) grepl(pat, l, fixed = TRUE), TRUE)]
chk("the anchored annotation regex is written exactly TWICE", length(hits) == 2L, length(hits))
chk("...and both are the named constants, nothing else",
    length(hits) == 2L &&
      all(grepl("^(ACTA_HEADER_ANNOTATION|ACTA_VALUE_ANNOTATION) *<-", trimws(hits))),
    paste(trimws(hits), collapse = " ;; "))
chk("the header helper reads the HEADER constant",
    any(grepl("^actaBareName *<-.*ACTA_HEADER_ANNOTATION", trimws(src))))
chk("the value helper reads the VALUE constant, not the header one",
    any(grepl("^actaDropTrailingParen *<-.*ACTA_VALUE_ANNOTATION", trimws(src))))
## The two security-relevant callers of the VALUE rule, named so a future edit has to come here.
chk("the equipment-serial scrub uses the value helper",
    any(grepl("actaDropTrailingParen", readLines(file.path(ACTA_REPO_ROOT, "R", "panel_inventory.R"),
                                                 warn = FALSE))))
chk("...and so does the TiterPubs model resolver",
    any(grepl("actaDropTrailingParen",
              readLines(file.path(ACTA_REPO_ROOT, "R", "TiterPubs_template_mapper.R"), warn = FALSE))))
## The GREEDY form may survive only where it belongs: dependency version constraints
## (`pkg (>= 1.0)`), which genuinely want it. It must never return to a header site.
greedy <- grep("[[:space:]]*\\\\(.*$", src, fixed = TRUE, value = TRUE)
chk("the greedy form survives at most once, and not at a header site",
    length(greedy) <= 1L && (!length(greedy) || grepl("version constraint", greedy[1])),
    paste(trimws(greedy), collapse = " ;; "))

cat("\n-- 6. gating_template is the ONE sheet read raw --\n")
## openCyto matches its ten columns by exact name, so annotating any of them would break gating.
## Pinned here because section 4 makes the three ACTA-owned columns hintable, and the next reader
## of that change could reasonably assume the whole sheet had become hintable.
scr <- readLines(file.path(ACTA_REPO_ROOT, "inst", "pipeline", "ACTA_Script.R"), warn = FALSE)
gl  <- grep("gating_template *<- *read_excel|gating_template<-read_excel", scr)
chk("gating_template is read without stripping", length(gl) == 1L &&
      !grepl("actaStripHeaderAnnotation", scr[gl[1]]), length(gl))
chk("Layout_Plate IS stripped",  any(grepl("names\\(metadata_file\\) *<- *actaStripHeaderAnnotation|names\\(metadata_file\\)<-actaStripHeaderAnnotation", scr)))
chk("Info IS stripped",          any(grepl("names\\(infoSheet\\) *<- *actaStripHeaderAnnotation|names\\(infoSheet\\)<-actaStripHeaderAnnotation", scr)))
chk("both sheets drop comment rows AT READ",
    sum(grepl("actaDropCommentRows", scr)) >= 2L, sum(grepl("actaDropCommentRows", scr)))

cat("\n-- 7. the published Info table never carries a hint --\n")
## Detail lives in test_info_report.R; this is the cross-check that the contract holds end to end,
## because this value reaches the deposited export.
ir <- h$actaInfoReport(data.frame(Category = "General", Variable = "Downsample_to (N)",
                                  Value = "100", Description = "d", stringsAsFactors = FALSE))
chk("a hinted variable publishes bare", identical(ir$Variable[1], "Downsample_to"), ir$Variable[1])

cat(if (ok) "\nWORKBOOK CONVENTION TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
