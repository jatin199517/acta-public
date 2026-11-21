## actaTiterPubsGating(): the parent population a reagent was measured inside, as published.
##
## THE DEFECT THIS FILE EXISTS FOR, and it had NO test at all until 2026-11-20.
##
## The function defaulted to the literal "Live" and the call site passed
## `tv("Gating_Parent", "Live")` -- a column that does not exist on the TiterPubs_template sheet,
## so the default always won. The walk therefore always began at Live and could not contain any
## gate below it. Measured across the seven validation datasets: two reagents sit lower --
## one on `CD3+` and one on `DUMP-` -- and all 8 of their published templates said
## "live single cells". The CD3 case was reported by the Metrdy maintainers, from the page rather
## than from the code; the dump-gated one nobody had noticed.
##
## A SECOND FAULT SAT BEHIND IT. `parent` cells hold NODE names (`CD3+`, `DUMP-`) while the alias
## column holds `CD3` / `DUMP`, so even once the right starting point was passed the walk broke on
## its first step and the chain held one element with no Live and no Single in it. Both halves are
## pinned below, because fixing either alone still publishes the wrong phrase.

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
f <- h$actaTiterPubsGating

## The three real shapes, as gating_template writes them. `parent` carries the node name.
gt <- function(...) {
  rows <- list(...)
  data.frame(alias  = vapply(rows, `[[`, "", 1),
             parent = vapply(rows, `[[`, "", 2),
             stringsAsFactors = FALSE)
}
LIVE  <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"),
            c("Reagent","Live"))
CD3   <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"),
            c("CD3","Live"), c("Reagent","CD3+"))
DUMP  <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"),
            c("DUMP","Live"), c("Reagent","DUMP-"))

cat("\n-- 1. the reagent's OWN parent is read, not assumed --\n")
chk("a reagent on Live publishes 'live single cells'",
    identical(f(LIVE), "live single cells"), f(LIVE))
## THE HEADLINE. Before the fix this returned "live single cells" for CD3 too.
chk("a reagent on CD3+ publishes 'live single CD3+'",
    identical(f(CD3), "live single CD3+"), f(CD3))
## A CONTROL on the fixture: if CD3 and LIVE ever produce the same answer the check above is
## vacuous, which is exactly the state the code was in.
chk("control: the CD3 and Live shapes do NOT produce the same phrase",
    !identical(f(CD3), f(LIVE)), sprintf("CD3=%s Live=%s", f(CD3), f(LIVE)))

cat("\n-- 2. a parent is a NODE name; an alias is not --\n")
## `CD3+` must resolve to the alias `CD3` or the walk stops on its first step.
chk("a +/- suffixed parent still finds its alias row",
    identical(f(CD3), "live single CD3+"))
chk("a '-' suffixed parent walks too (DUMP- reaches Live and Single)",
    identical(f(DUMP), "live single cells"), f(DUMP))
## ...and the proof that it really walked rather than stopping: a chain that stops at DUMP- has
## neither Live nor Single in it, and the function would say "list others below".
chk("control: a BROKEN walk would have said something else entirely",
    !identical(f(DUMP), "list others below"))
## A template that legitimately names an alias with a sign is still matched literally first.
LIT <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"),
          c("CD4+","Live"), c("Reagent","CD4+"))
chk("an alias that genuinely ends in '+' is matched literally",
    identical(f(LIT), "live single CD4+"), f(LIT))

cat("\n-- 2b. a NEGATIVE selection names what it excluded --\n")
## A dump gate selects by exclusion, so no CD\d+ token appears and the chain fell through to the
## bare "live single cells" -- true, but it omits the gate the reagent was measured inside, and it
## made that run publish the same phrase as a dataset with no dump gate at all. TiterPubs takes free
## text here and normalises it their side, so the accurate phrase is published rather than the
## nearest menu term. The DIMS name the markers; the alias ("DUMP") means nothing to a reader.
gtd <- function(...) {
  rows <- list(...)
  data.frame(alias  = vapply(rows, `[[`, "", 1),
             parent = vapply(rows, `[[`, "", 2),
             dims   = vapply(rows, `[[`, "", 3),
             stringsAsFactors = FALSE)
}
DUMPD <- gtd(c("Cells","root",""), c("Single_cells","Cells",""), c("Live","Single_cells","eFluor 780-A"),
             c("DUMP","Live","CD14_CD16, FSC-A"), c("Reagent","DUMP-",""))
chk("a dump gate publishes the markers it excluded, not 'live single cells'",
    identical(f(DUMPD), "live single CD14/CD16-negative cells"), f(DUMPD))
## A CONTROL: the generic phrase is what it used to say, so if these ever coincide the check is dead.
chk("control: that is NOT the phrase a no-dump dataset gets",
    !identical(f(DUMPD), f(LIVE)))
## The scatter axis a 2-D gate carries must not become the marker name.
chk("the FSC/SSC axis is dropped, not published as the marker",
    !grepl("FSC|SSC", f(DUMPD)), f(DUMPD))
## Without a dims column there is nothing to name, so fall back rather than invent.
chk("no dims column -> falls back to the generic phrase",
    identical(f(DUMP), "live single cells"), f(DUMP))
## A POSITIVE marker still wins over this branch -- CD3+ is more informative than its dims.
CD3D <- gtd(c("Cells","root",""), c("Single_cells","Cells",""), c("Live","Single_cells",""),
            c("CD3","Live","CD3, FSC-A"), c("Reagent","CD3+",""))
chk("a '+' parent still publishes the marker, not a negative phrase",
    identical(f(CD3D), "live single CD3+"), f(CD3D))

cat("\n-- 3. the override still works, and still wins --\n")
chk("an explicit parent overrides the reagent row", identical(f(CD3, "Live"), "live single cells"))
chk("a blank override falls back to the reagent row", identical(f(CD3, ""), "live single CD3+"))
chk("an NA override falls back too", identical(f(CD3, NA_character_), "live single CD3+"))

cat("\n-- 4. degenerate inputs do not error --\n")
chk("NULL gating -> NA", is.na(f(NULL)))
chk("a 0-row frame -> NA", is.na(f(LIVE[0, , drop = FALSE])))
chk("no alias/parent columns -> NA", is.na(f(data.frame(x = 1))))
## No Reagent row at all: fall back to Live rather than erroring, which is what every pre-3.6.12
## run effectively did.
NOREAG <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"))
chk("no Reagent row -> falls back to Live", identical(f(NOREAG), "live single cells"), f(NOREAG))
## A CYCLE must terminate. The guard is 20 steps.
CYC <- gt(c("A","B"), c("B","A"), c("Reagent","A"))
chk("a cyclic template terminates instead of hanging",
    is.character(f(CYC)) && length(f(CYC)) == 1L, f(CYC))
## `#`-commented rows are not live and must not be walked into.
HASH <- gt(c("Cells","root"), c("Single_cells","Cells"), c("Live","Single_cells"),
           c("#CD3","Live"), c("Reagent","Live"))
chk("a commented-out row is ignored", identical(f(HASH), "live single cells"), f(HASH))

cat("\n-- 5. the SHIPPED fixtures, through the real reader --\n")
## OQ_Test1/2/3 all gate the reagent on Live, so all three must still say "live single cells".
## They are the regression anchor: the fix must not move a dataset whose parent really is Live.
root <- file.path(ACTA_REPO_ROOT, "inst", "extdata", "oq_small")
for (ds in c("OQ_Test1", "OQ_Test2", "OQ_Test3")) {
  wb <- file.path(root, ds, sprintf("%s_Ab_Titration_Instructions.xlsx", ds))
  if (!file.exists(wb)) { chk(sprintf("%s workbook present", ds), FALSE, wb); next }
  g <- as.data.frame(suppressMessages(
    readxl::read_excel(wb, sheet = "gating_template", col_types = "text", .name_repair = "minimal")))
  a <- trimws(as.character(g$alias)); p <- trimws(as.character(g$parent))
  k <- which(!is.na(a) & !startsWith(a, "#") & tolower(a) %in% c("reagent", "antibody"))
  par <- if (length(k)) p[k[1]] else NA_character_
  chk(sprintf("%s: reagent parent is %s, published as '%s'", ds, par, f(g)),
      identical(f(g), "live single cells"))
}

cat(sprintf("\n%s\n", if (ok) "ALL PASS" else "FAILURES ABOVE"))
quit(status = if (ok) 0L else 1L)
