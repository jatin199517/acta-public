## Guards which FOLDERS the dashboard treats as runs, which is where the same export came to be
## pooled twice. Reported carried-forward from 3.2.1 and measured here.
##
## pick() reads a run folder AND that folder's Outputs/. Once the scan root itself counts as a run
## -- which it does as soon as an export sits loose in it -- enumerating scanRoot/Outputs as a
## sibling run reads the same export a second time.
##
## THE TRAP THIS GATE EXISTS TO HOLD DOWN is the obvious fix. Excluding Outputs/ from the subfolder
## scan unconditionally makes case A correct and silently breaks case B, where the scan root is NOT
## a run, pick(scanRoot) therefore never runs, and that subfolder is the only route to its export.
## A de-duplication that withholds a run is worse than the duplication it removes, so B is asserted
## with the same weight as A.
##
## Drives the REAL generator in a child process, because the defect was in how it enumerates
## folders, not in a helper that could be called directly.
## Rscript <this> [version_dir]
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
vd <- actaTestVersionDir()
cat("version_dir:", basename(vd), "\n")
ok <- TRUE
chk <- function(what, cond) { if (!cond) ok <<- FALSE; cat(sprintf("  [%s] %s\n", if (cond) "ok" else "FAIL", what)) }

## `vd` is the CODE folder in both layouts -- inst/pipeline under a package, the version folder
## under a script tree -- so the generator is beside it either way, and the cases come from
## actaTestCaseDirs() rather than a hand-built path. Spelling those paths by hand is what made the
## first draft of this gate skip silently on a tree that had everything it needed.
gen     <- list.files(vd, pattern = "^ACTA_Dashboard.*[.]R$", full.names = TRUE)
cases   <- actaTestCaseDirs()
exports <- Sys.glob(file.path(cases, "Outputs", "*TitrationExport*.xlsx"))

## VERIFIED BEFORE IT IS TRUSTED, not assumed: the fixture needs two real exports of DIFFERENT row
## counts, because equal counts cannot tell a double-read from a correct read. Without them there is
## nothing to measure, so this skips with a printed reason rather than asserting something weaker.
if (length(gen) != 1L || length(exports) < 2L) {
  cat("  [skip] needs one dashboard generator and two OQ exports on disk;",
      sprintf("found %d and %d.\n", length(gen), length(exports)))
  cat("         run the OQ cases first -- the exports are run artefacts, not fixtures.\n")
  cat(if (ok) "\nDASHBOARD RUN-SCAN TESTS SKIPPED\n" else "\nFAILURES ABOVE\n")
  quit(status = 0)
}
nrows <- function(f) nrow(suppressMessages(readxl::read_excel(f, sheet = "TitrationExport")))
n <- vapply(exports, nrows, integer(1))
small <- exports[[which.min(n)]]; big <- exports[[which.max(n)]]
if (min(n) == max(n)) {
  cat("  [skip] the exports on disk have equal row counts, so a double read is unmeasurable.\n")
  quit(status = 0)
}
cat(sprintf("fixture exports: %d row(s) and %d row(s)\n", min(n), max(n)))

## Reads the row count the generator itself reports, which is the number that goes into the file.
rowsFor <- function(scan) {
  out <- file.path(dirname(scan), "out"); unlink(out, recursive = TRUE); dir.create(out)
  old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR", "ACTA_LAYOUT_DIR"), unset = NA)
  Sys.setenv(ACTA_DASHBOARD_DIR = out, ACTA_EXPORT_DIR = scan, ACTA_LAYOUT_DIR = cases[[1]])
  on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
              do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(gen)), stdout = TRUE, stderr = TRUE))
  hit <- grep("row\\(s\\) from", log, value = TRUE)
  ## The html is left in place so the label assertions below can read what the page will show.
  LAST_OUT <<- out
  if (!length(hit)) return(NA_integer_)
  as.integer(sub(".*?([0-9]+) row\\(s\\) from.*", "\\1", hit[[1]]))
}
LAST_OUT <- NA_character_
mk <- function(nm, root_files, sub, sub_files) {
  d <- file.path(tempdir(), nm); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "scan", sub), recursive = TRUE)
  for (f in root_files) file.copy(f, file.path(d, "scan", basename(f)))
  for (f in sub_files)  file.copy(f, file.path(d, "scan", sub, basename(f)))
  file.path(d, "scan")
}

cat("=== A. the scan root and its own Outputs/ are ONE run ===\n")
a <- rowsFor(mk("acta_dash_a", small, "Outputs", big))
chk(sprintf("root + Outputs/ pools each export once (%s, expected %d)", a, min(n) + max(n)),
    identical(a, min(n) + max(n)))

cat("=== B. ...and Outputs/ is still the only route when the root is NOT a run ===\n")
b <- rowsFor(mk("acta_dash_b", character(0), "Outputs", big))
chk(sprintf("Outputs/ alone is still indexed (%s, expected %d)", b, max(n)), identical(b, max(n)))

cat("=== C. two runs sharing a basename are both read ===\n")
cc <- rowsFor(mk("acta_dash_c", small, "scan", big))
chk(sprintf("a basename collision neither double-counts nor drops (%s, expected %d)",
            cc, min(n) + max(n)), identical(cc, min(n) + max(n)))

cat("\n=== D. two columns that differ only by their annotation keep both, and neither gets a .1 ===\n")
## The shipped exports really do carry `Combinatorial_group (n)` and `Combinatorial_group` -- one
## supplied by the analyst, one derived -- so the collision is REAL DATA rather than a fixture.
## stripAnn() makes them the same string and make.unique() disambiguated the second as
## `Combinatorial_group.1`; the label is derived from the name, so ".1" reached the page. The
## annotation is restored in the LABEL only: the name stays stripped and unique, because keyOf(),
## EXPORT_HIDE, MONO and LABELS all key off it.
local({
  if (is.na(LAST_OUT) || !dir.exists(LAST_OUT)) {
    cat("  [skip] no generated html to read\n"); return(invisible(NULL))
  }
  f <- list.files(LAST_OUT, pattern = "[.]html$", full.names = TRUE)
  if (!length(f)) { cat("  [skip] the generator wrote no html\n"); return(invisible(NULL)) }
  txt  <- paste(readLines(f[[1]], warn = FALSE), collapse = "\n")
  labs <- unlist(regmatches(txt, gregexpr('"label"\\s*:\\s*"[^"]*"', txt)))
  labs <- sub('^"label"\\s*:\\s*"', "", sub('"$', "", labs))
  chk(sprintf("the page carries column labels at all (%d)", length(labs)), length(labs) > 0L)
  ## THE DEFECT: any label ending in a make.unique() suffix is a name that leaked into the UI.
  .dot <- grep("[.][0-9]+$", labs, value = TRUE)
  chk(sprintf("no label carries a make.unique() suffix%s", 
              if (length(.dot)) paste0(" -- found ", paste(.dot, collapse = ", ")) else ""),
      length(.dot) == 0L)
  ## AND THE ANNOTATION SURVIVES, which is what tells a reader which column came from the
  ## instructions sheet and which ACTA derived. Asserted on the pair that actually collides.
  chk(sprintf("both Combinatorial columns are labelled, annotation intact (%s)",
              paste(grep("ombinatorial", labs, value = TRUE), collapse = " | ")),
      any(grepl("^Combinatorial group [(]n[)]$", labs)) &&
      any(grepl("^Combinatorial group$", labs)))
})

cat("\n=== E. the Plate map column exists and sits before Concatenation ===\n")
local({
  if (is.na(LAST_OUT) || !dir.exists(LAST_OUT)) {
    cat("  [skip] no generated html to read\n"); return(invisible(NULL))
  }
  f <- list.files(LAST_OUT, pattern = "[.]html$", full.names = TRUE)
  if (!length(f)) { cat("  [skip] the generator wrote no html\n"); return(invisible(NULL)) }
  txt  <- paste(readLines(f[[1]], warn = FALSE), collapse = "\n")
  labs <- unlist(regmatches(txt, gregexpr('"label"\\s*:\\s*"[^"]*"', txt)))
  labs <- sub('^"label"\\s*:\\s*"', "", sub('"$', "", labs))
  .pm <- match("Plate map", labs); .cc <- match("Concatenation", labs)
  chk("the Plate map column is present", !is.na(.pm))
  ## ORDER, not just presence: it was asked for BEFORE Concatenation, matching the report -- the
  ## plate as laid out, then the raw separation, then the number derived from it.
  chk(sprintf("...and it comes before Concatenation (positions %s and %s)", .pm, .cc),
      !is.na(.pm) && !is.na(.cc) && .pm < .cc)
})

cat("\n=== F. the Plate map column actually links the figure ===\n")
local({
  ## NEEDS A REAL Outputs/ WITH PLOTS IN IT, which is a RUN ARTEFACT: gitignored, absent from a
  ## fresh clone and from CI. So this one skips loudly rather than asserting on an empty figure
  ## list, which would pass while linking nothing -- the shape of gate this suite keeps finding.
  src <- Filter(function(d) length(list.files(file.path(d, "Outputs", "Plots"),
                                              pattern = "^PlateMap.*[.]png$")), cases)
  if (!length(src)) {
    cat("  [skip] no case has Outputs/Plots/PlateMap*.png on disk (run an OQ case first)\n")
    return(invisible(NULL))
  }
  d <- file.path(tempdir(), "acta_dash_pm"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "scan"), recursive = TRUE); dir.create(file.path(d, "out"))
  file.copy(file.path(src[[1]], "Outputs"), file.path(d, "scan"), recursive = TRUE)
  file.rename(file.path(d, "scan", "Outputs"), file.path(d, "scan", "RUN1"))
  old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR", "ACTA_LAYOUT_DIR"), unset = NA)
  Sys.setenv(ACTA_DASHBOARD_DIR = file.path(d, "out"), ACTA_EXPORT_DIR = file.path(d, "scan"),
             ACTA_LAYOUT_DIR = src[[1]])
  on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
              do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
  invisible(suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(gen)), stdout = TRUE, stderr = TRUE)))
  h <- list.files(file.path(d, "out"), pattern = "[.]html$", full.names = TRUE)
  if (!length(h)) { chk("the generator produced a dashboard for the figure check", FALSE); return() }
  txt <- paste(readLines(h[[1]], warn = FALSE), collapse = "\n")
  m   <- regmatches(txt, regexpr('"platemap"\\s*:\\s*\\[[^]]*\\]', txt))
  chk(sprintf("a row carries a platemap link  [%s]", if (length(m)) substr(m, 1, 60) else "none"),
      length(m) == 1L && grepl("PlateMap", m, fixed = TRUE) && grepl("[.]png", m))
})

cat("\n=== G. the tab setting comes from the newest EXPORT, not the instructions workbook ===\n")
local({
  ## The workbook describes ONE RUN; how a collection is grouped into tabs is a property of the
  ## COLLECTION. It used to come from whatever workbook ACTA_LAYOUT_DIR pointed at, which need
  ## have nothing to do with the runs being pooled -- rebuild a dashboard over an archive and the
  ## tabbing was decided by whichever unrelated experiment happened to be open.
  ##
  ## THE SHIPPED CASES ALL SAY ScriptVersion, in both the workbook and the export, so there is no
  ## natural discrimination to lean on: the fixture has to make the two DISAGREE. Only the
  ## workbook copy is rewritten -- the generator reads nothing from it but the Info sheet, and it
  ## is a throwaway. Do not do this to a workbook that ships (see the note on openxlsx round-trips).
  if (!requireNamespace("openxlsx", quietly = TRUE) || !requireNamespace("readxl", quietly = TRUE)) {
    cat("  [skip] needs openxlsx and readxl\n"); return(invisible(NULL))
  }
  .case <- cases[[1]]
  .wb   <- Sys.glob(file.path(.case, "*Titration_Instructions*.xlsx"))
  .ex   <- Sys.glob(file.path(.case, "Outputs", "*TitrationExport*.xlsx"))
  if (!length(.wb) || !length(.ex)) {
    cat("  [skip] needs a workbook and a run export on disk\n"); return(invisible(NULL))
  }
  ## What the EXPORT says -- read independently, so the assertion is against the data rather than
  ## against a value this test invented.
  .ei <- suppressMessages(readxl::read_excel(.ex[[1]], sheet = "Info"))
  .ek <- grep("tabbed", names(.ei), ignore.case = TRUE, value = TRUE)
  .want <- if (length(.ek)) as.character(.ei[[.ek[[1]]]][1]) else "ScriptVersion"
  if (!nzchar(.want) || is.na(.want)) .want <- "ScriptVersion"

  d <- file.path(tempdir(), "acta_dash_tab"); unlink(d, recursive = TRUE)
  dir.create(file.path(d, "scan", "RUN1"), recursive = TRUE); dir.create(file.path(d, "out"))
  dir.create(file.path(d, "layout"))
  file.copy(.ex[[1]], file.path(d, "scan", "RUN1"))
  .lw <- file.path(d, "layout", basename(.wb[[1]])); file.copy(.wb[[1]], .lw)
  ## Make the workbook say something the export does not, and something that IS a real column so
  ## a regression would resolve rather than warn-and-fall-back -- otherwise the test could pass
  ## for the wrong reason.
  .w  <- openxlsx::loadWorkbook(.lw)
  .wi <- suppressMessages(readxl::read_excel(.lw, sheet = "Info"))
  .k  <- grep("tabbed", names(.wi), ignore.case = TRUE, value = TRUE)
  if (!length(.k)) { .wi$dashboard_tabbed_by <- "Panel"; .k <- "dashboard_tabbed_by" }
  .wi[[.k[[1]]]] <- "Panel"
  openxlsx::removeWorksheet(.w, "Info"); openxlsx::addWorksheet(.w, "Info")
  openxlsx::writeData(.w, "Info", .wi); openxlsx::saveWorkbook(.w, .lw, overwrite = TRUE)

  old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR", "ACTA_LAYOUT_DIR"), unset = NA)
  Sys.setenv(ACTA_DASHBOARD_DIR = file.path(d, "out"), ACTA_EXPORT_DIR = file.path(d, "scan"),
             ACTA_LAYOUT_DIR = file.path(d, "layout"))
  on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
              do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
  log <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(gen)), stdout = TRUE, stderr = TRUE))
  hit <- grep("tabbed by", log, value = TRUE)
  got <- if (length(hit)) trimws(sub(".*tabbed by ", "", hit[[1]])) else "<none>"
  chk(sprintf("the workbook says Panel, the export says %s -- the EXPORT wins (got %s)",
              .want, got), identical(got, .want))
  chk("...and the workbook's value is not what came out", !identical(got, "Panel"))

  ## AND AN INTERNAL COLUMN CANNOT BE NAMED. .file is the export's ABSOLUTE PATH; naming it in
  ## tabbed_by put /Users/<account>/.../OneDrive-SharedLibraries-<employer>/... into the page as
  ## every row's `project` and as the tab label. The same sink exists in the published version,
  ## but there the value came from the operator's OWN workbook -- it now comes from the newest
  ## export, so a foreign export dropped into a shared archive can steer it.
  .ex2 <- list.files(file.path(d, "scan"), pattern = "[.]xlsx$", recursive = TRUE, full.names = TRUE)
  if (length(.ex2)) {
    .w2 <- openxlsx::loadWorkbook(.ex2[[1]])
    .i2 <- suppressMessages(readxl::read_excel(.ex2[[1]], sheet = "Info"))
    .k2 <- grep("tabbed", names(.i2), ignore.case = TRUE, value = TRUE)
    if (length(.k2)) {
      .i2[[.k2[[1]]]] <- ".file"
      openxlsx::removeWorksheet(.w2, "Info"); openxlsx::addWorksheet(.w2, "Info")
      openxlsx::writeData(.w2, "Info", .i2); openxlsx::saveWorkbook(.w2, .ex2[[1]], overwrite = TRUE)
      .lg <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
        c("--vanilla", shQuote(gen)), stdout = TRUE, stderr = TRUE))
      .hh <- list.files(file.path(d, "out"), pattern = "[.]html$", full.names = TRUE)
      .tt <- if (length(.hh)) paste(readLines(.hh[[1]], warn = FALSE), collapse = "") else ""
      chk("naming an internal column falls back to the run folder, not the absolute path",
          any(grepl("tabbed by run folder", .lg)) && !grepl('"project":"/', .tt, fixed = TRUE))
    }
  }
})

cat("\n=== H. colliding run basenames get their OWN figures, not the first run's ===\n")
local({
  ## Case C already proves both runs are READ. This proves each row then resolves to its own
  ## run's FILES. runInfo[[<name>]] resolves a name to the first match, and the scan root's
  ## basename can equal a child's -- which is the shape built below. Third and last instance of
  ## that idiom in this generator; the other two caused a double-count and a wrong schema.
  ## Wrong figure links in a dashboard attached to a validation package is the damage here.
  .src <- Filter(function(d) length(list.files(file.path(d, "Outputs", "Plots"),
                                               pattern = "^PlateMap.*[.]png$")), cases)
  if (!length(.src)) {
    cat("  [skip] no case has Outputs/Plots/PlateMap*.png on disk (run an OQ case first)\n")
    return(invisible(NULL))
  }
  .base <- file.path(.src[[1]], "Outputs")
  .xl   <- list.files(.base, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)[1]
  .png  <- list.files(file.path(.base, "Plots"), pattern = "^PlateMap.*[.]png$", full.names = TRUE)[1]
  d <- file.path(tempdir(), "acta_dash_collide"); unlink(d, recursive = TRUE)
  .root <- file.path(d, "RUN"); .child <- file.path(.root, "RUN")
  dir.create(file.path(.root, "Plots"), recursive = TRUE)
  dir.create(file.path(.child, "Plots"), recursive = TRUE)
  dir.create(file.path(d, "out"), recursive = TRUE)
  file.copy(.xl, .root); file.copy(.xl, .child)
  ## Distinct figure names, or "each row got its own" cannot be told from "both got the same".
  file.copy(.png, file.path(.root,  "Plots", "PlateMap_EXP00000001_1.png"))
  file.copy(.png, file.path(.child, "Plots", "PlateMap_EXP00000002_1.png"))
  old <- Sys.getenv(c("ACTA_DASHBOARD_DIR", "ACTA_EXPORT_DIR", "ACTA_LAYOUT_DIR"), unset = NA)
  Sys.setenv(ACTA_DASHBOARD_DIR = file.path(d, "out"), ACTA_EXPORT_DIR = .root,
             ACTA_LAYOUT_DIR = .src[[1]])
  on.exit({ for (k in names(old)) if (is.na(old[[k]])) Sys.unsetenv(k) else
              do.call(Sys.setenv, setNames(list(old[[k]]), k)) }, add = TRUE)
  invisible(suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(gen)), stdout = TRUE, stderr = TRUE)))
  .h <- list.files(file.path(d, "out"), pattern = "[.]html$", full.names = TRUE)
  if (!length(.h)) { chk("the generator produced a dashboard for the collision check", FALSE); return() }
  .txt <- paste(readLines(.h[[1]], warn = FALSE), collapse = "")
  .pm  <- unlist(regmatches(.txt, gregexpr('"platemap":\\[[^]]*\\]', .txt)))
  chk(sprintf("both colliding runs produced a row (%d)", length(.pm)), length(.pm) == 2L)
  chk(sprintf("...and each links its OWN plate map, not the first run's (%d distinct)",
              length(unique(.pm))),
      length(.pm) == 2L && length(unique(.pm)) == 2L)
  if (length(unique(.pm)) < 2L) for (x in .pm) cat("          ", x, "\n")
})

cat(if (ok) "\nALL DASHBOARD RUN-SCAN TESTS PASS\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0 else 1)
