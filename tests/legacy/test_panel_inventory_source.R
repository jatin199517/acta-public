## The panel inventory must say WHICH FILE it consulted, and must not pass off the shipped
## demo inventory as the lab's.
##
## WHY THIS GATE EXISTS. A real run declaring an iteration the demo does not hold was silently
## matched against
## TEMPLATE_Master_Panel_Inventory.xlsx -- reached as a fallback because the workbook named no
## inventory -- and the report said the iteration "is not in the master panel inventory". True of
## the demo file, false of the real one, which held the panel all along. The caption named no
## file, so there was nothing in it to show which inventory had actually been opened.
##
## Every assertion below was run against the OLD behaviour first and FAILED there. A gate that
## has never seen the bug it guards is an assertion about nothing.

ok <- TRUE
## detail is COERCED, not trusted. The first run of this gate against the old behaviour passed a
## NULL attribute through sprintf(), got character(0) back, and died inside the failure reporter
## -- so the gate detected the regression and then crashed instead of naming it. A gate whose
## failure path is untested only works when it has nothing to say.
## AN ASSERTION THAT ERRORS IS A FAILURE, NOT THE END OF THE RUN. `pass` is a promise, so forcing
## it inside tryCatch catches a predicate that throws -- which is exactly what the bug this gate
## guards does. Without this the script aborted on the first such assertion and every later one
## went unreported, so a falsification run showed one problem where there were several.
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
pit  <- ACTA:::actaPanelIterationTable
pcap <- ACTA:::actaPanelIterationCaption
pinv <- ACTA:::actaPanelInventory
ppath <- ACTA:::actaPanelInventoryPath

example <- ppath(info = NULL, version_dir = NULL, code_dir = NULL)
chk("the shipped example resolves", !is.na(example) && file.exists(example))
chk("and is tagged as a fallback, not as a chosen file",
    identical(attr(example, "acta_inventory_source"), "example"),
    sprintf("source=%s", attr(example, "acta_inventory_source")))

inv <- pinv(example)
chk("the loaded inventory remembers its path",
    identical(basename(as.character(attr(inv, "acta_inventory_path"))),
              "TEMPLATE_Master_Panel_Inventory.xlsx"))
chk("...and survives the row subset inside actaPanelInventory()",
    !is.null(attr(inv, "acta_inventory_source")))

## 1. An iteration the DEMO does not hold, with the demo reached as a fallback.
r <- pit(inv, "Unlisted Panel_4", markers = "CD3", channels = "PE-A")
chk("a real panel absent from the demo is example_only, not unknown_iteration",
    identical(r$status, "example_only"), sprintf("status=%s", r$status))
cap <- pcap(r)
chk("...and its caption does NOT claim the panel is missing from the master inventory",
    !grepl("is not in", cap, fixed = TRUE), cap)
chk("...and does name the demo file it actually opened",
    grepl("TEMPLATE_Master_Panel_Inventory.xlsx", cap, fixed = TRUE), cap)
chk("...and tells the operator the one thing that fixes it",
    grepl("master_panel_inventory_path", cap, fixed = TRUE), cap)

## 2. The demo's OWN panels still resolve, or the three OQ cases lose their table.
## The demo inventory's iterations were RENAMED (example_*) so they no longer collide with the
## OQ cases' own names. Before that, deleting a case's inventory fell through to the demo and
## still produced a panel table -- a fallback that looked like a result.
r2 <- pit(inv, "example_minimal_panel", markers = "CD3", channels = "PE-A")
chk("a demo panel still resolves from the demo inventory", identical(r2$status, "ok"),
    sprintf("status=%s", r2$status))
chk("...and the ok caption names its source file",
    grepl("TEMPLATE_Master_Panel_Inventory.xlsx", pcap(r2), fixed = TRUE), pcap(r2))
## The ok caption is a TITLE: panel, file, count. The reagent list is eight names on a routine
## plate and only earns its space when the plate carries more than one panel.
chk("the ok caption is one short line with no reagent list",
    nchar(pcap(r2)) <= 160 && !grepl("Titrated here", pcap(r2)),
    sprintf("%d chars: %s", nchar(pcap(r2)), pcap(r2)))
r2b <- r2; r2b$reagents <- c("CD28", "TCF1")
chk("...and names the reagents only when the caller supplies them (>1 panel on the plate)",
    grepl("Titrated here: CD28, TCF1", pcap(r2b), fixed = TRUE), pcap(r2b))

## 3. A CHOSEN inventory still reports a genuinely unknown iteration as unknown.
##    Built here rather than reusing a fixture: the point is the source tag, not the contents.
fake <- inv
attr(fake, "acta_inventory_source") <- "info"
attr(fake, "acta_inventory_path")   <- "/somewhere/260519_Master_Panel_Inventory.xlsx"
r3 <- pit(fake, "No_Such_Panel_99", markers = "CD3", channels = "PE-A")
chk("a typo against a CHOSEN inventory is still unknown_iteration",
    identical(r3$status, "unknown_iteration"), sprintf("status=%s", r3$status))
chk("...and that caption names the chosen file, not a generic phrase",
    grepl("260519_Master_Panel_Inventory.xlsx", pcap(r3), fixed = TRUE), pcap(r3))

## 4. No caption may use the old file-less wording. This is the exact string that cost the
##    diagnosis; if it ever comes back, so does the dead end.
for (x in list(r, r2, r3)) {
  cp <- pcap(x)
  chk(sprintf("caption for status '%s' is not the old file-less sentence", x$status),
      !grepl("is not in the master panel inventory", cp, fixed = TRUE), cp)
}

## 5. An inventory with no provenance at all must not be treated as chosen.
bare <- inv
attr(bare, "acta_inventory_source") <- NULL
attr(bare, "acta_inventory_path")   <- NULL
r4 <- pit(bare, "No_Such_Panel_99", markers = "CD3", channels = "PE-A")
chk("an untagged inventory degrades to a generic phrase without erroring",
    is.character(pcap(r4)) && nzchar(pcap(r4)), pcap(r4))

## ---- the lab-wide default ---------------------------------------------------------------------
## Exists so finding the inventory does not require every workbook to carry an absolute path.
## EXP26000001's Info sheet is the legacy 9-column layout and HAS no such column.
tmpinv <- file.path(tempdir(), "LabDefault_Master_Panel_Inventory.xlsx")
invisible(file.copy(example, tmpinv, overwrite = TRUE))

withr_env <- function(val, code) { old <- Sys.getenv("ACTA_MASTER_PANEL_INVENTORY", unset = NA)
  if (is.na(val)) Sys.unsetenv("ACTA_MASTER_PANEL_INVENTORY") else Sys.setenv(ACTA_MASTER_PANEL_INVENTORY = val)
  on.exit(if (is.na(old)) Sys.unsetenv("ACTA_MASTER_PANEL_INVENTORY") else Sys.setenv(ACTA_MASTER_PANEL_INVENTORY = old))
  force(code) }

p_env <- withr_env(tmpinv, ppath(info = NULL, version_dir = NULL, code_dir = NULL))
chk("the env var is used when the workbook names no inventory",
    basename(as.character(p_env)) == basename(tmpinv), basename(as.character(p_env)))
chk("...and is tagged 'lab_default', not 'example'",
    identical(attr(p_env, "acta_inventory_source"), "lab_default"),
    attr(p_env, "acta_inventory_source"))
chk("...which reads as 'lab default' on the page",
    identical(unname(ACTA:::ACTA_INFO_SOURCE_LABEL[["lab_default"]]), "lab default"))

op <- options(acta.master_panel_inventory = tmpinv); on.exit(options(op), add = TRUE)
p_opt <- withr_env(NA, ppath(info = NULL, version_dir = NULL, code_dir = NULL))
chk("the option works when no env var is set",
    identical(attr(p_opt, "acta_inventory_source"), "lab_default"),
    attr(p_opt, "acta_inventory_source"))

## A default that is set and WRONG must be said out loud, not silently replaced by the demo.
bad <- file.path(tempdir(), "no_such_inventory_file.xlsx"); unlink(bad)
w <- NULL
p_bad <- withCallingHandlers(withr_env(bad, ppath(info = NULL, version_dir = NULL, code_dir = NULL)),
                             warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
chk("a missing lab default warns", !is.null(w) && grepl("does not exist", w), w)
chk("...and falls through to the shipped example rather than losing the run",
    identical(attr(p_bad, "acta_inventory_source"), "example"), attr(p_bad, "acta_inventory_source"))
options(op)

## A BAD LAB DEFAULT MUST NEVER BE TAGGED 'lab_default'. Each of these was accepted or mislabelled
## before the guards: file.exists() is TRUE for a directory, and a whitespace-only env var is
## nzchar() TRUE so it counted as "set" and warned back at the operator quoting '   '.
dirpath <- file.path(tempdir(), "bq_a_directory"); dir.create(dirpath, showWarnings = FALSE)
quiet <- function(x) withCallingHandlers(x, warning = function(c) invokeRestart("muffleWarning"))
for (nm in c("a directory" = dirpath, "whitespace only" = "   ", "a missing file" = "/nope/x.xlsx")) {
  got <- quiet(withr_env(nm, ppath(info = NULL, version_dir = NULL, code_dir = NULL)))
  chk(sprintf("a lab default that is %s falls through to the example",
              names(which(c(dirpath = dirpath, ws = "   ", miss = "/nope/x.xlsx") == nm))[1]),
      identical(attr(got, "acta_inventory_source"), "example"),
      attr(got, "acta_inventory_source"))
}
w <- NULL
quiet2 <- function(x) withCallingHandlers(x, warning = function(c) { w <<- conditionMessage(c); invokeRestart("muffleWarning") })
quiet2(withr_env(dirpath, ppath(info = NULL, version_dir = NULL, code_dir = NULL)))
chk("a directory warns that it is a folder, not just 'does not exist'",
    !is.null(w) && grepl("folder", w, fixed = TRUE), w)
w <- NULL
quiet2(withr_env("   ", ppath(info = NULL, version_dir = NULL, code_dir = NULL)))
chk("a whitespace-only default is treated as unset -- no warning quoting spaces",
    is.null(w) || !grepl("'   '", w, fixed = TRUE), w)

## The example_only caption must offer the route a legacy workbook can actually take.
r_ex <- pit(inv, "Unlisted Panel_4", markers = "CD3", channels = "PE-A")
chk("the example_only caption names the lab default, not only the Info row",
    grepl("ACTA_MASTER_PANEL_INVENTORY", pcap(r_ex), fixed = TRUE), pcap(r_ex))

cat(if (ok) "\nPANEL INVENTORY SOURCE TESTS PASSED\n" else "\nFAILURES ABOVE\n")
quit(status = if (ok) 0L else 1L)
