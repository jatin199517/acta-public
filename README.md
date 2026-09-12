<img src="assets/logo.png" align="right" width="140" alt="" />

# ACTA — Anybody Can Titrate Antibody

## Overview

![](assets/overview.png)

## Introduction & Workflow

This is an automated tool for flow cytometry antibody titer analysis.

The user provides a folder of FCS files, inside a parent folder named `Titration_FCS`, and one
Excel instructions workbook. ACTA gates the positive and negative populations for each titrated
reagent, computes a Stain Index at every stain quantity, fits a saturation curve, and writes the
plots, an export workbook, a PDF report and an HTML dashboard — so the analyst can pick the
reagent titer.

![](assets/workflow.png)

## Outputs

### Plots

1\) A plate map, based on the `Layout_Plate` sheet, generated using the ggplate package.

![](assets/example-plate-map.png)

Three plots are provided that help the user pick a titer — 2) a stain index plot, 3) a saturation
plot, and 4) a concatenation plot for each reagent quantity in the group.

| Stain Index against dose | Percent positive against dose | Every well on one axis |
|---|---|---|
| ![Stain Index per stain quantity, with a Michaelis-Menten fit and the maximum marked.](assets/example-stain-index.png) | ![Percent of parent population per stain quantity, with the saturation level marked.](assets/example-saturation.png) | ![Concatenated events for every well of the titration on one shared fluorescence axis.](assets/example-concatenated.png) |

Per-reagent panels are also written for each titration — the marker distribution, the pseudocolor,
and one gate-layout page per `gating_template` population flagged `layout_export`.

![](assets/example-pseudocolor.png)

### Titration export

Each export carries two suggested titers: the quantity at maximum Stain Index (`maxSI_StainQty`) and
the quantity at 90% of the Michaelis-Menten *V*max (`MM_90pct_Vmax`). They are suggestions for the
user to pick a titer.

`Titer` and `Titration_Status` inside the titration export file are to be manually filled on the
exported sheet; the dashboard reads them.

![](assets/Titration_export.png)

### Dashboard (optional)

An HTML dashboard allows quick retrieval of outputs and metadata from all past titrations.

![](assets/Dashboard_image.png)

### Report

The PDF report houses two sections — 1) MiFlowCyt-styled metadata and 2) Titration results.
Image shows the table of contents for the report.

![](assets/report_example.png)

## Expected Advantages

### Analysis time

Titration is arithmetic on a grid: *n* reagents × *m* dilutions. Analyzing using conventional
manual workflows entails annotating every FCS file, drawing every gate, moving medians into Excel,
and rebuilding the same plot for each point.

![](assets/why-acta.png)

### Deterministic and reproducible

Under identical instructions the outcome is the same, independent of who ran it. Every run writes
`acta_version.tsv` and `package_versions.tsv` beside its outputs, recording the seed, the script and
package versions, the pipeline folder that ran, and the version of every package the run could have
used. Three complete datasets ship with the tool so that it can be checked for function and
accuracy.

### Data provenance

All relevant metadata is captured: reagent vendor, catalogue and lot, clone, test material,
operator, the acquisition date and instrument taken per FCS file, and fixation/permeabilisation
state. The user can add more columns in the Layout sheet that they wish to track.

### Combinatorial titration

Several reagents can be titrated from one shared set of FCS files, each with its own gate and its own
Stain Index curve. `OQ_Test3`, one of the shipped diagnostic cases, is a worked example. For the experimental approach see
Burn OK, Mair F, Ferrer-Font L. *Combinatorial antibody titrations for high-parameter flow
cytometry.* Cytometry A, 2024. [doi:10.1002/cyto.a.24828](https://doi.org/10.1002/cyto.a.24828)

### Free, open source, and accessible

No licence to buy, and no R experience needed to run it: the Shiny app covers the whole workflow.

### AI-friendly

The instructions file is a spreadsheet and the gating template is plain text, so an LLM can help
fill in a workbook, suggest gating arguments for a panel, or execute `run_acta()`.

## Components

The tool has 2 components: the script and the dashboard generator.

### 1. The script

The script can be run in two ways.

#### With the Shiny app

For anyone who would rather not touch R. `ACTA App.command` (macOS) or `ACTA App.bat` (Windows)
launches it. On Linux, or anywhere without the launchers, run it from the folder you cloned
into — `shiny::runApp("inst/pipeline/ACTA_App.R")` — or, from an install, `acta_app()`. Five
sections:

| Section | What it does |
|---|---|
| **Checks** | Before anything runs: are the dependencies installed, are the version files resolvable, does the instructions workbook pass pre-validation, and is the dashboard template intact. Each check expands to show what it looked at. |
| **Run ACTA** | Names the workbook it resolved, sets compensation mode, toggles the report and the plots, then runs in a separate process. |
| **Generate dashboard** | Choose the folder to scan for titration exports and where to write the HTML. |
| **Log** | Streams the run as it happens. |
| **Diagnostics** | Runs the three test cases. Collapsed by default. |

#### Directly from R

Every action the app offers is available as a function call.

From an installed package. Point it at the folder holding your workbook and `Titration_FCS`, or
start R in that folder and call it with no arguments:

```r
library(ACTA)
r <- run_acta()                                   # the current folder
r <- run_acta("/path/to/working/folder")          # or name it
```

Or from a clone, without installing. Run this from the folder you cloned into:

```r
h <- new.env(); sys.source("R/ACTA_Functions.R", envir = h)  # helpers only; runs no analysis
r <- h$run_acta("/path/to/working/folder",
                report = TRUE, plots = TRUE, quiet = FALSE,
                code_dir = "inst/pipeline")                  # where ACTA_Script_*.R lives
```

`report` and `plots` both default to `TRUE`, and `quiet` to `FALSE`; the clone example passes them
only to show they exist.

`code_dir` is where the pipeline itself lives, and you do not normally pass it. It is resolved:
your working folder if that holds an `ACTA_Script*.R`, otherwise the installed package. When it
falls back to the package it says so, naming the version and the path, and `r$code_dir` records
what ran — worth reading if you are working in a clone whose code differs from your installed
copy, because the bare call will use the installed one. Pass `code_dir` yourself to be certain
which code you get.

`run_acta()` resolves the pipeline script beside `code_dir`, runs it in its own environment, and
returns a list. The fields you are most likely to branch on:

| field | meaning |
|---|---|
| `ok` | did the <u>analysis</u> complete. `TRUE` with `report_ok = FALSE` means the export and plots are on disk and usable, and you did not get a PDF — either the render failed or it was never attempted. `report_skipped` tells you which |
| `analysis_error` / `report_error` | the message from whichever stage actually failed |
| `report_ok`, `wrote_plots` | did the PDF render, and were the plots written. Both are flags; neither is the path. `report_ok` is `FALSE` rather than `NA` when you asked for a report and did not get one, with the reason in `report_error` — including "pandoc was not found", which is checked before the run starts so a missing pandoc costs you the PDF and nothing else |
| `export_file`, `report_file`, `plots_dir` | <u>this</u> run's outputs, by the names the script built. Each is `NA` rather than a stale path when that artefact was not produced — `plots_dir` is `NA` unless <u>this</u> run wrote into `Plots/`, so a folder left behind by an earlier run is not reported as yours |
| `report_skipped` | why a requested report was never started, or `NA`. Set when the pre-check found no pandoc, so a caller can tell "not attempted" from "attempted and failed" |
| `code_dir`, `script_version`, `seed`, `elapsed_s` | provenance for the run — `code_dir` is the pipeline that actually ran |
| `wrote_provenance` | did the run write `acta_version.tsv` and `package_versions.tsv` beside its outputs |
| `stats`, `qcList`, `mmFits`, `gs` | the per-well statistics, the QC verdicts, the Michaelis-Menten fits, and the GatingSet |

`ACTA_WORK_DIR` (the working folder) and `ACTA_CODE_DIR` (where the code lives) are set for the
script and are also how the app is pointed at a folder. They differ only when the code is
read-only, such as running from an installed package.

`actaOQRun()` is the same route to the diagnostic cases — see
[Validate your installation](#validate-your-installation).

### 2. The dashboard generator

`ACTA_Dashboard_*.R` is a second pass over work already done, driven either from the app's
**Generate dashboard** section or by running it directly. Point it at a folder whose immediate
subfolders are the runs: each run's `*TitrationExport*.xlsx` is read from the run folder or its
`Outputs/` subfolder, and every run it finds becomes a row in one sortable, filterable HTML table.

- **Cumulative.** Nothing has to be added to a list: any run folder directly under the scanned
  folder is included the next time you generate. To exclude a run, put it in a folder named
  `Archive` — any path containing "archive", in any case and at any depth, is skipped.
- **Local file links** to each run's export, report, and plots, so a row is one click from the
  evidence behind it.
- **Five themes** ship: `graph-blue` in `Template/`, and `lab-record`, `navy-chrome`,
  `maroon-chrome` and `teal-chrome` in `Template/Other_Templates/`. The generator uses whichever
  `*dashboard_template*.html` is in `Template/` and requires exactly one, so switching theme means
  moving the one you want into `Template/` and the current one out. Do that in a clone. The copy
  inside an installed package is usually not writable, and a reinstall would put it back.
- **Self-contained HTML.** The data is embedded, so the file can be emailed or archived and still
  opens.

The links are relative file paths. That makes the dashboard ideal for a single computer, or for a
network drive where everyone sees the same paths — and it means a dashboard emailed to someone whose
drive is mounted differently will show rows correctly but broken links.

## Installation

ACTA is an R package. There are two ways to install it.

**As a package**, to drive ACTA from R:

```r
# install.packages("remotes")
remotes::install_github("jatin199517/acta-public")
library(ACTA)
```

This provides `run_acta()` and `actaOQRun()`, and the three diagnostic cases. You can validate the
install straight away — see *Validate your installation* below.

That one call pulls in 142 packages. Most are on CRAN, but the flow stack — `flowCore`,
`flowWorkspace`, `openCyto`, `ggcyto`, `flowMeans` — is on Bioconductor, and about sixty more
packages sit behind those. `remotes` finds them because the `DESCRIPTION` declares `biocViews`,
which is the field it reads to decide whether to look at the Bioconductor repositories at all.
Nothing to configure. Expect 20-40 minutes the first time if none of it is already installed.

What it does not bring is anything that is not an R package. The PDF report needs two of those —
pandoc and a TeX engine — and this route installs neither, so the report is the one thing you will
not get out of the box. The analysis, the plots, the titration export and the dashboard all run
without them: use `run_acta(report = FALSE)`. To get the report as well, install both yourself; the
first table below lists what each route leaves you to do.

**From a clone**, for the Shiny app, the desktop launchers and the working folder layout:

```sh
git clone https://github.com/jatin199517/acta-public.git
cd acta-public
```

Then run this **in the folder you cloned into** — the one holding `Setup.R` next to `R/`.

```sh
Rscript Setup.R              # or: Rscript Setup.R --no-tex   to skip the PDF report
```

On **Windows**, double-click `Setup.bat` instead, or run it from a Command Prompt in that folder:

```bat
Setup.bat                    :: or: Setup.bat --no-tex
```

`Setup.bat` exists because the R for Windows installer does not put `Rscript` on `PATH`, so
`Rscript Setup.R` fails there with `'Rscript' is not recognized`; the launcher finds `Rscript.exe`
itself. `source("Setup.R")` from an R console installs the same packages.

`Setup.R` installs the R packages, ACTA itself, a TeX engine if there is none, and the LaTeX
packages the report needs. That is the whole of the R-side install — there is no second command
to install the package.

It installs ACTA because the pipeline loads its helper library from the installed package: in this
layout there is no copy of it beside the script, so dependencies alone would let the app start and
then stop a run with "the ACTA package is not installed". `Setup.R --no-package` skips that step if
you are deliberately testing an installed build against a checkout.

What it does *not* install is the two things that are not R packages and cannot be fetched from
one: pandoc, and XQuartz on macOS. It *does* install a TeX engine if you have none, unless you
pass `--no-tex`. See the table below.

It reads its package list out of `ACTA_Script_*.R` **and** `ACTA_Report_*.Rmd` — the report
installs two packages the script never mentions — so those two files together are the manifest.

**Requires R ≥ 4.4.0**, which `Setup.R` enforces before it installs anything.

What each way of installing gives you:

| | clone + `Setup.R` | `remotes::install_github` |
|---|---|---|
| the R packages | yes | yes |
| a TeX engine, for the PDF report | yes — TinyTeX, and the LaTeX packages the report needs | **no** — run `tinytex::install_tinytex()` yourself. The `tinytex` R package is already there; the TeX engine it drives is not |
| pandoc, for the PDF report | no — checks for it and prints the download link | **no** — get it from [pandoc.org](https://pandoc.org/installing.html) |
| XQuartz on macOS, to start a run | no — checks for it and prints the download link | **no** — get it from [xquartz.org](https://www.xquartz.org) |
| `Setup.R` itself | yes | not part of the package |
| the desktop launchers | `ACTA App.command`, `ACTA App.bat` | not part of the package |
| the Shiny app | launch it with `ACTA App.command` or `ACTA App.bat` | `acta_app()` — start R in your working folder and call it. `shiny::runApp()` on the app file will not work: it changes directory before reading the file, so the app would take the library as your working folder, and it refuses rather than doing that |
| the `Titration_FCS` folder layout | yes | you make it yourself |

So a package install leaves you with an analysis that runs and no PDF report, until you install
pandoc and a TeX engine yourself. Nothing else is missing.

What each platform needs beyond R:

| | Installed by `Setup.R` | Linux | macOS | Windows |
|---|---|---|---|---|
| **TeX engine** — PDF report only | **Yes** — TinyTeX, plus the LaTeX packages the report needs | installed | installed | installed |
| **pandoc** — PDF report only; `rmarkdown` shells out to it | No — `Setup.R` checks for it and prints the download link | **you install it** — your distro's package, or [pandoc.org](https://pandoc.org/installing.html) | **you install it** — [pandoc.org](https://pandoc.org/installing.html) | **you install it** — [pandoc.org](https://pandoc.org/installing.html) |
| **XQuartz / X11** — `flowMeans` loads `tcltk`, so this is needed to start a run, not to load the library | No — `Setup.R` checks for it and prints the download link | usually already present; install your distro's X11 dev libraries if `tcltk` fails to load | **you install it** — [xquartz.org](https://www.xquartz.org) | not needed, `tcltk` ships with R |
| **Perl** — used by `tlmgr` to fetch the report's LaTeX packages | No | system `perl` | system `perl` | not needed, TinyTeX ships its own |
| **Compiler toolchain** — only if a package has no binary | No | `build-essential` + the `-dev` libraries the flow stack names | Xcode command line tools | Rtools |
| **Setup launcher** | ships with the clone, not with the package | none — use `Rscript Setup.R` | none — use `Rscript Setup.R` | `Setup.bat` |
| **App launcher** | ships with the clone, not with the package | none — use `Rscript` or the R console | `ACTA App.command` | `ACTA App.bat` |

The PDF report needs both a TeX engine and pandoc. `rmarkdown` runs pandoc to turn the report into
LaTeX, then the TeX engine turns that into the PDF. `Setup.R` installs the TeX engine and the LaTeX
packages. It does not install pandoc — it checks for it and reports it in the closing summary.
Install pandoc from [pandoc.org](https://pandoc.org/installing.html).

A package install brings neither, so install both if you want the report:
`tinytex::install_tinytex()` for the TeX engine, and pandoc from the link above.

If you do not want the PDF, skip both and use `run_acta(report=FALSE)`. If you ask for one
without pandoc installed, `run_acta()` says so and carries on without it — the export, the plots
and the dashboard are unaffected. A missing TeX engine is caught later, when the render fails, and
`report_error` names it.

## Validate your installation

Three diagnostic cases ship with the tool. A pass means that the run finished and it returned
values as expected.

If you installed the package, the cases came with it and nothing else is needed:

```r
library(ACTA)
r <- actaOQRun(system.file("extdata", "oq_small", "OQ_Test1", package = "ACTA"),
               code_dir = system.file("pipeline", package = "ACTA"))
r$verdict        # "PASS", "PASS (with notes)" or "FAIL"
```

`OQ_Test2` and `OQ_Test3` are the other two cases — substitute the name. To run all three:

```r
for (nm in c("OQ_Test1", "OQ_Test2", "OQ_Test3"))
  cat(nm, as.character(actaOQRun(system.file("extdata", "oq_small", nm, package = "ACTA"),
                                 code_dir = system.file("pipeline", package = "ACTA"))$verdict), "\n")
```

`packageVersion("ACTA")` is the version you have. A run also reports `script_version`, which is
the pipeline's own coarser stamp — `3_0` for every 3.0.x — so the two do not match and are not
meant to.

A case that lives inside your R library is copied to a temporary folder and run there, and the
run says where. A diagnostic run deletes and rewrites everything under the case folder, and a
library is often shared with other people or not writable at all, so it is not a place to do
that. `r$log` is the path to the log it wrote.

From a clone, run this in the folder you cloned into:

```r
h <- new.env(); sys.source("R/ACTA_Functions.R", envir = h)
r <- h$actaOQRun("inst/extdata/oq_small/OQ_Test1", code_dir = "inst/pipeline")
r$verdict        # "PASS", "PASS (with notes)" or "FAIL"
```

They are also the worked examples. If you want to see a correct instructions workbook, read theirs.

Each dataset covers a unique test case, across two instruments from different vendors:

| | **OQ_Test1** | **OQ_Test2** | **OQ_Test3** |
|---|---|---|---|
| Instrument | Cytek Aurora | Bio-Rad ZE5 | Cytek Aurora |
| Detectors in the file | 13 | 32 | 14 |
| Titrations / wells | 1 / 11 | 3 / 21 | 3 / 14 |
| Events per well | 60,000 | 12,000 | 20,000 |
| Compensation | none | **matrix from CSV** | none |
| Combinatorial titration | no | no | **yes** |
| What it covers | the whole path end to end, single reagent | a second vendor's channel naming (`FS00-A`, not `FSC-A`) and the compensation path | **combinatorial titration** — one set of FCS files carrying two independently titrated reagents |

## Running

Copy the blank instructions workbook out of `Template/` and fill it in. If you installed the
package and have no clone, the template ships inside it — copy it out to somewhere you can write:

```r
file.copy(dir(system.file("pipeline", "Template", package = "ACTA"),
              pattern = "^TEMPLATE.*\\.xlsx$", full.names = TRUE), ".")
```

You may replace the text `TEMPLATE` from this file but keep the rest of the filename as is — the
run looks for one `*Titration_Instructions*.xlsx` in the folder and stops if it finds none or
several.

The trailing `_3_0` is a version stamp, and it is **not** checked: renaming a workbook does not
make it compatible, and does not make it incompatible either. What has to match is the workbook's
layout — the sheets and the column names this version reads. There are no compatibility shims for
an older layout, so start from this version's template rather than carrying a workbook forward and
hoping. A missing column is reported by name before the analysis runs. Move your FCS files to a
folder (as specified in `Dirname` in the Layout sheet) and move this folder inside the umbrella
folder `Titration_FCS`. Then either launch the app or call `run_acta()`. The script resolves and
sets its own working directory.

The entire gating hierarchy — including each reagent's marker gate and the method used — is
configured in the workbook's **`gating_template`** sheet, which is an openCyto gating template. It
is not configured in code.

> **Reagent.** This tool refers to "Reagent" as any staining reagent, which includes both antibody
> and non-antibody staining reagents, such as fluorophore-conjugated streptavidin and tetramers.

## Inputs

The instructions workbook has three sheets. Here is what the tool does with the instructions
workbook sheets:

| Step | Sheet | What it reads | Fails the run if |
|---|---|---|---|
| 1 | `Layout_Plate` | one row per well: `PlateID`, `Row`, `Column`, `Dirname`, `Alias`, `StainType`, `StainQty` | `Dirname` is empty everywhere, or `Fix`/`Perm` are missing |
| 2 | `Layout_Plate` | the titrated channel per reagent: `Markername ($PnS)` and/or `Channel ($PnN)` | neither column is present |
| 3 | `Layout_Plate` | reagent metadata for the labels and the export: `Vendor`, `Cat_Num`, `Lot_Num`, `Clone`, `Test_Material` | never — these are display-only |
| 4 | `gating_template` | the openCyto hierarchy, one row per population, with exactly one aliased `Reagent` row | there is no marker row, or `dims`/`pop` cannot be parsed |
| 5 | `gating_template` | `gating_args` / `preprocessing_args` | they contain anything that is not a literal value |
| 6 | `Info` | run-level settings: compensation mode, optional per-run event cap | a setting is present but unreadable |

> **Exactly one `gating_template` row must be aliased `Reagent`.** That is the row whose channel is
> titrated: its `dims` names the detector, and its gating method decides the positive population the
> Stain Index is computed from. `Antibody` is accepted as a legacy alias for the same row.

Column headers can contain the suffix `(L)` or `(N)` to denote that a logical (TRUE/FALSE) or a
numeric value is expected.

### Instructions workbook column requirement and case-sensitivity

| Column | Sheet | Case-sensitive | Required — the run stops without it |
|---|---|---|---|
| `Dirname` | `Layout_Plate` | No | Yes — matches each well to its folder of FCS files |
| `PlateID`, `Row`, `Column` | `Layout_Plate` | No | Yes — matches each FCS file to its well, via `$WELLID` |
| `StainType` | `Layout_Plate` | No | Yes — `Stain`, `Costain` or `Unstained`, and it is the **only** thing that classifies a well. FCS filenames are never inspected, so a file called `CD3_Unstained_comparison.fcs` is a Stain well if the layout says so. Check the plate map in the report against what you loaded |
| `StainQty` | `Layout_Plate` | No | Yes — the x axis of every curve |
| `Markername ($PnS)` <u>or</u> `Channel ($PnN)` | `Layout_Plate` | No | Yes — either will do; `Channel` wins if both are set |
| `Fix`, `Perm` | `Layout_Plate` | No | Yes — the export's `Fix_Perm` column. `Perm` true with `Fix` false is rejected as a layout error |
| `alias`, `parent`, `pop` | `gating_template` | **Yes** | Yes — they become openCyto node names |
| exactly one `Costain` well per titration | `Layout_Plate` | No | Yes — it sets the gate floor. None leaves the floor at −∞, which looks like a successful run; more than one pools their events, so the floor belongs to no single well. Both stop the run |
| an `Unstained` well | `Layout_Plate` | No | No — it is dropped before gating and changes no result, so a titration without one is fine. More than one is a note, not an error. But if the FCS folder *contains* an unstained file, it needs its own layout row like any other well — once per titration in a combinatorial folder — because filenames are not inspected |
| `groupBy` | `gating_template` | **Yes** | No — but when used it must name a `pData` column exactly |
| `ELN_ID`, `ELN_Name`, `Operator` | `Layout_Plate` | No | Yes — the pre-flight stops the run if any of the three columns is absent. Per well, so a run can carry more than one operator. `ELN_ID` also names your export and plate-map files |

## Files written

| Output | File |
|---|---|
| Stain Index curve | `Plots/SIPlot_*.png` |
| Saturation curve | `Plots/SatPlot_*.png` |
| Per-reagent panels | `Plots/*_histogram.png`, `*_pseudo.png`, `*_layout_<alias>.png` |
| Concatenation view | `Plots/ConcatPlot_*.png` |
| Plate map | `Plots/PlateMap_<ELN_ID>_<PlateID>.png` |
| Titration export | `<date>_<ELN_ID>_TitrationExport_<version>.xlsx` |
| Report | `ACTA_Report_*.pdf` |
| Dashboard | a self-contained `.html` |
| Run provenance | `acta_version.tsv`, `package_versions.tsv` |

## Licence

ACTA — automated antibody titration and stain-index analysis for flow cytometry.
Copyright (C) 2026 Lyell Immunopharma, Inc.

Published from a personal repository with the permission of Lyell Immunopharma, Inc., which
retains copyright. **No separate licence from Lyell is required to use ACTA** — the AGPL-3 grant
below is the whole permission you need.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU
Affero General Public License, **version 3 only** — not "or any later version", because openCyto and
flowWorkspace are themselves `AGPL-3.0-only`. The full text is in [LICENSE](LICENSE).

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
Affero General Public License for more details.

AGPL is copyleft: a modified version, or a work based on ACTA, that you distribute carries the same
licence. The licence covers the software and grants no trademark rights.

## Citing ACTA

Use the **Cite this repository** button, or [CITATION.cff](CITATION.cff) directly.

## Further reading

[`docs/INTERNALS.md`](docs/INTERNALS.md) documents the pipeline stage by stage — startup, the
per-reagent loop, marker-channel resolution, compensation, the dashboard, and the helper library.
Read it if you are planning to extend ACTA.
