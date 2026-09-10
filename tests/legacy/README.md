# ACTA test suite

Three tiers, in the order you would use them:

| Tier | What it proves | How long |
|---|---|---|
| **Fast gates** (`run_all.R`) | the helper library behaves — node resolution, pre-flight, metadata, the app's server logic | seconds |
| **OQ cases** (`Diagnostics/OQ_Test1`, `OQ_Test2`, `OQ_Test3`) | the whole pipeline still produces the same numbers, exports, plots and PDF | ~2–3 min each |
| **Regression gate** (`regression_capture.R` + `regression_compare.R`) | a refactor changed *nothing* observable, at a finer grain than the OQ assertions | ~2 min per snapshot |

```sh
Rscript 3_0/tests/run_all.R          # all fast gates, one line each, exit 0 = all passed
Rscript 3_0/tests/test_preflight.R   # or any single gate
```

> **On the `3_0/` prefix in every command below.** These paths are written for the DEVELOPMENT
> repository, where each version lives in its own folder alongside the others. A standalone checkout
> -- which is what the published repository is -- has no version folder: the code sits at the root,
> so drop the prefix and run `Rscript tests/run_all.R`. Nothing else changes; every script finds its
> own version from its own location (see *Working directory* below), which is why the same commands
> work either way.

## Working directory: it does not matter

Every script in this folder locates **itself** and derives the version folder and the repo root from
there, so all of these are equivalent:

```sh
Rscript 3_0/tests/run_all.R                       # from the repo root
(cd 3_0 && Rscript tests/run_all.R)                # from the version folder
(cd /tmp && Rscript "$ACTA/3_0/tests/run_all.R")   # from anywhere at all
```

This was not always true, and it mattered: the scripts used to disagree — most resolved the version
folder as the literal string `"2_88_WIP"` relative to the current directory, while
`test_compensation.R` sourced `ACTA_Functions.R` out of the current directory and `smoke_app.R` did
`setwd("2_88_WIP")`. Whichever way you ran the suite, several scripts failed for a reason that had
nothing to do with the code under test, which is worse than having no gate: a spurious failure
teaches people to ignore the output.

The mechanism is one line at the top of each script, plus `acta_test_paths.R`:

```r
source(file.path(this.path::this.dir(), "acta_test_paths.R"))
```

which provides `ACTA_TEST_DIR`, `ACTA_VERSION_DIR`, `ACTA_REPO_ROOT`, `actaTestVersionDir()` and
`actaTestFunctions()`. A first argument overrides the version folder
(`Rscript 3_0/tests/test_preflight.R 2_89`); a path that does not exist is a **hard error**,
never a silent fall back to this folder's own version, because falling back would report a PASS for a
version nobody tested.

> **Earlier versions are frozen and do NOT have this contract.** `run_all.R` refuses them by name.
> Pointed at `2_87` it once printed "2 of 8 passed" — and those two passes were worthless: 2_87's
> copies of the scripts default to the literal string `"2_88_WIP"`, so they tested *this* version's
> code while the header said 2_87. Run a frozen version's gates one at a time, from the repo root,
> and check which version each one actually resolved.

## The fast gates

| Script | Guards |
|---|---|
| `test_marker_nodes.R` | `actaMarkerNodes()` — the marker `+`/`-` pair comes from the marker ROW, not a suffix scan. Includes the case that broke it: a quadrant row sharing the marker's parent. |
| `test_quad_pops.R` | every `pop=` form openCyto's template accepts, **pinned against `openCyto:::.preprocess_row()`** so a change in openCyto's own naming is caught rather than inferred. The only place an openCyto internal is touched, and only in the test. |
| `test_preflight.R` | the instructions-check records: unresolvable `dims`, unsupported `pop`, the mandatory marker row, commented-out `#` rows ignored — and no false positives against the real workbooks in `Template/` and `Diagnostics/`. |
| `test_miflowcyt.R` | `actaMiFlowCyt()` and `actaMiFlowCytAppendix()` — section coverage, the honest `structured-not-complete` flag, ELN references resolved from the layout, and the appendix's MIFlowCyt **1.0** numbering (four sections, reagents at 2.4, analysis at 4). |
| `test_compensation.R` | compensation pre-flight for every mode and the csv staging/restore cycle, including `self` mode against the real OQ_Test1 FCS. |
| `test_app_support.R` | app-only helpers: version-file resolution by pattern, the dashboard template, and the LaTeX package check (`actaLatexPackages()` / `actaLatexMissing()`). |
| `smoke_app.R` | the Shiny **server** logic through `shiny::testServer` — no browser, no `runApp()`. |
| `verify_oq_sanitised.R` | that the published OQ cases leak nothing: no company or programme name, no operator, donor code, instrument serial or absolute path, in the workbook **or** the FCS keywords. Scans every `Diagnostics/OQ_Test*` by default. |

## The OQ cases

```r
h <- new.env(); sys.source("3_0/ACTA_Functions.R", envir = h)
r <- h$actaOQRun("3_0/Diagnostics/OQ_Test1")   # writes Outputs/log.txt with the verdict table
```

Each case folder holds only its **inputs** — the instructions workbook, `Titration_FCS/`,
`expected.json` (and OQ_Test2's compensation csv). `Outputs/` is created by the run, is git-ignored,
and should be deleted once the result is recorded: the report and dashboard embed the absolute path
of whoever ran them and `log.txt` records the machine name.

`expected.json` is where the current shapes live — `stats_rows`, `stats_cols`, `n_groups`,
`qc_checks`, `qc_verdicts`, `n_plots`, `export_rows`, `dashboard_rows`, `events_per_well` — plus
`*_note` fields explaining *why* each number is what it is. Deliberately no numbers are quoted in
this README: they move by design whenever a column or a plot is added, and a doc that repeats them
is stale the day after. A missing key is reported as a **SKIP**, never a silent pass.

The two cases are chosen to be different on purpose: OQ_Test1 is a single group on a Cytek Aurora
with no compensation; OQ_Test2 is three groups on a Bio-Rad ZE5 with a supplied spillover matrix,
non-standard scatter names, `$PnS` gating dimensions, an 18-bit `$PnR` and `Downsample_to` set — so
it is the case that proves vendor portability.

## The regression gate

The pipeline seeds its RNG per antibody group, so two runs of the same code on the same data produce
an **identical** snapshot. That makes this a pass/fail gate rather than a judgement call: any
difference after a refactor is a real behaviour change.

```sh
Rscript 3_0/tests/regression_capture.R 3_0 baseline.rds   # before the change
#   ... make the change ...
Rscript 3_0/tests/regression_capture.R 3_0 after.rds      # after
Rscript 3_0/tests/regression_compare.R baseline.rds after.rds  # exit 0 = identical
```

The snapshot captures the per-well `stats`, the QC verdicts, the full gate tree with counts and
parent percentages, the Michaelis-Menten fits, the resolved transform arguments, and the titration
export as written to disk. `captured_at` and `elapsed_s` are excluded from the comparison; everything
else must match exactly, with a zero tolerance, so a 1e-16 drift is reported rather than rounded
away.

Two variants drive the same capture through different entry points, which is the point of having
them:

* `regression_capture_via_run_acta.R` — through `run_acta()` from a **different** working directory,
  so it also proves the callable path does not depend on the caller's cwd.
* `regression_capture_via_report.R` — through `run_acta(report = TRUE, plots = TRUE)`, the app's
  default mode.

Snapshots (`*.rds`) are run artefacts and are not tracked — regenerate them locally.

## Case builders — these REWRITE inputs

Not gates. Run them only when you intend to rebuild a case, and expect the OQ baselines to move.

| Script | What it does |
|---|---|
| `build_oq_test.R` | builds a publicly shareable case from a real run: sanitises the workbook **and** the FCS keywords. The FCS were the real exposure — they embed the company name, an operator username, the programme name, the instrument serial, a donor code and the construct marker, none of it visible from the layout sheet. |
| `downsample_oq_fcs.R` | downsamples every OQ FCS to N events so the case is small enough for git and fast enough that people actually run it. Seeded **per file** from the well id, so a file always yields the same events — a gate whose input changes between runs cannot assert on numbers at all. |

After either one, rerun `verify_oq_sanitised.R` and both OQ cases, and re-record `expected.json`.
