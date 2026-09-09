<!-- Reference material for anyone reading or extending the pipeline. The front page
(../README.md) is what a new user needs; this is the level below it. -->

# ACTA internals

> **Why "reagent" and not "antibody".** Plenty of detection reagents are not antibodies --
> streptavidin conjugates, viability dyes, peptide-MHC multimers, Fc-fusion proteins -- and they are
> titrated the same way. The pipeline therefore says *reagent* or *staining reagent* throughout, and
> the `gating_template` row holding the titrated channel is aliased **`Reagent`**.


The pieces, and what each one owns:

- **`ACTA_Script_2_98.R`** — the analysis driver (does all the work)
- **`ACTA_Functions.R`** — helper functions, sourced by everything
- **`ACTA_Report_2.98.Rmd`** — report wrapper that `source()`s the script into the
  knit environment and lays the outputs out as a landscape PDF
- **`ACTA_Dashboard_2_98.R`** — the standalone HTML dashboard generator
- **`ACTA_App.R`** — the Shiny front end
- **`Setup.R`**, **`tests/`**, **`Diagnostics/`** — one-step first-run setup, the gates, and
  the three self-contained OQ cases

You can either run the script standalone (generates plots + Excel export) or **Knit the
`.Rmd`**, which runs the same script and then assembles the PDF report. The `.Rmd`
sources the script via `source(ScriptFile, local = knitr::knit_global())` so the
script's objects (`gs`, `SIPlot`, `SatPlot`, `plotList_hist`, `plotList_pseudo`,
`plotLayoutList`, …) are visible to the report chunks. It locates its own folder with
`this.path::this.dir()` (falling back to
`knitr::current_input()`) and passes the script an **absolute** path — deliberately *not*
`rstudioapi::getActiveDocumentContext()`, which reports whatever file is focused in the
editor and will happily run the report against a different copy of the pipeline.

### Startup

1. Self-installs every package in `listOfLibrary`, trying **CRAN → Bioconductor** in
   turn; if a package resolves from neither, `library()` raises a clear error.
2. Sets the working directory to the script's own folder from **`this.path()`** (normalised
   and absolute — correct under Source, `Rscript` *and* knit), falling back to
   `this.path(original = TRUE)` for Positron (which can return a percent-encoded `file://`
   URI, so it is `URLdecode()`d and stripped of its scheme) and finally to `rstudioapi`.
   Run the script via **Source**, not line-by-line — the console has no script context in
   Positron, and the script stops with an explicit error rather than guessing.

   > **Do not switch this back to `this.path(original = TRUE)` as the primary.** When the
   > `.Rmd` sources the script by a *relative* name, `original = TRUE` returns that bare
   > filename, `dirname()` is then `"."`, and the `setwd()` quietly becomes a no-op — the
   > run silently inherits the caller's directory and can load **a different copy's**
   > `ACTA_Functions.R`. The helpers are therefore sourced by absolute path out of the
   > script's own folder (erroring unless exactly one `ACTA_Function*.R` is found), the
   > sourced path is echoed with `message()`, and a guard rejects a helpers file that
   > predates the current plot API instead of failing later with `unused argument`.
3. Sources `ACTA_Functions.R` and registers the custom openCyto plugins: gating methods
   `gate_flowmeans`, `gate_flowmeans_costain`, `gate_auto_costain_dip`,
   `gate_auto_costain_gmm`, `gate_auto2_costain`, `gate_manual`, and preprocessing methods
   `pp_costain_floor`, `pp_auto2_costain`. Each registration is wrapped in `tryCatch` so
   re-sourcing within one R session doesn't error on re-registration.

### Inputs

- **Instructions workbook** `*Ab_Titration_Instructions*.xlsx` —
  the sheet **`Layout_Plate`** is read
  into `metadata_file`; the script stops with an explicit message naming the sheets it did find
  if that name is absent. (There is ONE layout sheet, since every
  run to date has used one plate.) Key columns the script relies on:
  `Dirname` (antibody group / FCS subfolder), `Markername ($PnS)` (target **marker name** — see
  *Marker channel resolution* below), the optional `Channel ($PnN)` (target **detector name**),
  `Alias`,
  `PlateID`/`Row`/`Column` (well → sample mapping), `StainType` (`Unstained`|`Costain`|
  titration wells), `StainQty` (titration dose, the x-axis), `ELN_ID` (used for file
  naming and plot titles), `Operator`, and reagent fields (`Cat_Num`, `Vendor`, `Lot_Num`,
  `Test_Material`, `e6_cells_per_well`) used for plot labels. Rows with a blank `Dirname`
  (unused template wells) are filtered out throughout.
  The template's layout sheets also carry **`Fix`** and **`Perm`** (immediately
  before `StainQty`), pre-set to `TRUE` on every populated well — i.e. every row with a
  `StainType`. They are real Excel booleans, so `read_excel()` types them as `logical`, and
  `fixPermLabel()` collapses them into the export's `Fix_Perm` column.
- **`gating_template` sheet** in the same workbook — an openCyto gating template
  (columns `alias`, `pop`, `parent`, `dims`, `gating_method`, `gating_args`, plus the
  standard optional `collapseDataForGating`, `groupBy`, `preprocessing_method`,
  `preprocessing_args`). Defines the population hierarchy applied to every antibody
  (see gating step below). It must contain **exactly one marker row**, aliased `Reagent`
  or `Antibody` (case-insensitive) with `pop = +/-` — that is the titrated-marker gate,
  and the script errors out if there isn't exactly one. A row whose `alias` is blank or
  **starts with `#`** is ignored by the whole pipeline — use a leading `#` to keep a
  reference/example row in the sheet without it being read. You can also add **extra
  documentation columns** (the shipped sheet uses `default_gating_args_reference`,
  `default_preprocessing_args_reference` and `help_notes`) — the script keeps only
  openCyto's recognised columns, so any others are dropped before `gt_gating` and serve
  purely as in-sheet reminders. The sheet ships with `#`-commented placeholder rows, one
  per gating/preprocessing method, pre-filled with its full default argument list.
  Three **per-population opt-in columns** decide what each row produces:
  **`layout_report`** (plot printed as a report page), **`layout_export`** (plot written to
  `Plots/`) and **`stats_export`** (population contributes count / percent / MeFI / robustSD to
  the `StatsExport` workbook). A plot is *built* if either layout flag is TRUE, so an
  export-only population still renders and a row with both FALSE is never built at all.
  Every flag **defaults to TRUE** in three senses — absent column, blank cell, `NA` cell — so a
  layout file written before they existed behaves as it always did; only an explicit FALSE turns
  a population off. Names are matched **case-insensitively** — the template ships them as
  `Layout_Report`, `Layout_Export` and `Stats_Export`, and either case works: because an
  unmatched name falls through to the
  TRUE default rather than erroring, an exact match would turn a renamed column into a silent
  "everything on". Flags on the marker `+/-` row are meaningless (it yields no single population
  node) and are skipped — quietly if blank, with a message if explicitly set TRUE.
- **`Info` sheet** in the same workbook — run-level settings. Several are read, the most
  consequential being **`compensation`**, which selects how conventional-flow spillover is handled
  for the whole run (`csv` / `self` / anything else; see *Compensation* below). Also read:
  `Downsample_to` (per-file event cap), `ELN_ID`, `dashboard_dir`, `titration_export_dir` and
  `dashboard_tabbed_by` / `tabbed_by`. The sheet is optional and so is every column — no
  `compensation` column means no compensation.
- **Compensation matrix** `*CompMatrix*.csv` in the working directory — read only when
  `Info!compensation` is `csv`. A square matrix with channel names as the header row and the first
  column (the shape flowCore's `spillover()`, FlowJo and Diva all export).
- **FCS data** under `Titration_FCS/<antibody>/…` (local only, not in the repo). That FLAT form is
  the current one, from 2_85 onward, and it is what every shipped fixture uses. The older
  `Titration_FCS/PLATE_1/<antibody>/…` is still read so existing folders keep working; any other
  `PLATE_*` folder is
  skipped with both a `message()` and a `warning()` — the `message()` because R collapses more
  than ten warnings into an unreadable *"There were N warnings"* summary and the text is lost.

### Per-reagent loop

For each unique `Dirname`, the script:

1. Validates the gating method and reads that antibody's FCS folder (dropping
   `Unstain` wells) into one `flowSet`.
2. `annotate_changeParam_pdWrite()` — matches each FCS to its layout row by well
   (`$WELLID` + plate via `which_plate()`), rewrites descriptions/filenames, and attaches
   the metadata as `pData`. Unstained samples are then removed.
3. **Compensates** the set (`actaCompensate()`), then applies a **FlowJo biexponential**
   transform to fluorescence channels (`flowjo_biexp_transform()`). Order matters and is
   enforced by construction: un-mixing is a *linear* operation on raw channel values, so it must
   precede the non-linear biexp — compensating after transforming is simply wrong. Spectral
   (Aurora / ZE5) data arrives already unmixed and needs no compensation, which is why this step
   is opt-in.
4. Builds a `GatingSet` and applies the **openCyto `gatingTemplate`** from the
   `gating_template` sheet via `gt_gating()`, replacing the former gate-by-gate
   construction. The template is rebuilt per antibody, because the marker row's x/first
   `dims` entry is filled in from that antibody's layout `Dim` (resolved to a channel via
   the `boundary` marker↔channel map); any dim authored in the sheet for that row is kept
   as the y/second dim. The current template defines `Cells` (`gate_flowclust_2d`) →
   `Single_cells` (`singletGate`) → `Live` (`gate_flowmeans`, a custom flowMeans plugin
   registered from `ACTA_Functions.R`) → the `Reagent+/-` marker split. The script
   normalises a few method-name variants to their openCyto 2.x registered tokens and
   single-quotes `gating_args` for the CSV round-trip openCyto uses.
   Any additional upstream gating a panel needs is added as rows in the sheet; the template
   shipped in `Template/` deliberately carries only the minimum.
5. **Marker gate** — produced by the template's `Reagent`/`Antibody` row on parent `Live`,
   which yields two nodes named `<alias>+` / `<alias>-`; the script finds them by their
   `+`/`-` suffix. The **Costain floor** reaches the gate through a *preprocessing* plugin
   (`pp_costain_floor`, `groupBy = Dirname`), which reads the Costain well at
   `costain_quantile` and passes that fluorescence value to the gating plugin via `pp_res`
   — a gating plugin only ever sees one flowFrame and so cannot reach the control well
   itself. Every gate is `c(bound, Inf)`: right of the boundary is positive, and the `+`/`-`
   side comes from the `pop` column, never from the `cluster` argument.
   If the marker row's method is **`gate_manual`**, gating runs in **two passes**: the auto
   template (marker row excluded) builds up to the parent, `drawManualGates()` draws the
   gate interactively with flowGate, and the full template is then re-applied so openCyto
   builds the `+/-` split from the cached hand-drawn gate. Scope comes from the marker
   row's `groupBy` column — a column name (e.g. `Dirname`) means one gate per group value,
   drawn on that group's highest-`StainQty` well; empty means one gate per well. Drawn
   gates persist as `.rds` under `Manual_Gates/` and are reused until `manualGateRedraw`
   is set `TRUE`.
6. Computes per-sample stats and the **Stain Index**:

   ```
   SI = (median_pos − median_neg) / (2 × robustSD_neg)
   ```

   where `robustSD_neg` is the MAD of the negative population (`pop.rsd`) and medians come
   from `pop.MeFI`. It also records `% positive` (saturation), and tracks `maxSI` /
   `maxSat` with their corresponding stain volumes.
7. Builds per-antibody plots: gated histogram (with the gate line), pseudocolor, and the
   gating-layout panels. The layout panels are generated **generically, one per
   `gating_template` population** — for each row the script resolves its `dims` to
   channels (`resolveDimsToChannels()`), locates its node, and renders a faceted gate
   plot (`makeGateLayoutGreatAgain()`). Adding a population to the sheet automatically adds its
   panel; nothing per-population is hardcoded. The marker `+/-` row is excluded from this
   scaffolding — its `pop` yields two `+`/`-` suffixed nodes with no single node matching
   the alias — and is plotted by the histogram/pseudocolor panels instead.

   All three per-antibody plots share one caption/subtitle convention: the *caption* carries
   the **full gating provenance** and the *subtitle* is `Parent pop: <pop> | <label>`,
   where `label` is the antibody descriptor (alias, channel, catalogue number, vendor, lot,
   test material and cell count). That same string is the facet label on `SIPlot`/`SatPlot`,
   built once per antibody so the two never drift apart.

   The caption is built by `gateCaption()` from a `gating_template` row — that population's row
   for a layout plot, and the **marker (`Reagent`) row** for the histogram and pseudocolor, since
   those two show the marker gate. It
   reproduces **every field that decided the gate** — `gating_method`, `gating_args`,
   `collapseDataForGating`, `groupBy`, then `preprocessing_method` and `preprocessing_args` on a
   second line — each separated by `||`. So a plot is self-documenting: the exact arguments that
   produced the drawn gate are on the figure, not only in the workbook, and two runs that differ
   only in a `quantile` are told apart from the PNG alone. `wrapToWidth()` then reflows it to
   90% of the figure width — measured, not character-counted — so the caption fills the device it
   is actually drawn on: one line on a wide 12-inch export, two on a narrow report page.

   **Facet sizing and pagination.** Every faceted per-antibody plot picks its grid from
   `facetLayout()`, which chooses the `ncol` whose resulting panel aspect ratio best matches the
   report's text block and then sizes the device from the panel count — so a facet stays the
   same physical size whether an antibody has 5 wells or 16, instead of shrinking as wells are
   added. In the **report only**, a series longer than `REPORT_FACETS_PER_PAGE` (12, laid out
   4 × 3) is split by `facetPages()` into pages of at most 12 — 13 facets become 12 + 1 — and
   each page is re-built against that subset with the x limits and bin widths computed **once
   over the full series**, so axes and gate lines are identical across pages. **Exported PNGs
   are never paginated**: they always render the whole series in one image, so the file on disk
   remains the complete titration. At or below the threshold the report reuses the full plot
   verbatim, so a typical run is byte-for-byte what it was before pagination existed.

   The histogram facets on `scales = "free"` (both axes per-panel): a density peak is not
   flattened by the brightest well setting a shared y maximum, and with no shared y range there
   is nothing for a paginated subset to rescale. The pseudocolor keeps `scales = "free_x"`,
   because its y is FSC-A with explicit limits that *should* stay common across panels.

### Marker channel resolution

Which channel gets titrated is resolved once per antibody group by `resolveMarkerChannel()`, from
two layout columns, `Channel` taking precedence whenever it is filled in:

| column | matched against | value looks like | precedence |
|---|---|---|---|
| `Channel ($PnN)` | `colnames()` | `Alexa Fluor 647-A` | used when populated |
| `Markername ($PnS)` | `markernames()` | `cPARP` | the fallback, and the usual case |

Each header carries the FCS keyword it resolves against as a suffix, so the sheet itself records
which slot is being matched — `$PnS` is what `markernames()` returns, `$PnN` what `colnames()` does.

Both values are matched **exactly** — the whole name, ignoring case and surrounding whitespace
(`.actaSame()`) — and **not** as regular expressions. So `cparp` and `cPARP` are interchangeable,
`CD1` selects `CD1` and never `CD14`, and a name that happens to contain regex metacharacters
(`CD8+`, `PE-A`, `CD19 (BV421)`) is typed as-is with nothing to escape. A value matching nothing is
a hard error that lists every available marker or detector, because a no-match and a wrong match
are indistinguishable in the finished report.

Matching used to be a pair of regexes, unanchored for `Markername` and anchored for `Channel`. Two
things made that untenable: unanchored, `CD1` was ambiguous with `CD14` and `CD19` on any real
panel, so the common case needed an anchor the analyst had to know to write; and a metacharacter in
an ordinary marker name (`CD8+`) matched the wrong parameter or none. With exact matching, an
ambiguous value is only possible when the PANEL itself repeats a name — a data problem — so it is
fatal for both columns rather than a warning that takes the first hit. Zero values, or two
different values within one antibody group, is a hard error either way.

**Naming.** The legacy header for this column was `Dim`, still read for older workbooks. The current name is used because `Dim` had
always been a *markername* field despite its channel-sounding name — every lookup ran it against
`boundary$Target`, which is built from `markernames()`. `Channel ($PnN)` is the newer opt-in route
for panels where the markername is missing, duplicated, or simply isn't how the analyst thinks about
the parameter — common on conventional flow, where `$PnS` may never have been filled in.

Headers are resolved by `actaLayoutCol()`, which compares names with everything but letters and
digits stripped. That is not for backward compatibility — **a layout workbook is version-locked to
its script**, and the matching blank workbook ships in `Template/`. It is because the header text
travels into `pData` and the FCS `@description`, where `merge()` and `data.frame()` mangle
`Markername ($PnS)` into `Markername...PnS.`; normalising means the lookup survives that round trip,
and that the suffix's exact spacing or `$` can change without touching code.

> Deliberately absent: a positional `mkname` fallback that assigned a hardcoded Cytek Aurora panel
> by acquisition order when `$PnS` was absent. It was a manual override for one panel; marker
> names now come from the FCS, and `Channel` covers the case where they are unusable.

### Dashboard (`ACTA_Dashboard_*.R`)

A **standalone** generator that turns the titration exports into one self-contained HTML dashboard.
It is deliberately independent: the analysis never calls it, it never sources `ACTA_Functions.R`,
and it needs no lookup workbook or network — everything comes from the exports ACTA already wrote.
That is what makes it shareable on its own.

Settings live on the workbook's `Info` sheet (header annotations are stripped, so
`dashboard_creation (L)` and `dashboard_creation` both resolve):

| column | meaning |
|---|---|
| `dashboard_creation` | `TRUE` builds. `FALSE`, blank or absent does nothing — the script is safe to run unconditionally |
| `dashboard_dir` | where the `.html` is written. Blank → the script's own folder |
| `titration_export_dir` | the folder whose **immediate subfolders** are the runs. Blank → the ACTA root |

Each immediate subfolder is one run, scanned for a `*TitrationExport*.xlsx`, a report `.pdf` and
figures. Any path containing **`archive`** (case-insensitive, at any depth) is ignored. Figure links
are written **relative** to the dashboard, so the file stays portable.

**Pooling across versions.** Exports are combined with `bind_rows()`, not `rbind()`: the pool takes
the **union** of columns and fills gaps with `NA`, so a run whose export predates a column sits
beside a newer one without either being re-run. Matching is by **name**, so reordering columns in
the export cannot shift a value into the wrong cell.

**Columns are derived from the exports, not fixed in the template.** The HTML carries
`__COLS_JSON__` / `__GROUPABLE_JSON__` alongside `__META_JSON__` / `__DATA_JSON__`, and the
generator injects all four. Consequences, all intended: a column added to a future export appears
with no edit to the HTML (that is how `Panel` arrived); a column that exists in no export is never
rendered, so nothing sits permanently blank; and ordering follows the **newest** export, with
older-only columns appended after it. Per-well internals (raw statistics for the winning well) are
hidden by an explicit list. A template that still hardcodes its own `COLS` keeps working — only the
placeholders actually present are substituted.

The template is found in `Template/` **by the pattern `dashboard_template`**, so it can be swapped
for another as long as it keeps the placeholders. Zero or several matches is a hard error rather
than a guess, since picking one would make the output depend on file ordering; keep the spare in a
subfolder (`Other_Templates/`, which is not searched) or one named `Archive`.

### Reproducibility

`gate_flowclust_2d` and flowMeans initialise randomly. Unseeded, that would make a run
**non-reproducible** — two renders of identical data disagreed by ~2.5 percentage points on
`%Live`, with a visibly different FSC-A range. `ACTA_SEED <- 123` is now set at startup **and at
the top of every antibody iteration**. The per-iteration reset is the point: seeding only once
would make each group's gates depend on how many groups ran before it, so adding an antibody would
silently move the gates of every group after it. Verified by running the pipeline in two separate
R processes and diffing all 36 gate percentages at 10 decimal places — identical.

### Case sensitivity

Layout text is matched case-insensitively, via `isLayoutValue()` for the enum-like columns and
`.actaSame()` for the name lookups:

| value | why it matters |
|---|---|
| `StainType` | `unstained` used to slip past `!= "Unstained"` and be titrated as a real sample — silent corruption, not an error |
| `Row` | `$WELLID` reports an upper-case letter, so a layout typed `a` matched nothing and the well was silently dropped by the `!is.na(Dirname)` guards |
| `Markername ($PnS)` / `Channel ($PnN)` | matched exactly, ignoring case and whitespace (see above) |
| `Fix` / `Perm`, `compensation`, the three opt-in flags | already insensitive, values and column names both |

`Dirname` → FCS subfolder is matched case-insensitively **and literally** (`startsWith()` on an
upper-cased copy). It used to be an unescaped regex, so a `Dirname` containing `+`, `(` or `.`
matched the wrong folder or none.

Deliberately still case-**sensitive**: the gating_template's `alias`, `parent` and `pop`, which
become openCyto node names, and `groupBy`, which must name a `pData` column. Case-folding those
would mean normalising the whole gating tree. Everything else in the layout (`Vendor`, `Clone`,
`Cat_Num`, `Lot_Num`, `Test_Material`, `Operator`, `ELN_ID`, `Alias`) is display-only and never
compared.

### Compensation

Driven entirely by `Info!compensation`, one value for the whole run — a titration compares wells
to each other, so compensating them differently would invalidate every cross-well comparison the
report makes.

| value | behaviour |
|---|---|
| `csv` | Reads the single `*CompMatrix*.csv` in the working directory, filters it to the channels present in the data, and applies it to every sample |
| `self` | Each frame is compensated with **its own** acquisition-time matrix, from `$SPILLOVER` or `$SPILL`. Fails loudly, naming the sample, if a frame carries neither |
| anything else, blank, or column/sheet absent | **Identity** matrix over the fluorescence channels |

Two deliberate choices:

- **Identity rather than skip.** The "no compensation" branch still applies a matrix, so the
  object handed to the `GatingSet` is a compensated set in every case and there is one downstream
  path instead of two. Multiplying by `solve(diag(n)) == diag(n)` is exact in floating point, so
  an identity run is bit-identical to not compensating at all (verified: `identical(exprs(...))`).
- **Unrecognised values warn.** A typo like `spill` or `CSV_matrix` falls back to identity, but
  says so — silently running uncompensated would look like a clean run while skewing every gate.

Every per-antibody plot — histogram, pseudocolor and each layout page — carries a
`Compensation: <value>` line at the end of its caption, showing the `Info` cell **verbatim** (an
empty cell renders as an em dash). It reports what the workbook asked for, not what was applied;
what ACTA actually did with it is logged at run time by `actaCompensate()`.

Failure modes are fatal by design rather than best-effort: `csv` with **no** matching CSV errors,
and `csv` with **more than one** errors listing the candidates, because silently compensating with
the wrong matrix is invisible in the report and unrecoverable downstream. When the matrix covers a
wider panel than the acquisition, the extra channels are dropped with a logged message — note that
this also drops their spill *into* the retained channels, so a filtered matrix approximates the
full un-mixing rather than reproducing it.

### Outputs

- **`SIPlot`** — Stain Index vs. stain volume, faceted per antibody, with dashed lines
  marking the max SI and its optimal volume. This is the primary titration read-out
  (highest SI = best signal-to-noise → recommended antibody volume).
- **`SatPlot`** — `% positive` (saturation) vs. stain volume.
- PNGs written to `Plots/`, plus an Excel export
  `<date>_<ELN_ID>_TitrationExport_<version>.xlsx` containing the max-SI row per
  antibody (the recommended stain volume), tagged with `ScriptVersion` (parsed from the
  script's own filename, so it tracks the folder version automatically) and
  with **`Fix_Perm`** — `No Fix/Perm` / `Fix only` / `Fix+Perm`, derived from the layout's
  `Fix`/`Perm` columns by `fixPermLabel()`. The export **requires** those two columns: the
  script stops with an explicit message if they are absent, and `Perm = TRUE` with
  `Fix = FALSE` is rejected outright as a layout error. An
  **`Acquisition_date`** column, taken **per FCS file** from its `$DATE` keyword — so it
  reflects when that winning well was actually acquired, and is deliberately independent of
  the render date that names the export file — and an **`Equipment`** column, likewise
  per file, formatted `"$CYT ($CYTSN)"` (e.g. `Aurora (SN1234)`) so both the instrument model
  and the individual machine are on the record. Leading columns are ordered
  `label, Acquisition_date, Equipment, Fix, Perm, Fix_Perm, …`. Per antibody the
  PNGs are `…_histogram.png`, `…_pseudo.png` and one `…_layout_<alias>.png` per
  gating_template population.
- When knit, the `.Rmd` assembles a **landscape PDF with a linked table of contents**
  (`toc_depth: 2`, xelatex, running head and footer built in plain LaTeX so the report needs
  only six packages and renders on a minimal TeX install): the gating scheme (`plot(gs)`),
  `SIPlot`, `SatPlot`,
  the per-antibody layout pages, and a Session Information section.
  Each antibody is emitted as a `\subsection`, so it **nests under "Layouts for each
  antibody"** in the TOC, and every plot gets **its own page** — histogram, then
  pseudocolor, then one page per gating_template population. (The histogram
  and pseudocolor were stacked in one `patchwork` and the population plots were crammed
  into two half-and-half pages.)
- Session Information uses **`sessioninfo::session_info()`**, not base `sessionInfo()` —
  it additionally reports each package's *source* (CRAN / Bioconductor / GitHub + version)
  and a separate environment block, which is what makes a run reproducible after the
  fact. It is preceded by a short provenance block (script version, render timestamp,
  operator/host, layout workbook).

### Helper functions (`ACTA_Functions.R`)

All the `gate_*` / `pp_*` entries are **openCyto plugins**, registered by the script at
startup and selected by name from the `gating_template` sheet. Every `*_costain` gating
method requires its matching preprocessing method on the same row, or its floor silently
falls back to `-Inf` (a no-op).

| Function | Role |
|----------|------|
| `gate_flowmeans()` | Custom openCyto gating plugin (method `gate_flowmeans`): 1D flowMeans gate for the `gating_template` sheet; args `K`, `cluster` (`neg`/`pos`), `quantile`. No Costain floor |
| `.flowmeans_bound()` | Internal: runs flowMeans (k=`K`) on one channel and returns the `quantile`-th percentile of the chosen cluster. Shared boundary maths behind every flowMeans-based plugin |
| `pp_costain_floor()` | **Preprocessing** plugin: computes the group's Costain floor (Costain well at `costain_quantile`, default 0.99) and hands it to the `*_costain` gating methods via `pp_res`. Required by `gate_flowmeans_costain` / `gate_auto_costain_dip` / `gate_auto_costain_gmm` |
| `.resolve_costain_floor()` | Internal: coerces `pp_res` to the numeric floor, or `-Inf` when absent |
| `gate_flowmeans_costain()` | Like `gate_flowmeans`, but the boundary is floored at the Costain value: `bound = max(flowmeans, floor)`. The floor arrives via `pp_res` from `pp_costain_floor` — there is no numeric `costain_quantile` gating arg; that percentile lives solely in `preprocessing_args` |
| *(retired)* `gate_static` | Removed — use openCyto's built-in **`boundary`** for a fixed rectangle gate: `gating_method = boundary`, `gating_args = "min=c(x,y), max=c(x,y)"` (1D or 2D). A static quadrant = four `boundary` rows |
| `gate_auto_costain_dip()` | Custom plugin (method `gate_auto_costain_dip`): Hartigan dip test picks the method per frame — **bimodal** → `gate_flowmeans_costain` behaviour; **unimodal** → static gate at the `costain_quantile` value. Args as `gate_flowmeans_costain` plus `dip_pval` |
| `gate_auto_costain_gmm()` | Same as `gate_auto_costain_dip` but the switch uses **mclust GMM model selection (G=1:2) + Ashman's D separation guard** instead of the dip test (avoids the dip p-value's n-sensitivity; gives an explicit, tunable separation threshold). Args as `gate_flowmeans_costain` plus `ashman_D` (default 2) |
| `gate_auto2_costain()` + `pp_auto2_costain()` | **Group-level** auto method (method `gate_auto2_costain` + preprocessing `pp_auto2_costain`). The preprocessing plugin applies the Costain floor to every sample and goes **static at the floor for the whole group** only when one and the same well is **both** (a) saturated — % positive above the floor > `sat_threshold` (default 0.90) — **and** (b) unimodal by Hartigan's dip test (p ≥ `dip_pval`, default 0.05). A saturated but still clearly bimodal well keeps flowMeans, since it has a real boundary to find. Otherwise every sample uses flowMeans floored at the Costain value. Saturation is assessed over *all* wells in the group, not just the top `StainQty`. It logs which branch it took. Gating args = `gate_flowmeans` (`K`, `cluster`, `quantile`); preprocessing args = `costain_quantile`, `sat_threshold`, `dip_pval`. **`dip_pval` runs counter-intuitively: lower ⇒ static more likely; `dip_pval = 0` reduces it to a saturation-only rule, `1` disables static** |
| `gate_manual()` | Manual/interactive plugin (method `gate_manual`): applies gates pre-drawn by `drawManualGates()` and cached per sample in `.acta_manual_cache`. No auto-gating — full user control; fallback when automated gates are unreliable |
| `drawManualGates()` | Draws the manual gate(s) interactively with flowGate and returns the sample→gate cache. Scope from the marker row's `groupBy` (column ⇒ one gate per group value, drawn on the brightest well; empty ⇒ one per well). Persists `.rds` under `Manual_Gates/` and reuses them unless `redraw = TRUE` |
| `actaCompMode()` | Normalise `Info!compensation` to `csv` / `self` / `none`. Absent sheet, absent column, blank and `NA` all mean `none`; an unrecognised value falls back to `none` **with a warning**, since a silent fallback would look like a clean run |
| `actaCompensate()` | Apply compensation and return the compensated set (`fs_comp`) — CSV matrix, per-sample `$SPILLOVER`/`$SPILL`, or identity. Generic over `flowSet`/`cytoset` (`flowCore::compensate()` handles both), and restores `pData` afterwards, which `compensate()` can otherwise reduce to just `name` |
| `.actaReadCompCsv()` / `.actaFilterSpill()` / `.actaFrameSpill()` | Internals: locate + read the one `*CompMatrix*.csv` (fatal on zero or multiple matches); restrict a matrix to the channels present in the data (logging what was dropped); pull one frame's acquisition matrix, trying `$SPILLOVER` then `$SPILL` then vendor spellings |
| `flowjo_biexp_transform()` | FlowJo biexponential transform of the **fluorescence** channels only — scatter stays linear, since biexp-ing it distorts the upstream `Cells`/`Single_cells` gates |
| `plotLimits()` | Per-channel 1st–99th percentile range across a flowSet, used for histogram axis limits |
| `annotate_changeParam_pdWrite()` / `which_plate()` | Join FCS to layout metadata; detect plate from filename |
| `pop.rsd()` / `pop.MeFI()` | Custom flow stats passed to `gs_pop_get_stats()` — robust SD (MAD) and median FI. **Both read the global `channel`** set by the driver loop, since `gs_pop_get_stats(type=)` calls them with the flowFrame only |
| `validateLayoutGroups()` | Per-antibody layout sanity check, run **before any FCS is read**. **Errors** on a duplicate `StainQty` among the `Stain` wells (the plots facet on volume and `maxSI_StainQty` becomes ambiguous) and on a **missing Costain** well (which otherwise fails obscurely as `gs[[NA]]` → *"missing sample: NA"*, and silently gives a `-Inf` gate floor). **Warns** on >1 Costain (events are pooled) or >1 Unstained (all dropped). Controls are excluded from the duplicate check since Unstained and Costain are both volume 0 |
| `fixPermLabel()` | Collapse the layout's `Fix`/`Perm` flags into the export's `Fix_Perm` value: `No Fix/Perm` / `Fix only` / `Fix+Perm`. Errors on **Perm without Fix** (`Perm is TRUE without Fix, check template`) and on a blank flag, naming the offending wells. Accepts logicals or text (`TRUE`/`Yes`/`1`) |
| `fcsAcquisitionDate()` | Acquisition date of one flowFrame from its FCS **`$DATE`** keyword, as a `Date`. Called per file by `annotate_changeParam_pdWrite()`, so each well carries the day it was acquired. Handles `dd-Mon-yyyy` (SpectroFlo/BD), ISO and slash forms; month names resolved from a lookup rather than `%b`, which is locale-dependent. Missing/unparseable → `NA` with a warning, never a guess |
| `fcsKeyword()` / `fcsEquipment()` | `fcsKeyword()` reads any plain-text FCS keyword (trimmed, `NA` + warning if absent). `fcsEquipment()` composes the export's `Equipment` value as **`"$CYT ($CYTSN)"`** — e.g. `Aurora (SN1234)` — so both the model and the individual machine are recorded. Degrades to model-only or `(serial)`-only, and warns only when both keywords are absent |
| `actaLayoutCol()` | Resolve a layout column by name, comparing with all non-alphanumerics stripped. Survives R mangling the header (`Markername ($PnS)` → `Markername...PnS.`) as it passes through `merge()`/`pData`/`@description`, and keeps the suffix's exact spelling out of the code. Returns the name as actually spelled in the sheet, so messages quote what the analyst typed |
| `resolveMarkerChannel()` | Resolve one antibody group's titrated channel (`$PnN`) from its layout row: `Channel` against `colnames()`, else `Markername` (legacy `Dim`) against `markernames()`. Both matched exactly by `.actaSame()`, ignoring case and whitespace, never as a regex. Errors on neither-populated, no match, two different values in one group, or a name the panel itself repeats. Called **once** per group — it replaced three separate copies of the same lookup that could otherwise drift apart |
| `resolveDimsToChannels()` | Resolve a template `dims` string (marker/channel names) to channel names via the `boundary` map |
| `makeGateLayoutGreatAgain()` | Build a faceted (per StainQty) gate-layout plot for one gating_template population. Title is `<alias> <channel> : <pop> \| <ELN_ID> \| <operator>` (either stamp omitted if not supplied), the caption is the provenance string from `gateCaption()`, and the subtitle is `Parent pop: <pop> \| <label>`. `layout_ncol` and `caption_width` come from `facetLayout()`. Returns a **raw `ggcyto` object — do not wrap it in `as.ggplot()`** (see below) |
| `gateCaption()` | Compose one population's plot caption from its `gating_template` row: `gating_method \|\| args \|\| collapse \|\| groupBy \|\| pp \|\| pp args`, as **one unbroken string** — line breaking is `wrapToWidth()`'s job. Puts every gate-determining argument on the figure itself, so a PNG is self-documenting without the workbook. Empty fields render as an em dash, so "not set" is visibly different from "not shown" |
| `wrapToWidth()` | Greedily wrap text to 90% of a figure's width. Widths are **measured** on a throwaway `pdf()` device rather than counted as characters — these captions are mostly narrow glyphs (`\|\|`, `=`, digits), so a character budget breaks early and wastes space, and ggplot does not wrap captions itself. Measuring on a scratch device keeps it safe to call while knitr's device is open |
| `actaTemplateFlag()` | Read one opt-in column (`layout_report` / `layout_export` / `stats_export`) off the gating_template and return a logical vector named by alias. Case-insensitive on the column name; TRUE by default for absent column / blank / `NA`. Deliberately type-loose — Excel round-trips a boolean column with one text `NA` back as `character`, so `TRUE`/`T`/`YES`/`1` and their negatives all parse. `attr(,"explicit")` marks the cells that said TRUE out loud, letting callers tell an intentional TRUE from a defaulted one |
| `facetLayout()` | Choose `ncol` (and the implied row count) for a faceted plot, then size the output device from the panel count so **each facet keeps a constant physical size** as facets are added or removed. Picks the `ncol` whose panel aspect ratio best matches the report text block, with a small `empty_penalty` discouraging ragged final rows. `ncol_fixed` overrides the choice — used to pin report pages to 4 columns |
| `facetPages()` | Split a run's `StainQty` vector into ordered page-index groups of at most `per_page` (report default 12). Returns a **single group** when the series fits, so the unpaginated path reuses the full plot untouched. Report-only: the exported PNGs always render the whole series |
| `adaptiveBins()` / `computePlotParams()` / `flow_bin2d()` | Adaptive bin counts, percentile axis limits + per-axis binwidth, and the log-scaled 2D-bin colour ramp |

> **Never pre-convert a plot with `ggcyto::as.ggplot()`.** It is *not idempotent* and does
> not drop the `ggcyto` class, so a pre-converted plot is converted a **second** time when
> `ggsave()` / `print()` dispatches `print.ggcyto` on it — and that second pass dies with
> `object 'name' not found`, because the resolved data no longer carries the sample `name`
> column. Wrapping the converted plots in a `patchwork` does not work: it draws its
> elements as plain ggplots and hides the problem; printing them directly (one plot per
> page) exposes it. Keep plots as raw `ggcyto` — `ggsave()` and the report's
> `print()` each convert exactly once.

