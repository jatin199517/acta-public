## TiterPubs export: fill the community titration template from a finished ACTA run.
##
## TiterPubs (https://metrdy.com/titerpubs) publishes antibody titrations as a shared reference so
## that the next person to use a reagent does not have to re-titrate it. Their template v2 names
## ACTA by name -- "a .zip (for example an ACTA export that includes this template)" -- so the
## bundle this file assembles is the submission shape they already accept.
##
## WHAT THE TEMPLATE EXPECTS, and it is not a table:
##   row 1  column headers (43 cells, A..AP are the 42 real fields)
##   row 2  THE DATA ROW. One submission = one reagent = one file.
##   row 3  the sanctioned escape hatch -- "if the menu in row 2 does not contain a relevant
##          option, add text to row 3". Off-menu values are a DESIGNED feature, not a workaround.
## So a run with N reagents produces N filled templates in one zip, not one file with N rows.
##
## DATA LICENSE: anything submitted is published CC BY 4.0. The trigger is therefore OFF by
## default (Info!titerpubs_export = FALSE) and has to be turned on per run, deliberately.

## Columns A..AP of the `to publish` sheet, in sheet order. The letters are the write targets, so
## this vector is the single place that knows the template's geometry.
## COLUMNS ARE RESOLVED BY HEADER NAME, FROM THE TEMPLATE IN HAND -- the maintainer asked for
## this directly ("Please fill columns by header name"), and it is what let v3 work the day it
## arrived: v3 appends "Acquisition date" and "Protocol reference" and moves nothing, so a
## hardcoded letter map would have been right by luck. The next revision may not be so kind.
##
## ACTA_TP_COLS below stays as the FALLBACK for a template whose header row cannot be read, and
## as the canonical spelling of each field name.
.tpColLetter <- function(i) { s <- ""; while (i > 0) { r <- (i - 1L) %% 26L
                                s <- paste0(LETTERS[r + 1L], s); i <- (i - 1L) %/% 26L }; s }

## Named character vector: field name -> column letter, in the template's own order. Comparison
## is on letters and digits only, because a header may carry a trailing space (v3's do).
.tpHeaderCols <- function(template, sheet = 1) {
  h <- tryCatch(names(as.data.frame(suppressMessages(
         readxl::read_excel(template, sheet = sheet, col_types = "text", n_max = 0)))),
       error = function(e) character(0))
  if (!length(h)) return(NULL)
  stats::setNames(vapply(seq_along(h), .tpColLetter, ""), trimws(h))
}

.tpKeyName <- function(x) tolower(gsub("[^a-z0-9]", "", tolower(trimws(as.character(x)))))

ACTA_TP_COLS <- c(
  A  = "Your name",                       B  = "Your ORCID (https://orcid.org/)",
  C  = "Email",                           D  = "Employer/Affiliation",
  E  = "Month of analysis",               F  = "Year of analysis",
  G  = "Collection Software",             H  = "Analysis Software",
  I  = "Plug-ins, other analysis tools",  J  = "References",
  K  = "Quality control measures",        L  = "Antigen",
  M  = "Clone",                           N  = "Conjugate",
  O  = "Target Species",                  P  = "Antibody host species",
  Q  = "Isotype",                         R  = "Clonality",
  S  = "Supplier",                        T  = "Part#",
  U  = "Lot#",                            V  = "Concentration for this Lot#",
  W  = "Other antibodies and fluorophores in tube",
  X  = "Instrument Manufacturer",         Y  = "Instrumentation",
  Z  = "Lasers (nm)",                     AA = "Incubation Time",
  AB = "Temperature (C)",                 AC = "Incubation volume (uL)",
  AD = "Secondary Conjugate",             AE = "Secondary Supplier",
  AF = "Secondary Catalogue #",           AG = "Secondary Concentration used",
  AH = "Gating",                          AI = "Tissue",
  AJ = "Cell type",                       AK = "Sample details",
  AL = "Preparation protocol",            AM = "Cell count",
  AN = "Fresh or Frozen",                 AO = "Live or Fixed",
  AP = "Fixed after antibody incubation?",
  ## NEW IN TEMPLATE v3 (appended; nothing above moved).
  AQ = "Acquisition date",                AR = "Protocol reference")

## DELIBERATELY BLANK. Every one is recoverable by a reader from Supplier + Part#, so asking an
## operator to retype them buys nothing and invents a way to be wrong. Recorded here rather than
## left implicit, because "we forgot" and "we decided" look identical in an empty cell.
## Host species, Isotype and Clonality LEFT THIS LIST 2026-10-23: they are recoverable from the
## master panel inventory by catalogue, so "we decided not to" became "we can".
##
## Tissue LEFT THIS LIST 2026-11-07, and the reason is PBMC. Their Cell type menu has no `PBMC`
## option, so `.tpCellType()` maps it to `mononuclear cells` -- correct, on-menu, and silently
## one half of the material: `mononuclear cells` could be marrow, cord blood, spleen or node,
## while PBMC names peripheral blood. The missing half has a home. Their Tissue menu offers
## `blood`, so the pair (Tissue = blood, Cell type = mononuclear cells) reconstructs PBMC in
## THEIR controlled vocabulary -- machine-readable and facet-searchable, which free text is not.
##
## It is an INFO CELL, not an inference. ACTA does not derive `blood` from "PBMC": the inference
## is safe for PBMC and wrong for a T cell product that could be apheresis-, marrow- or
## tissue-derived, and ACTA has no business guessing a clinical fact. Blank publishes blank.
## EMPTIED IN 3.6.8. `Sample details` is now Info!Sample_Details, free text for anything the other
## columns do not cover; `Concentration for this Lot#` now comes from the inventory's Lot_info
## sheet. Kept as an (empty) constant rather than deleted, because the LAST thing this function
## does is `v[ACTA_TP_OMIT] <- NA_character_` and that line is the one that silently defeated the
## first attempt to map Tissue -- a mapping entry read correctly in the source and published
## nothing. An empty vector keeps the mechanism visible instead of letting it be re-added blind.
ACTA_TP_OMIT <- character(0)

## Vendor shorthand -> the Supplier menu's 11 legal spellings. The layout writes what is on the
## tube; the menu wants the corporate name.
ACTA_TP_VENDOR <- c(
  "BD" = "BD Biosciences", "BDB" = "BD Biosciences", "BD Pharmingen" = "BD Biosciences",
  "BL" = "BioLegend", "Biolegend" = "BioLegend",
  "TF" = "Thermo Fisher/ebiosciences/Invitrogen", "Thermo" = "Thermo Fisher/ebiosciences/Invitrogen",
  "Invitrogen" = "Thermo Fisher/ebiosciences/Invitrogen",
  "eBioscience" = "Thermo Fisher/ebiosciences/Invitrogen",
  "Miltenyi" = "Miltenyi Biotec", "CST" = "Cell Signaling Technology",
  "Sigma" = "Millipore/Sigma", "Millipore" = "Millipore/Sigma",
  "SCBT" = "Santa Cruz/SCBT", "Santa Cruz" = "Santa Cruz/SCBT", "Biorad" = "Bio-Rad")

## Marker shorthand -> the Antigen menu. `Strep` is ours and `Streptavidin` is theirs; both name
## the same bacterial protein, and nothing downstream should care which one the panel typed.
## ONLY SYNONYMS WHOSE TARGET IS ACTUALLY ON THEIR MENU.
##
## `Streptavidin` IS one of their 370 Antigen options and `Strep`/`SA`/`SAV` are not, so this
## normalisation earns its place: it moves a value from the overflow row into row 2, where it is
## facet-searchable, without changing what it means.
##
## `FVD` -> `Viability` and `Live/Dead` -> `Viability` were REMOVED in 3.6.8. Measured: their
## Antigen menu has no viability entry at all -- no `Viability`, no `Live/Dead`, no `Zombie`, no
## `Ghost`. So the rewrite swapped a specific product class for a generic word and STILL landed
## off-menu in row 3. The operator lost "this was a fixable viability dye" and gained nothing.
## Unmapped, the dye name reaches row 3 as written, which is strictly more informative.
ACTA_TP_ANTIGEN <- c("Strep" = "Streptavidin", "SA" = "Streptavidin", "SAV" = "Streptavidin")

## Test material -> the Cell type menu. PBMC is absent from their 22 options; `mononuclear cells`
## is literally what the P-M-C in PBMC stands for, so it is a rename, not an interpretation.
ACTA_TP_CELLTYPE <- c("PBMC" = "mononuclear cells", "HD PBMC" = "mononuclear cells",
                      "HD human PBMC" = "mononuclear cells",
                      "HD Human PBMCs" = "mononuclear cells",
                      "Human PBMC" = "mononuclear cells", "Human PBMCs" = "mononuclear cells",
                      "Human T cells" = "T cells", "HD Human T cells" = "T cells",
                      "HD T cells" = "T cells", "T cells" = "T cells",
                      "Tcells" = "T cells", "WBC" = "white blood cells/leukocytes")

## MATCHED AFTER NORMALISING, so the table does not have to list every spelling a lab uses.
##
## Adding literal aliases one at a time does not converge: `HD PBMC` was in the table, then
## `HD Human PBMCs` was not, then `HD human PBMC` was not -- three spellings of one material,
## two of which published a BLANK cell type before anyone noticed. The `HD ` prefix is house
## style at this site and will keep appearing.
##
## So the lookup strips a leading donor-health prefix, folds case, and tolerates a trailing
## plural. It does NOT do fuzzy matching: an unrecognised material is still unrecognised, and
## now publishes in row 3 rather than vanishing, so a near-miss costs a reader nothing.
.tpCellTypeKey <- function(x) {
  s <- tolower(.tpNorm(x))
  s <- sub("^(hd|healthy donor|healthy)\\s+", "", s)   # donor-health prefix, not a cell type
  s <- sub("s$", "", s)                                 # PBMCs -> PBMC, T cells -> T cell
  trimws(s)
}

.tpCellType <- function(x) {
  if (.tpBlank(x)) return(NA_character_)
  k <- .tpCellTypeKey(x)
  ## Keys first, then the menu values themselves, so a workbook already typed as "T cells"
  ## resolves without needing its own entry.
  j <- match(k, vapply(names(ACTA_TP_CELLTYPE), .tpCellTypeKey, ""))
  if (!is.na(j)) return(unname(ACTA_TP_CELLTYPE[[j]]))
  j <- match(k, vapply(unname(ACTA_TP_CELLTYPE), .tpCellTypeKey, ""))
  if (!is.na(j)) return(unname(ACTA_TP_CELLTYPE[[j]]))
  as.character(x)                                       # unrecognised -> row 3, not blank
}

## ACTA_TP_SAMPLE_STATUS WAS HERE, AND ITS REMOVAL IS THE POINT.
##
## It mapped Layout_Plate!Sample_Status onto the `Fresh or Frozen` menu. All four of that menu's
## options state TWO facts -- thaw state AND whether the cells were cultured before staining --
## so a bare `Frozen` could only be published by ALSO asserting "not cultured", which no operator
## had said. Every run shipped that claim.
##
## 3.6.8 splits the input (Sample_Status + Culture_Status, both Layout_Plate columns, both
## published verbatim in the bundle's instructions workbook and export) and stops composing the
## sentence. See the `Fresh or Frozen` entry below for why nothing replaced it.

## Laser COLOUR NAME -> wavelength. Read from the FCS `LASERnNAME` keywords rather than looked up
## from the instrument model: a Cytek Aurora ships in 3L/4L/5L trims and the model name does not
## say which one is in the room. The file does.
ACTA_TP_LASER_NM <- c("UV" = "355", "Violet" = "405", "Blue" = "488",
                      "YellowGreen" = "561", "Yellow-Green" = "561", "Green" = "532",
                      "Red" = "640", "Infrared" = "785", "NIR" = "785")

## Instrument -> who makes it, and what drives it. Both are facts about the model, so neither is
## worth an operator's keystroke.
ACTA_TP_INSTRUMENT_MFR <- c(
  "Aurora" = "Cytek", "Northern Lights" = "Cytek",
  "FACSAria" = "BD Biosciences", "FACSAria Fusion" = "BD Biosciences",
  "FACSCanto II" = "BD Biosciences", "FACSymphony A5" = "BD Biosciences",
  "FACSDiscover S8" = "BD Biosciences", "LSRFortessa" = "BD Biosciences",
  "LSR-II" = "BD Biosciences", "CytoFLEX" = "Beckman Coulter",
  "CytoFLEX LX" = "Beckman Coulter", "CytoFLEX S" = "Beckman Coulter",
  "Navios" = "Beckman Coulter", "Gallios (405, 488, 638)" = "Beckman Coulter",
  "ZE5" = "Bio-Rad", "EVEREST" = "Bio-Rad", "YETI" = "Bio-Rad", "ID7000" = "Sony", "CyTOF" = "Fluidigm/Standard Biotools",
  "CyTOF 3 Helios" = "Fluidigm/Standard Biotools", "Helios Mass Cytometer" = "Fluidigm/Standard Biotools")
ACTA_TP_COLLECTION_SW <- c(
  "Aurora" = "SpectroFlo", "Northern Lights" = "SpectroFlo",
  "FACSAria" = "Diva", "FACSAria Fusion" = "Diva", "FACSCanto II" = "Diva",
  "FACSymphony A5" = "Diva", "LSRFortessa" = "Diva", "LSR-II" = "Diva",
  "CytoFLEX" = "CytExpert", "CytoFLEX LX" = "CytExpert", "CytoFLEX S" = "CytExpert",
  ## A Bio-Rad ZE5 reports its SOFTWARE in $CYT rather than its model, so "EVEREST" arrives where
  ## an instrument name is expected -- mapped from both. EVEREST is not on TiterPubs' four-option
  ## Collection Software menu, so it lands in row 3; that is still better than leaving the field
  ## blank, because EVEREST really is what collected the data. Raised with the maintainers.
  "ZE5" = "EVEREST", "EVEREST" = "EVEREST", "YETI" = "EVEREST",
  "ID7000" = "ID7000")

## Instrument string -> the MODEL as their Instrumentation menu spells it. Needed because $CYT does
## not always hold a model: a Bio-Rad ZE5 reports "EVEREST", its acquisition software, so the
## cytometer has to be recovered from the software name. Kept SEPARATE from the manufacturer and
## collection-software tables even though all three are keyed the same way -- they answer three
## different questions and an entry that is right for one can be wrong for the others.
##
## Only genuine model renames belong here. A cytometer whose $CYT already IS the model (Aurora,
## CytoFLEX, ID7000) needs no entry: .tpAlias() passes an unrecognised value through unchanged.
##
## BD IS DELIBERATELY ABSENT. A BD/Diva file carries no $CYT and no $CYTSN at all -- verified on
## raw instrument-written files -- so there is no string to map from and fcsEquipment() returns
## NA. The only model hint BD leaves is CYTOMETER CONFIG NAME, and that is free text an
## administrator typed: on real files it names the nozzle, the organisation and the laser trim all
## in one string. Mining it would publish the site's identity into a CC BY 4.0 dataset, so it is
## not a safe fallback. BD support needs an operator-declared value, not a keyword guess.
ACTA_TP_INSTRUMENT_MODEL <- c("EVEREST" = "ZE5", "YETI" = "ZE5", "ZE5" = "ZE5")

## ZERO-WIDTH CHARACTERS ARE REAL AND THEY ARE INVISIBLE. The panel inventory's Antigen cell holds
## "Streptavidin" followed by U+200B -- thirteen characters that look like twelve, pasted in from
## somewhere years ago. An exact menu match fails on it and the cell looks correct in Excel while
## it does, which is the worst way for a lookup to be wrong. Stripped here, once, for everything.
## WRITTEN AS \uXXXX, NOT AS PASTED CHARACTERS, and the reason is narrower than it looks.
##
## These literals used to BE the invisible characters they match, so the one line whose job is to
## remove invisible characters was itself unreadable, undiffable, and silently damaged by any
## editor or transfer that re-encoded the file. The escapes make the source pure ASCII.
##
## WHAT THIS DOES NOT DO -- measured 2026-11-07, recorded so nobody re-derives it. It does NOT
## make normalisation work under a non-UTF-8 locale. With DESCRIPTION's Encoding: UTF-8 both
## spellings behave identically under any UTF-8 locale, which is the only locale ACTA is tested
## in. Under a true LC_ALL=C the PASTED form silently returned "CD3BUV805" for "CD3<NBSP>BUV805"
## -- the NBSP deleted rather than spaced, a wrong answer with no warning -- while the escaped
## form raises "input string is invalid". That trade is deliberate: a reader that cannot read must
## not report success. Making it actually WORK off UTF-8 needs byte-mode matching done properly,
## which is a separate change and is NOT this one.
.tpNorm <- function(x) {
  x <- as.character(x)
  x <- gsub("[\u200B-\u200D\uFEFF\u00AD]", "", x)   # ZWSP, ZWNJ, ZWJ, BOM, soft hyphen
  x <- gsub("\u00A0", " ", x)                        # non-breaking space
  trimws(gsub("[[:space:]]+", " ", x))
}

.tpBlank <- function(x) is.null(x) || !length(x) || all(is.na(x)) || !nzchar(.tpNorm(x)[1])

## THE STAINING PROTOCOL NOW HAS AN INFO FIELD OF ITS OWN: Info!Protocol_reference.
##
## It used to be DERIVED from Info!References by .tpProtocolUrl(), which split that field on ";",
## kept the URLs, dropped any github.com/*/acta one so a run could not publish a link to ACTA as
## though it were the bench protocol, and took the last survivor. That worked, but nothing about
## it was visible to the operator: the value published in TiterPubs' "Protocol reference" column
## depended on how many URLs were in References, what order they were in, and a name-based
## exclusion rule written in the source. An operator reading the workbook could not predict it,
## and a second non-ACTA URL in References silently changed the answer.
##
## Removed on the user's instruction (2026-11-07): one Info cell, published verbatim. If
## Protocol_reference is blank the column is NA, which is honest -- rather than guessing from a
## citation list. Info!References keeps its own column and is published verbatim too; it is a
## citation list, and it is no longer parsed for anything.
##
## CONSEQUENCE, STATED: a workbook authored before this field existed has no Protocol_reference
## row, so its "Protocol reference" column is now NA where it previously carried a derived URL.
## That is the version-locked-template rule working as intended -- ACTA ships no back-compat
## shim -- but it IS a visible change for an archived workbook re-run on this version.

## "BUV395-A" is a DETECTOR; "BUV395" is the dye. Only the trailing -A/-H/-W area/height/width
## suffix goes: a dye name can legitimately end in a letter, so an unanchored strip would eat one.
.tpDye <- function(x) sub("-(A|H|W)$", "", .tpNorm(x))

## "Aurora (SN0000)" is one instrument; "Aurora" is the model the menu knows. The serial sits in
## the parenthetical by ACTA's own convention, so dropping a trailing (...) is exact here.
.tpModel <- function(x) trimws(sub("\\s*\\([^()]*\\)\\s*$", "", .tpNorm(x)))

## Look up through an alias table, case-insensitively, and fall back to the input unchanged -- an
## unknown value is passed through to be judged against the menu, not silently blanked.
## DERIVED fields get a STRICT lookup: no match means blank, not the input echoed back.
##
## .tpAlias() below falls back to its input, which is right when input and output are the same
## kind of thing -- an unrecognised vendor is still a vendor, so passing it through lets the
## reader judge it. It is wrong when the output is a DIFFERENT kind of thing. An unrecognised
## instrument fell through to "Instrument Manufacturer", so a run on a Bio-Rad ZE5 -- whose $CYT
## reads "EVEREST", the acquisition software -- published "EVEREST" as the manufacturer. Blank is
## honest; a software name in a manufacturer field is a false statement about the instrument.
.tpAliasStrict <- function(x, tbl) {
  v <- .tpNorm(x)
  if (.tpBlank(v)) return(NA_character_)
  hit <- match(tolower(v), tolower(names(tbl)))
  if (is.na(hit)) NA_character_ else unname(tbl[[hit]])
}

.tpAlias <- function(x, tbl) {
  v <- .tpNorm(x)
  if (.tpBlank(v)) return(NA_character_)
  hit <- match(tolower(v), tolower(names(tbl)))
  if (is.na(hit)) v else unname(tbl[[hit]])
}

## The controlled vocabularies, read from the submission template itself rather than copied into
## this file. Copying them would freeze a list that its maintainers revise -- the template is
## versioned and dated (v2, 2026-10-02) precisely because the menus move.
actaTiterPubsMenus <- function(template) {
  if (!file.exists(template)) return(list())
  m <- tryCatch(as.data.frame(suppressMessages(
         readxl::read_excel(template, sheet = "menu options", col_types = "text"))),
       error = function(e) NULL)
  if (is.null(m)) return(list())
  out <- lapply(m, function(col) {
    v <- .tpNorm(stats::na.omit(col))
    v[nzchar(v) & v != "choose from menu"]
  })
  stats::setNames(out, .tpNorm(names(m)))
}

## Columns whose `menu options` entries are EXAMPLES, not a vocabulary. References lists two
## sample citations; treating them as the only legal values would push every real citation into
## the overflow row. The giveaway is semantic, not structural -- a two-entry menu looks the same
## as a two-option one -- so the exception is named here rather than inferred.
ACTA_TP_FREETEXT <- c("References")

## Columns that take SEVERAL menu values at once. Lasers (nm) offers 521 individual wavelengths
## and a 5L instrument legitimately answers with five of them. Checked part by part, but only
## after the whole string fails: "ficoll, RBC lysis" is ONE legal option that contains a comma,
## and splitting every value would turn that into two bogus ones.
ACTA_TP_MULTI <- c("Lasers (nm)")

## Is `value` one of the menu's legal options? An empty menu (a free-text column) counts as legal,
## so free text is never pushed into the overflow row.
## THEIR MATCHING RULE, NOT A STRICTER ONE. The maintainers match a submitted value against a
## menu "ignoring case, spaces, hyphens and slashes" and publish the accepted spellings per menu
## in TiterPubs_menus_v4.json. ACTA folded case and nothing else, and the gap was not theoretical:
## a ZE5 writes its detectors as "BV 605-A" and "BV 421-A", so .tpDye() hands this "BV 605" while
## the menu says "BV605" -- OFF MENU, into the overflow row, for a single space. Two of OQ_Test2's
## three reagents went that way, and a reader of the published dataset would have seen two
## perfectly ordinary dyes sitting in the row reserved for things the vocabulary cannot express.
.tpMenuKey <- function(x) gsub("[-/[:space:]]", "", tolower(.tpNorm(x)))

.tpOnMenu <- function(value, field, menus) {
  if (.tpBlank(value)) return(TRUE)
  if (field %in% ACTA_TP_FREETEXT) return(TRUE)
  opts <- menus[[field]]
  if (is.null(opts) || !length(opts)) return(TRUE)
  keys <- .tpMenuKey(opts)
  if (.tpMenuKey(value) %in% keys) return(TRUE)
  if (field %in% ACTA_TP_MULTI) {
    parts <- .tpMenuKey(strsplit(.tpNorm(value), ",")[[1]])
    parts <- parts[nzchar(parts)]
    return(length(parts) > 0L && all(parts %in% keys))
  }
  FALSE
}

## The MENU'S OWN SPELLING for a value that matches it loosely. Publishing "BV 605" where the
## vocabulary says "BV605" is a legal submission under the rule above, but it puts a spelling into
## a public dataset that their menu does not use -- and two datasets meaning the same dye would
## then not group together. Canonicalising costs nothing and is the whole point of a controlled
## vocabulary.
##
## Returns the value UNCHANGED when nothing matches, so a genuinely off-menu value still reaches
## the overflow row exactly as typed. Never invents: no match means no substitution.
.tpCanonical <- function(value, field, menus) {
  if (.tpBlank(value) || field %in% ACTA_TP_FREETEXT) return(value)
  opts <- menus[[field]]
  if (is.null(opts) || !length(opts)) return(value)
  keys <- .tpMenuKey(opts)
  one  <- function(x) { h <- match(.tpMenuKey(x), keys)
                        if (is.na(h)) as.character(x) else as.character(opts[[h]]) }
  if (field %in% ACTA_TP_MULTI) {
    parts <- trimws(strsplit(.tpNorm(value), ",")[[1]])
    parts <- parts[nzchar(parts)]
    if (!length(parts)) return(value)
    return(paste(vapply(parts, one, ""), collapse = ", "))
  }
  one(.tpNorm(value))
}

## Applied to a whole value vector just before the off-menu split is computed, so the two always
## agree: a field cannot be canonicalised into the menu and still be reported as overflow.
.tpCanonicaliseAll <- function(v, menus) {
  for (fld in names(v)) {
    cv <- tryCatch(.tpCanonical(v[[fld]], fld, menus), error = function(e) v[[fld]])
    if (length(cv) == 1L && !is.na(cv)) v[[fld]] <- cv
  }
  v
}

## The gating chain a reagent was measured inside, collapsed to the one phrase TiterPubs asks for.
## Their menu describes the PARENT POPULATION ("live single cells"), which is a different question
## from ACTA's GatingMethod ("gate_auto2_costain") -- the algorithm is not the population. Walk the
## gating_template from the reagent's parent back to root and name what the chain selected.
actaTiterPubsGating <- function(gating, parent = "Live") {
  if (is.null(gating) || !is.data.frame(gating) || !nrow(gating)) return(NA_character_)
  nm <- tolower(trimws(names(gating)))
  ai <- which(nm == "alias"); pi <- which(nm == "parent")
  if (!length(ai) || !length(pi)) return(NA_character_)
  alias <- .tpNorm(gating[[ai[1]]]); par <- .tpNorm(gating[[pi[1]]])
  keep <- !is.na(alias) & nzchar(alias) & !startsWith(alias, "#")
  alias <- alias[keep]; par <- par[keep]

  chain <- character(0); cur <- .tpNorm(parent); guard <- 0L
  while (!.tpBlank(cur) && !identical(tolower(cur), "root") && guard < 20L) {
    chain <- c(chain, cur)
    hit <- match(tolower(cur), tolower(alias))
    if (is.na(hit)) break
    cur <- par[hit]; guard <- guard + 1L
  }
  if (!length(chain)) return(NA_character_)
  low <- tolower(chain)
  live   <- any(grepl("live|viab", low))
  single <- any(grepl("single|singlet", low))
  mark   <- grep("^cd[0-9]+\\+?$", chain, ignore.case = TRUE, value = TRUE)

  if (length(mark) && live && single) return(sprintf("live single %s", sub("\\+?$", "+", mark[1])))
  if (live && single) return("live single cells")
  if (single)         return("single cells")
  "list others below"
}

## Laser lines for the instrument that actually wrote the file.
## openCyto + flowCore, as "name x.y.z", for the Plug-ins column.
##
## MEASURED, NOT DECLARED. Reads the versions actually loaded rather than DESCRIPTION's declared
## minimums, because the point of publishing them is reproducibility and a declared floor is not
## what ran. A package that is somehow absent is simply left out rather than reported at a guessed
## version; all absent returns NA and the column publishes blank.
.tpEngineVersions <- function(pkgs = c("openCyto", "flowCore")) {
  out <- vapply(pkgs, function(p) {
    v <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
    if (is.na(v)) NA_character_ else paste(p, v)
  }, "")
  out <- out[!is.na(out)]
  if (!length(out)) NA_character_ else paste(out, collapse = "; ")
}

## An FCS CREATOR string reduced to the Collection Software menu term it names.
##
## CREATOR is "<software> <version>" -- "SpectroFlo 3.3.0" on every Cytek file measured here --
## and their menu holds bare names. So match on the LEADING menu option and publish that; the
## version is dropped rather than carried into a controlled-vocabulary cell where it would push
## the whole run into the overflow row.
##
## NO MATCH MEANS PASS THROUGH, not blank: an acquisition software their menu does not list is
## still worth publishing, and row 3 is what the template provides for it. Longest option first,
## so a menu that ever gains "SpectroFlo SE" cannot be shadowed by "SpectroFlo".
.tpCollectionSw <- function(x, menus) {
  v <- .tpNorm(x)
  if (.tpBlank(v)) return(NA_character_)
  opts <- menus[["Collection Software"]] %||% character(0)
  if (!length(opts)) return(v)
  opts <- opts[order(nchar(opts), decreasing = TRUE)]
  for (o in opts)
    if (grepl(sprintf("^%s(\\b|$)", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", o)), v,
              ignore.case = TRUE)) return(o)
  v
}

## One FCS keyword from the run's keyword list, normalised, NA when absent or empty.
##
## Matched case-insensitively and with the leading `$` optional, because the standard keywords
## carry one ($CYT) and the vendor extensions do not (CREATOR, SPILL). A caller asking for
## "CREATOR" should not have to know which kind it is.
.tpKeyword <- function(keys, name) {
  if (is.null(keys) || !length(keys) || is.null(names(keys))) return(NA_character_)
  want <- tolower(sub("^\\$", "", name))
  hit  <- match(want, tolower(sub("^\\$", "", names(keys))))
  if (is.na(hit)) return(NA_character_)
  v <- .tpNorm(as.character(keys[[hit]])[1])
  if (.tpBlank(v) || toupper(v) == "NA") NA_character_ else v
}

actaTiterPubsLasers <- function(keys) {
  if (is.null(keys) || !length(keys)) return(NA_character_)
  nm <- names(keys)[grepl("^LASER[0-9]+NAME$", names(keys))]
  if (!length(nm)) return(NA_character_)
  nmv <- vapply(keys[nm], function(x) .tpNorm(as.character(x)[1]), "")
  nmv <- unname(nmv[nzchar(nmv)])
  out <- vapply(nmv, function(v) {
    hit <- match(tolower(v), tolower(names(ACTA_TP_LASER_NM)))
    if (is.na(hit)) NA_character_ else unname(ACTA_TP_LASER_NM[[hit]])
  }, "")
  out <- stats::na.omit(out)
  if (!length(out)) return(NA_character_)
  paste(sort(as.integer(unique(out))), collapse = ", ")
}

## ONE REAGENT -> the 42 values, as a named character vector keyed by template column header.
## `row` is that reagent's row of the titration export; `tp` its row of the workbook's
## TiterPubs_template sheet (defaults when absent, which is the normal case).
## SUPPLIER NAMES ARE MAPPED ONTO THE MENU, not passed through.
##
## An inventory writes "Thermo Fisher Scientific" and "Invitrogen"; the menu offers
## "Thermo Fisher/ebiosciences/Invitrogen". Without this the value is correct, on their list, and
## still lands in the off-menu overflow row -- which looks like a gap in their vocabulary when it
## is really a spelling difference in ours.
## A MENU VALUE THAT DIFFERS ONLY IN PUNCTUATION IS THE SAME VALUE.
##
## Their Conjugate menu spells it "BV421"; a panel sheet writes "BV 421". Without this the value
## is correct, on their list, and still lands in the off-menu overflow row -- which reads as a
## gap in their vocabulary when it is a space. Returns the MENU's spelling so the submission
## matches their controlled list exactly. A genuinely absent dye (RB780, newer than the menu) is
## left alone and goes to row 3, which is what row 3 is for.
.tpMenuSpelling <- function(x, field, menus) {
  v <- .tpNorm(x); if (.tpBlank(v)) return(NA_character_)
  opts <- menus[[field]]
  if (is.null(opts) || !length(opts)) return(v)
  k <- function(z) gsub("[^a-z0-9]", "", tolower(z))
  hit <- opts[k(opts) == k(v)]
  if (length(hit)) hit[1] else v
}

.tpSupplierMenu <- function(x, menus) {
  v <- .tpNorm(x); if (.tpBlank(v)) return(NA_character_)
  opts <- menus[["Supplier"]]
  if (is.null(opts) || !length(opts)) return(v)
  k <- function(z) gsub("[^a-z0-9]", "", tolower(z))
  hit <- opts[k(opts) == k(v)]
  if (length(hit)) return(hit[1])
  ## A menu entry that LISTS several trading names, slash-separated, matches any one of them.
  for (o in opts) for (part in strsplit(o, "/", fixed = TRUE)[[1]])
    if (nzchar(part) && (k(part) == k(v) || grepl(k(part), k(v), fixed = TRUE))) return(o)
  v
}

## HOST SPECIES AND ISOTYPE, parsed out of the inventory's single free-text "Species / Isotype".
##
## That column is written by hand and shows it: "Mouse IgG2a", "MS, IgG1, k", "Mouse BALB/c IgG1,
## kappa", "Ms IgG1, Anti Human", and one entry naming a hybridoma cross before the isotype. Both
## targets are MENU-DRIVEN, so the job is to land on a menu value or produce nothing -- a half-
## parsed isotype is worse than a blank, because blank reads as "not supplied" while "IgG" when
## the truth is "IgG2b" reads as a fact.
##
## Returns NA when it cannot land on the menu. The value is not lost: the raw string stays in the
## bundled inventory sheet, where a reader can see it.
.tpHostSpecies <- function(x, menus) {
  v <- .tpNorm(x); if (.tpBlank(v) || toupper(v) == "NA") return(NA_character_)
  opts <- menus[["Antibody host species"]] %||% character(0)
  ## Longest first, so "Armenian Hamster" wins over "Hamster".
  known <- c("Armenian Hamster" = "Armenian Hamster", "Syrian Hamster" = "Syrian Hamster",
             "Hamster" = "Hamster", "Mouse" = "Mouse", "Ms" = "Mouse", "Rabbit" = "Rabbit",
             "Rat" = "Rat", "Human" = "Human", "Goat" = "Goat", "Sheep" = "Sheep",
             "Chicken" = "Chicken")
  for (nm in names(known)) {
    if (grepl(sprintf("(^|[^A-Za-z])%s([^A-Za-z]|$)", nm), v, ignore.case = TRUE, perl = TRUE)) {
      out <- unname(known[[nm]])
      if (!length(opts) || out %in% opts) return(out) else return(NA_character_)
    }
  }
  NA_character_
}

.tpIsotype <- function(x, menus) {
  v <- .tpNorm(x); if (.tpBlank(v) || toupper(v) == "NA") return(NA_character_)
  m <- regmatches(v, regexpr("Ig[ADEGM][0-9]*[a-z]?", v, perl = TRUE))
  if (!length(m)) return(NA_character_)
  base <- m[1]
  ## Light chain, in any of the spellings the inventory uses. Unicode kappa and lambda are what
  ## the menu is written with.
  lc <- if (grepl("(\u03BA|kappa|[^A-Za-z]k([^A-Za-z]|$))", v, ignore.case = TRUE, perl = TRUE)) "\u03ba"
        else if (grepl("(\u03BB|lambda|[^A-Za-z]l([^A-Za-z]|$))", v, ignore.case = TRUE, perl = TRUE)) "\u03bb"
        else ""
  opts <- menus[["Isotype"]] %||% character(0)
  for (cand in c(paste0(base, lc), base)) if (!length(opts) || cand %in% opts) return(cand)
  NA_character_
}

## One inventory row for the TITRATED reagent, found by CATALOGUE ACROSS THE WHOLE INVENTORY.
##
## NOT restricted to the run's panel iteration: the titrated reagent is by definition the one NOT
## in the costain panel, so looking it up there finds nothing. a two-reagent run titrates CD4 and CD8
## while its panel holds CD3 and Viability. Catalogue is the key that works in both directions.
.tpInvByCatalog <- function(inv, catalog) {
  if (is.null(inv) || !nrow(inv) || .tpBlank(catalog)) return(NULL)
  k <- function(z) gsub("[^a-z0-9]", "", tolower(as.character(z)))
  hit <- inv[k(inv$catalog) == k(catalog), , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  hit[1, , drop = FALSE]
}

actaTiterPubsValues <- function(row, info, tp = NULL, gating = NULL, keys = NULL, menus = NULL,
                                inv = NULL, panel = NULL) {
  iv <- function(k, default = NA_character_) {
    v <- actaInfoValue(info, k); if (is.na(v)) default else v
  }
  rv <- function(k) { if (!k %in% names(row)) return(NA_character_)
                      v <- .tpNorm(row[[k]][1]); if (.tpBlank(v) || v == "NA") NA_character_ else v }
  tv <- function(k, default = NA_character_) {
    if (is.null(tp) || !k %in% names(tp) || !nrow(tp)) return(default)
    v <- .tpNorm(tp[[k]][1]); if (.tpBlank(v)) default else v
  }

  ## The titrated reagent's own inventory row, found by CATALOGUE across the whole inventory --
  ## not within the run's panel iteration, which by definition does not contain it.
  .invRow <- .tpInvByCatalog(inv, rv("Cat_Num"))
  .invIso <- if (is.null(.invRow)) NA_character_ else .invRow$isotype[1]

  instrument <- .tpModel(rv("Equipment"))
  acq <- suppressWarnings(as.Date(rv("Acquisition_date")))

  ## Info!Tissue, falling back to Info!Tissue_Fallback WHEN TISSUE IS EMPTY.
  ##
  ## Deliberately a different rule from Cell_Type_Fallback below, which fires on an UNRECOGNISED
  ## value rather than an absent one. Tissue has no alias table and nothing to fail to recognise:
  ## the only question it can ask is "did the operator say". So the fallback is a site default for
  ## runs where nobody filled the cell -- a lab that only ever titrates on blood sets it once --
  ## and it cannot overwrite an answer the operator actually gave.
  ##
  ## Both blank still publishes blank. ACTA does not infer tissue from the material; see the note
  ## on ACTA_TP_OMIT for why `PBMC` implies blood but `T cells` does not.
  tissue <- rv("Tissue_Type")
  if (.tpBlank(tissue)) tissue <- iv("Tissue_Fallback")

  ## IS THIS A COMBINATORIAL RUN? Decides which column names the antigen, below. Read from the
  ## layout rather than recomputed: actaGroups() already sets a `combinatorial` attribute from the
  ## FCS folder structure, and a second opinion derived differently is the two-readers-of-one-value
  ## shape that has already cost this file a release. Any well carrying a Combinatorial_group is
  ## enough -- the column is blank on every non-combinatorial run measured.
  combinatorial <- !.tpBlank(rv("Combinatorial_group"))
  ## Date_Analysis is written by the pipeline when the analysis completes. An export that predates
  ## the column yields NA here and the Month/Year fields fall back to `acq`, which is what those
  ## fields have always held -- so an old export keeps publishing the value it used to publish
  ## rather than silently going blank.
  ana <- suppressWarnings(as.Date(rv("Date_Analysis")))
  if (is.na(ana)) ana <- acq

  ## Fixation: ACTA records WHETHER, TiterPubs asks WHEN. The two questions are only the same once
  ## you know whether the titrated target is on the surface or inside, which no ACTA column holds
  ## -- hence Target_Location, defaulting to Surface. Across every archived run the titrated marker
  ## has been a surface one (CCR7, CD14, HLA-DR, PD1, Strep), so the default is the observed case
  ## and the override exists for the first intracellular titration rather than for every run.
  fix  <- toupper(rv("Fix")) %in% c("TRUE", "T", "YES", "1")
  perm <- toupper(rv("Perm")) %in% c("TRUE", "T", "YES", "1")
  intracellular <- identical(tolower(tv("Target_Location", "Surface")), "intracellular")
  if (!fix && !perm) {
    liveFixed <- "Live at the time of antibody incubation"
    fixedAfter <- "No, no fix after antibody incubation"
  } else if (perm && intracellular) {
    liveFixed <- "Fixed at the time of antibody incubation"
    fixedAfter <- "No, no fix after antibody incubation"
  } else {
    liveFixed <- "Live at the time of antibody incubation"
    fixedAfter <- "Yes, antibody then fixed"
  }

  cells <- suppressWarnings(as.numeric(rv("e6_cells_per_well")))
  v <- c(
    "Your name"                   = iv("Submitter_Name"),
    "Your ORCID (https://orcid.org/)" = iv("Submitter_ORCID"),
    ## NEVER EMITTED. A bundle is published for anyone to download, so it cannot carry a contact
    ## address -- and the maintainer pointed out that the scrub placeholder is worse than useless
    ## here, because `johndoe@anonymouscompany.com` reads as a real address to anyone who did not
    ## write it. TiterPubs collects the address on the upload form instead, from the submitter who
    ## is standing there. Unconditional: NOT gated on scrub_confidential, because the reason is a
    ## property of publishing, not of this site's confidentiality setting.
    "Email"                       = NA_character_,
    "Employer/Affiliation"        = iv("Organization/Affiliation"),
    ## ANALYSIS, NOT ACQUISITION. These two read off `ana` (the export's Date_Analysis, set when
    ## the analysis completed), with `acq` only as a fallback for an export written before that
    ## column existed. They used to read `acq` outright, which answered "when was this acquired"
    ## to a question that asks "when was this analysed" -- the same month on a fresh run and the
    ## wrong year on any re-analysis of archived data.
    "Month of analysis"           = if (is.na(ana)) NA_character_ else format(ana, "%B"),
    "Year of analysis"            = if (is.na(ana)) NA_character_ else format(ana, "%Y"),
    ## INFO FIRST, THEN THE FILE'S OWN RECORD, THEN BLANK.
    ##
    ## This was derived from the instrument MODEL through a lookup -- two inferences deep
    ## ($CYT -> model -> table -> "SpectroFlo") -- while the FCS carries the answer literally.
    ## Measured across all six validation sets: CREATOR = "SpectroFlo 3.3.0" on every Cytek file,
    ## absent on the Bio-Rad one. Reading it is not ACTA guessing; it is ACTA reporting what the
    ## acquisition software wrote about itself, with the version attached for free.
    ## CREATOR CARRIES A VERSION AND THEIR MENU DOES NOT, so the keyword is reduced to the menu
    ## term it starts with. Measured: their options are bare names -- Diva, SpectroFlo,
    ## CytExpert, ID7000, Everest -- while every Cytek file here reads `SpectroFlo 3.3.0`.
    ## Published verbatim it is off-menu on EVERY Cytek run, which is worse than the model lookup
    ## it replaced. Reduced, it lands in row 2 and stays facet-searchable.
    ##
    ## An operator's own Info value is NOT reduced: they get to say exactly what they ran, and
    ## row 3 is there for a value their menu does not list.
    "Collection Software"         = { cs <- iv("Collection_Software")
                                      if (!.tpBlank(cs)) cs
                                      else .tpCollectionSw(.tpKeyword(keys, "CREATOR"), menus) },
    "Analysis Software"           = "ACTA",
    ## THE GATING ENGINE, WITH VERSIONS. `Analysis Software` says "ACTA"; this says what ACTA
    ## gated with, which is what another lab needs to reproduce a cut. Read from the loaded
    ## namespaces rather than hardcoded, so the string cannot drift from what actually ran.
    "Plug-ins, other analysis tools" = .tpEngineVersions(),
    "References"                  = iv("References"),
    "Quality control measures"    = iv("Quality_Control_Measures"),
    ## ALIAS NAMES THE ANTIGEN, except on a combinatorial run.
    ##
    ## Alias is what the ANALYST called the reagent; Markername is $PnS, which the instrument
    ## wrote. On a Cytek Aurora $PnS holds the marker and either works. On a Bio-Rad ZE5 $PnS
    ## holds the DYE -- measured on OQ_Test2, whose three reagents carry "BV 421-A", "RB780-A"
    ## and "BV 605-A" there while Alias holds CD19, CD7, CD56. Preferring Markername would
    ## publish a fluorochrome in the Antigen field for every ZE5 run.
    ##
    ## A COMBINATORIAL RUN INVERTS THAT. Alias carries the grouping suffix ACTA needs to keep two
    ## series in one folder apart -- OQ_Test3's `CD14_combo`, `CCR7_combo` -- and `CD14_combo` is
    ## off their menu while plain `CD14` is on it. So a combinatorial run reads Markername first
    ## and keeps Alias as the fallback.
    ##
    ## .tpAntigen() still applies the isDye guard either way, so a combinatorial run on a ZE5 --
    ## where the preferred source holds a dye name -- falls through to the other candidate rather
    ## than publishing the dye. The two rules compose; neither leaves a gap.
    "Antigen"                     = if (combinatorial)
                                      .tpAntigen(rv("Markername"), rv("Alias"), menus)
                                    else
                                      .tpAntigen(rv("Alias"), rv("Markername"), menus),
    "Clone"                       = rv("Clone"),
    ## THE CONJUGATE IS THE DYE, AND WHICH COLUMN HOLDS IT DEPENDS ON THE INSTRUMENT.
    ##
    ## A Cytek Aurora writes the MARKER into $PnS, so the dye is in the detector name and
    ## .tpDye(Fl) is right. A ZE5/EVEREST writes the DYE into $PnS and leaves $PnN an opaque
    ## code -- FL05, FL18 -- so the same call publishes "FL18" as the conjugate. Measured on
    ## OQ_Test2, whose three reagents came out as FL18 / FL05 / FL19 when the real conjugates
    ## (BV 421, RB780, BV 605) were sitting in Markername all along.
    ##
    ## So: use the detector unless it is an opaque code, and fall back to Markername when it is.
    ## Detected by SHAPE rather than by instrument name -- ACTA never sees a vendor string here,
    ## and a shape test keeps working for the next cytometer that numbers its detectors.
    "Conjugate"                   = local({
      fl <- .tpDye(rv("Fl"))
      ## NARROW ON PURPOSE: only "FL" + digits, which is the ZE5/EVEREST detector code. A wider
      ## prefix list looked tempting and was wrong -- V450, B515 and R670 are REAL DYE NAMES on
      ## BD instruments, so treating them as opaque would fall back to Markername and publish a
      ## marker ("CD4") as the conjugate. Better to miss a future instrument than to corrupt a
      ## field that is right today; widen when one actually turns up.
      opaque <- !.tpBlank(fl) && grepl("^FL[0-9]+$", trimws(fl), ignore.case = TRUE)
      out <- if (.tpBlank(fl) || opaque) {
        alt <- .tpDye(rv("Markername"))
        if (!.tpBlank(alt)) alt else fl
      } else fl
      .tpMenuSpelling(out, "Conjugate", menus)
    }),
    ## NO DEFAULT. This was iv("Target_Species", "Human"), so a blank cell published `Human` as
    ## though the operator had stated it. A reader can work the target species out from the cell
    ## type and the catalogue number; ACTA asserting it is the same class of invention as deriving
    ## `blood` from `PBMC`, and is not ACTA's call to make.
    "Target Species"              = iv("Target_Species"),
    ## FROM THE MASTER PANEL INVENTORY, by catalogue -- no second source and nothing retyped.
    ## The inventory's "Species / Isotype" is one free-text field; both targets here are menu
    ## driven, so each parser lands on a menu value or returns NA. A half-parsed isotype would
    ## read as a fact.
    "Antibody host species"       = .tpHostSpecies(.invIso, menus),
    "Isotype"                     = .tpIsotype(.invIso, menus),
    ## A NAMED CLONE IS A MONOCLONAL. Stated as an inference rather than left blank, because the
    ## clone is already published in the next column and the two would otherwise disagree by
    ## omission. No clone -> no claim.
    ## FROM THE INVENTORY, AS WRITTEN. Was `a named Clone means monoclonal` -- an inference ACTA
    ## had no standing to make, and wrong for a polyclonal antiserum carrying a catalogue
    ## designation in the Clone field. Now a reagent-level inventory column; absent or blank
    ## publishes blank, because .piReadInventory() fills a missing column with NA rather than
    ## failing and the operator's silence is not a claim.
    "Clonality"                   = if (is.null(.invRow)) NA_character_
                                    else .tpNorm(.invRow$clonality[1]),
    "Supplier"                    = .tpSupplierMenu(.tpAlias(rv("Vendor"), ACTA_TP_VENDOR), menus),
    "Part#"                       = rv("Cat_Num"),
    "Lot#"                        = rv("Lot_Num"),
    ## FROM THE INVENTORY'S Lot_info SHEET, keyed by catalogue AND lot. Per-lot by nature: the
    ## Master sheet carries one row per reagent and a lab buys the same catalogue many times, so
    ## a concentration taken from the wrong lot would be a confident wrong number. No sheet, no
    ## matching row, or a blank cell all publish blank.
    "Concentration for this Lot#" = actaLotConcentration(attr(inv, "acta_lot_info"),
                                                         rv("Cat_Num"), rv("Lot_Num")),
    ## THE PANEL'S BASE ROWS, which is what the maintainer asked for -- the other reagents that
    ## were physically in the tube. Combinatorial_group was the old source and describes something
    ## else entirely (the analyst's own grouping), so it was blank on every run that did not use
    ## that feature. Falls back to it when no panel resolved.
    "Other antibodies and fluorophores in tube" =
      if (!is.null(panel) && nrow(panel)) {
        b <- panel[!is.na(panel$Type) & tolower(panel$Type) == "base", , drop = FALSE]
        if (nrow(b)) paste(sprintf("%s %s", b$Antigen, b$Fluorochrome), collapse = "; ")
        else rv("Combinatorial_group")
      } else rv("Combinatorial_group"),
    ## INFO FIRST, THEN DERIVED FROM THE MODEL -- and derived is the only option when Info is
    ## blank, because NO FCS KEYWORD CARRIES A MANUFACTURER. $CYT holds the model ("Aurora") or,
    ## on a Bio-Rad, the acquisition software ("EVEREST"); "Cytek" appears nowhere in the file.
    ## Stated here rather than left implicit: this one field cannot be as-written unless the
    ## operator writes it.
    "Instrument Manufacturer"     = { mk <- iv("Equipment_Make")
                                      if (!.tpBlank(mk)) mk
                                      else .tpAliasStrict(instrument, ACTA_TP_INSTRUMENT_MFR) },
    ## THE MODEL, resolved through an alias table. A Bio-Rad ZE5 reports its SOFTWARE in $CYT, so
    ## `instrument` is the string "EVEREST" and this field published a software name where the
    ## menu wants a cytometer. `ZE5` is on their Instrumentation menu and `EVEREST` is not, so the
    ## old value also went to the off-menu overflow row on every ZE5 run. Falls back to the
    ## instrument string unchanged when no alias applies, which is right: an unrecognised
    ## cytometer is still a cytometer, and the reader can judge it.
    ## Info!Equipment_Model first; otherwise `instrument`, which fcsEquipment() already resolved
    ## from $CYT (and which maps EVEREST/YETI -> ZE5, the model Bio-Rad's software stands in for).
    "Instrumentation"             = { md <- iv("Equipment_Model")
                                      if (!.tpBlank(md)) md
                                      else .tpAlias(instrument, ACTA_TP_INSTRUMENT_MODEL) },
    "Lasers (nm)"                 = actaTiterPubsLasers(keys),
    ## stored as a bare number, emitted the way the menu spells it
    "Incubation Time"             = { t <- iv("Incubation_Time")
                                      if (is.na(t)) NA_character_
                                      else if (grepl("^[0-9]+$", t)) paste(t, "minutes") else t },
    "Temperature (C)"             = iv("Incubation_Temperature_C"),
    "Incubation volume (uL)"      = rv("Well_volume_uL"),
    "Secondary Conjugate"         = tv("Secondary_Conjugate"),
    "Secondary Supplier"          = tv("Secondary_Supplier"),
    "Secondary Catalogue #"       = tv("Secondary_Cat_Num"),
    "Secondary Concentration used" = tv("Secondary_Concentration"),
    "Gating"                      = actaTiterPubsGating(gating, tv("Gating_Parent", "Live")),
    ## ITS OWN INFO CELL, with Info!Tissue_Fallback behind it -- resolved above. Menu-driven, so
    ## .tpCanonicaliseAll() folds `Blood` to the menu's own `blood` before the off-menu split is
    ## computed; a tissue their menu does not list reaches row 3 rather than being discarded. See
    ## the note on ACTA_TP_OMIT above for why PBMC needs this column and why ACTA will not infer it.
    "Tissue"                      = tissue,
    "Cell type"                   = .tpCellType(rv("Cell_Type")),
    ## FREE TEXT, RUN-LEVEL, PUBLISHED AS WRITTEN. The column their template provides for anything
    ## its other 43 do not cover. Deliberately NOT fed from Cell_Type or Culture_Status: a field
    ## that silently mirrors another one stops being a place to say something new.
    "Sample details"              = iv("Sample_Details"),
    ## PUBLISHED EVEN WHEN OFF MENU, in row 3, for the reason given at the Cell type block below.
    ##
    ## This was "menu value or nothing" after every run pushed the literal words "See references"
    ## into the overflow row -- which was the right call for THAT value, and the URL has had a
    ## column of its own since. But the rule it produced blanks every other off-menu answer too,
    ## and an isolation method a site actually used is worth more to a reader in row 3 than it is
    ## discarded. Row 3 is what the template provides for exactly this.
    "Preparation protocol"        = iv("Preparation_Protocol"),
    ## NEW IN TEMPLATE v3.
    "Acquisition date"            = rv("Acquisition_date"),
    ## ITS OWN INFO CELL, PUBLISHED VERBATIM -- no longer derived from References. See the note
    ## on the removal of .tpProtocolUrl() above for what changed and why.
    "Protocol reference"          = iv("Protocol_reference"),
    "Cell count"                  = if (is.na(cells)) NA_character_
                                    else format(cells * 1e6, scientific = FALSE, trim = TRUE),
    ## PER WELL FIRST, run-level second. Sample_Status is a Layout_Plate column, so a plate
    ## that titrated a frozen vial beside a fresh draw publishes the right answer for each reagent
    ## instead of one Info value for both. Info!Fresh_or_Frozen stays as the fallback and is what
    ## an export written before the column existed still resolves to, so no archived run changes.
    ## DELIBERATELY UNMAPPED SINCE 3.6.8, and this is the honest answer rather than a gap.
    ##
    ## All four of their options state TWO facts -- thaw state AND whether the cells were cultured
    ## before staining. There is no culture-neutral option. ACTA used to map a bare `Frozen` to
    ## "Frozen, thawed and incubated with antibody", which asserts NO CULTURE STEP: a claim the
    ## operator never made, published on every run.
    ##
    ## Sample_Status and Culture_Status are separate Layout_Plate columns and travel in the
    ## bundle's own instructions workbook and export, where a reader (and TiterPubs) can see both
    ## as written. Composing one menu sentence out of them would mean parsing free text to decide
    ## a fact about someone else's experiment, which is exactly what this release stopped doing.
    "Fresh or Frozen"             = NA_character_,
    "Live or Fixed"               = liveFixed,
    "Fixed after antibody incubation?" = fixedAfter)

  ## AN UNRECOGNISED Cell type IS PUBLISHED, in row 3, like every other off-menu value.
  ##
  ## This used to replace it with Info!Cell_Type_Fallback, on the reasoning that routing
  ## Cell_Type through the alias table "keeps those identifiers out of a public submission".
  ## IT DOES NOT, and that is measured: the same string is published verbatim TWICE MORE in the
  ## same zip -- the export's sharedStrings.xml and the bundled Layout_Plate. Checked on a v3.6.5
  ## OQ_Test1 bundle carrying "HD Human T cells". So the replacement removed a useful field and
  ## prevented no disclosure; a reader lost the cell type while the string sat two parts away.
  ##
  ## Confidentiality for this field belongs to acta.cell_type_rules -- site-configured,
  ## shipped EMPTY, applied before any of this, and never in the public tree. ACTA cannot dictate
  ## how another lab names its materials, and silently blanking their metadata is the wrong
  ## default for a tool other people run.
  ##
  ## The alias table STAYS, as a normaliser: a value it recognises still lands in row 2 as the
  ## proper menu term, which is what makes a submission queryable. Everything else goes to row 3,
  ## the template's own escape hatch -- "if the menu in row 2 does not contain a relevant option,
  ## add text to row 3" -- which this mapper already uses for Clone and Supplier.
  ##
  ## Info!Cell_Type_Fallback is now an OPT-IN OVERRIDE rather than a silent default: set it and it
  ## wins, leave it blank and the operator's own words are published.
  ## Normalised through the SAME key function the resolver uses, so "is this on the menu" cannot
  ## answer differently here than it did there -- the two-readers-of-one-value shape that has
  ## already cost this file one release.
  ## BLANK-TRIGGERED SINCE 3.6.8, matching Tissue_Fallback. It used to fire on an UNRECOGNISED
  ## value, which published false facts: with the shipped fallback set, `Whole blood` and
  ## `Jurkat cell line` both came out as `mononuclear cells`. Measured, both wrong -- whole blood
  ## includes granulocytes and red cells, and a Jurkat line is neither. It also did NOT fire on a
  ## blank cell, the one case a site default is actually for.
  ##
  ## So: it fills a gap, it never overrules an answer. An unrecognised material now reaches row 3
  ## as written, which is unsearchable but true -- better than searchable and false.
  ctFallback <- iv("Cell_Type_Fallback", NA_character_)
  if (!.tpBlank(ctFallback) && .tpBlank(v[["Cell type"]]))
    v[["Cell type"]] <- .tpCellType(ctFallback)

  v[ACTA_TP_OMIT] <- NA_character_
  v
}

## ONE ARCHIVE WRITER, because there are two things to zip and only one correct way to do it.
##
## zip::zip() rather than utils::zip(): utils::zip shells out to an external `zip` executable,
## which a stock Windows does not have. That is the same shape of fault as the report render's
## missing pandoc -- it works everywhere it was developed and fails on the machines the users
## actually have. `zip` is a declared Import (and already present, since openxlsx depends on it),
## so the external-binary path is gone rather than merely preferred against.
.tpZip <- function(zipfile, files = NULL, root) {
  if (file.exists(zipfile)) unlink(zipfile)
  ## MIRROR, NOT CHERRY-PICK. cherry-pick stores each entry at the ARCHIVE ROOT, stripping its
  ## directory -- so an .xlsx repacked that way had "xl/workbook.xml" written as "workbook.xml"
  ## and every [Content_Types] Override pointed at a part that was no longer there. Excel offers
  ## to repair such a file; the OPC gate simply failed it, which is how this was found.
  ## mirror keeps each path relative to `root`, which is what both callers need.
  if (is.null(files))
    files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  zip::zip(zipfile = zipfile, files = files, root = root, mode = "mirror")
  invisible(zipfile)
}

## WHICH FIELD HOLDS THE ANTIGEN DEPENDS ON THE INSTRUMENT, so it is decided on evidence.
##
## A Cytek Aurora writes the marker into $PnS -- "HLA-DR", "CCR7", "PD1" -- and ACTA carries that
## through as Markername. A Bio-Rad ZE5 does not: EVEREST names its detectors by DYE, so $PnS
## reads "BV 421-A" and $PnN reads "FL18-A". Confirmed across the archived runs, both instruments,
## so this is how the ZE5 writes files rather than anything mis-entered in a layout. On a ZE5 the
## marker identity exists only in the layout's Alias.
##
## Rather than branch on the instrument -- which would need a list of every cytometer that does
## this -- the two candidates are tested against TiterPubs' own vocabularies: a value that is a
## known FLUOROCHROME cannot be the antigen, and a value on the Antigen menu almost certainly is.
## Markername is tried first because it is the more canonical source when it is usable at all.
.tpAntigen <- function(markername, alias, menus = NULL) {
  cand <- unique(stats::na.omit(c(markername, alias)))
  cand <- cand[nzchar(cand)]
  if (!length(cand)) return(NA_character_)
  ## unname(): vapply over a CHARACTER vector uses the input values as names, so without this
  ## the function returns c("CD19" = "CD19") and the caller's c("Antigen" = .tpAntigen(...))
  ## produces an element called "Antigen.CD19". Every later v[["Antigen"]] then fails with
  ## "subscript out of bounds" -- which is how this surfaced, as a bundle that would not build.
  cand <- unname(vapply(cand, function(z) .tpAlias(z, ACTA_TP_ANTIGEN), ""))
  sq <- function(z) gsub("[^a-z0-9]", "", tolower(z))
  dyes <- if (!is.null(menus) && length(menus[["Conjugate"]])) sq(menus[["Conjugate"]]) else character(0)
  ags  <- if (!is.null(menus) && length(menus[["Antigen"]]))   sq(menus[["Antigen"]])   else character(0)
  ## A detector suffix is stripped before the dye test: "BV 421-A" is the dye BV421.
  isDye <- unname(vapply(cand, function(z) sq(sub("-(A|H|W)$", "", z)) %in% dyes, TRUE))
  keep <- cand[!isDye]
  if (!length(keep)) keep <- cand          # everything looked like a dye; report what we have
  onMenu <- keep[sq(keep) %in% ags]
  if (length(onMenu)) return(unname(onMenu[1]))
  unname(keep[1])
}

## XML escaping for a cell's text. The five predefined entities only -- the writer emits inline
## strings, so nothing here ever reaches sharedStrings.xml.
.tpEsc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&apos;", x, fixed = TRUE)
}

## Replace rows 2 and 3 of the `to publish` sheet, AT THE XML LEVEL, and copy every other part of
## the package through byte for byte.
##
## WHY NOT openxlsx: loadWorkbook + saveWorkbook rewrites the entire archive and mangles shared
## strings carrying xml:space="preserve" -- that is how 25 cells in ACTA's own template were
## corrupted in sheets nobody had touched. This workbook is somebody else's artefact, 1.2 MB of
## it, with 3008 styled rows and a 737-row vocabulary sheet. Rewriting all of it to set 42 cells
## would be a poor trade even if openxlsx were lossless.
##
## Values go in as INLINE strings (t="inlineStr"), so xl/sharedStrings.xml is never opened. The
## styles of the cells being replaced are preserved where the original had them.
## A whole worksheet from a character matrix, as inline strings. Shared by the template filler
## and the Panel iteration sheet so there is one place that knows how to spell a cell.
.tpSheetXml <- function(m) {
  colL <- function(n) { s <- ""; while (n > 0) { r <- (n - 1) %% 26
                          s <- paste0(LETTERS[r + 1], s); n <- (n - 1) %/% 26 }; s }
  rows <- vapply(seq_len(nrow(m)), function(i) {
    cs <- character(0)
    for (j in seq_len(ncol(m))) {
      v <- m[i, j]
      if (is.na(v) || !nzchar(v)) next
      cs <- c(cs, sprintf('<c r="%s%d" t="inlineStr"><is><t>%s</t></is></c>',
                          colL(j), i, .tpEsc(v)))
    }
    sprintf('<row r="%d" spans="1:%d">%s</row>', i, ncol(m), paste(cs, collapse = ""))
  }, "")
  paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
         '<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">',
         sprintf('<dimension ref="A1:%s%d"/>', colL(ncol(m)), nrow(m)),
         '<sheetViews><sheetView workbookViewId="0"/></sheetViews>',
         '<sheetFormatPr defaultRowHeight="15"/><sheetData>',
         paste(rows, collapse = ""), '</sheetData>',
         '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>',
         '</worksheet>')
}

## The costain panel, as a sheet TiterPubs can ingest.
##
## Their template has no home for "what else was in the tube" beyond one free-text cell, so the
## panel arrives as its own sheet: iteration name, then one row per constituent reagent. The
## maintainers have been asked to take this as a standard sheet plus a
## `costain_panel_iteration (ACTA only)` column on the main sheet; until they do, a sheet they do
## not read is harmless and a sheet they do read is immediately useful.
.tpPanelMatrix <- function(res, iteration) {
  hdr <- c("Panel_Iteration", "Antigen", "Fluorochrome", "Matched_to", "Verdict",
           "Catalog", "Vendor", "Clone", "Type")
  if (is.null(res) || is.null(res$table) || !nrow(res$table))
    return(rbind(hdr, c(iteration, rep("", length(hdr) - 1L))))
  d <- res$table
  body <- cbind(iteration, d$Antigen, d$Fluorochrome, d$Matched_to, d$Verdict,
                d$Catalog, d$Vendor, d$Clone, d$Type)
  body[is.na(body)] <- ""
  rbind(hdr, body)
}

actaTiterPubsWrite <- function(template, out, values, overflow = character(0),
                               panel = NULL, iteration = NA_character_) {
  stopifnot(file.exists(template))
  tmp <- file.path(tempdir(), sprintf("actatp_%s", basename(tempfile(""))))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  names_in <- utils::unzip(template, list = TRUE)$Name
  ## A SharePoint-synced library injects [trash]/NNNN.dat into any xlsx sitting in it, including
  ## the shipped blank template this reads. Carrying them through would hand every submitter a
  ## workbook Excel offers to repair, for a defect introduced by somebody else's sync client.
  names_in <- names_in[!startsWith(names_in, "[trash]/")]
  utils::unzip(template, exdir = tmp, setTimes = TRUE)

  wbf <- file.path(tmp, "xl/workbook.xml")
  wb  <- paste(readLines(wbf, warn = FALSE), collapse = "\n")
  order <- regmatches(wb, gregexpr('<sheet [^>]*>', wb))[[1]]
  nms <- vapply(order, function(s) sub('.*\\sname="([^"]*)".*', "\\1", s), "")
  idx <- match("to publish", nms)
  if (is.na(idx)) stop("TiterPubs template has no 'to publish' sheet", call. = FALSE)
  spath <- file.path(tmp, sprintf("xl/worksheets/sheet%d.xml", idx))
  if (!file.exists(spath)) stop("TiterPubs template: missing ", basename(spath), call. = FALSE)
  x <- paste(readLines(spath, warn = FALSE), collapse = "\n")

  ## Resolved from the template in hand; falls back to the built-in letters if its header row
  ## cannot be read, so a damaged template degrades instead of writing nothing.
  .tpFields <- .tpHeaderCols(template)
  if (is.null(.tpFields)) .tpFields <- stats::setNames(names(ACTA_TP_COLS), unname(ACTA_TP_COLS))
  ## `values` is keyed by the canonical field names; a template header that differs only by
  ## punctuation or a trailing space still finds its value.
  .tpCanon <- stats::setNames(as.list(unname(ACTA_TP_COLS)), .tpKeyName(unname(ACTA_TP_COLS)))

  ## Keep each cell's existing style index so the filled row still looks like the template.
  styleOf <- function(rowxml, ref) {
    m <- regmatches(rowxml, regexpr(sprintf('<c r="%s"[^>]*>', ref), rowxml))
    if (!length(m) || !grepl('s="', m)) return(NULL)
    sub('.*\\ss="([0-9]+)".*', "\\1", m)
  }
  buildRow <- function(rowno, vals) {
    old <- regmatches(x, regexpr(sprintf('<row[^>]*r="%d"[^>]*>.*?</row>', rowno), x))
    attrs <- if (length(old)) sub('^(<row[^>]*>).*', "\\1", old) else
             sprintf('<row r="%d" spans="1:43">', rowno)
    cells <- character(0)
    ## Template order, so the cells still ascend by column -- the OOXML rule that makes Excel
    ## offer to repair a file when broken.
    for (field in names(.tpFields)) {
      col <- .tpFields[[field]]
      v <- vals[[.tpCanon[[.tpKeyName(field)]] %||% field]]
      if (.tpBlank(v)) next
      ref <- paste0(col, rowno)
      st  <- if (length(old)) styleOf(old, ref) else NULL
      cells <- c(cells, sprintf('<c r="%s"%s t="inlineStr"><is><t>%s</t></is></c>',
                                ref, if (is.null(st)) "" else sprintf(' s="%s"', st),
                                .tpEsc(.tpNorm(v))))
    }
    paste0(attrs, paste(cells, collapse = ""), "</row>")
  }

  ## row 2 = everything the menus accept; row 3 = everything they do not, which is where the
  ## template itself says to put it.
  onRow2 <- values; onRow2[overflow] <- NA_character_
  onRow3 <- stats::setNames(rep(NA_character_, length(values)), names(values))
  if (length(overflow)) onRow3[overflow] <- values[overflow]

  for (rn in c(2L, 3L)) {
    new <- buildRow(rn, if (rn == 2L) onRow2 else onRow3)
    pat <- sprintf('<row[^>]*r="%d"[^>]*>.*?</row>', rn)
    ## BACKSLASHES DOUBLED, because `new` is a REPLACEMENT and carries operator text.
    ##
    ## sub() reads `\\` and `\1`-`\9` in a replacement as escape syntax, so a cell containing a
    ## backslash was silently rewritten before it was ever written out. Measured end to end:
    ## Cell_Type "C:\lab\donor \1 PBMC" published as "C:labdonor  PBMC" -- no warning, the
    ## package well formed, actaOpcFindings() clean, the workbook opening fine with the wrong text.
    ## A backslash is ordinary lab notation (CD4\CD8, a pasted Windows path), and this row is now
    ## also where an unmapped Cell_Type lands, so free text reaches it by design.
    ## Same fix and same reason as .tpRenameToken()'s note further down this file.
    newEsc <- gsub("\\\\", "\\\\\\\\", new)
    if (grepl(pat, x)) x <- sub(pat, newEsc, x) else
      x <- sub("</sheetData>", paste0(newEsc, "</sheetData>"), x)
  }
  writeLines(x, spath, useBytes = TRUE)

  ## ---- the Panel iteration sheet, added only when there is a panel to describe --------------
  if (!is.null(panel) && !.tpBlank(iteration)) {
    used <- as.integer(sub(".*sheet([0-9]+)\\.xml", "\\1",
              grep("^xl/worksheets/sheet[0-9]+\\.xml$", names_in, value = TRUE)))
    n <- if (length(used)) max(used) + 1L else 1L
    newfile <- sprintf("worksheets/sheet%d.xml", n)
    writeLines(.tpSheetXml(.tpPanelMatrix(panel, iteration)),
               file.path(tmp, "xl", newfile), useBytes = TRUE)
    names_in <- c(names_in, paste0("xl/", newfile))

    relf <- file.path(tmp, "xl/_rels/workbook.xml.rels")
    rels <- paste(readLines(relf, warn = FALSE), collapse = "\n")
    ids <- as.integer(sub('.*?([0-9]+).*', "\\1",
             regmatches(rels, gregexpr('Id="rId[0-9]+"', rels))[[1]]))
    newid <- sprintf("rId%d", if (length(ids)) max(ids) + 1L else 1L)
    rels <- sub("</Relationships>", sprintf(
      '<Relationship Id="%s" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="%s"/></Relationships>',
      newid, newfile), rels)
    writeLines(rels, relf, useBytes = TRUE)

    sids <- as.integer(vapply(order, function(s) sub('.*sheetId="([0-9]+)".*', "\\1", s), ""))
    wb2 <- sub("</sheets>", sprintf('<sheet name="Panel iteration" sheetId="%d" r:id="%s"/></sheets>',
                                    max(sids, na.rm = TRUE) + 1L, newid), wb)
    writeLines(wb2, wbf, useBytes = TRUE)

    ctf <- file.path(tmp, "[Content_Types].xml")
    ct <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
    ct <- sub("</Types>", sprintf(
      '<Override PartName="/xl/%s" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/></Types>',
      newfile), ct)
    writeLines(ct, ctf, useBytes = TRUE)
  }

  ctf <- file.path(tmp, "[Content_Types].xml")
  if (file.exists(ctf)) {
    ct <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
    for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
      prt <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
      if (!identical(prt, el) && !(prt %in% names_in)) ct <- sub(el, "", ct, fixed = TRUE)
    }
    writeLines(ct, ctf, useBytes = TRUE)
  }
  .tpZip(file.path(tmp, "filled.xlsx"), names_in, tmp)
  if (!file.copy(file.path(tmp, "filled.xlsx"), out, overwrite = TRUE))
    stop("the filled TiterPubs template could not be written to ", out, call. = FALSE)
  ## The blank template is read from the package's Template folder, which on a synced machine has
  ## been rewritten in place -- the filled copy was carrying nine customXml parts holding the
  ## library's SharePoint column GUIDs straight into a public submission. Stripped through
  ## .tpCleanWorkbook() rather than by filtering the part list here: dropping parts without also
  ## repairing the relationships and content types that name them just trades one malformed
  ## package for another, which is exactly what the first attempt did (4 dangling references).
  ## CHECKED, NOT DISCARDED. The bundle scan is blind to a human display name by design, so
  ## .tpCleanWorkbook() is the ONLY control that keeps docProps/core.xml out of a CC BY 4.0
  ## archive. It warns and returns FALSE on error, and its file.copy() can fail silently --
  ## so an unchecked call means the leak reopens with nothing downstream able to see it.
  if (!isTRUE(.tpCleanWorkbook(out)))
    stop(sprintf("TiterPubs: %s could not be de-identified, so it was not bundled.",
                 basename(out)), call. = FALSE)

  ## A package that Excel offers to repair is not a submission. Checked here, not at upload.
  bad <- actaOpcFindings(out)
  if (length(bad))
    warning(sprintf("%s: filled template is not a well-formed package -- %s", basename(out),
                    paste(utils::head(bad, 3L), collapse = "; ")), call. = FALSE)
  invisible(out)
}

## Copy Titration_FCS into the bundle, blanking the instrument serial on the way.
##
## SCRUBBING THE EXPORT IS NOT ENOUGH. stats$Equipment loses its serial before the export and the
## report are written, but the FCS files carry it independently in the $CYTSN keyword -- so a
## bundle with a scrubbed PDF and unscrubbed FCS publishes the serial anyway, in the one place
## nobody would think to look. Measured, not assumed: the first bundle built this way had
## "Aurora" in the export and SN0000 in all twelve FCS.
##
## The rewrite is LOSSLESS. write.FCS re-encodes the file, so it was verified first: 970 events,
## 13 parameters, max absolute difference 0 across the event matrix, and all 163 keywords
## preserved except the one being blanked. The Dirname folder structure is preserved because ACTA
## reads Titration_FCS/<Dirname>/ and the instructions workbook names those folders.
##
## NOTHING ELSE IS TOUCHED. $OP, $PROJ, $DATE and the tube names stay exactly as acquired -- they
## are often identifying, and deciding what of that may be published is the submitting site's
## call under its own data-sharing rules, not a judgement ACTA can make for somebody else.
## `rename` is the scrub map. A FILENAME IS A CARRIER THE CELL SUBSTITUTION CANNOT REACH: the
## operator names the files, the bundle ships them under those names, and the zip entry name is
## published whether or not anyone opens the file. Measured on a scrubbed bundle with the toggle
## ON: 12 entry names carried the pre-scrub ELN id while every cell value was correctly
## placeholdered, and the leak scan reported clean because it has no pattern for a site token.
##
## Returns the count, with the old -> new relative paths attached as `renamed`. The caller feeds
## those back into the workbook scrub, because the export's `fcs_file` cells POINT AT these files
## -- MiFlowCyt section 4.1 is explicitly "the path relative to Titration_FCS/, which is exactly
## what the bundle contains". Renaming the file without rewriting the pointer would trade a leak
## for a dangling reference, which is the same trade the hyperlink fix had to avoid.
.tpStageFcs <- function(src, dest, scrub_serial = TRUE, keywords = NULL, rename = list()) {
  if (!dir.exists(src)) return(invisible(0L))
  files <- list.files(src, pattern = "[.]fcs$", recursive = TRUE, full.names = FALSE,
                      ignore.case = TRUE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  n <- 0L
  renamed <- list()
  for (rel in files) {
    ## '/' AND ':' OUT OF THE SHIPPED NAMES. The maintainer's upload form rejects them, and ':'
    ## is a path separator on classic macOS and illegal on Windows -- an instrument that writes
    ## "A1 Sample 12:30.fcs" produces a bundle that cannot be unpacked everywhere it is sent.
    ## The DIRECTORY structure is kept (Titration_FCS/<Dirname>/), only the leaf is cleaned.
    rel <- file.path(dirname(rel), gsub("[:/]", "_", basename(rel)))
    rel <- sub("^[.]/", "", rel)
    ## THE SOURCE PATH AND THE BUNDLED PATH PART COMPANY HERE. Everything below still READS from
    ## `rel` under src -- the operator's own files are never touched -- while the bundle gets
    ## `relOut`. The whole relative path is rewritten, not just the leaf: the group DIRECTORY is
    ## named from Dirname, which can itself be a panel name, and "/" is a token boundary so the
    ## components are handled by the same single rule.
    relOut <- .tpRenameToken(rel, rename)
    if (!identical(relOut, rel)) renamed[[rel]] <- relOut
    out <- file.path(dest, relOut)
    dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
    done <- FALSE
    if (isTRUE(scrub_serial)) {
      done <- tryCatch({
        ff <- flowCore::read.FCS(file.path(src, rel), truncate_max_range = FALSE,
                                 transformation = FALSE)
        kk <- flowCore::keyword(ff)
        touched <- FALSE
        if ("$CYTSN" %in% names(kk)) { flowCore::keyword(ff)[["$CYTSN"]] <- ""; touched <- TRUE }
        ## scrub_confidential adds the operator and the programme, which live in the file
        ## independently of the layout and would otherwise survive every other scrub.
        for (kw in names(keywords))
          if (kw %in% names(kk)) { flowCore::keyword(ff)[[kw]] <- keywords[[kw]]; touched <- TRUE }
        ## NEVER WRITE A KEYWORD THE ORIGINAL DID NOT HAVE.
        ##
        ## read.FCS() SYNTHESISES `FILENAME` holding the full path it was read from, and
        ## write.FCS() then PERSISTS it -- so every rewritten file in the bundle carried the
        ## local absolute path: the account name and the OneDrive library, i.e. an email address
        ## and the company. Found by the TiterPubs maintainer in the bundle we sent him.
        ##
        ## An earlier check in this project looked at the ORIGINAL files, found no FILENAME in
        ## them, and concluded there was no leak. That was the wrong artefact: the keyword does
        ## not exist on disk beforehand, it is created in memory by the read and only becomes
        ## real when we write. Scrubbing has to be verified on the OUTPUT.
        ##
        ## Diffed against the file's own header rather than hardcoding "FILENAME", so a future
        ## flowCore that synthesises something else is covered too. "$"-prefixed keywords are the
        ## FCS standard set and write.FCS maintains them itself, so they are left alone.
        orig  <- tryCatch(names(flowCore::read.FCSheader(file.path(src, rel))[[1]]),
                          error = function(e) character(0))
        added <- setdiff(names(flowCore::keyword(ff)), orig)
        for (a in added[!startsWith(added, "$")]) flowCore::keyword(ff)[[a]] <- NULL
        if (touched) { suppressWarnings(flowCore::write.FCS(ff, out)); TRUE } else FALSE
      }, error = function(e) {
        ## A file that cannot be re-written is COPIED, and said out loud. Dropping it would make
        ## the bundle unreproducible; copying it silently would hide that the serial survived.
        warning(sprintf("TiterPubs: could not rewrite '%s' to remove the instrument serial (%s); it is bundled unchanged.",
                        rel, conditionMessage(e)), call. = FALSE)
        FALSE
      })
    }
    if (!done) file.copy(file.path(src, rel), out, overwrite = TRUE)
    n <- n + 1L
  }
  invisible(structure(n, renamed = renamed))
}

## The run's own panel inventory, written into the bundle.
##
## Without it a bundle is not re-runnable in the way it claims to be. ACTA resolves
## Costain_Panel_Iteration against a master panel inventory, and a reader has neither ours nor any
## reason to. Worse, with scrub_confidential on the bundled workbook says "Iteration1", which no
## inventory on earth contains -- so a re-run reported the panel as unresolvable and the costain
## reagents, catalogue and vendor included, simply vanished from the reproduction.
##
## So the bundle carries a one-file inventory holding exactly the panels this run used, keyed by
## whatever name the workbook ships (scrubbed or not). actaPanelInventoryPath() prefers an
## inventory in the run folder, so a re-run finds this one with no configuration.
##
## It is written with ACTA's own column names -- the same sheet shape the shipped example uses --
## so it is also a working starting point for a reader's own inventory.
.tpWriteInventory <- function(panels, path) {
  if (is.null(panels) || !length(panels)) return(invisible(NULL))
  cols <- c("Antigen", "Fluorochrome", "Fluorochrome_Alias", "Marker_Alias", "Vendor", "Catalog",
            "Supplier Cat", "Clone", "Species / Isotype", "Size", "Flow Cytometry Assay", "Type",
            "Easy Panel name", "Easy Panel ID", "Subset Panel?", "Panel Iteration",
            "Storage Temperature")
  rows <- list()
  for (nm in names(panels)) {
    tb <- panels[[nm]]$table
    if (is.null(tb) || !nrow(tb)) next
    for (i in seq_len(nrow(tb))) {
      r <- stats::setNames(rep("", length(cols)), cols)
      r[["Antigen"]]         <- tb$Antigen[i]      %||% ""
      r[["Fluorochrome"]]    <- tb$Fluorochrome[i] %||% ""
      ## THE ALIAS THAT MADE IT MATCH, not the antigen.
      ##
      ## This wrote tb$Antigen into Marker_Alias, which silently broke the one thing the file
      ## exists for: a reader who re-runs the bundle resolves the same reagents. A reagent whose
      ## antigen differs from its marker name in the FCS matches on Marker_Alias, so overwriting
      ## that column loses the match. MEASURED on OQ_Test1: the viability dye's antigen is
      ## "Live Dead eFluor 780 (LD) (Viability)" while its FCS marker is "Viability", so the
      ## original run reported 4 of 4 reagents matched and the same run re-resolved against the
      ## inventory this function had written reported 3 of 4 -- correct numbers either way, but a
      ## reproduction artefact that cannot reproduce.
      ##
      ## Matched_to IS that alias: actaPanelIterationTable() puts the marker each row matched
      ## there. NA when the row did not match, and then the antigen is the honest fallback --
      ## writing a blank would lose the only name the reagent has.
      .al <- if ("Matched_to" %in% names(tb)) tb$Matched_to[i] else NA_character_
      r[["Marker_Alias"]]    <- if (!is.na(.al) && nzchar(trimws(as.character(.al))))
                                  as.character(.al) else (tb$Antigen[i] %||% "")
      r[["Vendor"]]          <- tb$Vendor[i]       %||% ""
      r[["Catalog"]]         <- tb$Catalog[i]      %||% ""
      r[["Clone"]]           <- tb$Clone[i]        %||% ""
      r[["Type"]]            <- tb$Type[i]         %||% ""
      r[["Panel Iteration"]] <- nm
      rows[[length(rows) + 1L]] <- ifelse(is.na(r), "", r)
    }
  }
  if (!length(rows)) return(invisible(NULL))
  m <- rbind(cols, do.call(rbind, rows))

  tmp <- file.path(tempdir(), sprintf("tpinv_%s", basename(tempfile(""))))
  parts <- c("[Content_Types].xml", "_rels/.rels", "xl/workbook.xml",
             "xl/_rels/workbook.xml.rels", "docProps/core.xml", "xl/worksheets/sheet1.xml")
  body <- list(
    '[Content_Types].xml' = paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
      '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
      '<Default Extension="xml" ContentType="application/xml"/>',
      '<Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>',
      '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>',
      '<Override PartName="/docProps/core.xml" ContentType="application/vnd.openxmlformats-package.core-properties+xml"/>',
      '</Types>'),
    '_rels/.rels' = paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>',
      '<Relationship Id="rId2" Type="http://schemas.openxmlformats.org/package/2006/relationships/metadata/core-properties" Target="docProps/core.xml"/>',
      '</Relationships>'),
    'xl/workbook.xml' = paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" ',
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">',
      '<sheets><sheet name="Master" sheetId="1" r:id="rId1"/></sheets></workbook>'),
    'xl/_rels/workbook.xml.rels' = paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
      '<Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>',
      '</Relationships>'),
    ## A blank core.xml on purpose: a workbook with none gets one invented by a sync client, and
    ## the one it invents names whoever it was synced by.
    'docProps/core.xml' = paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
      '<cp:coreProperties xmlns:cp="http://schemas.openxmlformats.org/package/2006/metadata/core-properties" ',
      'xmlns:dc="http://purl.org/dc/elements/1.1/">',
      '<dc:title>Panel inventory for this ACTA run</dc:title><dc:creator></dc:creator>',
      '<cp:lastModifiedBy></cp:lastModifiedBy></cp:coreProperties>'),
    'xl/worksheets/sheet1.xml' = .tpSheetXml(m))
  for (nm in parts) {
    f <- file.path(tmp, nm); dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines(body[[nm]], f, useBytes = TRUE)
  }
  .tpZip(file.path(tmp, "inv.xlsx"), parts, tmp)
  ok <- file.copy(file.path(tmp, "inv.xlsx"), path, overwrite = TRUE)
  unlink(tmp, recursive = TRUE)
  invisible(if (isTRUE(ok)) path else NULL)
}

## ONE RUN -> ONE .zip: everything needed to re-run the analysis, not a summary of it.
##
## The earlier design shipped a bundle per reagent holding a handful of figures. This ships the
## run: the FCS files as they were read, the instructions workbook that drove them, the whole
## Plots folder, the report and the titration export. A reader can therefore REPRODUCE the
## numbers rather than take them on trust, which is the thing a titration repository is short of.
## It is also far less machinery than curating per-reagent file lists -- the run already laid these
## out correctly on disk, so the bundle copies them rather than re-deriving them.
##
## THE DASHBOARD IS EXCLUDED. It is a cross-run artefact: it summarises every titration the lab
## has ever done, so it cannot go into a submission about one of them.
.tpRunFiles <- function(outputs_dir, run_dir, comp = NA_character_) {
  out <- list()
  ## The FCS, with the Dirname folders intact -- ACTA reads Titration_FCS/<Dirname>/*.fcs and the
  ## instructions workbook names those folders, so flattening them would break the very
  ## reproduction this bundle exists to enable.
  fcs <- file.path(run_dir, "Titration_FCS")
  if (dir.exists(fcs)) out$Titration_FCS <- fcs
  wb <- list.files(run_dir, pattern = "Titration_Instructions.*[.]xlsx$", full.names = TRUE)
  wb <- wb[!grepl("^([~][$]|[.]_)", basename(wb))]
  if (length(wb)) out$instructions <- wb[1]
  plots <- file.path(outputs_dir, "Plots")
  if (dir.exists(plots)) out$Plots <- plots
  rp <- list.files(outputs_dir, pattern = "^ACTA_Report.*[.]pdf$", full.names = TRUE)
  if (length(rp)) out$report <- rp[1]
  ex <- list.files(outputs_dir, pattern = "TitrationExport.*[.]xlsx$", full.names = TRUE)
  if (length(ex)) out$export <- ex[1]
  ## THE VERSION STAMPS TRAVEL WITH THE BUNDLE. The maintainer asked for them by name: without
  ## them a downloaded zip cannot say which ACTA produced it, and "the bundle currently says
  ## 3.5.0" is a question about the export filename rather than a recorded fact. Both are written
  ## to the run folder already; they were simply never packed.
  for (vf in c("acta_version.tsv", "package_versions.tsv")) {
    p <- file.path(outputs_dir, vf)
    if (file.exists(p)) out[[sub("[.]tsv$", "", vf)]] <- p
  }
  ## THE COMPENSATION MATRIX, when the run actually used one. Without it a downloaded bundle
  ## cannot reproduce the numbers it publishes: the instructions workbook says compensation='csv'
  ## and the matrix it names is nowhere in the zip. The maintainer asked for exactly this -- "if a
  ## matrix was used, please include it, so the bundle re-runs to the same numbers".
  ##
  ## Gated on the RESOLVED mode, not on the file being present. A stray *CompMatrix*.csv left in a
  ## run folder that compensated 'self' or not at all is not part of that run, and shipping it
  ## would invite a reader to apply a matrix the numbers were never computed with. Exactly one
  ## candidate, too: >1 is already fatal to a run, so anything else here means the folder changed
  ## under us and silence is better than a guess.
  if (identical(comp, "csv")) {
    cm <- actaCompCsvCandidates(run_dir)
    if (length(cm) == 1L) out$comp_matrix <- cm[1]
  }
  out
}

## ONE RUN -> one .zip, ready to upload at https://metrdy.com/titerpubs.
##
## One filled TEMPLATE PER REAGENT still, inside that single archive: the template's data row is
## row 2 and only row 2 -- their About sheet says "Fill in row 2 ... (row 3 for anything the menus
## do not cover)" -- so a template holds exactly one titration no matter how the files are packed.
actaTiterPubsBundle <- function(stats, info, outputs_dir, template, run_dir = outputs_dir,
                                tp = NULL, gating = NULL, keys = NULL, eln = NULL,
                                panels = NULL) {
  scrub <- actaScrubConfidential(info)
  ## The inventory the run resolved, re-read here so the mapper can look up the TITRATED reagent
  ## by catalogue. Best effort: no inventory simply means those columns stay blank.
  inv <- tryCatch(actaPanelInventory(actaPanelInventoryPath(info, version_dir = run_dir)),
                  error = function(e) NULL)
  ## This reagent's resolved costain panel, for "Other antibodies and fluorophores in tube".
  .panelFor <- function(panels, row) {
    if (is.null(panels) || !length(panels)) return(NULL)
    it <- if ("Costain_Panel_Iteration" %in% names(row)) trimws(as.character(row[["Costain_Panel_Iteration"]][1])) else NA
    if (is.na(it) || !nzchar(it) || !it %in% names(panels)) return(NULL)
    panels[[it]]$table
  }
  if (is.null(stats) || !nrow(stats)) {
    warning("titerpubs_export is TRUE but the titration export is empty; no bundle written.",
            call. = FALSE)
    return(invisible(NULL))
  }
  if (!file.exists(template)) {
    warning(sprintf("titerpubs_export is TRUE but the TiterPubs template is missing (%s); no bundle written.",
                    template), call. = FALSE)
    return(invisible(NULL))
  }
  ## CATALOGUE AND VENDOR ARE REQUIRED, and the run stops without them.
  ##
  ## Everything else in a submission describes how a titration was done; these two say WHICH
  ## REAGENT it was. A public record of "the optimal volume is 2 uL" is worthless -- and
  ## potentially misleading -- if a reader cannot tell which catalogue item it refers to. So this
  ## is the one place the export refuses rather than degrades.
  ##
  ## Checked for EVERY reagent before any file is written, and reported in one message: fixing a
  ## layout one error per re-run would mean six runs for six reagents.
  if (all(c("Cat_Num", "Vendor") %in% names(stats))) {
    .lbl <- if ("Alias" %in% names(stats)) .tpNorm(stats$Alias) else as.character(seq_len(nrow(stats)))
    .bad <- vapply(seq_len(nrow(stats)), function(i) {
      m <- c(if (.tpBlank(stats$Cat_Num[i])) "Cat_Num",
             if (.tpBlank(stats$Vendor[i]))  "Vendor")
      if (length(m)) sprintf("%s (missing %s)", .lbl[i], paste(m, collapse = " and ")) else ""
    }, "")
    .bad <- .bad[nzchar(.bad)]
    if (length(.bad))
      stop(sprintf(paste0("titerpubs_export is TRUE but %d reagent(s) have no catalogue number ",
                          "or vendor: %s.\n  A TiterPubs submission must identify the reagent it ",
                          "describes, so these are required. Fill Cat_Num and Vendor in ",
                          "Layout_Plate and re-run, or set Info!titerpubs_export to FALSE."),
                   length(.bad), paste(.bad, collapse = "; ")), call. = FALSE)
  }

  menus <- actaTiterPubsMenus(template)
  if (is.null(eln)) eln <- {
    e <- if ("ELN_ID" %in% names(stats)) .tpNorm(stats$ELN_ID[1]) else NA_character_
    if (.tpBlank(e)) "ACTA_run" else e
  }

  stage <- file.path(tempdir(), sprintf("tpb_%s", basename(tempfile(""))))
  dir.create(stage, recursive = TRUE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  ## One template per reagent, named Catalog_Vendor_Lot so a template can be matched to the
  ## reagent it describes without opening it.
  made <- character(0); offmenu <- list()
  for (i in seq_len(nrow(stats))) {
    row <- stats[i, , drop = FALSE]
    alias <- if ("Alias" %in% names(row)) .tpNorm(row$Alias[1]) else NA_character_
    if (.tpBlank(alias)) alias <- sprintf("reagent%d", i)

    cat_ <- if ("Cat_Num" %in% names(row)) .tpNorm(row$Cat_Num[1]) else NA_character_
    ven_ <- if ("Vendor"  %in% names(row)) .tpNorm(row$Vendor[1])  else NA_character_
    lot_ <- if ("Lot_Num" %in% names(row)) .tpNorm(row$Lot_Num[1]) else NA_character_
    ## Catalogue and vendor are guaranteed present by the check above; only Lot can be missing,
    ## and a missing lot is a gap in the record rather than a reason to refuse -- the reagent is
    ## still identified. Named Catalog_Vendor in that case.
    parts <- c(cat_, ven_, lot_)
    stem <- if (.tpBlank(lot_)) {
      warning(sprintf("TiterPubs: %s has no Lot_Num; its template is named %s_%s.",
                      alias, cat_, ven_), call. = FALSE)
      paste(c(cat_, ven_), collapse = "_")
    } else paste(parts, collapse = "_")
    safe <- gsub("[^A-Za-z0-9._-]", "_", stem)

    tpi <- NULL
    if (!is.null(tp) && nrow(tp) && "Alias" %in% names(tp)) {
      h <- which(tolower(.tpNorm(tp$Alias)) == tolower(alias))
      if (length(h)) tpi <- tp[h[1], , drop = FALSE]
    }
    v <- actaTiterPubsValues(row, info, tpi, gating, keys, menus, inv = inv,
                             panel = .panelFor(panels, row))
    ## Canonicalise FIRST, then split: see .tpCanonicaliseAll(). Doing it the other way round
    ## would report a field as overflow and then write the menu spelling into row 2.
    v <- .tpCanonicaliseAll(v, menus)
    over <- names(v)[!vapply(names(v), function(f) .tpOnMenu(v[[f]], f, menus), TRUE)]
    if (length(over)) offmenu[[alias]] <- stats::setNames(unname(v[over]), over)

    iter <- if ("Costain_Panel_Iteration" %in% names(row)) .tpNorm(row$Costain_Panel_Iteration[1])
            else NA_character_
    pan <- if (!is.null(panels) && !.tpBlank(iter) && iter %in% names(panels)) panels[[iter]] else NULL

    actaTiterPubsWrite(template, file.path(stage, sprintf("TiterPubs_%s.xlsx", safe)), v, over,
                       panel = pan, iteration = iter)
    made <- c(made, safe)
  }

  ## The run itself.
  ## The mode as the RUN resolved it, so the matrix ships exactly when it was applied.
  files <- .tpRunFiles(outputs_dir, run_dir, comp = actaCompMode(info, quiet = TRUE))

  ## THE FCS GO FIRST, and the order is load-bearing rather than tidy. Staging is what produces
  ## the old -> new path map, and the export scrub below CONSUMES it to rewrite the `fcs_file`
  ## cells that point at those files. Left to .tpRunFiles()' own ordering this worked only by
  ## luck, and the failure would have been silent: an export pointing at names no longer in the
  ## bundle, which no structural check looks for.
  nms <- names(files)
  nms <- c(intersect("Titration_FCS", nms), setdiff(nms, "Titration_FCS"))

  ## Only when the operator asked for a scrub. Renaming a submitter's data files when they did
  ## NOT ask to be anonymised would be a surprise, and a bad one -- the names are how they find
  ## their own wells again.
  ## `eln` was resolved above and is the SAME value the run published everywhere else -- the export,
  ## the report, the zip name. Passing it here is what keeps the FCS rename agreeing with them.
  fcsMap <- if (scrub && !is.null(files[["instructions"]]) &&
                file.exists(files[["instructions"]]))
              .tpScrubMap(files[["instructions"]], eln = eln)
            else list()
  fcsExtra <- list()

  for (nm in nms) {
    src <- files[[nm]]
    if (!file.exists(src)) next
    if (identical(nm, "Titration_FCS")) {
      nf <- .tpStageFcs(src, file.path(stage, "Titration_FCS"), scrub_serial = TRUE,
                        keywords = if (scrub) ACTA_SCRUB_FCS_KEYWORDS else NULL,
                        rename = fcsMap)
      ## BOTH FORMS THE WORKBOOKS USE. The export's `fcs_file` column holds the path relative to
      ## Titration_FCS/ ("<Dirname>/<file>.fcs"); MiFlowCyt 4.1 holds the same path WITH the
      ## "Titration_FCS/" prefix. A whole-cell substitution has to be given the exact cell value,
      ## so both spellings are registered or the one that was missed keeps pointing at a file
      ## that is no longer there under that name.
      for (k in names(attr(nf, "renamed"))) {
        v <- attr(nf, "renamed")[[k]]
        fcsExtra[[k]] <- v
        fcsExtra[[file.path("Titration_FCS", k)]] <- file.path("Titration_FCS", v)
      }
      message(sprintf("  %d FCS file(s) staged, serial removed%s%s", nf,
                      if (scrub) " and confidential keywords replaced" else "",
                      if (length(attr(nf, "renamed")))
                        sprintf("; %d renamed", length(attr(nf, "renamed"))) else ""))
    } else if (dir.exists(src)) {
      file.copy(src, stage, recursive = TRUE)
    } else {
      dst <- file.path(stage, basename(src))
      file.copy(src, dst, overwrite = TRUE)
      ## THE WORKBOOK CARRIES THE SAME IDENTIFIERS AS THE EXPORT, and it is copied byte for byte,
      ## so scrubbing stats alone would leave Operator and ELN_ID sitting in Layout_Plate inside
      ## the same zip -- the identical mistake the FCS $CYTSN already taught once.
      ## Sync residue goes for every bundle; the confidential scrub only when asked.
      ## THE EXPORT TOO, not just the instructions. The export carries VERBATIM AUDIT COPIES of
      ## Info, gating_template and Layout_Plate -- re-read from the workbook on disk, which is the
      ## whole point of an audit copy -- so the in-memory scrub never reaches them. Measured: the
      ## bundled instructions said johndoe@anonymouscompany.com while the export two files away
      ## still said the operator's address. Same identifiers, same zip, one of them scrubbed.
      if (nm %in% c("instructions", "export")) {
        ## Checked for the reason given at the other call site: this is the only control.
        if (!isTRUE(.tpCleanWorkbook(dst)))
          stop(sprintf("TiterPubs: %s could not be de-identified, so it was not bundled.",
                       basename(dst)), call. = FALSE)
        ## CHECKED, like its sibling above. Round 3 made .tpCleanWorkbook() stop on FALSE
        ## "because this is the only control" and left these two as bare calls -- yet both have
        ## an error handler that warns and returns FALSE with the note "it is bundled
        ## unchanged", which is precisely the outcome that must not be silent. .tpApplyRepl()
        ## now also returns FALSE when a key matched no cell or a value survived the rewrite.
        if (scrub && !isTRUE(.tpScrubWorkbook(dst, extra_repl = fcsExtra, eln = eln)))
          stop(sprintf("TiterPubs: %s could not be scrubbed, so it was not bundled.",
                       basename(dst)), call. = FALSE)
        ## ALWAYS, and AFTER the scrub. The scrub rewrites a real address to the placeholder, so
        ## running this first would blank the real one and then let the placeholder back in --
        ## and the placeholder is the value the maintainer specifically objected to.
        if (!isTRUE(.tpStripEmail(dst)))
          stop(sprintf("TiterPubs: the submitter address could not be removed from %s.",
                       basename(dst)), call. = FALSE)
      }
    }
  }

  ## The panel inventory this run resolved, so the bundle can resolve it again.
  if (!is.null(panels) && length(panels)) {
    iv <- .tpWriteInventory(panels, file.path(stage, "Master_Panel_Inventory.xlsx"))
    if (!is.null(iv)) message("  panel inventory written for re-runs (Master_Panel_Inventory.xlsx)")
  }

  ## A BUNDLE WITHOUT THE REPORT IS NOT A SUBMISSION -- HARD STOP.
  ##
  ## The maintainer's dataset page displays this PDF. When the render failed, ACTA built the
  ## bundle anyway, listed its contents without "report", and announced it "ready to upload":
  ## an incomplete artefact declared complete, which is the same failure shape as the FCS
  ## FILENAME leak and is only ever caught by someone reading the contents line.
  ##
  ## Stopping is right rather than warning: the operator asked for a submission bundle, and the
  ## honest answer to "the report did not render" is that there is nothing to submit yet.
  if (!"report" %in% names(files))
    stop(paste0("TiterPubs bundle NOT built: the run produced no report PDF, and the submission ",
                "page displays it. Fix the report render and re-run -- run_acta() reports the ",
                "cause in $report_error. To analyse without a bundle, set ",
                "Info!titerpubs_export = FALSE."), call. = FALSE)

  ## DATE AND TIME PREFIX, because the ELN alone does not identify a bundle.
  ##
  ## With scrub_confidential on, EVERY run's ELN becomes the same placeholder -- so two scrubbed
  ## bundles kept side by side in a submissions folder collide on name and the second silently
  ## replaces the first. The stamp also records when a bundle was cut, which is the question
  ## anyone asks of a file they are about to upload.
  ##
  ## %y%m%d_%H%M%S to match the stamp the diagnostics bundle already uses; seconds included so
  ## two runs of the same dataset in one minute stay distinct.
  zipf <- file.path(outputs_dir, sprintf("%s_%s_ACTA_TiterPubs.zip",
                                         format(Sys.time(), "%y%m%d_%H%M%S"), eln))
  ok <- tryCatch({ .tpZip(zipf, root = stage); TRUE },
                 error = function(e) { warning(sprintf("TiterPubs bundle could not be zipped: %s",
                                                       conditionMessage(e)), call. = FALSE); FALSE })
  if (!ok || !file.exists(zipf)) return(invisible(NULL))
  ## THE LEAK SCAN IS A GATE, not a report: it reads the finished zip the way a recipient would
  ## and deletes it if anything blocking is inside. Placed before the success message so a failed
  ## bundle is never announced as ready.
  ## SEEDED WITH THE VALUES THE SCRUB WAS ASKED TO REMOVE.
  ##
  ## The built-in pattern set is OPERATOR AND MACHINE identity -- home paths, account and machine
  ## names, the OneDrive library. It has nothing for a SITE token: an ELN id, a programme code, a
  ## panel name. Those were trusted entirely to substitution, and substitution reaches cell values
  ## only -- so a bundle whose filenames carried the ELN id scanned clean with the toggle on.
  ## Checking for exactly what we claimed to replace closes the loop: the rename above handles the
  ## common case, and anything it could not safely reach is now REPORTED instead of shipped.
  ##
  ## WORD-BOUNDED, AND THAT IS NOT OPTIONAL. These go in as regex (extra_regex is matched with
  ## perl = TRUE; `extra` would be fixed substrings). OQ_Test1's Operator is "OP", and "OP" as a
  ## substring matches OPERATOR, SOP and a great deal of binary noise -- every bundle would be
  ## refused, which is the fail-always shape three earlier fixes in this file already took. \Q..\E
  ## quotes the value so a panel name containing "(Gen2)" or "5+5" cannot act as syntax.
  ##
  ## The PLACEHOLDERS are excluded: EXP26000001 and "JD" are what we substituted IN, so matching
  ## them would refuse every correctly-scrubbed bundle.
  ## FIXED SUBSTRINGS, not the rename's boundary rule -- see the note on .tpSiteTokens() for why
  ## the detector and the rewriter must differ, and what it cost to discover that.
  siteTok <- character(0)
  if (scrub && length(fcsMap)) {
    st <- .tpSiteTokens(fcsMap)
    if (length(st$use))
      siteTok <- stats::setNames(st$use,
                                 sprintf("a confidential value the scrub replaced (%s)", st$use))
    if (length(st$short))
      message(sprintf(paste0("  bundle scan: %s too short to search for reliably, so %s replaced ",
                             "but not re-checked."),
                      paste(sQuote(st$short), collapse = ", "),
                      if (length(st$short) == 1L) "it was" else "they were"))
  }
  actaAssertBundleClean(zipf, extra = siteTok)

  message(sprintf("ACTA: TiterPubs bundle -- %s (%.1f MB, %d template(s): %s)",
                  basename(zipf), file.size(zipf) / 1e6, length(made),
                  paste(made, collapse = ", ")))
  message(sprintf("  contents: %s", paste(names(files), collapse = ", ")))
  for (a in names(offmenu))
    message(sprintf("  %s: %d value(s) in row 3, not on the menu: %s", a, length(offmenu[[a]]),
                    paste(sprintf("%s=%s", names(offmenu[[a]]), offmenu[[a]]), collapse = "; ")))
  message("  ready to upload at https://metrdy.com/titerpubs")
  invisible(zipf)
}


## The panel-iteration table as a PNG, for the submission bundle only.
##
## The report renders this table in LaTeX and the bundle cannot carry the report -- the PDF holds
## identifiers a submission has no business publishing. So the same table is drawn again as a
## figure, from the SAME data and with the SAME caption (actaPanelIterationCaption), because two
## renderings of one table that describe themselves differently is how a reader ends up trusting
## the wrong one.
##
## Drawn with geom_text on a bare coordinate grid rather than a table grob: gridExtra is not a
## dependency and adding one for a figure that ships five times a year is a poor trade.
actaPanelIterationPlot <- function(res, path, width = 10, height = NULL) {
  if (is.null(res) || is.null(res$table) || !nrow(res$table)) return(invisible(NULL))
  d <- res$table
  d[] <- lapply(d, function(x) ifelse(is.na(x), "—", as.character(x)))
  hdr <- c("Antigen", "Fluorochrome", "Matched to", "Verdict",
           "Catalogue", "Vendor", "Clone", "Type")
  nr <- nrow(d); nc <- ncol(d)
  cells <- expand.grid(row = seq_len(nr), col = seq_len(nc))
  cells$lab <- vapply(seq_len(nrow(cells)), function(i) d[cells$row[i], cells$col[i]], "")
  ## y counts DOWN from the header, so row 1 sits directly under it and the table reads in the
  ## same order as the data frame rather than upside down.
  cells$y <- nr - cells$row + 1L
  heads <- data.frame(col = seq_len(nc), lab = hdr, y = nr + 1L, stringsAsFactors = FALSE)
  ## "no match" is the row a reader is looking for, so it is the only thing given colour.
  bad <- which(d$Verdict == "no match")
  stripes <- data.frame(y = nr - bad + 1L)
  if (is.null(height)) height <- max(2.2, 0.32 * (nr + 3))

  p <- ggplot2::ggplot() +
    { if (nrow(stripes))
        ggplot2::geom_rect(data = stripes,
          ggplot2::aes(xmin = 0.4, xmax = nc + 0.6, ymin = y - 0.45, ymax = y + 0.45),
          fill = "#FBE9E7", colour = NA) } +
    ggplot2::geom_hline(yintercept = nr + 0.5, linewidth = 0.4) +
    ggplot2::geom_text(data = heads, ggplot2::aes(x = col, y = y, label = lab),
                       fontface = "bold", size = 3.1, hjust = 0.5) +
    ggplot2::geom_text(data = cells, ggplot2::aes(x = col, y = y, label = lab),
                       size = 2.9, hjust = 0.5) +
    ggplot2::scale_x_continuous(limits = c(0.4, nc + 0.6), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0.4, nr + 1.6), expand = c(0, 0)) +
    ggplot2::labs(title = "Costain panel composition",
                  caption = paste(strwrap(actaPanelIterationCaption(res), 120), collapse = "\n")) +
    ggplot2::theme_void(base_size = 10) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 10,
                                                      margin = ggplot2::margin(b = 6)),
                   plot.caption = ggplot2::element_text(hjust = 0, size = 8,
                                                        margin = ggplot2::margin(t = 8)),
                   plot.margin = ggplot2::margin(10, 10, 10, 10))
  ggplot2::ggsave(path, p, width = width, height = height, dpi = 200, units = "in")
  invisible(path)
}

## WHERE THE BLANK TEMPLATE LIVES. Shipped beside the instructions template so a submission does
## not depend on a folder that happens to be sitting next to the working copy. Versioned in its
## filename on purpose -- TiterPubs date their releases (v2 is 2026-10-02) and revise the menus
## between them, so a run records which template it filled rather than silently tracking the
## newest one. The search order is the same two-layout question the pipeline loader asks: a
## sibling Template/ folder when running from a checkout, the installed package otherwise.
actaTiterPubsTemplate <- function(code_dir = NULL) {
  pat <- "^TiterPubs_template_v[0-9]+\\.xlsx$"
  cand <- character(0)
  if (!is.null(code_dir) && nzchar(code_dir))
    cand <- c(cand, file.path(code_dir, "Template"), file.path(dirname(code_dir), "Template"))
  inst <- tryCatch(system.file("pipeline", "Template", package = "ACTA"), error = function(e) "")
  if (nzchar(inst)) cand <- c(cand, inst)
  for (d in cand) {
    if (!dir.exists(d)) next
    f <- list.files(d, pattern = pat, full.names = TRUE)
    ## HIGHEST VERSION BY NUMBER, not by string. order() on the filename puts v10 BELOW v2, so a
    ## tenth revision would silently be ignored in favour of the second -- and the symptom would
    ## be columns quietly left blank, which is exactly how v3 was missed: the package shipped only
    ## v2, the two fields v3 adds had nowhere to go, and the filled template looked merely
    ## incomplete rather than wrong.
    if (length(f)) {
      v <- suppressWarnings(as.integer(sub(".*_v([0-9]+)\\.xlsx$", "\\1", basename(f))))
      v[is.na(v)] <- -1L
      return(f[order(v, decreasing = TRUE)][1])
    }
  }
  NA_character_
}

## Strip sync-client residue from a workbook on its way into the bundle.
##
## A workbook that has sat in a SharePoint-synced library picks up parts nobody authored:
## customXml/item*.xml holding the library's own column GUIDs, docProps/custom.xml, and
## [trash]/NNNN.dat. The bundle copies the instructions workbook straight out of the run folder,
## so all of it rode along -- 27 parts where the committed workbook has 16 -- and the customXml
## items declare namespace URIs that are bare GUIDs rather than absolute URIs, which is the shape
## of thing that makes Excel offer to repair a file on open.
##
## None of it is content. Removing it leaves the bundled workbook structurally identical to the
## committed one, which is the version known to open cleanly. Done for EVERY bundle, not only
## scrubbed ones: it is a packaging defect, not a confidentiality one.
.tpCleanWorkbook <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  ok <- tryCatch({
    parts <- utils::unzip(path, list = TRUE)$Name
    drop <- grep("^(customXml/|\\[trash\\]/|docProps/custom[.]xml$)", parts, value = TRUE)
    ## docProps/core.xml and app.xml NAME THE OPERATOR, and nothing in the bundle path reached
    ## them: this function only DROPPED parts, and .tpApplyRepl -- which both .tpScrubWorkbook and
    ## .tpStripEmail route through -- targets sharedStrings and the sheet XML only. So a workbook
    ## saved in Excel carried <cp:lastModifiedBy>Firstname Lastname</> into a CC BY 4.0 archive
    ## while Layout_Plate!Operator was correctly placeholdered two parts away. The bundle gate
    ## cannot see it either: it has no pattern for a human display name, and the one that could --
    ## the e-mail shape -- is advisory on purpose.
    ##
    ## BLANKED, NOT DROPPED. core.xml also carries legitimate timestamps, and a workbook with no
    ## core.xml gets one invented by a sync client naming whoever it synced -- which is why
    ## .tpWriteInventory() already ships a deliberately blank one.
    meta <- c(intersect(c("docProps/core.xml", "docProps/app.xml"), parts),
              grep("^xl/(comments[0-9]*[.]xml$|threadedComments/|persons/)", parts, value = TRUE))
    ## The worksheet .rels are in this condition because the hyperlink sweep below reads them.
    ## Leaving them out would have made the early return a silent bypass for the one workbook
    ## shape that needs the sweep most: no docProps, no comments, nothing else to do -- and a
    ## mailto: Target that then rode out untouched.
    links <- grep("^xl/worksheets/_rels/.*[.]rels$", parts, value = TRUE)
    ## TRUE: NOTHING TO DO IS SUCCESS. This returned FALSE, which the callers could not tell
    ## apart from "the clean failed" -- and they discarded the value anyway. Both now stop on
    ## FALSE, so the two must mean different things.
    if (!length(drop) && !length(meta) && !length(links)) return(invisible(TRUE))
    keep <- setdiff(parts, drop)
    tmp <- file.path(tempdir(), sprintf("tpcw_%s", basename(tempfile(""))))
    dir.create(tmp, recursive = TRUE)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
    utils::unzip(path, exdir = tmp, setTimes = TRUE)

    ## The author fields, emptied IN PLACE. The elements stay -- some readers expect them, and an
    ## empty value is what a freshly created workbook has.
    for (rel in meta) {
      f <- file.path(tmp, rel); if (!file.exists(f)) next
      x <- paste(readLines(f, warn = FALSE), collapse = "\n"); was <- x
      ## EVERY OPERATOR-FACING FIELD, not two of seven. core.xml also holds dc:title, dc:subject,
      ## dc:description, cp:keywords, cp:category and cp:contentStatus -- all filled from Excel's
      ## File > Info pane, all names, all previously left alone. A legacy cell NOTE puts the
      ## author's display name in xl/comments*.xml and a threaded comment puts display name AND the
      ## AAD address in xl/persons/*.xml. The bundle scan sees none of it: no pattern for a human
      ## display name, and the e-mail shape is advisory on purpose.
      ##
      ## BLANKED, NOT DROPPED, for the comment parts too -- dropping them would leave a sheet's
      ## <legacyDrawing r:id> pointing at a relationship that is gone, which is exactly what makes
      ## Excel offer to repair a workbook: the fault this function exists to prevent.
      for (tag in c("dc:creator", "cp:lastModifiedBy", "dc:title", "dc:subject", "dc:description",
                    "cp:keywords", "cp:category", "cp:contentStatus",
                    "Company", "Manager", "HyperlinkBase", "author"))
        x <- gsub(sprintf("<%s>[^<]*</%s>", tag, tag), sprintf("<%s></%s>", tag, tag), x)
      ## Threaded-comment persons carry the name and the directory id as ATTRIBUTES, not text.
      for (att in c("displayName", "userId"))
        x <- gsub(sprintf('%s="[^"]*"', att), sprintf('%s=""', att), x)
      ## THE COMMENT BODY, not only who wrote it. Blanking <author> and displayName left the
      ## prose untouched, so "Ran this again after <name> flagged the gate" published in full
      ## while the authorship two elements away was correctly emptied. A comment body is free
      ## text nobody re-reads before submission -- it is invisible in every reader but Excel,
      ## which is exactly why it survives review -- and no ACTA field is ever read from one, so
      ## emptying it costs the archive nothing. Done on every bundle, like the authorship above:
      ## the reason is that the bundle is published, not that this site asked for a scrub.
      ##
      ## <text> IS A DIFFERENT TYPE IN THE TWO PART FAMILIES, and blanking both the same way is
      ## how this fix nearly reintroduced the repair prompt it is written inside a function to
      ## avoid. In xl/comments*.xml <text> is CT_Rst -- rich text, whose content is <r>/<t> runs,
      ## so an empty <t/> is the correct empty value. In xl/threadedComments/ <text> is a plain
      ## xsd:string, so putting a <t> element inside it is a child element where only character
      ## data is allowed: schema-invalid, and Excel offers to repair the workbook.
      if (grepl("^xl/comments[0-9]*[.]xml$", rel)) {
        x <- gsub("<text>.*?</text>", "<text><t></t></text>", x)
        x <- gsub("(<comment\\b[^>]*>)[^<]*", "\\1", x)
      } else if (grepl("^xl/threadedComments/", rel)) {
        x <- gsub("<text>.*?</text>", "<text></text>", x)
      }
      if (!identical(x, was)) writeLines(x, f, useBytes = TRUE)
    }

    ## A mailto: or file: HYPERLINK, removed with the sheet element that points at it.
    ##
    ## An address Excel auto-linked is stored THREE ways: the cell text (which .tpApplyRepl
    ## rewrites), a <hyperlink> element in the sheet, and an External relationship whose Target
    ## is "mailto:<the address>". Only the first was ever touched, so .tpStripEmail() reported
    ## success on a workbook that still carried the address in
    ## xl/worksheets/_rels/sheetN.xml.rels -- and no control could see it: .tpApplyRepl targets
    ## <t> elements, and the post-condition compares whole CELL VALUES, so neither looks at an
    ## attribute. file: and UNC targets are the same shape with a home directory or a share name
    ## in place of the address.
    ##
    ## THE RELATIONSHIP AND THE SHEET ELEMENT GO TOGETHER. Dropping the relationship alone leaves
    ## <hyperlink r:id="rIdN"> pointing at an id that no longer resolves, which is the dangling
    ## reference this whole function exists to prevent -- and an empty <hyperlinks/> wrapper is
    ## itself schema-invalid, so it goes too when it is the last one out.
    for (rel in grep("^xl/worksheets/_rels/.*[.]rels$", keep, value = TRUE)) {
      f <- file.path(tmp, rel); if (!file.exists(f)) next
      x <- paste(readLines(f, warn = FALSE), collapse = "\n"); was <- x
      gone <- character(0)
      for (el in regmatches(x, gregexpr("<Relationship[^>]*?/>", x))[[1]]) {
        tgt <- sub('.*\\sTarget="([^"]*)".*', "\\1", el)
        if (identical(tgt, el)) next
        if (!grepl("^(mailto:|file:|\\\\\\\\)", tgt, ignore.case = TRUE)) next
        id <- sub('.*\\sId="([^"]*)".*', "\\1", el)
        if (!identical(id, el)) gone <- c(gone, id)
        x <- sub(el, "", x, fixed = TRUE)
      }
      if (identical(x, was)) next
      writeLines(x, f, useBytes = TRUE)
      sh <- file.path(tmp, sub("_rels/(.*)[.]rels$", "\\1", rel))
      if (!length(gone) || !file.exists(sh)) next
      y <- paste(readLines(sh, warn = FALSE), collapse = "\n")
      for (el in regmatches(y, gregexpr("<hyperlink\\b[^>]*/>", y))[[1]])
        if (any(vapply(gone, function(g) grepl(sprintf('r:id="%s"', g), el, fixed = TRUE),
                       TRUE))) y <- sub(el, "", y, fixed = TRUE)
      y <- gsub("<hyperlinks>\\s*</hyperlinks>|<hyperlinks\\s*/>", "", y)
      writeLines(y, sh, useBytes = TRUE)
    }

    ## UNREFERENCED SHARED STRINGS, EMPTIED. A value no cell points at is still in the file.
    ##
    ## Writing a cell as an INLINE string -- which is what every XML-level edit in this project
    ## does, deliberately, so the shared-string table never has to be renumbered -- leaves the
    ## PREVIOUS value sitting in xl/sharedStrings.xml with nothing referencing it. It is invisible
    ## in Excel and invisible to anything that reads cells, and the scrub cannot reach it either:
    ## .tpApplyRepl() keys on the value it was TOLD to replace, and an orphan holds whatever was
    ## there before that. So a workbook can be correct in every visible cell and still publish the
    ## string it used to hold.
    ##
    ## MEASURED, on a bundle that every other control passed: a run whose costain panel iteration
    ## was corrected from one name to another published the ORIGINAL panel name from an orphaned
    ## <si>, while the live cell, the export, the report and ACTA's own bundle scan were all
    ## clean. Only a sweep that decompressed the workbook and read its parts found it -- which is
    ## the same lesson as the FCS filename leak: a value can be inside a published artefact
    ## without being inside anything a reader of that artefact would look at.
    ##
    ## BLANKED, NOT DELETED, and this is the part not to get wrong. Removing an <si> renumbers
    ## every index after it, and every t="s" cell in every sheet addresses them BY POSITION -- so
    ## a delete means rewriting all of those too, with a corrupt workbook if it goes wrong. An
    ## orphan carries no live data, so emptying it in place is lossless and touches no cell at all.
    ##
    ## It does NOT make <sst count=>/uniqueCount accurate, and an earlier version of this comment
    ## claimed it did. Blanking leaves both attributes at their original values while the number of
    ## DISTINCT texts drops, so uniqueCount over-reports afterwards. That is tolerated rather than
    ## corrected: Excel does not validate either attribute -- OQ_Test3 already ships count=522
    ## against 108 entries, from long before any of this -- and recomputing uniqueCount means
    ## deciding what "unique" means across rich-text runs, which is a bigger change than the
    ## problem deserves. Said out loud because a comment asserting an invariant the code does not
    ## hold is worse than no comment.
    ssf <- file.path(tmp, "xl/sharedStrings.xml")
    if (file.exists(ssf)) {
      ss <- paste(readLines(ssf, warn = FALSE), collapse = "\n"); wasSS <- ss
      used <- integer(0)
      ## EVERY part under xl/worksheets/, not a `sheet[0-9]+` pattern, and the asymmetry is the
      ## reason. Missing a part that references the table marks LIVE strings dead and blanks them
      ## -- silent data loss. Scanning a part that references nothing costs an orphan left behind.
      ## So this errs towards scanning: a case-different name (`Sheet1.xml`), a chartsheet, or
      ## anything else a writer puts there is read rather than assumed irrelevant. `_rels` is
      ## excluded because it holds no cells, and that exclusion is by directory, not by guesswork.
      for (sheet in grep("^xl/worksheets/[^/]+[.]xml$", keep, value = TRUE, ignore.case = TRUE)) {
        sf <- file.path(tmp, sheet); if (!file.exists(sf)) next
        sx <- paste(readLines(sf, warn = FALSE), collapse = "\n")
        ## Only t="s" cells index the table. An inline string carries its own text and a formula
        ## cached result is t="str", neither of which points here.
        hits <- regmatches(sx, gregexpr('<c[^>]*\\st="s"[^>]*>\\s*<v>[0-9]+</v>', sx))[[1]]
        if (length(hits))
          used <- c(used, as.integer(sub('.*<v>([0-9]+)</v>.*', "\\1", hits)))
      }
      used <- unique(used)
      ## REPLACED BY POSITION, NOT BY TEXT. An earlier version of this did
      ## sub(si[[j]], "<si><t></t></si>", ss, fixed = TRUE), which rewrites the FIRST occurrence of
      ## that text -- so if two <si> elements were byte-identical and the LIVE one came first, a
      ## dead duplicate at index j silently blanked the live string instead, and every cell
      ## pointing at it would read empty. A shared-string table is meant to hold unique values, so
      ## this needs a workbook some other tool wrote badly; "the input is supposed to be well formed"
      ## is not a control, and the cost of being right here is one assignment.
      ## regmatches<- substitutes the Nth match with the Nth replacement, which is exactly the
      ## index-addressed semantics the sheet cells use.
      ## BOTH <si> FORMS, AND THE SELF-CLOSING ONE FIRST.
      ##
      ## "<si>.*?</si>" cannot see a schema-valid <si/>, and because this blanking is POSITIONAL
      ## every entry after one shifts by a place: live strings get marked dead and emptied, the
      ## function still returns TRUE, the workbook still opens, and actaOpcFindings() still reports
      ## clean. Measured on a table of 4 entries with <si/> at index 0 -- the enumeration found 3
      ## and blanked the first LIVE string instead of the orphan. On a real instructions workbook
      ## that cost five cells including a gating_template row, which is the reproducibility the
      ## sibling change in this release exists to protect.
      ##
      ## ORDER MATTERS: "<si\\b[^>]*>" also matches "<si/>", so a self-closing entry put second
      ## would be swallowed as the opening tag of the next pair and the desync would survive the
      ## fix. The self-closing alternative has to be tried first.
      ##
      ## No producer in this pipeline emits <si/> -- Excel, openxlsx, openpyxl and LibreOffice all
      ## write <si><t/></si>, which is handled correctly either way. It is fixed anyway, for the
      ## reason the `used` side of this same block already gives: the failure is silent data loss
      ## in a published artefact, and the cost of being right is one alternation.
      m <- gregexpr("<si\\b[^>]*/>|<si\\b[^>]*>.*?</si>", ss)
      si <- regmatches(ss, m)[[1]]
      if (length(si)) {
        ## 0-based in the sheet XML, 1-based here.
        dead <- setdiff(seq_along(si), used + 1L)
        ## Leave anything already textless alone, so `blanked` counts real changes only.
        dead <- dead[vapply(si[dead], function(s)
                     nzchar(trimws(gsub("<[^>]*>", "", s))), TRUE)]
        if (length(dead)) {
          si[dead] <- "<si><t></t></si>"
          regmatches(ss, m) <- list(si)
          if (!identical(ss, wasSS)) {
            writeLines(ss, ssf, useBytes = TRUE)
            message(sprintf("  %d unreferenced shared string(s) emptied in %s",
                            length(dead), basename(path)))
          }
        }
      }
    }

    ## Every .rels file, resolved against its own owner: a relationship left pointing at a part
    ## that is no longer there is exactly the malformed package this is meant to prevent.
    for (rel in grep("[.]rels$", keep, value = TRUE)) {
      f <- file.path(tmp, rel); if (!file.exists(f)) next
      x <- paste(readLines(f, warn = FALSE), collapse = "\n"); was <- x
      base <- dirname(dirname(rel))
      for (el in regmatches(x, gregexpr("<Relationship[^>]*?/>", x))[[1]]) {
        tgt <- sub('.*\\sTarget="([^"]*)".*', "\\1", el)
        if (identical(tgt, el) || grepl('TargetMode="External"', el)) next
        ## .actaOpcResolve(), NOT normalizePath(). Its own comment says why: whether the part
        ## exists is the QUESTION, so "../" has to be collapsed by hand. normalizePath() leaves a
        ## non-existent relative path unresolved, so "xl/worksheets/../tables/table3.xml" never
        ## became "xl/tables/table3.xml", looked dangling, and the table relationship was deleted
        ## -- while sheet3 still declared <tablePart r:id="rId3"/> and the table part was still
        ## in the archive. A sheet pointing at a relationship that no longer exists is exactly
        ## what makes Excel offer to repair a workbook, and it is what it did.
        abs <- if (startsWith(tgt, "/")) sub("^/", "", tgt) else .actaOpcResolve(base, tgt)
        if (!(abs %in% keep)) x <- sub(el, "", x, fixed = TRUE)
      }
      if (!identical(x, was)) writeLines(x, f, useBytes = TRUE)
    }
    ctf <- file.path(tmp, "[Content_Types].xml")
    if (file.exists(ctf)) {
      ct <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
      for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
        prt <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
        if (!identical(prt, el) && !(prt %in% keep)) ct <- sub(el, "", ct, fixed = TRUE)
      }
      writeLines(ct, ctf, useBytes = TRUE)
    }
    .tpZip(file.path(tmp, "clean.xlsx"), keep, tmp)
    file.copy(file.path(tmp, "clean.xlsx"), path, overwrite = TRUE)
  }, error = function(e) {
    warning(sprintf("TiterPubs: could not strip sync residue from %s (%s).",
                    basename(path), conditionMessage(e)), call. = FALSE)
    FALSE
  })
  invisible(isTRUE(ok))
}

## ONE literal text substitution pass over a workbook, shared by every caller that needs one.
##
## Extracted from .tpScrubWorkbook() when the email removal below had to run on EVERY bundle
## rather than only on a scrubbed one. Two copies of this would have been two chances to get the
## OPC bookkeeping wrong, and that bookkeeping is the part that makes Excel offer to repair a file.
##
## PURE TEXT SUBSTITUTION, NO CELL SURGERY -- see the note on .tpScrubWorkbook(). Swapping text
## inside <t> elements that already exist cannot change a cell's type or its attributes, which is
## the whole reason this shape was chosen.
## `within` names keys in `repl` that are replaced ANYWHERE INSIDE a cell's text rather than as
## the whole of it, and that are allowed to match nothing.
##
## ONE CELL IN THE EXPORT HOLDS A LIST. MiFlowCyt section 4.1 is written as a single sentence --
## "11 FCS file(s): Titration_FCS/<dir>/<file>.fcs; Titration_FCS/..." -- so the whole-cell rule
## below cannot reach the paths inside it. Measured: after the FCS rename landed and every entry
## name and `fcs_file` cell was correct, that one composed cell still named all eleven original
## files, which is both a leak and a set of pointers into the bundle that no longer resolve.
##
## SUBSTRINGS ARE SAFE FOR THESE AND ONLY THESE. The whole-cell rule exists because the map holds
## short identifiers -- "OP" inside OPERATOR and SOP is the fault it was written to stop. A
## `within` key is a full relative path ending in .fcs: long, structural, and appearing nowhere
## in this format but cell text. Do not put a bare identifier in here.
.tpApplyRepl <- function(path, repl, within = character()) {
  ## NOTHING TO DO IS SUCCESS, NOT FAILURE. The caller now stop()s on FALSE, so every path
  ## that means "there was nothing here to change" must say TRUE or it aborts the bundle for
  ## a workbook that is simply already clean -- the same shape as making the PDF reader fail
  ## closed while its package stayed in Suggests. A genuine failure still returns FALSE.
  if (!length(repl)) return(invisible(TRUE))
  ## A VALUE THAT ALREADY EQUALS ITS PLACEHOLDER NEEDS NO REWRITE -- and must not be reported as a
  ## survivor afterwards, because it IS the correct output. OQ_Test2 ships ELN_Name = "ELN", which
  ## is exactly ACTA_SCRUB_CONFIDENTIAL[["ELN_Name"]], so the map held ELN -> ELN; the rewrite was
  ## a no-op, and the post-condition then found "ELN" still present and failed a workbook that was
  ## already clean. Measured: OQ_Test2 was the one fixture of three that still returned FALSE.
  repl <- repl[vapply(names(repl), function(k) !identical(k, as.character(repl[[k]])), TRUE)]
  if (!length(repl)) return(invisible(TRUE))
  tmp <- file.path(tempdir(), sprintf("tpar_%s", basename(tempfile(""))))
  dir.create(tmp, recursive = TRUE)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  parts <- utils::unzip(path, list = TRUE)$Name
  parts <- parts[!startsWith(parts, "[trash]/")]
  utils::unzip(path, exdir = tmp, setTimes = TRUE)

  ## Every part that can hold cell text. Matched LITERALLY (fixed = TRUE) -- a catalogue number
  ## or a panel name is full of characters a regex would read as syntax.
  targets <- c("xl/sharedStrings.xml",
               grep("^xl/worksheets/sheet[0-9]+[.]xml$", parts, value = TRUE))
  ## THE WHOLE <t> ELEMENT, AS A REGEX -- not two hard-coded open tags.
  ##
  ## This matched exactly `<t>VALUE</t>` and `<t xml:space="preserve">VALUE</t>`. An operator who
  ## types or pastes an operator code with ONE TRAILING SPACE -- the commonest way a lab cell
  ## differs from what it displays -- makes Excel store `<t xml:space="preserve">OPER </t>`,
  ## with the space INSIDE the text. readxl trims, so the key is "OPER", neither literal matches,
  ## and the replacement never happens. Measured on a real workbook: the real value survived, the
  ## placeholder was never written, and .tpScrubWorkbook() still returned TRUE -- because `changed`
  ## was ONE counter across all keys, so the replacements that DID land masked the one that did
  ## not. The bundle gate cannot compensate: it has no pattern for an operator code or an ELN ID.
  ##
  ## The value is quoted CHARACTER BY CHARACTER (.tpReEsc), so a catalogue number or panel name
  ## full of regex syntax is still matched literally -- the property the old fixed = TRUE was
  ## there for.
  ##
  ## IT USED TO BE \Q...\E, AND \Q...\E HAS A SENTINEL. PCRE stops quoting at the first `\E` IN
  ## THE VALUE, and .tpEsc() escapes the five XML entities -- it never touches a backslash -- so a
  ## cell holding one reaches the pattern intact. A Windows path typed into SOP or Sample_Details
  ## is the realistic carrier: `C:\ELN\Exports\...`. Measured on `a\Ed`: the pattern no longer
  ## matched its OWN value, and instead matched `<t>ad</t>` -- an unrelated cell rewritten to the
  ## confidential placeholder, with `hitKey` set TRUE so the "every key must hit" post-condition
  ## passed and the bundle shipped. Corruption with every control green, which is the one outcome
  ## this file is written to prevent. Per-character escaping has no sentinel to find.
  ## The strict post-condition governs the CONFIDENTIAL keys only. A `within` key is an optional
  ## pointer -- the workbook may legitimately not mention that file -- so it is excluded from the
  ## "every key must hit" rule rather than weakening that rule for everything.
  strict <- setdiff(names(repl), within)
  hitKey <- stats::setNames(logical(length(strict)), strict)
  for (rel in intersect(targets, parts)) {
    fp <- file.path(tmp, rel)
    if (!file.exists(fp)) next
    x <- paste(readLines(fp, warn = FALSE), collapse = "\n"); was <- x
    for (from in strict) {
      pat <- sprintf("<t([^>]*)>\\s*%s\\s*</t>", .tpReEsc(.tpEsc(from)))
      if (grepl(pat, x, perl = TRUE)) {
        ## Backslashes doubled: this is a replacement string, where they are backreference syntax.
        to <- gsub("\\\\", "\\\\\\\\", .tpEsc(repl[[from]]))
        x <- gsub(pat, sprintf("<t\\1>%s</t>", to), x, perl = TRUE)
        hitKey[[from]] <- TRUE
      }
    }
    ## LONGEST FIRST, so a path that is a prefix of another cannot consume it. fixed = TRUE with
    ## useBytes = TRUE: a workbook part is raw bytes and gsub(fixed=) validates the locale
    ## otherwise, which is how an earlier gate died on a real file.
    for (from in within[order(-nchar(within))])
      x <- gsub(.tpEsc(from), .tpEsc(as.character(repl[[from]])), x, fixed = TRUE, useBytes = TRUE)
    if (!identical(x, was)) writeLines(x, fp, useBytes = TRUE)
  }
  if (length(strict) && !any(hitKey)) return(invisible(FALSE))
  ## A KEY THAT MATCHED NOWHERE IS A FAILURE, not a detail folded into a global counter. Every key
  ## here came from reading THIS workbook, so a key with no cell is a rewrite that did not happen.
  ## Nothing is written back in that case -- the original file is left untouched and the caller stops.
  if (!all(hitKey))
    return(invisible(structure(FALSE, missed = names(hitKey)[!hitKey])))

  ctf <- file.path(tmp, "[Content_Types].xml")
  if (file.exists(ctf)) {
    ct <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
    for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
      prt <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
      if (!identical(prt, el) && !(prt %in% parts)) ct <- sub(el, "", ct, fixed = TRUE)
    }
    writeLines(ct, ctf, useBytes = TRUE)
  }
  .tpZip(file.path(tmp, "applied.xlsx"), parts, tmp)
  file.copy(file.path(tmp, "applied.xlsx"), path, overwrite = TRUE)
  ## POST-CONDITION, ON EXTRACTED TEXT. actaAssertBundleClean() is a post-condition for the zip;
  ## this is the same idea for the rewrite. It must run on text with the TAGS STRIPPED, because a
  ## value Excel stored as rich text sits in two <r><t> runs -- Excel and readxl read the cell
  ## whole, while no contiguous substring of the XML contains it. Any check done before stripping
  ## would miss exactly that case.
  surv <- .tpReplSurvivors(path, names(repl))
  if (length(surv)) return(invisible(structure(FALSE, survived = surv)))
  invisible(TRUE)
}

## Which of `keys` still survives as a WHOLE CELL VALUE. Returns empty when the rewrite landed.
##
## WHOLE VALUES, NOT SUBSTRINGS OVER CONCATENATED TEXT. The first version of this stripped the tags
## off sharedStrings + sheet XML, joined everything, and matched each key with fixed = TRUE. That
## made scrub_confidential = TRUE UNABLE TO BUILD A BUNDLE AT ALL: an operator code of "OP" matches
## inside "SOP" (a shipped column), inside "Operator", and inside the export's own `name` column,
## which ACTA composes as "<eln>_P1_A3_tgt<marker>_Stain_5_Op<operator>.fcs". Measured on all three
## shipped fixtures: every one returned FALSE, i.e. the control failed 100% of the time, and its
## only workaround was to turn the scrub off and publish the real values. 132 of the 676 two-letter
## codes collide with the shipped template, including ACTA's own placeholder "JD".
##
## The question this answers is narrow and is the only thing .tpApplyRepl() promised: does any cell
## STILL EQUAL a key. A key embedded in composite text is a different problem -- real, but not this
## function's, and failing the whole bundle on it helps nobody.
##
## Read through readxl rather than the XML: it gives the value Excel shows, so a value stored as
## rich text across several runs is compared whole, and header text is covered too.
.tpReplSurvivors <- function(path, keys) {
  sh <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
  if (!length(sh)) return("<the workbook could not be re-read to verify>")
  vals <- unlist(lapply(sh, function(s) {
    d <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = s, col_types = "text")),
                  error = function(e) NULL)
    if (is.null(d) || !ncol(d)) return(character(0))
    c(names(d), unlist(lapply(d, as.character), use.names = FALSE))
  }), use.names = FALSE)
  vals <- trimws(vals[!is.na(vals)])
  keys[keys %in% vals]
}


## Blank the submitter's email in a bundled workbook. ALWAYS, on every bundle.
##
## NOT GATED ON scrub_confidential, and that is the point. A TiterPubs bundle is published for
## anyone to download, so it cannot carry a contact address whatever this site's confidentiality
## setting says -- the reason is a property of publishing, not of the setting. TiterPubs collects
## the address on the upload form instead.
##
## The PLACEHOLDER IS THE WORSE CASE, which is what prompted this: a scrubbed bundle shipped
## `johndoe@anonymouscompany.com`, and the maintainer's note was that it "looks real". A reader
## cannot tell a placeholder address from a live one, so a plausible fake is a worse artefact
## than either the truth or a blank.
##
## Targets the Submitter_Email VALUE specifically rather than sweeping for anything shaped like an
## address: References legitimately holds URLs and a broad pattern would start eating them.
.tpStripEmail <- function(path) {
  if (!file.exists(path)) return(invisible(FALSE))
  ok <- tryCatch({
    inf <- suppressMessages(readxl::read_excel(path, sheet = "Info", col_types = "text"))
    infd <- as.data.frame(inf)
    if (!nrow(infd)) return(invisible(TRUE))
    was <- if (actaInfoIsLong(infd)) {
      j <- which(tolower(trimws(as.character(infd$Variable))) == "submitter_email")
      if (length(j)) trimws(as.character(infd$Value[j[1]])) else NA_character_
    } else {
      nmv <- tolower(trimws(actaStripHeaderAnnotation(names(infd))))
      j <- which(nmv == "submitter_email")
      if (length(j)) trimws(as.character(infd[[j[1]]][1])) else NA_character_
    }
    ## The FIFTH 'nothing to do' path. An operator who leaves Submitter_Email blank -- which this
    ## function's own header recommends, since TiterPubs collects it on the upload form -- had
    ## the bundle aborted. It also made the step non-idempotent: a second pass over an
    ## already-stripped workbook returned FALSE.
    if (is.na(was) || !nzchar(was)) return(invisible(TRUE))
    .tpApplyRepl(path, stats::setNames(list(""), was))
  }, error = function(e) {
    warning(sprintf("TiterPubs: could not remove the submitter email from %s (%s).",
                    basename(path), conditionMessage(e)), call. = FALSE)
    FALSE
  })
  invisible(isTRUE(ok))
}

## Replace the confidential Layout_Plate values in the BUNDLE'S copy of the instructions
## workbook. The original in the run folder is never touched.
##
## PURE TEXT SUBSTITUTION, NO CELL SURGERY. The first version rewrote each target cell into an
## inline string, which means changing a cell's type, its attributes and its body -- three
## chances to produce something Excel will not open, and it took two of them: a duplicate
## `t="s" t="inlineStr"` attribute, and then a workbook that still prompted for repair for
## reasons no structural check could name (every part parsed, parts and overrides matched the
## source exactly, no cells were lost). Swapping text inside elements that already exist cannot
## restructure anything, so the class of fault is removed rather than chased.
##
## Substitution is BY VALUE, not by column: a confidential value is replaced wherever it appears,
## in the shared-string table and in any inline string. For these identifiers that is what you
## want anyway -- an operator code appearing in a second column is still the operator -- and it
## also fixes the orphaned shared string the column-wise version left behind.
## THE REPLACEMENT MAP, BUILT ONCE AND SHARED. original value -> placeholder.
##
## Extracted from .tpScrubWorkbook() when the bundle builder needed the SAME map for two more
## jobs: renaming the staged FCS files (a filename is a carrier the cell-value substitution
## cannot reach) and seeding the leak scan with the very values being substituted. Three callers
## deriving this independently would be three chances to disagree about what "confidential"
## means, and the one that disagreed silently would be the one that leaks.
.tpScrubMap <- function(path, eln = NULL) {
  if (!file.exists(path)) return(list())
  repl <- tryCatch({
    lp <- suppressMessages(readxl::read_excel(path, sheet = "Layout_Plate", col_types = "text"))
    names(lp) <- actaStripHeaderAnnotation(names(lp))
    tgt <- intersect(c(names(ACTA_SCRUB_CONFIDENTIAL), "Costain_Panel_Iteration"), names(lp))
    ## THE PUBLISHED ELN ID IS HANDED IN, NOT RE-DERIVED HERE. An earlier version of this resolved
    ## Info!scrub_eln_placeholder from the workbook on disk, which was wrong three ways at once:
    ##
    ##   * TWO READS, TWO ANSWERS. ACTA_Script.R reads Info with no col_types; this read it as
    ##     "text". For a DATE cell -- which is what Excel makes of "2026-10-23", and dating a
    ##     dataset is an obvious way to tell a published set apart -- the script resolved
    ##     "2026-10-23" while this resolved the serial "46318". The report, the plots and the zip
    ##     name would say one thing and both bundled workbooks another: one bundle naming two
    ##     experiments, the exact drift actaScrubElnPlaceholder()'s header promises cannot happen.
    ##   * A VALIDATION ERROR BECAME A SILENT NO-OP. actaScrubElnPlaceholder() stop()s on a bad
    ##     placeholder, and this function's own handler below turns any error into list() -- an
    ##     EMPTY map, which .tpScrubWorkbook() reads as "nothing to do" and reports success for.
    ##     A refused placeholder would have bundled a wholly unscrubbed workbook while every gate
    ##     said clean, with fcsMap empty and the site tokens unseeded. Fail-open, again.
    ##   * ITS SAFETY WARNING FIRED ON EVERY CORRECT RUN. The `current` comparison needs the REAL
    ##     ELN_ID, and .tpScrubWorkbook() calls this on the staged EXPORT, whose Layout_Plate audit
    ##     copy has already been scrubbed in memory -- so the placeholder always equalled the
    ##     column and the one control guarding the feature's only invisible leak cried wolf every
    ##     time. That comparison belongs at ACTA_Script.R's call, which is the single point in a run
    ##     where the real value is still there.
    ##
    ## The caller resolves it once and passes it down. NULL keeps the shipped constant.
    scrubRepl <- ACTA_SCRUB_CONFIDENTIAL
    if (!is.null(eln) && length(eln) == 1L && !is.na(eln) && nzchar(eln))
      scrubRepl[["ELN_ID"]] <- as.character(eln)
    repl <- list()
    ## ONE VALUE IN TWO SCRUB COLUMNS IS AMBIGUOUS, and it resolved as last-writer-wins.
    ##
    ## `repl[[z]] <- ...` simply overwrites, so a value that appears in two of the scrubbed columns
    ## produced ONE map entry and whichever column came later in `tgt` decided what it became. The
    ## realistic form is an ELN_ID that is also typed into Costain_Panel_Iteration, which publishes
    ## the experiment id as "Iteration1" -- or the reverse, which publishes the iteration as the ELN
    ## placeholder and silently merges two panels in the re-run inventory. Either way the OTHER
    ## column's value is wrong in the bundle and every control reports success, because the map is
    ## self-consistent: .tpApplyRepl()'s post-condition only asks that each key HIT something, and
    ## .tpSiteTokens() seeds the leak scan from this same map, so it looks for the value that WAS
    ## replaced and never for the column that was mis-replaced.
    ##
    ## There is no right answer to pick -- the two columns genuinely disagree about what the value
    ## should become -- so it is recorded and stopped on, like the cross-column collision below.
    ## `owner` is a plain list so a value with regex or `[[`-unfriendly characters is still a key.
    ambig <- list()
    owner <- list()
    for (cn in tgt) {
      v <- unique(trimws(stats::na.omit(as.character(lp[[cn]]))))
      v <- v[nzchar(v)]
      if (!length(v)) next
      for (i in seq_along(v)) {
        z   <- v[i]
        new <- if (identical(cn, "Costain_Panel_Iteration"))
                 paste0(ACTA_SCRUB_ITERATION_PREFIX, i) else scrubRepl[[cn]]
        if (!is.null(repl[[z]]) && !identical(as.character(repl[[z]]), as.character(new)))
          ambig[[z]] <- unique(c(ambig[[z]], owner[[z]], cn))
        repl[[z]]  <- new
        owner[[z]] <- cn
      }
    }

    ## Cell_Type, WHEN THE SITE HAS RULES FOR IT -- and this map is the only thing that
    ## reaches the bundled instructions workbook.
    ##
    ## actaApplyScrub() maps Cell_Type on the stats frame, which covers the export, the report
    ## and the submission template. The bundled Ab_Titration_Instructions.xlsx is a BYTE COPY of
    ## the operator's own file with only THIS map applied, so a site whose rules rewrite a donor
    ## identifier out of Cell_Type still shipped the RAW string in that workbook's
    ## Layout_Plate. Every control reported success: the map never named the column, so there was
    ## nothing to fail, and .tpSiteTokens() seeds the leak scan from this same map -- so the gate
    ## could not see it either. A reader that cannot read reporting "clean", once more.
    ##
    ## Mapped through actaMapCellType(), the SAME function actaApplyScrub() uses, so the two
    ## paths cannot disagree about what a rule means. Only values the rules actually CHANGE are
    ## added: an unmapped material is published as written, by design, and seeding it here would
    ## make .tpApplyRepl()'s "every key must hit" post-condition fail on a correct bundle.
    if ("Cell_Type" %in% names(lp) && length(actaCellTypeRules())) {
      tm <- unique(trimws(stats::na.omit(as.character(lp[["Cell_Type"]]))))
      tm <- tm[nzchar(tm)]
      for (z in tm) {
        mapped <- actaMapCellType(z)
        if (length(mapped) == 1L && !is.na(mapped) && nzchar(mapped) && !identical(mapped, z))
          repl[[z]] <- mapped
      }
    }
    inf <- tryCatch(suppressMessages(readxl::read_excel(path, sheet = "Info", col_types = "text")),
                    error = function(e) NULL)
    if (!is.null(inf) && nrow(inf)) {
      infd <- as.data.frame(inf)
      long <- actaInfoIsLong(infd)
      for (k in names(ACTA_SCRUB_INFO)) {
        was <- if (long) {
          j <- which(tolower(trimws(as.character(infd$Variable))) == tolower(k))
          if (length(j)) trimws(as.character(infd$Value[j[1]])) else NA_character_
        } else {
          nmv <- tolower(trimws(names(infd)))
          j <- which(nmv == tolower(k))
          if (length(j)) trimws(as.character(infd[[j[1]]][1])) else NA_character_
        }
        if (!is.na(was) && nzchar(was) && !identical(was, unname(ACTA_SCRUB_INFO[[k]])))
          repl[[was]] <- unname(ACTA_SCRUB_INFO[[k]])
      }
    }
    ## COLLISION DETECTION, recorded here and ENFORCED OUTSIDE the handler below.
    ##
    ## This map is VALUE-keyed: every key rewrites any cell in the bundled workbook whose whole
    ## text equals it. That is safe only while a confidential value is unique to its own column.
    ## It is not always: `SOP` is free text and a lab may type a bare number into it -- OQ_Test1
    ## and OQ_Test2 ship SOP = "001" -- and `sharedStrings.xml` is DEDUPLICATED, so one edit
    ## rewrites every column holding that value at once. Measured: with SOP in the scrub, a
    ## Lot_Num of "001" published as the SOP replacement, .tpApplyRepl() returned TRUE, and
    ## .tpSiteTokens() classed "001" as `short` so the leak gate never re-checked it. Lot_Num is
    ## the identifier actaRestorePData()'s own header calls "the one that matters".
    ##
    ## NOT silently skipped. Dropping the key would swap data corruption for a LEAK -- the
    ## confidential value would simply stay in the bundled workbook with every control reporting
    ## success, which is the failure mode this file exists to prevent. The run stops instead and
    ## names the column, because the operator can fix it in one cell and nobody else can.
    ## ONLY COLUMNS THE REWRITE CAN ACTUALLY REACH. `.tpApplyRepl()` matches whole `<t>` elements,
    ## so it only ever touches cells Excel stored as STRINGS. A numeric cell is `<v>100</v>` and a
    ## boolean is `<v>1</v>`; neither can be hit, so flagging them stops a run over a collision
    ## that cannot happen. That is not hypothetical -- `Well_volume_uL` is the number 100 in all
    ## four shipped workbooks, so an operator whose SOP is "100" was refused and told to go and
    ## change a cell that was perfectly correct.
    ##
    ## Storage type comes from reading the sheet a SECOND time with readxl's own type guessing
    ## (the first read forces "text" to get exact values). A column readxl calls numeric, logical
    ## or Date is stored as `<v>` throughout; one it calls character holds strings. A MIXED column
    ## comes back character, which keeps it in scope -- the conservative direction, and the one
    ## that matters: "001" is text-stored precisely because of the leading zero, which is the
    ## collision this guard exists for. If the second read fails for any reason, every column
    ## stays in scope, so a reader that cannot read falls back to stricter, never looser.
    reach <- tryCatch({
      lpn <- suppressMessages(readxl::read_excel(path, sheet = "Layout_Plate"))
      names(lpn) <- actaStripHeaderAnnotation(names(lpn))
      vapply(names(lpn), function(cn) is.character(lpn[[cn]]), logical(1))
    }, error = function(e) NULL)
    cols <- setdiff(names(lp), c(tgt, "Cell_Type"))
    if (!is.null(reach)) {
      known <- intersect(cols, names(reach))
      cols  <- c(setdiff(cols, known), known[reach[known]])
    }
    ## A KEY THAT ALREADY EQUALS ITS PLACEHOLDER IS NOT A COLLISION. .tpApplyRepl() drops those
    ## keys outright (see its own note: OQ_Test2 ships ELN_Name = "ELN", which IS the placeholder),
    ## so there is no rewrite to collide with -- the cell is already the correct published value
    ## wherever it appears. Scanning them refused a workbook over a substitution that never runs.
    scan <- names(repl)[vapply(names(repl),
                               function(k) !identical(k, as.character(repl[[k]])), TRUE)]
    coll <- list()
    for (k in scan) {
      hit <- cols[vapply(cols, function(cn)
        any(trimws(as.character(lp[[cn]])) == k, na.rm = TRUE), logical(1))]
      if (length(hit)) coll[[k]] <- hit
    }
    attr(repl, "acta_collisions") <- coll
    ## AMBIGUITY IS NOT FILTERED BY `scan`, deliberately. If the last column to claim a value hands
    ## it a placeholder equal to itself, the key is dropped by .tpApplyRepl() and the OTHER column's
    ## cell ships RAW -- Operator = "ELN" alongside ELN_Name = "ELN" is the measured case, and it is
    ## the worst one, not an exemption from it.
    attr(repl, "acta_ambiguous")  <- ambig
    repl
  }, error = function(e) list())

  ## OUTSIDE the tryCatch ON PURPOSE. That handler turns any error into list(), which
  ## .tpScrubWorkbook() reads as "nothing to do" and reports success for -- so a stop() raised
  ## inside would bundle a wholly unscrubbed workbook while every gate said clean. Fail-open is
  ## exactly the shape this guard exists to avoid, so the check lives where it cannot be caught.
  amb <- attr(repl, "acta_ambiguous")
  if (length(amb)) {
    msg <- vapply(names(amb), function(k)
      sprintf("  one Layout_Plate value is in both of these scrubbed columns: %s",
              paste(amb[[k]], collapse = ", ")), character(1))
    stop(sprintf(paste0(
      "ACTA: the same value appears in two columns that are scrubbed to DIFFERENT placeholders, ",
      "so one of them would be published wrong.\n%s\nGive each column its own value in ",
      "Layout_Plate and re-run (an ELN id typed into Costain_Panel_Iteration is the usual cause). ",
      "The run is stopped rather than publishing a value under the other column's placeholder."),
      paste(msg, collapse = "\n")), call. = FALSE)
  }
  coll <- attr(repl, "acta_collisions")
  if (length(coll)) {
    msg <- vapply(names(coll), function(k)
      sprintf("  the confidential value in Layout_Plate also appears in: %s",
              paste(coll[[k]], collapse = ", ")), character(1))
    stop(sprintf(paste0(
      "ACTA: a value being scrubbed is not unique to its own column, so replacing it would ",
      "rewrite unrelated cells.\n%s\nChange that cell in Layout_Plate (a Lot_Num, PlateID or ",
      "SOP that is a bare number is the usual cause) and re-run. The run is stopped rather than ",
      "corrupting the bundle or leaving the value unscrubbed."),
      paste(msg, collapse = "\n")), call. = FALSE)
  }
  attr(repl, "acta_collisions") <- NULL
  attr(repl, "acta_ambiguous")  <- NULL
  repl
}

## Apply the map to a FILE NAME, on token boundaries only.
##
## NOT A SUBSTRING PASS, and that is the whole design. The map legitimately contains SHORT keys --
## OQ_Test1 ships Operator "OP" -- and a substring replacement of OP -> JD rewrites OPERATOR to
## JDERATOR and SOP to SJD. That is the identical fault that made .tpReplSurvivors() report every
## fixture dirty (OP matching inside SOP, Operator and the export's composed name column), and
## here it would corrupt the names of the data files themselves.
##
## A filename is a sequence of tokens separated by non-alphanumerics, so a key matches only when
## it is bounded by one of those or by the start/end of the name. `EXP00000001_CD4_A1.fcs` has
## its id replaced; `OPERATOR_notes.fcs` is untouched by the key `OP`.
##
## WHAT THIS DELIBERATELY DOES NOT CATCH: a key glued to other characters inside one token
## (`EXP00000001Stain.fcs`). Widening the rule to find it is what reintroduces the OP problem.
## That case is covered by the OTHER half of this fix instead -- the leak scan is seeded with the
## same keys, so an identifier the rename could not safely reach is REPORTED rather than missed.
## Which scrubbed values the leak scan should look for, and which are too short to look for.
##
## A DETECTOR IS NOT A REWRITER, and conflating the two is a defect this function exists to keep
## separate. .tpRenameToken() matches on TOKEN BOUNDARIES because it edits names and must not
## corrupt OPERATOR into JDERATOR. The scan must do the opposite: find the value ANYWHERE,
## including glued inside a longer token, because that is precisely the case the rename declined
## to touch. Measured: giving the scan the rename's boundary rule made it blind to
## `EXP00000001Glued_A1.fcs` -- the bundle published, with the gate reporting no findings, while
## the comment above the call claimed that case was covered.
##
## So these go in as FIXED SUBSTRINGS, and the collision problem is handled by LENGTH instead. A
## short code cannot be told from structure in what this scan reads -- FCS keywords arrive as
## "$OP = JD" and workbooks as raw XML -- and seeding every key refused a correctly-scrubbed
## bundle with 15 findings, 14 of them that collision. Six characters admits ELN ids and panel
## names and excludes initials. `short` is returned rather than discarded so the caller can SAY
## which values were replaced but not re-checked: it is a limit on verification, not on scrubbing.
.tpSiteTokens <- function(map, min_chars = 6L) {
  if (!length(map)) return(list(use = character(0), short = character(0)))
  keys <- unique(names(map)[!vapply(names(map),
            function(k) identical(k, as.character(map[[k]])), TRUE)])
  keys <- keys[nzchar(keys)]
  ## The placeholders are what we substituted IN. Searching for them would refuse every
  ## correctly-scrubbed bundle.
  keys <- setdiff(keys, unique(as.character(unlist(map))))
  list(use = keys[nchar(keys) >= min_chars], short = keys[nchar(keys) < min_chars])
}

## ESCAPED CHARACTER BY CHARACTER, NOT WITH \Q...\E. A key containing the two characters \E ends
## the quoted span early and the remainder of the key is read as pattern -- so the key silently
## matches nothing and the file is NOT renamed, while every control reports success. Measured:
## the key `AB\Ec` left `AB\Ec_A1.fcs` untouched. In PCRE a backslash before any non-alphanumeric
## is that literal character, so escaping the whole class removes the failure mode rather than
## enumerating which metacharacters matter.
.tpReEsc <- function(x) gsub("([^A-Za-z0-9_])", "\\\\\\1", x, perl = TRUE)

.tpRenameToken <- function(name, repl) {
  if (!length(repl) || !nzchar(name)) return(name)
  out <- name
  ## Longest key first: a short key that is a prefix of a longer one must not consume it.
  for (k in names(repl)[order(-nchar(names(repl)))]) {
    to <- as.character(repl[[k]])
    if (identical(k, to) || !nzchar(k)) next
    ## Backslashes doubled on the REPLACEMENT side too: there they are backreference syntax, and
    ## \1/\2 in a placeholder would splice the captured boundaries back in.
    out <- gsub(sprintf("(^|[^A-Za-z0-9])%s($|[^A-Za-z0-9])", .tpReEsc(k)),
                sprintf("\\1%s\\2", gsub("\\\\", "\\\\\\\\", to)), out, perl = TRUE)
  }
  out
}

## `extra_repl` carries replacements the map itself cannot derive -- specifically the FCS path
## rewrites, old relative path -> new, which only exist once the files have been staged. They go
## through the SAME whole-cell pass: the export's `fcs_file` cells hold exactly one path each, so
## a cell-value substitution reaches them, and routing them here means the renamed files and the
## cells that point at them are rewritten by one mechanism rather than two.
.tpScrubWorkbook <- function(path, extra_repl = list(), eln = NULL) {
  if (!file.exists(path)) return(invisible(FALSE))
  ok <- tryCatch({
    repl <- .tpScrubMap(path, eln = eln)
    ## OPTIONAL, AND SUBSTRING-MATCHED. Skipping either property is how this fix would have become
    ## the fourth fail-always regression in this file:
    ##   * OPTIONAL, because .tpApplyRepl()'s post-condition requires every key to match a cell --
    ##     deliberately, since a confidential value that matched nothing means the scrub did not
    ##     do what it claimed. These pointers do not share that property: the instructions
    ##     workbook has no `fcs_file` column at all, and the export names only files that produced
    ##     a statistic, so the unstained wells are legitimately absent.
    ##   * SUBSTRING, because MiFlowCyt 4.1 joins every path into ONE cell, which a whole-cell
    ##     rule cannot reach. See the note on `within` in .tpApplyRepl().
    for (k in names(extra_repl)) repl[[k]] <- extra_repl[[k]]

    ## Nothing to replace: already clean, see the note in .tpApplyRepl().
    if (!length(repl)) return(invisible(TRUE))
    .tpApplyRepl(path, repl, within = names(extra_repl))
  }, error = function(e) {
    warning(sprintf("TiterPubs: could not scrub the bundled instructions workbook (%s); it is bundled unchanged.",
                    conditionMessage(e)), call. = FALSE)
    FALSE
  })
  invisible(isTRUE(ok))
}
