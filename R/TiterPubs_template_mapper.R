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
ACTA_TP_OMIT <- c("Concentration for this Lot#", "Sample details", "Tissue")

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
ACTA_TP_ANTIGEN <- c("Strep" = "Streptavidin", "SA" = "Streptavidin", "SAV" = "Streptavidin",
                     "FVD" = "Viability", "Live/Dead" = "Viability")

## Test material -> the Cell type menu. PBMC is absent from their 22 options; `mononuclear cells`
## is literally what the P-M-C in PBMC stands for, so it is a rename, not an interpretation.
ACTA_TP_CELLTYPE <- c("PBMC" = "mononuclear cells", "HD PBMC" = "mononuclear cells",
                      "Human T cells" = "T cells", "T cells" = "T cells",
                      "Tcells" = "T cells", "WBC" = "white blood cells/leukocytes")

## Layout_Plate!Test_Material_Status -> the `Fresh or Frozen` menu. The layout holds one word per
## well; the menu holds four whole sentences, each naming BOTH the thaw state and whether the
## cells were cultured before staining. A bare `Frozen` answers the first half and says nothing
## about the second, so it maps to the no-culture option -- that is what the shipped templates
## mean by it, and the two-word forms exist for the run that cultured.
##
## .tpAlias(), not .tpAliasStrict(): input and output are the same kind of thing here, so a value
## already typed as one of the four sentences passes through untouched rather than being blanked
## for not being in this table.
ACTA_TP_SAMPLE_STATUS <- c(
  "Frozen"          = "Frozen, thawed and incubated with antibody",
  "Thawed"          = "Frozen, thawed and incubated with antibody",
  "Frozen, cultured" = "Frozen, thawed, cultured then incubated with antibody",
  "Frozen cultured" = "Frozen, thawed, cultured then incubated with antibody",
  "Fresh"           = "Fresh, never frozen, incubated with antibody day of collection",
  "Fresh, cultured" = "Fresh, never frozen, cultured then incubated with antibody",
  "Fresh cultured"  = "Fresh, never frozen, cultured then incubated with antibody")

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
.tpNorm <- function(x) {
  x <- as.character(x)
  x <- gsub("[​-‍﻿­]", "", x)   # ZWSP, ZWNJ, ZWJ, BOM, soft hyphen
  x <- gsub(" ", " ", x)                        # non-breaking space
  trimws(gsub("[[:space:]]+", " ", x))
}

.tpBlank <- function(x) is.null(x) || !length(x) || all(is.na(x)) || !nzchar(.tpNorm(x)[1])

## The STAINING PROTOCOL's URL out of Info!References, for TiterPubs' "Protocol reference" column.
##
## References is a semicolon-separated list and holds two different kinds of citation: the
## analysis tool (ACTA's own repository) and the protocol the cells were stained by. Only the
## second answers "where is the protocol", so ACTA's repository is excluded by name -- otherwise
## every run would publish a link to ACTA as though it were the bench protocol.
##
## Returns NA when there is no URL. The field previously carried Info!Preparation_Protocol, whose
## value on every shipped workbook is the words "See references" -- a sentence, not a reference,
## and not on their menu either. NA is honest; a dangling instruction is not.
.tpProtocolUrl <- function(refs) {
  if (.tpBlank(refs)) return(NA_character_)
  parts <- trimws(strsplit(as.character(refs)[1], ";", fixed = TRUE)[[1]])
  urls  <- parts[grepl("^https?://", parts, ignore.case = TRUE)]
  if (!length(urls)) return(NA_character_)
  keep <- urls[!grepl("github[.]com/[^/]+/acta", urls, ignore.case = TRUE)]
  if (!length(keep)) return(NA_character_)
  keep[length(keep)]
}

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
  lc <- if (grepl("(κ|kappa|[^A-Za-z]k([^A-Za-z]|$))", v, ignore.case = TRUE, perl = TRUE)) "\u03ba"
        else if (grepl("(λ|lambda|[^A-Za-z]l([^A-Za-z]|$))", v, ignore.case = TRUE, perl = TRUE)) "\u03bb"
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
    "Collection Software"         = .tpAliasStrict(instrument, ACTA_TP_COLLECTION_SW),
    "Analysis Software"           = "ACTA",
    "Plug-ins, other analysis tools" = NA_character_,
    "References"                  = iv("References"),
    "Quality control measures"    = iv("Quality_Control_Measures"),
    "Antigen"                     = .tpAntigen(rv("Markername"), rv("Alias"), menus),
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
    "Target Species"              = iv("Target_Species", "Human"),
    ## FROM THE MASTER PANEL INVENTORY, by catalogue -- no second source and nothing retyped.
    ## The inventory's "Species / Isotype" is one free-text field; both targets here are menu
    ## driven, so each parser lands on a menu value or returns NA. A half-parsed isotype would
    ## read as a fact.
    "Antibody host species"       = .tpHostSpecies(.invIso, menus),
    "Isotype"                     = .tpIsotype(.invIso, menus),
    ## A NAMED CLONE IS A MONOCLONAL. Stated as an inference rather than left blank, because the
    ## clone is already published in the next column and the two would otherwise disagree by
    ## omission. No clone -> no claim.
    "Clonality"                   = if (.tpBlank(rv("Clone"))) NA_character_ else "monoclonal",
    "Supplier"                    = .tpSupplierMenu(.tpAlias(rv("Vendor"), ACTA_TP_VENDOR), menus),
    "Part#"                       = rv("Cat_Num"),
    "Lot#"                        = rv("Lot_Num"),
    "Concentration for this Lot#" = NA_character_,
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
    "Instrument Manufacturer"     = .tpAliasStrict(instrument, ACTA_TP_INSTRUMENT_MFR),
    ## THE MODEL, resolved through an alias table. A Bio-Rad ZE5 reports its SOFTWARE in $CYT, so
    ## `instrument` is the string "EVEREST" and this field published a software name where the
    ## menu wants a cytometer. `ZE5` is on their Instrumentation menu and `EVEREST` is not, so the
    ## old value also went to the off-menu overflow row on every ZE5 run. Falls back to the
    ## instrument string unchanged when no alias applies, which is right: an unrecognised
    ## cytometer is still a cytometer, and the reader can judge it.
    "Instrumentation"             = .tpAlias(instrument, ACTA_TP_INSTRUMENT_MODEL),
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
    "Tissue"                      = NA_character_,
    "Cell type"                   = .tpAlias(rv("Test_Material"), ACTA_TP_CELLTYPE),
    "Sample details"              = NA_character_,
    ## MENU VALUE OR NOTHING -- the maintainer's words. "See references" is not on their menu, so
    ## every run pushed it into the off-menu overflow row. Blank is the honest answer, and the URL
    ## now has a column of its own.
    "Preparation protocol"        = local({ v <- iv("Preparation_Protocol")
                                            if (.tpOnMenu(v, "Preparation protocol", menus)) v else NA_character_ }),
    ## NEW IN TEMPLATE v3.
    "Acquisition date"            = rv("Acquisition_date"),
    ## THE PROTOCOL'S URL, not the Preparation_Protocol cell. That cell holds a menu value (an
    ## isolation method) or the words "See references", and the maintainer asked for a URL here.
    ## Info!References is a semicolon-separated list whose entries are the analysis tool and the
    ## staining protocol; .tpProtocolUrl() takes the protocol one -- the last URL that is not
    ## ACTA's own repository -- and returns NA rather than guessing when there is no URL at all.
    "Protocol reference"          = .tpProtocolUrl(iv("References")),
    "Cell count"                  = if (is.na(cells)) NA_character_
                                    else format(cells * 1e6, scientific = FALSE, trim = TRUE),
    ## PER WELL FIRST, run-level second. Test_Material_Status is a Layout_Plate column, so a plate
    ## that titrated a frozen vial beside a fresh draw publishes the right answer for each reagent
    ## instead of one Info value for both. Info!Fresh_or_Frozen stays as the fallback and is what
    ## an export written before the column existed still resolves to, so no archived run changes.
    "Fresh or Frozen"             = { s <- .tpAlias(rv("Test_Material_Status"),
                                                    ACTA_TP_SAMPLE_STATUS)
                                      if (is.na(s)) iv("Fresh_or_Frozen") else s },
    "Live or Fixed"               = liveFixed,
    "Fixed after antibody incubation?" = fixedAfter)

  ## An unmapped Cell type falls back to the Info default rather than publishing a donor code.
  ## Test_Material routinely carries a donor identifier rather than a cell type, and routing the
  ## value through an alias table is what keeps those identifiers out of a public submission:
  ## only a value that resolves to one of their 22 menu options is ever emitted.
  ct <- v[["Cell type"]]
  if (!.tpBlank(ct) && is.na(match(tolower(.tpNorm(ct)), tolower(names(ACTA_TP_CELLTYPE)))) &&
      is.na(match(tolower(.tpNorm(ct)), tolower(unname(ACTA_TP_CELLTYPE)))))
    v[["Cell type"]] <- iv("Cell_Type_Fallback", NA_character_)

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
    if (grepl(pat, x)) x <- sub(pat, new, x) else
      x <- sub("</sheetData>", paste0(new, "</sheetData>"), x)
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
      r[["Marker_Alias"]]    <- tb$Antigen[i]      %||% ""
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
  fcsMap <- if (scrub && !is.null(files[["instructions"]]) &&
                file.exists(files[["instructions"]])) .tpScrubMap(files[["instructions"]])
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
        if (scrub && !isTRUE(.tpScrubWorkbook(dst, extra_repl = fcsExtra)))
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
  ## \Q...\E quotes the value, so a catalogue number or panel name full of regex syntax is still
  ## matched literally -- the property the old fixed = TRUE was there for.
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
      pat <- sprintf("<t([^>]*)>\\s*\\Q%s\\E\\s*</t>", .tpEsc(from))
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
.tpScrubMap <- function(path) {
  if (!file.exists(path)) return(list())
  tryCatch({
    lp <- suppressMessages(readxl::read_excel(path, sheet = "Layout_Plate", col_types = "text"))
    names(lp) <- actaStripHeaderAnnotation(names(lp))
    tgt <- intersect(c(names(ACTA_SCRUB_CONFIDENTIAL), "Costain_Panel_Iteration"), names(lp))
    repl <- list()
    for (cn in tgt) {
      v <- unique(trimws(stats::na.omit(as.character(lp[[cn]]))))
      v <- v[nzchar(v)]
      if (!length(v)) next
      if (identical(cn, "Costain_Panel_Iteration"))
        for (i in seq_along(v)) repl[[v[i]]] <- paste0(ACTA_SCRUB_ITERATION_PREFIX, i)
      else
        for (z in v) repl[[z]] <- ACTA_SCRUB_CONFIDENTIAL[[cn]]
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
    repl
  }, error = function(e) list())
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
.tpScrubWorkbook <- function(path, extra_repl = list()) {
  if (!file.exists(path)) return(invisible(FALSE))
  ok <- tryCatch({
    repl <- .tpScrubMap(path)
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
