## Insert a row into one worksheet of an .xlsx, at the XML level.
##
## Companion to set_workbook_cells.R, which only SETS cells in rows that already exist. Adding a
## gating_template row needed a new row, and the reason not to reach for openxlsx is the same one
## that script gives: its load/save round-trip rewrites the whole workbook and mangles shared
## strings carrying xml:space="preserve". This touches one sheet and copies every other part
## through byte for byte.
##
## IN THE REPO ON PURPOSE, for the reason set_workbook_cells.R gives: anything that produces a
## tracked artefact has to be tracked itself, or the only way to regenerate a shipped workbook
## disappears with the scratch directory it was written in.
##
## ONLY SAFE ON A SHEET WITH NOTHING ELSE THAT COUNTS ROWS. Inserting shifts every row below it,
## and a merge range, data validation, conditional format, hyperlink, autoFilter, table part or
## defined name that spans the insertion point would all still point at the old numbers. This
## script REFUSES such a sheet rather than silently corrupting it -- gating_template is plain
## sheetData and passes; Layout_Plate carries a table and does not.
##
## Usage:
##   Rscript insert_workbook_rows.R <file.xlsx> <SheetName> <afterRow> <spec>
##     afterRow  the existing sheet row the new row is placed AFTER (0 = before row 1). Its
##               row attributes and per-column cell styles are copied, so the new row matches.
##     spec      newline-separated COL=value, e.g. $'A=#Reagent\nB=+/-'
##               a column left out of the spec becomes no cell at all, i.e. reads back as NA
args  <- commandArgs(TRUE)
path  <- args[1]
sheet <- args[2]
after <- as.integer(args[3])
spec  <- lapply(strsplit(args[4], "\n", fixed = TRUE)[[1]], function(s) {
  i <- regexpr("=", s, fixed = TRUE); c(substr(s, 1, i - 1), substring(s, i + 1))
})

col_index <- function(L) { n <- 0L
  for (ch in strsplit(L, "")[[1]]) n <- n * 26L + (utf8ToInt(ch) - 64L)
  n }
esc <- function(x) { x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE)
                     x <- gsub(">","&gt;",x,fixed=TRUE); gsub('"',"&quot;",x,fixed=TRUE) }

tmp <- file.path(tempdir(), paste0("ir_", basename(tempfile(""))))
dir.create(tmp, recursive = TRUE)
parts <- utils::unzip(path, list = TRUE)$Name
## The SharePoint sync client injects [trash]/NNNN.dat into any xlsx in a synced library, within
## seconds. They are never content, and carrying them through makes the package malformed.
trash <- grep("^\\[trash\\]/", parts, value = TRUE)
if (length(trash)) cat("  dropping", length(trash), "injected [trash] part(s)\n")
parts <- setdiff(parts, trash)
utils::unzip(path, exdir = tmp, setTimes = TRUE)

rdf <- function(f) paste(readLines(file.path(tmp, f), warn = FALSE), collapse = "\n")
wb  <- rdf("xl/workbook.xml")
sh  <- regmatches(wb, gregexpr('<sheet [^>]*/?>', wb))[[1]]
nms <- vapply(sh, function(s) sub('.*\\sname="([^"]*)".*', "\\1", s), "")
ii  <- match(sheet, nms); stopifnot(!is.na(ii))
rid <- sub('.*\\sr:id="([^"]*)".*', "\\1", sh[ii])
rels <- rdf("xl/_rels/workbook.xml.rels")
tgt  <- sub(sprintf('.*<Relationship Id="%s"[^>]*Target="([^"]*)".*', rid), "\\1", rels)
spath <- file.path(tmp, "xl", tgt)
x <- paste(readLines(spath, warn = FALSE), collapse = "\n")

## REFUSE anything that would still point at the pre-insert numbering. See the header.
for (tag in c("mergeCell", "dataValidation", "conditionalFormatting", "hyperlink",
              "autoFilter", "tableParts"))
  if (grepl(sprintf("<%s[ />]", tag), x))
    stop(sprintf("insert_workbook_rows: sheet '%s' contains <%s>, which counts rows. Shift it by hand.",
                 sheet, tag), call. = FALSE)
if (grepl(sprintf("'?%s'?!", sheet), rdf("xl/workbook.xml"), fixed = FALSE) &&
    grepl("<definedNames>", wb) &&
    grepl(sprintf("<definedName[^>]*>[^<]*'?%s'?!", sheet), wb))
  stop(sprintf("insert_workbook_rows: a definedName refers to '%s'. Shift it by hand.", sheet),
       call. = FALSE)

## Split sheetData into whole <row> blocks. Both forms occur: <row .../> and <row ...>...</row>.
rx   <- '<row[^>]*(/>|>.*?</row>)'
rows <- regmatches(x, gregexpr(rx, x))[[1]]
rnum <- as.integer(sub('.*?\\sr="([0-9]+)".*', "\\1", substr(rows, 1, 40)))
stopifnot(!any(is.na(rnum)))
src <- match(after, rnum)
if (after > 0L && is.na(src))
  stop(sprintf("insert_workbook_rows: no row %d in '%s'.", after, sheet), call. = FALSE)

## Build the new row from the source row's attributes and per-column styles, so it matches.
attrs <- if (is.na(src)) "" else
  sub('^<row\\s*r="[0-9]+"\\s*', "", sub('(/>|>).*$', "", rows[src]))
style_of <- function(col) {
  if (is.na(src)) return(NULL)
  cur <- regmatches(rows[src], regexpr(sprintf('<c r="%s%d"[^>]*', col, after), rows[src]))
  if (length(cur) && grepl('\\ss="', cur)) sub('.*\\ss="([0-9]+)".*', "\\1", cur) else NULL
}
newr <- after + 1L
spec <- spec[order(vapply(spec, function(s) col_index(s[1]), 0L))]   ## OOXML: cells must ascend
cells <- vapply(spec, function(s) {
  col <- s[1]; val <- if (length(s) > 1) s[2] else ""
  if (!nzchar(val)) return("")
  sty <- style_of(col)
  sprintf('<c r="%s%d"%s t="inlineStr"><is><t>%s</t></is></c>',
          col, newr, if (is.null(sty)) "" else sprintf(' s="%s"', sty), esc(val))
}, "")
new_row <- sprintf('<row r="%d"%s>%s</row>', newr,
                   if (nzchar(attrs)) paste0(" ", trimws(attrs)) else "",
                   paste(cells, collapse = ""))

## Renumber everything at or below the insertion point, then splice the new row in.
bump <- function(blk, from, to) {
  blk <- sub(sprintf('(<row[^>]*\\sr=")%d(")', from), sprintf("\\1%d\\2", to), blk)
  gsub(sprintf('(<c r="[A-Z]+)%d(")', from), sprintf("\\1%d\\2", to), blk)
}
out <- character(0)
for (i in seq_along(rows)) {
  blk <- if (rnum[i] > after) bump(rows[i], rnum[i], rnum[i] + 1L) else rows[i]
  out <- c(out, blk)
  if (rnum[i] == after) out <- c(out, new_row)
}
if (after == 0L) out <- c(new_row, out)
## Rebuild sheetData from the blocks rather than patching in place: an in-place pass would
## renumber row 12 to 13 and then find the ORIGINAL row 13 again on the next iteration.
x <- sub(paste0("<sheetData>.*</sheetData>"),
         paste0("<sheetData>", paste(out, collapse = ""), "</sheetData>"), x)

## <dimension> is the sheet's used range; stale last-row leaves Excel recalculating it, but a
## range naming a row that no longer bounds the data is the kind of mismatch that prompts repair.
dim_cur <- regmatches(x, regexpr('<dimension ref="[^"]*"/>', x))
if (length(dim_cur) && grepl(":", dim_cur)) {
  last <- max(c(rnum, newr) + ifelse(c(rnum, newr) > after, 1L, 0L))
  x <- sub('(<dimension ref="[A-Z]+[0-9]+:[A-Z]+)[0-9]+("/>)', sprintf("\\1%d\\2", last), x)
}
writeLines(x, spath, useBytes = TRUE)

## An Override naming a part that is no longer in the archive is what makes Excel offer a repair.
## WRITTEN BACK ONLY IF AN OVERRIDE ACTUALLY WENT. readLines/writeLines normalises the CRLF after
## the XML declaration to LF, so rewriting unconditionally puts this part in the diff of every
## run for no reason -- and the point of editing at the XML level is that a reviewer can see
## exactly one part changed.
ctf <- file.path(tmp, "[Content_Types].xml")
ct0 <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
ct  <- ct0
for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
  part <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
  if (identical(part, el) || part %in% parts) next
  ct <- sub(el, "", ct, fixed = TRUE)
}
if (!identical(ct, ct0)) writeLines(ct, ctf, useBytes = TRUE)

old <- setwd(tmp); utils::zip(zipfile = "out.xlsx", files = parts, flags = "-Xq9"); setwd(old)
stopifnot(file.copy(file.path(tmp, "out.xlsx"), path, overwrite = TRUE))
unlink(tmp, recursive = TRUE)
cat("inserted row", newr, "in", sheet, "of", basename(path), "\n")
