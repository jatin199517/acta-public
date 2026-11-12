## Set individual cells in one worksheet of an .xlsx, at the XML level.
##
## Companion to add_layout_col.py, and here for the same reason: openxlsx's load/save round-trip
## rewrites the whole workbook and mangles shared strings carrying xml:space="preserve". This
## touches one sheet and copies every other part through byte for byte.
##
## IN THE REPO ON PURPOSE. This and the workbook migration were written in a scratch directory
## the first time, and the shipped workbooks were generated with them -- which meant that when
## /tmp was cleared, the only way to regenerate a shipped workbook was gone. Anything that
## produces a tracked artefact has to be tracked itself.
##
## Usage:
##   Rscript set_workbook_cells.R <file.xlsx> <SheetName> <rows> <spec>
##     rows  comma-separated sheet row numbers, e.g. 2,3,4
##     spec  newline-separated COL=value, e.g. $'J=BD\nK=554061\nL='
##           an empty value DELETES the cell, so the sheet reads back as NA
args  <- commandArgs(TRUE)
path  <- args[1]
sheet <- args[2]
rows  <- as.integer(strsplit(args[3], ",")[[1]])
spec  <- lapply(strsplit(args[4], "\n", fixed = TRUE)[[1]], function(s) {
  i <- regexpr("=", s, fixed = TRUE); c(substr(s, 1, i - 1), substring(s, i + 1))
})

col_index <- function(L) { n <- 0L
  for (ch in strsplit(L, "")[[1]]) n <- n * 26L + (utf8ToInt(ch) - 64L)
  n }
esc <- function(x) { x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE)
                     x <- gsub(">","&gt;",x,fixed=TRUE); gsub('"',"&quot;",x,fixed=TRUE) }

## A BACKSLASH IN A sub() REPLACEMENT IS SYNTAX, NOT TEXT.
##
## Every write below goes through sub(pattern, new, row) with the CELL VALUE inside `new`. In an R
## replacement string \1-\9 are backreferences and \\ is one literal backslash, so a value
## carrying a backslash is silently rewritten: MEASURED on a canary cell, `a\Ed` was stored as
## `aEd` -- the backslash and the E both gone, no error, every structural check clean.
##
## This is the SAME defect .tpApplyRepl() already fixes for the scrub (it doubles backslashes
## before using a value as a replacement), reached here by a different route. It matters because
## this tool is how the SHIPPED workbooks are edited -- a silent character loss here is a wrong
## value in a published artefact, which is the whole class of fault this folder exists to catch.
repEsc <- function(x) gsub("\\\\", "\\\\\\\\", x)

tmp <- file.path(tempdir(), paste0("sc_", basename(tempfile(""))))
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

n <- 0L
for (r in rows) {
  rpat <- sprintf('<row[^>]*r="%d"[^>]*>.*?</row>', r)
  m <- regmatches(x, regexpr(rpat, x))
  if (!length(m)) { cat("  no row", r, "\n"); next }
  row <- m
  for (s in spec) {
    col <- s[1]; val <- if (length(s) > 1) s[2] else ""
    ref <- paste0(col, r)
    cpat <- sprintf('<c r="%s"[^>]*(/>|>.*?</c>)', ref)
    cur  <- regmatches(row, regexpr(cpat, row))
    sty  <- if (length(cur) && grepl('\\ss="', cur)) sub('.*\\ss="([0-9]+)".*', "\\1", cur) else NULL
    new  <- if (!nzchar(val)) "" else
      sprintf('<c r="%s"%s t="inlineStr"><is><t>%s</t></is></c>',
              ref, if (is.null(sty)) "" else sprintf(' s="%s"', sty), esc(val))
    if (length(cur)) { row <- sub(cpat, repEsc(new), row); n <- n + 1L }
    else if (nzchar(val)) {
      ## INSERTED IN COLUMN ORDER, NOT APPENDED.
      ##
      ## Cells within a <row> must appear in ascending column order -- a hard OOXML rule. A blank
      ## value is written as NO CELL AT ALL, so setting one later finds nothing to replace; an
      ## earlier version appended before </row> and produced a row ordered A,B,D,C. Every
      ## structural check passed -- the XML parsed, the package was well formed, no part was
      ## missing -- and Excel offered to repair the file on open. Gated now by the "every row
      ## ascends" assertion in test_workbook_package.R.
      nb <- regmatches(row, gregexpr('<c r="[A-Z]+[0-9]+"', row))[[1]]
      after <- NA_character_
      for (c0 in nb) {
        L <- sub('<c r="([A-Z]+)[0-9]+"', "\\1", c0)
        if (col_index(L) > col_index(col)) { after <- c0; break }
      }
      row <- if (is.na(after)) sub("</row>", repEsc(paste0(new, "</row>")), row)
             else sub(after, paste0(new, after), row, fixed = TRUE)
      n <- n + 1L
    }
  }
  x <- sub(rpat, repEsc(row), x)
}
writeLines(x, spath, useBytes = TRUE)

## An Override naming a part that is no longer in the archive is what makes Excel offer a repair.
ctf <- file.path(tmp, "[Content_Types].xml")
ct  <- paste(readLines(ctf, warn = FALSE), collapse = "\n")
for (el in regmatches(ct, gregexpr("<Override[^>]*?/>", ct))[[1]]) {
  part <- sub('.*\\sPartName="/([^"]*)".*', "\\1", el)
  if (identical(part, el) || part %in% parts) next
  ct <- sub(el, "", ct, fixed = TRUE)
}
writeLines(ct, ctf, useBytes = TRUE)

old <- setwd(tmp); utils::zip(zipfile = "out.xlsx", files = parts, flags = "-Xq9"); setwd(old)
stopifnot(file.copy(file.path(tmp, "out.xlsx"), path, overwrite = TRUE))
unlink(tmp, recursive = TRUE)
cat("set", n, "cell(s) in", sheet, "of", basename(path), "\n")
