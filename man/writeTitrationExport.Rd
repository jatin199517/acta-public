\name{writeTitrationExport}
\alias{writeTitrationExport}
\title{Write the titration export as a formatted worksheet}
\description{
Write a titration results table to \code{.xlsx} with two colour families, so the block an analyst
reads and fills in is unmistakable against the derived data.
}
\usage{
writeTitrationExport(df, path, sheet = "TitrationExport", audit = NULL,
                     primary = ACTA_EXPORT_LEAD_COLS,
                     primary_palette = c(header = "#93398F", band = "#ECCFED"),
                     secondary_palette = c(header = "#1F4E79", band = "#DCE9F5"),
                     border_col = "#000000", font_name = "Aptos Narrow",
                     font_size = 12)
}
\arguments{
  \item{df}{the data frame to write.}
  \item{path}{the \code{.xlsx} to write. Overwritten if it exists.}
  \item{sheet}{name of the data worksheet.}
  \item{audit}{a named list of data frames, written as extra sheets \emph{after} the data sheet.}
  \item{primary}{names of the columns that get the primary colour family -- the block the operator
    reads and fills in. Everything else is secondary. Defaults to ACTA's own lead block, so a
    caller that re-emits an export does not silently lose it; pass \code{character()} for none.}
  \item{primary_palette, secondary_palette}{named character vectors with \code{header} and
    \code{band} hex colours.}
  \item{border_col}{hex colour for the thin border every cell gets.}
  \item{font_name, font_size}{applied to the whole sheet, not only to styled cells.}
}
\details{
All the hex values are arguments, so a shade that is off can be corrected at the call site rather
than by editing this function. Headers are white bold text on the dark colour and values are black
on the light band, so both clear the WCAG AA 4.5:1 contrast floor. Every cell, header and body in
both families, gets the same thin border and is centred horizontally and vertically -- the borders
deliberately do not follow the block colour.

The header row is frozen and filterable, per-column widths are capped at 40 so a long multi-line
label cannot blow the sheet out, and \code{Date} columns are given an explicit \code{yyyy-mm-dd}
number format, which Excel would otherwise render per locale.

Values are untouched: this is presentation only. Columns that are entirely \code{NA} -- the
operator-fillable titer fields, typically -- come out as genuinely empty cells, still styled.

\code{font_name} defaults to \code{"Aptos Narrow"}. Excel shows the Office body font as
"Aptos Narrow (Body)"; the name written into the file is the plain one, since "(Body)" is only
Excel's label for the theme slot. If the font is not installed, Excel substitutes silently.

The audit sheets are verbatim copies of the instructions workbook, carried along so the export is
self-contained: whoever reads it a year later can see the plate map, the gating template and the
run settings that produced these numbers without needing the original file. Nothing reads them
back, and they are deliberately not styled like the data sheet so nobody mistakes them for output.
Sheet names are capped at the format's 31 characters.
}
\value{
\code{path}, invisibly.
}
\note{
The workbook is repaired at the OPC level after \pkg{openxlsx} saves it. Do not round-trip an ACTA
workbook through \code{openxlsx::loadWorkbook()} and \code{saveWorkbook()} to make a small edit --
that corrupts cells in sheets you never touched.
}
\examples{
f <- tempfile(fileext = ".xlsx")
writeTitrationExport(
  data.frame(Marker = c("CD3", "CD4"), StainQty = c(0.5, 1), SI = c(12.1, 18.4)),
  f, primary = "Marker")
file.exists(f)
unlink(f)
}
