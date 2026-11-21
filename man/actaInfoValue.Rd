\name{actaInfoValue}
\alias{actaInfoValue}
\title{One setting from the instructions workbook's Info sheet}
\description{
Look up a single setting on the \code{Info} sheet, in either of the two layouts the sheet has had,
and return it as a character scalar or \code{NA}.
}
\usage{
actaInfoValue(info, field)
}
\arguments{
  \item{info}{the \code{Info} sheet as read from the workbook, long or wide.}
  \item{field}{the setting's name, without any annotation.}
}
\details{
The sheet has two shapes and this reader accepts both. A \emph{wide} sheet is one row with a column
per setting; a \emph{long} sheet -- the one the template has shipped since 3.6.0 -- is
\code{Category | Variable | Value | Description}, one row per setting. A long sheet is pivoted
first, so a caller reads either layout without knowing which it got.

Matching is case-insensitive, and tolerates a trailing header annotation: the template writes
\code{miflowcyt_export (L)}, so a caller asking for \code{miflowcyt_export} must still find it. The
exact name is tried first and the annotation-stripped name second, so a literal header always wins
and the fallback can never shadow a real column.

\strong{This is the only Info reader in the package, deliberately.} The Shiny app matched \code{field}
against the sheet's column names directly, which is correct for a wide sheet only; when the sheet
became long in 3.6.0 that reader began returning \code{NA} for every setting, and kept doing so
through 3.6.11. Nothing failed, because \code{NA} is also the honest answer for a setting nobody
filled in -- the app simply showed defaults where the workbook had values. It is exported so the app
can delegate here rather than carry a second implementation.
}
\value{
The setting as a character scalar, or \code{NA_character_} when the sheet is absent or empty, the
setting is not present, its cell is blank, or its cell holds the literal text \code{"NA"} -- a blank
somebody typed, not a value.
}
\examples{
info <- data.frame(Category = c("General", "General"),
                   Variable = c("Downsample_to (N)", "miflowcyt_export (L)"),
                   Value    = c("10000", "FALSE"),
                   stringsAsFactors = FALSE)

## The annotation in the sheet does not reach the caller:
actaInfoValue(info, "Downsample_to")

## A setting that is not there is NA, not an error:
actaInfoValue(info, "no_such_setting")
}
