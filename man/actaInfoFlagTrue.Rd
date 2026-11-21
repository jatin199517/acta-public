\name{actaInfoFlagTrue}
\alias{actaInfoFlagTrue}
\title{Is an Info sheet flag set?}
\description{
Read a workbook flag cell as a logical, accepting every spelling an analyst produces.
}
\usage{
actaInfoFlagTrue(x)
}
\arguments{
  \item{x}{the cell's value, or \code{NA}.}
}
\details{
Excel round-trips a logical column badly -- one text cell turns the whole column to character --
so the flags on the \code{Info} sheet arrive as \code{TRUE}, \code{T}, \code{YES} or \code{1}
depending on who filled the sheet in and how it was saved. This is that test, written once.

\strong{It is exported because the Shiny app needs it.} The app is loaded by
\code{shiny::runApp}, calls \code{library(ACTA)}, and then calls helpers by bare name, so anything
it uses must be exported -- an unexported one is simply not there, and the app stops with
\dQuote{could not find function}. That is not hypothetical: this function reached the app in
3.6.12 without being added here, and the app failed on the first workbook that set
\code{titerpubs_export}. \code{tests/legacy/test_app_exports.R} now parses the app and asserts
every bare-name \code{acta*} call it makes is on the export list.
}
\value{
\code{TRUE} when the value is one of \code{TRUE}, \code{T}, \code{YES}, \code{1} (case- and
whitespace-insensitive); \code{FALSE} for anything else, including \code{NA} and an absent cell.
Never \code{NA}, so a caller can use it directly in \code{if ()}.
}
\examples{
actaInfoFlagTrue("TRUE")
actaInfoFlagTrue(" yes ")
actaInfoFlagTrue(NA)        # FALSE, not NA
actaInfoFlagTrue("FALSE")
}
