\name{ACTA_GLYPH}
\alias{ACTA_GLYPH}
\docType{data}
\title{Status glyphs for the four check outcomes}
\description{
The characters ACTA uses to render a status record's outcome: a tick, a cross, an exclamation mark
and an en dash, named \code{pass}, \code{fail}, \code{warn} and \code{skip}.
}
\usage{ACTA_GLYPH}
\format{
A named character vector of length four, with names \code{"pass"}, \code{"fail"}, \code{"warn"}
and \code{"skip"}.
}
\details{
Four states, and never colour alone -- the glyph carries the outcome for anyone who cannot
distinguish the colours, and it survives being pasted into a plain-text log.

A skip is not a pass. A check whose input was absent gets \code{skip}, and must not be rendered as
though it had succeeded.
}
\seealso{\link{ACTA-package} for the status-record convention these label.}
\examples{
ACTA_GLYPH
ACTA_GLYPH[["skip"]]
}
\keyword{datasets}
