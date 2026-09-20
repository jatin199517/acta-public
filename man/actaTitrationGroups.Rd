\name{actaTitrationGroups}
\alias{actaTitrationGroups}
\title{One row per titration series}
\description{
Turn the long-form \code{Layout_Plate} rows into the pipeline's unit of work: one row per titrated
marker, not one per FCS folder.
}
\usage{
actaTitrationGroups(metadata, alias_col = "Alias", channel_col = NULL)
}
\arguments{
  \item{metadata}{the \code{Layout_Plate} sheet as a data frame. Must have a \code{Dirname}
    column.}
  \item{alias_col}{name of the alias column. A legacy sheet may not have one, and a blank alias on
    an otherwise good row is a partially filled workbook rather than an error; both fall back to
    the \code{Dirname}, which is what the pipeline did before aliases existed.}
  \item{channel_col}{name of the channel column, or \code{NULL} to resolve it.}
}
\details{
\code{Dirname} has always meant two things at once: \emph{where} the events live -- the
\code{Titration_FCS} subfolder -- and \emph{which} titration series this is. Combinatorial
titration separates them, because several markers can be titrated in the same wells, so one
\code{Dirname} carries several series, each with its own marker, channel, reagent metadata and
stain quantities. The layout expresses that in long form: one row per well per titrated marker,
same \code{Dirname}, different marker, channel, alias and stain quantity.

The key is the \code{Dirname} whenever that is unambiguous, which is every workbook written before
combinatorial support existed. That is deliberate rather than merely conservative: the key names
the plot files, keys the QC list and labels the dashboard, and the alias is not interchangeable
with the \code{Dirname} even now -- a diagnostic case has \code{Dirname} "Strep_PE" with alias
"Strep". Keying on the alias unconditionally would rename that case's artefacts and move a
baseline for no reason. One alias in a \code{Dirname}, and the key is the \code{Dirname},
byte-identical to before; several, and the key is the alias, because that is what distinguishes
them.

Order is first appearance, not sorted, and that is load-bearing: it decides the report's section
order, the row order of the exports, and which group's representative FCS supplies the MiFlowCyt
detector-voltage table. Sorting alphabetically reordered a diagnostic case's groups, which the
diagnostic run caught. A user orders the sheet deliberately, so it is honoured.
}
\value{
A data frame of \code{dirname}, \code{alias} and \code{key}, one row per titration, with the
attribute \code{"combinatorial"} set to \code{TRUE} when any \code{Dirname} carries more than one
series.
}
\seealso{\code{\link{actaPrevalidateRecords}}}
\examples{
md <- data.frame(Dirname = c("CD3_BV421", "CD3_BV421", "Strep_PE"),
                 Alias   = c("CD3", "CD8", "Strep"))
g <- actaTitrationGroups(md)
g
attr(g, "combinatorial")   # TRUE: CD3_BV421 carries two series
}
