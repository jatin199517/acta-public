\name{gate_manual}
\alias{gate_manual}
\title{openCyto gating plugin for pre-drawn gates}
\description{
An \pkg{openCyto} gating method that returns the gate already drawn for the sample being gated.
Never interactive.
}
\usage{
gate_manual(fr, pp_res, channels, \dots)
}
\arguments{
  \item{fr}{the \code{\link[flowCore]{flowFrame}} being gated. Its \code{identifier()} is the
    cache key.}
  \item{pp_res}{preprocessing result, as passed by \pkg{openCyto}. Unused.}
  \item{channels}{the resolved channel names, as passed by \pkg{openCyto}. Unused: the cached gate
    already carries its own dimensions.}
  \item{\dots}{further arguments from the gating template row. Unused.}
}
\details{
Registered with \pkg{openCyto} as the gating method \code{"gate_manual"}, so a
\code{gating_template} row can name it like any other. The gates themselves are drawn beforehand
with \pkg{flowGate}, persisted as \code{.rds} and reused on re-run, and the scope of a drawn gate
is set by the marker row's \code{groupBy} column: blank means one gate per well,
\code{groupBy = "Dirname"} means one gate for the whole reagent, drawn on the brightest well of
the group and shared by the rest.

This function only reads that cache. An interactive session is needed to \emph{draw} a gate, never
to apply one, which is what lets a drawn-gate analysis re-run unattended.
}
\value{
The cached gate object for \code{fr}. Stops if no gates have been drawn, or if none was cached for
this sample -- naming the sample in both cases.
}
\seealso{\code{\link[openCyto]{register_plugins}}}
\examples{
\dontrun{
## In the gating_template sheet:
##   alias  pop  parent  dims        gating_method  groupBy
##   Cells  +    root    FSC-A,SSC-A gate_manual    Dirname
}
}
