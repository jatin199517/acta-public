\name{actaTiterOptions}
\alias{actaTiterOptions}
\title{The two recommended titers per reagent}
\description{
Derive the two titer recommendations ACTA offers for each titration group: the stain quantity with
the highest observed Stain Index, and the quantity at 90 percent of the fitted Michaelis-Menten
\code{Vmax}.
}
\usage{
actaTiterOptions(mmFits, stats, r2_threshold = 0.9)
}
\arguments{
  \item{mmFits}{the Michaelis-Menten fit table from a run, one row per group.}
  \item{stats}{the per-sample statistics table from a run.}
  \item{r2_threshold}{the smallest fit R-squared that will still support the 90 percent
    \code{Vmax} recommendation.}
}
\details{
The 90 percent \code{Vmax} option is \emph{disabled}, with the reason shown, whenever the fit
cannot support it. Michaelis-Menten is a saturating isotherm, so a Stain Index curve that turns
over cannot be represented by one: \code{Vmax} is then pushed above every observed Stain Index and
9\code{Km} lands outside the doses actually tested. Handing that back as a one-click titer would
recommend a concentration nobody measured.

Three reasons disable it, in this order: no usable fit at all, a fit R-squared below
\code{r2_threshold}, or a result outside the tested range of stain quantities.
}
\value{
A list, one element per row of \code{mmFits}, each with \code{dirname} and two option lists
\code{maxsi} and \code{vmax90}. Each option carries \code{value}, \code{enabled} and a display
\code{label}; \code{vmax90} also carries \code{reason} (\code{NA} when enabled) and \code{r2}. A
disabled option's label states why, so a UI never has to construct that sentence itself.
}
\seealso{\code{\link{run_acta}}}
