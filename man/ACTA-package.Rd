% THESE .Rd FILES ARE HAND-WRITTEN. DO NOT RUN roxygen2 ON THIS PACKAGE.
% NAMESPACE is hand-written too, and it carries four import() directives whose absence is
% SILENTLY WRONG rather than merely broken: without import(flowCore), a bare colnames(flowSet)
% inside the namespace resolves to base::colnames, does not dispatch, returns NULL, and the run
% builds a 0 x 0 compensation matrix before dying somewhere else entirely. There are no @export or
% @import tags anywhere in R/, so roxygen2::roxygenise() would regenerate NAMESPACE with neither
% the exports nor the imports and reintroduce exactly that fault. See the header of NAMESPACE.
\name{ACTA-package}
\alias{ACTA-package}
\alias{ACTA}
\docType{package}
\title{Anybody Can Titrate Antibody: automated flow cytometry reagent titration}
\description{
An R workflow for analysing flow cytometry reagent titration experiments. One run reads a folder
of FCS files plus a single Excel instructions workbook, gates positive and negative populations
with \pkg{openCyto}, and reports a Stain Index per stain quantity together with a
Michaelis-Menten fit, a PDF report, and an HTML dashboard across runs.

"Reagent" covers antibodies and non-antibody stains alike, including fluorophore-conjugated
streptavidin, viability dyes and peptide-MHC multimers.
}
\details{
There are three supported ways in:
\describe{
  \item{\code{\link{acta_app}()}}{the desktop (Shiny) app, and the usual entry point.}
  \item{\code{\link{run_acta}()}}{one analysis run, from the console or a script.}
  \item{\code{\link{actaOQRun}()}}{a shipped diagnostic case, for installation qualification.}
}

The exported surface is small on purpose. The analysis pipeline and the report are sourced by
\code{run_acta()} into an environment parented on the package namespace, so they reach every
internal helper by lexical scoping and need nothing exported. What is exported is what the Shiny
app calls by bare name after \code{library(ACTA)}, plus the two documented entry points above.
The remaining exports are therefore mostly pre-flight and diagnostic helpers: they exist so the
app can show an operator what is wrong with a workbook, a folder or a machine \emph{before} a run
starts, and they are documented here because they are reachable, not because a typical analysis
calls them directly.

Two conventions run through those helpers:
\describe{
  \item{Records}{Many of them return a list of \emph{status records} rather than throwing. Each
    record is a list with \code{id} (a stable key), \code{label} (what was checked, in the
    analyst's terms), \code{status} (one of \code{"pass"}, \code{"fail"}, \code{"warn"},
    \code{"skip"}), \code{detail} and \code{where} (the sheet or folder to go and edit). A
    renderer can show passes and skips, which a \code{stop()} cannot carry.}
  \item{A skip is not a pass}{A check whose input was absent reports \code{"skip"}, and must never
    be rendered as though it had succeeded.}
}
}
\section{Code folder and working folder}{
\code{code_dir} holds the pipeline script, the report \code{.Rmd} and the dashboard generator;
\code{version_dir} (or \code{work_dir}) holds the instructions workbook, \code{Titration_FCS/}
and everything a run writes. They were one folder before ACTA became a package. Keeping them
separate is what lets an installed, read-only package library supply the code while the operator's
own folder takes the outputs.
}
\references{
Stain Index as defined by Maecker, Frey, Nomura and Trotter (2004), \emph{Selecting fluorochrome
conjugates for maximum sensitivity}, Cytometry Part A 62(2):169-173.
}
% NO \author{} BLOCK, AND THAT IS DELIBERATE -- DO NOT ADD ONE BACK.
% The first draft of this page carried the maintainer's name and address and the copyright holder,
% and verify_oq_sanitised.R flagged both the moment man/ became tracked. It was right to. The
% maintainer address is allowed in exactly ONE file -- DESCRIPTION, where R requires it for the
% `cre` role and where the allowance is declared, masked and PRINTED on every run -- and the
% copyright holder in the short list the private token file names. Widening either allowance to
% cover a help page would have bought a reader nothing: both facts already ship in DESCRIPTION and
% are one call away.
%     maintainer("ACTA")            packageDescription("ACTA")$Author
% R does not require \author in an -package.Rd, and R CMD check is clean without it.
\seealso{
\url{https://github.com/jatin199517/acta-public}
}
\keyword{package}
