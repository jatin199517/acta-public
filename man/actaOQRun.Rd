\name{actaOQRun}
\alias{actaOQRun}
\alias{actaOQStageOutOfLibrary}
\title{Run an ACTA diagnostic (installation qualification) case}
\description{
Run one of the shipped diagnostic cases end to end and check every artefact it produces against
the case's recorded expectations. \code{actaOQStageOutOfLibrary()} is the guard that keeps such a
run from writing inside an R library.
}
\usage{
actaOQRun(oq_dir, quiet = TRUE, progress = function(...) invisible(),
          code_dir = NULL, work_dir = NULL)

actaOQStageOutOfLibrary(oq_dir, dest_parent = file.path(tempdir(), "acta_oq"))
}
\arguments{
  \item{oq_dir}{the case folder, e.g.
    \code{system.file("extdata", "oq_small", "OQ_Test1", package = "ACTA")}. Must exist.}
  \item{quiet}{suppress the pipeline's progress messages.}
  \item{progress}{a function called with a status string as the run advances. The default discards
    them; the app passes one that updates its panel.}
  \item{code_dir}{folder holding the pipeline code. \code{NULL} means use the working version's,
    which is the point: a case carries only its inputs -- layout, FCS, \code{expected.json} -- and
    can never be running a stale copy of the pipeline.}
  \item{work_dir}{where to run. \code{NULL} runs in the case folder (or its staged copy).}
  \item{dest_parent}{where to stage a case that sits inside an R library.}
}
\details{
A diagnostic run deletes and rewrites \code{oq_dir/Outputs}. On a group-writable library that
crosses a user boundary, so \code{actaOQRun()} calls \code{actaOQStageOutOfLibrary()} first and
runs a copy when the case is inside one.

\code{actaOQRun()} warns whenever \code{validated_packages.tsv} is missing beside the case,
staged or not. It used to warn only on the staging path, so the case that matters in practice --
one already sitting in an operator's \code{Diagnostics/} folder without the sibling \code{.tsv} --
recorded the package-version check as \code{skip} while the verdict still read
\code{"PASS (with notes)"}, and nothing said the check had stopped comparing.
}
\value{
\code{actaOQRun()} returns a list: \code{ok} (all checks passed), \code{verdict} (one of
\code{"PASS"}, \code{"PASS (with notes)"}, \code{"FAIL"}), \code{recs} (the status records, one
per check), \code{log} (path to the written \code{log.txt}), \code{out_dir} and \code{elapsed_s}.

\code{actaOQStageOutOfLibrary()} returns a list: \code{dir} (the folder to run in -- the input
unchanged, or the copy), \code{staged} (whether it copied), and \code{baseline} (whether
\code{validated_packages.tsv} is available beside the folder named in \code{dir}). \code{baseline}
is reported rather than assumed even when nothing was staged, so a caller trusting the field gets
the truth about the folder it is actually going to run.
}
\seealso{\code{\link{run_acta}}, \code{\link{actaDependencyChecks}}}
\examples{
## The shipped cases:
list.files(system.file("extdata", "oq_small", package = "ACTA"))

\dontrun{
## A full case takes a minute or two and needs LaTeX for the report step.
res <- actaOQRun(system.file("extdata", "oq_small", "OQ_Test1", package = "ACTA"))
res$verdict
readLines(res$log)
}
}
