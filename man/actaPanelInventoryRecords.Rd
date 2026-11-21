\name{actaPanelInventoryRecords}
\alias{actaPanelInventoryRecords}
\title{Master panel inventory pre-flight}
\description{
Answer, before a run starts, which master panel inventory the run is going to open, where that
choice came from, and whether the co-stain panels the plate declares are actually in it.
}
\usage{
actaPanelInventoryRecords(info = NULL, version_dir = NULL, code_dir = NULL,
                          iterations = character(0), override = NULL)
}
\arguments{
  \item{info}{the \code{Info} sheet as a data frame, or \code{NULL}. Read for
    \code{master_panel_inventory_path} and for \code{titerpubs_export}.}
  \item{version_dir}{the run folder. An inventory sitting in it is used when the workbook names
    none, and a relative path resolves against it.}
  \item{code_dir}{the folder holding the pipeline, used to find the shipped example inventory.}
  \item{iterations}{the \code{Costain_Panel_Iteration} values the plate declares, as they are
    written in \code{Layout_Plate}. One record is returned per distinct value.}
  \item{override}{an inventory chosen for this run, outranking the workbook. \code{NULL} for none,
    in which case \code{getOption("acta.panel_inventory_override")} is consulted instead.}
}
\details{
The file is resolved by \code{actaPanelInventoryPath()} and the per-iteration wording comes from
\code{actaPanelIterationCaption()}, so this pre-flight and the report's panel table cannot
disagree about the same run.

No record is ever a \code{"fail"}. The master panel inventory is provenance, not QC: a mismatch is
reported in the report's table and changes no gate, no number and no artefact, so blocking a run
over one would be the wrong trade. The hardest verdict is \code{"warn"}.

Severity is scaled by what is at stake. An unresolved inventory costs nothing on a plate that
declares no co-stain panel and builds no TiterPubs bundle, and costs a submission its reagent
provenance on one that does -- so the resolution record is amber only in the second case. The
shipped example inventory is treated the same way: it is a fallback nobody chose, and reporting it
as a pass is what once let a report describe a lab's real panel as unknown.

An inventory that resolves but cannot be read is reported separately from one that does not
resolve at all. That case is otherwise silent: the path is shown, the run skips the table, and
nothing says why.
}
\value{
A list of status records (see \link{ACTA-package}), the first with \code{id} \code{"panel_inventory"}
and one per declared iteration after it. The resolved path, its provenance tag and that tag's
human label ride on the returned list as the attributes \code{"acta_inventory_path"},
\code{"acta_inventory_source"} and \code{"acta_inventory_label"}, so a caller can render the
resolved file and the check rows from a single resolution.
}
\examples{
## No workbook and no run folder: nothing is chosen, so the shipped example is reported
## if one is installed, and otherwise the absence is.
r <- actaPanelInventoryRecords()
r[[1]]$status
}
