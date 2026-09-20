\name{actaValidateDashboardTemplate}
\alias{actaValidateDashboardTemplate}
\title{Check a dashboard HTML template against its contract}
\description{
Validate the HTML template the dashboard generator substitutes data into, before a run relies on
it.
}
\usage{
actaValidateDashboardTemplate(path)
}
\arguments{
  \item{path}{the template \code{.html} file.}
}
\details{
Four checkable properties, and the placeholder count is the one that matters most. The generator
substitutes with \code{sub()} -- first match only -- so a token named twice, in a comment say,
swallows the substitution and leaves the real one unreplaced: a dashboard that is broken on
arrival. Each of \code{__META_JSON__}, \code{__DATA_JSON__}, \code{__COLS_JSON__} and
\code{__GROUPABLE_JSON__} must therefore appear exactly once.

One of the others is a unit-rendering trap worth knowing about. The generator uppercases column
headers, and the CSS \code{text-transform} maps U+00B5 MICRO SIGN to U+039C GREEK CAPITAL MU, so
an uppercased \code{"(uL)"} silently renders as \code{"(ML)"} -- microlitres reported as
millilitres. Both halves are checked: the CSS rule alone is not a guard, because the generator has
to mark the unit as well.
}
\value{
A list of status records (see \link{ACTA-package}).
}
\examples{
tpl <- file.path(system.file("pipeline", "Template", package = "ACTA"),
                 "ACTA_dashboard_template_graph-blue.html")
recs <- actaValidateDashboardTemplate(tpl)
vapply(recs, `[[`, "", "status")
}
