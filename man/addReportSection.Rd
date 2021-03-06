% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Report-methods.R
\name{addReportSection}
\alias{addReportSection}
\title{addReportSection}
\usage{
addReportSection(report, title, description, level = 1L,
  collapsed = FALSE)
}
\arguments{
\item{report}{Report to write the text to.}

\item{title}{Section header. This must be a single-element \code{character} vector.}

\item{description}{Human-readable paragraph text of the section in the form of a \code{character} vector. Elements
of this vector are concatenated without a separator to form the full description. Set this to
\code{NULL} if the section does not (yet) contain text.}

\item{level}{Section level as a single \code{integer}. It must be one of \code{1}, \code{2} or \code{3},
denoting section, subsection and sub-subsection, respectively.}

\item{collapsed}{Flag indicating if the contents of this section is to be initially collapsed. Possible values are
\code{TRUE} (the section is not visible), \code{FALSE} (default, the section is expanded) and
\code{"never"} (the section cannot be collapsed or expanded).}
}
\value{
The modified report.
}
\description{
Generates HTML code for a new section in the specified report.
}
\examples{
\donttest{
report <- createReport("example.html", "Example", init.configuration = TRUE)
report <- addReportSection(report, "Introduction", "This is how it's done.")
}
}
\seealso{
\code{\linkS4class{Report}} for other functions adding contents to an HTML report
}
\author{
adapted by Fabian Mueller from RnBeads code by Yassen Assenov
}
