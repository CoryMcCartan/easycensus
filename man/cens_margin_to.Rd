% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{cens_margin_to}
\alias{cens_margin_to}
\title{Helper function to sum over nuisance variables}
\usage{
cens_margin_to(data, ...)
}
\arguments{
\item{data}{The output of \code{\link[=cens_get_dec]{cens_get_dec()}} or \code{\link[=cens_get_acs]{cens_get_acs()}}}

\item{...}{The variables of interest, which will be kept. Remaining variables
will be marginalized out.}
}
\value{
A new data frame that has had \code{\link[dplyr:group_by]{dplyr::group_by()}} and
\code{\link[dplyr:summarise]{dplyr::summarize()}} applied.
}
\description{
For ACS data, margins of error will be updated appropriately, using
the functionality in \code{\link[=estimate]{estimate()}}.
}
\examples{
\dontrun{
d_cens = cens_get_acs("state", "B25042")
cens_margin_to(d_cens, bedrooms)
}
}
