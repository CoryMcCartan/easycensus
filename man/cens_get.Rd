% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cens_get.R
\name{cens_get}
\alias{cens_get}
\alias{cens_get_dec}
\alias{cens_get_acs}
\title{Download data from a decennial census or ACS table}
\usage{
cens_get_dec(
  geography,
  table,
  state = NULL,
  county = NULL,
  ...,
  drop_total = FALSE
)

cens_get_acs(
  geography,
  table,
  year = 2019,
  state = NULL,
  county = NULL,
  survey = c("acs5", "acs1"),
  ...,
  drop_total = FALSE
)
}
\arguments{
\item{geography}{The geography level to download data for. Usually one of
\code{state}, \code{county}, \code{tract}, \verb{block group}, \code{block}, \code{zcta}, etc. Consult
\url{https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus}
for more information.}

\item{table}{The table code to download. See \code{\link[=cens_find_dec]{cens_find_dec()}} for help
identifying a table of interest. Note: some tables are split into
A/B/C/etc. versions by race; this function unifies all of these tables
under one code. So, for example, use \code{P012}, not \code{P012A}.}

\item{state}{The state to get data for, if any.}

\item{county}{The state to get data for, if any.}

\item{...}{Further arguments passed to \code{\link[tidycensus:get_decennial]{tidycensus::get_decennial()}} or
\code{\link[tidycensus:get_acs]{tidycensus::get_acs()}}, e.g. \code{year}, \code{state}, \code{county}, \code{geometry}.}

\item{drop_total}{Whether to filter out variables which are totals across
another variable. Recommended only after inspection of the underlying
table.}

\item{year}{For ACS data, the survey year to get data for.}

\item{survey}{For ACS data, whether to use the one-year or
five-year survey (the default). Make sure to check availability using
\code{\link[=cens_find_acs]{cens_find_acs()}}.}
}
\value{
A tibble of census data in tidy format, with columns
\code{GEOID}, \code{NAME}, \code{variable} (containing the Census variable code),
\code{value} or \code{estiamte}, \code{moe} in the case of ACS tables,
and additional factor columns specific to the table.
}
\description{
Leverages \code{\link[tidycensus:get_decennial]{tidycensus::get_decennial()}} and \code{\link[tidycensus:get_acs]{tidycensus::get_acs()}} to
download tables of census data. Tables are returned in tidy format, with
variables given tidy, human-readable names.
}
\examples{
\dontrun{
cens_get_dec("state", "P003")
cens_get_dec("state", "H002")
cens_get_dec("county", "H002", state="WA", drop_total=TRUE)

cens_get_acs("county subdivision", "B09001", state="WA", county="King")
}

}