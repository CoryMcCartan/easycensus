% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_labels.R
\name{tidiers}
\alias{tidiers}
\alias{tidy_race}
\alias{tidy_race_detailed}
\alias{tidy_ethnicity}
\alias{tidy_age}
\alias{tidy_age_bins}
\alias{tidy_income_bins}
\alias{tidy_simplify}
\alias{tidy_parens}
\title{Tidy labels in census tables}
\usage{
tidy_race(x)

tidy_race_detailed(x, x2, x3)

tidy_ethnicity(x)

tidy_age(x)

tidy_age_bins(x, as_factor = FALSE)

tidy_income_bins(x, as_factor = FALSE)

tidy_simplify(x)

tidy_parens(x)
}
\arguments{
\item{x}{A factor, which will be re-leveled. Character vectors will be converted to factors.}

\item{x2, x3}{Additional character columns containing detailed information for certain variables (e.g. detailed race)}

\item{as_factor}{if \code{TRUE}, return a factor with levels of the form \verb{[35,40]}.}
}
\value{
A re-leveled factor, except for \code{\link[=tidy_age_bins]{tidy_age_bins()}}, which by default
returns a data frame with columns \code{age_from} and \code{age_to} (inclusive).
}
\description{
Some table labels are quite verbose, and users will often want to shorten them.
These functions make tidying common types of labels easy.
Most produce straightforward output, but there are several more generic tidiers:
\itemize{
\item \code{\link[=tidy_simplify]{tidy_simplify()}} attempts to simplify labels by removing words common to all labels.
\item \code{\link[=tidy_parens]{tidy_parens()}} attempts to simplify labels by removing all terms in parentheses.
\item \code{\link[=tidy_race_detailed]{tidy_race_detailed()}} creates logical columns for each of the six racial categories.
}
}
\examples{
ex_race_long = c("american indian and alaska native alone", "asian alone",
    "black or african american alone", "hispanic or latino",
    "native hawaiian and other pacific islander alone",
    "some other race alone", "total", "two or more races",
    "white alone", "white alone, not hispanic or latino")
tidy_race(ex_race_long)

tidy_age_bins(c("10 to 14 years", "21 years", "85 years and over"))

tidy_parens(c("label one (fake)", "label two (fake)"))
tidy_simplify(c("label one (fake)", "label two (fake)"))

\dontrun{ # requires API key
d = cens_get_acs("B02003", "us", year=2019, survey="acs1")
dplyr::mutate(d, tidy_race_detailed(dtldr_1, dtldr_2, dtldr_3))
}

}
