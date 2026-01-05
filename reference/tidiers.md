# Tidy labels in census tables

Some table labels are quite verbose, and users will often want to
shorten them. These functions make tidying common types of labels easy.
Most produce straightforward output, but there are several more generic
tidiers:

- `tidy_simplify()` attempts to simplify labels by removing words common
  to all labels.

- `tidy_parens()` attempts to simplify labels by removing all terms in
  parentheses.

- `tidy_race_detailed()` creates logical columns for each of the six
  racial categories.

## Usage

``` r
tidy_race(x)

tidy_race_detailed(x, x2, x3)

tidy_ethnicity(x)

tidy_age(x)

tidy_age_bins(x, as_factor = FALSE)

tidy_income_bins(x, as_factor = FALSE)

tidy_simplify(x)

tidy_parens(x)
```

## Arguments

- x:

  A factor, which will be re-leveled. Character vectors will be
  converted to factors.

- x2, x3:

  Additional character columns containing detailed information for
  certain variables (e.g. detailed race)

- as_factor:

  if `TRUE`, return a factor with levels of the form `[35,40]`.

## Value

A re-leveled factor, except for `tidy_age_bins()`, which by default
returns a data frame with columns `age_from` and `age_to` (inclusive).

## Examples

``` r
ex_race_long = c("american indian and alaska native alone", "asian alone",
    "black or african american alone", "hispanic or latino",
    "native hawaiian and other pacific islander alone",
    "some other race alone", "total", "two or more races",
    "white alone", "white alone, not hispanic or latino")
tidy_race(ex_race_long)
#>  [1] aian     asian    black    hisp     nhpi     other    total    two     
#>  [9] white    white_nh
#> Levels: aian asian black hisp nhpi other total two white white_nh

tidy_age_bins(c("10 to 14 years", "21 years", "85 years and over"))
#> # A tibble: 3 × 2
#>   age_from age_to
#>      <dbl>  <dbl>
#> 1       10     14
#> 2       21     21
#> 3       85    Inf

tidy_parens(c("label one (fake)", "label two (fake)"))
#> [1] label one label two
#> Levels: label one label two
tidy_simplify(c("label one (fake)", "label two (fake)"))
#> [1] one two
#> Levels: one two

if (FALSE)  # requires API key
d = cens_get_acs("B02003", "us", year=2019, survey="acs1")
dplyr::mutate(d, tidy_race_detailed(dtldr_1, dtldr_2, dtldr_3))
#> Error: object 'd' not found
 # \dontrun{}
```
