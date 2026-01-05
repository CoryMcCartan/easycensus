# Download data from a decennial census or ACS table

Leverages
[`censusapi::getCensus()`](https://www.hrecht.com/censusapi/reference/getCensus.html)
to download tables of census data. Tables are returned in tidy format,
with variables given tidy, human-readable names.

## Usage

``` r
cens_get_dec(
  table,
  geo = NULL,
  ...,
  sumfile = "sf1",
  pop_group = NULL,
  check_geo = FALSE,
  drop_total = FALSE,
  show_call = FALSE
)

cens_get_acs(
  table,
  geo = NULL,
  ...,
  year = 2019,
  survey = c("acs5", "acs1"),
  check_geo = FALSE,
  drop_total = FALSE,
  show_call = FALSE
)

cens_get_raw(
  table,
  geo = NULL,
  ...,
  year = 2010,
  api = NULL,
  check_geo = FALSE,
  show_call = TRUE
)
```

## Arguments

- table:

  The table to download, either as a character vector or a table object
  as produced by
  [`cens_find_dec()`](http://corymccartan.com/easycensus/reference/cens_find.md),
  [`cens_find_acs()`](http://corymccartan.com/easycensus/reference/cens_find.md)
  or
  [`cens_parse_tables()`](http://corymccartan.com/easycensus/reference/cens_parse_tables.md),
  or as included in `tables_dec` and `tables_acs`. Note: some tables are
  split into A/B/C/etc. versions by race; this function unifies all of
  these tables under one code. So, for example, use `P012`, not `P012A`.

- geo:

  The geographic level to return. One of the machine-readable or
  human-readable names listed in the "Details" section of
  [`cens_geo()`](http://corymccartan.com/easycensus/reference/cens_geo.md).
  Will return all matching geographies of this level, as filtered by the
  further arguments to `...`. For example, setting `geo="tract"` is
  equivalent to setting `tract="all"`.

- ...:

  Geographies to return, as supported by the Census API. Order matters
  here—the first argument will be the geographic level to return (i.e.,
  it corresponds to the `geo` argument) and additional arguments will
  filter the results. Use `"all"`, `"*"`, `NA`, or `TRUE` to return all
  units of a particular geography. See the examples of
  [`cens_geo()`](http://corymccartan.com/easycensus/reference/cens_geo.md)
  for details.

- sumfile:

  For decennial data, the summary file to use. SF2 contains more
  detailed race and household info.

- pop_group:

  For decennial data using summary file SF2, the population group to
  filter to. See
  <https://www2.census.gov/programs-surveys/decennial/2010/technical-documentation/complete-tech-docs/summary-file/sf2.pdf#page=347>.

- check_geo:

  If `TRUE`, validate the provided geographies against the available
  geographies from the relevant Census API.

- drop_total:

  Whether to filter out variables which are totals across another
  variable. Recommended only after inspection of the underlying table.

- show_call:

  Whether to show the actual call to the Census API. May be useful for
  debugging.

- year:

  For ACS data, the survey year to get data for.

- survey:

  For ACS data, whether to use the one-year or five-year survey (the
  default). Make sure to check availability using
  [`cens_find_acs()`](http://corymccartan.com/easycensus/reference/cens_find.md).

- api:

  A Census API programmatic name such as `"acs/acs5"`.

## Value

A tibble of census data in tidy format, with columns `GEOID`, `NAME`,
`variable` (containing the Census variable code), `value` or `estimate`
in the case of ACS tables, and additional factor columns specific to the
table.

## Functions

- `cens_get_dec()`: Get decennial census data.

- `cens_get_acs()`: Get American Community Survey (ACS) data.

- `cens_get_raw()`: Get raw data from another Census Bureau API. Output
  will be minimally tidied but will likely require further manipulation.

## Examples

``` r
if (FALSE) { # \dontrun{
cens_get_dec("P3", "state")
cens_get_dec(tables_sf1$H2, "state")
cens_get_dec("H2", "county", state="WA", drop_total=TRUE)

cens_get_acs("B09001", county="King", state="WA")
} # }
```
