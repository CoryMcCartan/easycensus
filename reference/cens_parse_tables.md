# Attempt to Parse Tables from a Census API

Uses the same parsing code as that which generates
[tables_sf1](http://corymccartan.com/easycensus/reference/tables.md) and
[tables_acs](http://corymccartan.com/easycensus/reference/tables.md) See
<https://www.census.gov/data/developers/data-sets.html> for a list of
APIs and corresponding years, or use
[`censusapi::listCensusApis()`](https://www.hrecht.com/censusapi/reference/listCensusApis.html).

## Usage

``` r
cens_parse_tables(api, year)
```

## Arguments

- api:

  A Census API programmatic name such as `"acs/acs5"`.

- year:

  The year for the data

## Value

A list of `cens_table` objects, which are just lists with four elements:

- `concept`, a human-readable name

- `tables`, the constituent table codes

- `surveys`, the supported surveys

- `dims`, the parsed names of the dimensions of the tables

- `vars`, a `tibble` with all of the parsed variable values

## Examples

``` r
if (FALSE) { # \dontrun{
cens_parse_tables("dec/pl", 2020)
} # }
```
