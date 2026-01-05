# Parsed Census SF1 and ACS Tables

Contains parsed table information for the 2010 Decennial Summary File 1
and 2019 ACS 5-year and 1-year tables. This parsed information is used
internally in
[`cens_find_dec()`](http://corymccartan.com/easycensus/reference/cens_find.md),
[`cens_find_acs()`](http://corymccartan.com/easycensus/reference/cens_find.md),
[`cens_get_dec()`](http://corymccartan.com/easycensus/reference/cens_get.md),
and
[`cens_get_acs()`](http://corymccartan.com/easycensus/reference/cens_get.md).
For other sets of tables, try using
[`cens_parse_tables()`](http://corymccartan.com/easycensus/reference/cens_parse_tables.md).

## Usage

``` r
tables_sf1

tables_acs
```

## Format

A list of `cens_table` objects, which are just lists with four elements:

- `concept`, a human-readable name

- `tables`, the constituent table codes

- `surveys`, the supported surveys

- `dims`, the parsed names of the dimensions of the tables

- `vars`, a `tibble` with all of the parsed variable values

An object of class `list` of length 83.

An object of class `list` of length 848.
