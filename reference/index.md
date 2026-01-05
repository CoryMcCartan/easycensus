# Package index

## Find Tables

Functions to parse and locate tables with variables of interest

- [`cens_find()`](http://corymccartan.com/easycensus/reference/cens_find.md)
  [`cens_find_dec()`](http://corymccartan.com/easycensus/reference/cens_find.md)
  [`cens_find_acs()`](http://corymccartan.com/easycensus/reference/cens_find.md)
  : Find a decennial or ACS census table with variables of interest
- [`cens_parse_tables()`](http://corymccartan.com/easycensus/reference/cens_parse_tables.md)
  : Attempt to Parse Tables from a Census API
- [`tables_sf1`](http://corymccartan.com/easycensus/reference/tables.md)
  [`tables_acs`](http://corymccartan.com/easycensus/reference/tables.md)
  : Parsed Census SF1 and ACS Tables

## Get Data

Functions to download and clean up data for a specific table and
geography

- [`cens_get_dec()`](http://corymccartan.com/easycensus/reference/cens_get.md)
  [`cens_get_acs()`](http://corymccartan.com/easycensus/reference/cens_get.md)
  [`cens_get_raw()`](http://corymccartan.com/easycensus/reference/cens_get.md)
  : Download data from a decennial census or ACS table
- [`cens_margin_to()`](http://corymccartan.com/easycensus/reference/cens_margin_to.md)
  : Helper function to sum over nuisance variables
- [`tidy_race()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_race_detailed()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_ethnicity()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_age()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_age_bins()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_income_bins()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_simplify()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  [`tidy_parens()`](http://corymccartan.com/easycensus/reference/tidiers.md)
  : Tidy labels in census tables

## Work with Noisy Estimates

A data type that keeps track of margins of error

- [`estimate()`](http://corymccartan.com/easycensus/reference/estimate.md)
  [`is_estimate()`](http://corymccartan.com/easycensus/reference/estimate.md)
  [`as_estimate()`](http://corymccartan.com/easycensus/reference/estimate.md)
  : Estimate class
- [`est_prop()`](http://corymccartan.com/easycensus/reference/est_special.md)
  [`est_pct_chg()`](http://corymccartan.com/easycensus/reference/est_special.md)
  : Specialized margin-of-error calculations
- [`get_est()`](http://corymccartan.com/easycensus/reference/est_extract.md)
  [`get_se()`](http://corymccartan.com/easycensus/reference/est_extract.md)
  [`get_moe()`](http://corymccartan.com/easycensus/reference/est_extract.md)
  [`to_rvar()`](http://corymccartan.com/easycensus/reference/est_extract.md)
  : Extract estimates, standard errors, and margins of error
- [`format(`*`<estimate>`*`)`](http://corymccartan.com/easycensus/reference/format.estimate.md)
  : Format an estimate

## Miscellaneous

Other package functions

- [`cens_geo()`](http://corymccartan.com/easycensus/reference/cens_geo.md)
  : Construct a Geography Specification for Census Data
- [`cens_auth()`](http://corymccartan.com/easycensus/reference/cens_auth.md)
  : Authorize use of the Census API
