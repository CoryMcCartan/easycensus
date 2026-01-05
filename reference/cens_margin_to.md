# Helper function to sum over nuisance variables

For ACS data, margins of error will be updated appropriately, using the
functionality in
[`estimate()`](http://corymccartan.com/easycensus/reference/estimate.md).

## Usage

``` r
cens_margin_to(data, ...)
```

## Arguments

- data:

  The output of
  [`cens_get_dec()`](http://corymccartan.com/easycensus/reference/cens_get.md)
  or
  [`cens_get_acs()`](http://corymccartan.com/easycensus/reference/cens_get.md)

- ...:

  The variables of interest, which will be kept. Remaining variables
  will be marginalized out.

## Value

A new data frame that has had
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
and
[`dplyr::summarize()`](https://dplyr.tidyverse.org/reference/summarise.html)
applied.

## Examples

``` r
if (FALSE) { # \dontrun{
d_cens = cens_get_acs("state", "B25042")
cens_margin_to(d_cens, bedrooms)
} # }
```
