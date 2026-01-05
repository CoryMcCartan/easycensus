# Format an estimate

Format an estimate for pretty printing

## Usage

``` r
# S3 method for class 'estimate'
format(x, conf = 0.9, digits = 2, trim = FALSE, ..., formatter = fmt_plain)
```

## Arguments

- x:

  An
  [estimate](http://corymccartan.com/easycensus/reference/estimate.md)
  vector

- conf:

  The confidence level to use in converting the margin of error to a
  standard error. Defaults to 90%, which is what the Census Bureau uses
  for ACS estimates.

- digits:

  The number of dig

- trim:

  logical; if `FALSE`, logical, numeric and complex values are
  right-justified to a common width: if `TRUE` the leading blanks for
  justification are suppressed.

- ...:

  Ignored.

- formatter:

  the formatting function to use internally
