# Specialized margin-of-error calculations

Proportions and percent-change-over-time calculations require different
standard error calculations.

## Usage

``` r
est_prop(x, y)

est_pct_chg(x, y)
```

## Arguments

- x, y:

  An
  [estimate](http://corymccartan.com/easycensus/reference/estimate.md)
  vector. For `est_pct_chg()`, calculates the % change from `x` to `y`
  (i.e., \\(y-x)/x\\)

## Value

An [estimate](http://corymccartan.com/easycensus/reference/estimate.md)
vector.

## Examples

``` r
x = estimate(1, 0.1)
y = estimate(1.5, 0.1)
est_prop(x, y)
#> <estimate[1]>
#> [1] 0.67 ± 0.12
est_pct_chg(x, y)
#> <estimate[1]>
#> [1] 0.5 ± 0.3
```
