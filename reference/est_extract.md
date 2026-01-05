# Extract estimates, standard errors, and margins of error

Getter functions for
[`estimate()`](http://corymccartan.com/easycensus/reference/estimate.md)
vectors.

The [posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)
class may be useful in handling standard errors for more complicated
mathematical expressions. This function assumes a Normal distribution
centered on the estimate, with standard deviation equal to the standard
error of the estimate. The `posterior` package is required for this
function.

## Usage

``` r
get_est(x)

get_se(x)

get_moe(x, conf = 0.9)

to_rvar(x, n = 500)
```

## Arguments

- x:

  An
  [estimate](http://corymccartan.com/easycensus/reference/estimate.md)
  vector.

- conf:

  The confidence level to use in constructing the margin of error.

- n:

  How many samples to draw.

## Value

An [estimate](http://corymccartan.com/easycensus/reference/estimate.md)
vector.

A [posterior::rvar](https://mc-stan.org/posterior/reference/rvar.html)
vector.

## Examples

``` r
x = estimate(1, 0.1)
get_est(x)
#> [1] 1
get_moe(x)
#> [1] 0.1644854

x = estimate(1, 0.1)
if (requireNamespace("posterior", quietly=TRUE)) {
    rv_x = to_rvar(x)
    (rv_x^2 / rv_x) - rv_x # std. errors zero (correct)
    x^2 / x - x # std. errors not zero
}
#> <estimate[1]>
#> [1] 0 ± 0.4
```
