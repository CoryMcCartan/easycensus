# Estimate class

A numeric vector that stores margin-of-error information along with it.
The margin of error will update through basic arithmetic operations,
using a first-order Taylor series approximation. The implicit assumption
is that the errors in each value are uncorrelated. If in fact there is
correlation, the margins of error could be wildly under- or
over-estimated.

## Usage

``` r
estimate(x, se = NULL, moe = NULL, conf = 0.9)

is_estimate(x)

as_estimate(x)
```

## Arguments

- x:

  A numeric vector containing the estimate(s).

- se:

  A numeric vector containing the standard error(s) for the estimate(s).
  Users should supply either `se` or `moe` and `conf`.

- moe:

  A numeric vector containing the margin(s) of error. Users should
  supply either `se` or `moe` and `conf`.

- conf:

  The confidence level to use in converting the margin of error to a
  standard error. Defaults to 90%, which is what the Census Bureau uses
  for ACS estimates.

## Value

An `estimate` vector.

## Examples

``` r
estimate(5, 2) # 5 with std. error  2
#> <estimate[1]>
#> [1] 5 ± 3.3
estimate(15, moe=3) - estimate(5, moe=4)
#> <estimate[1]>
#> [1] 10 ± 5
estimate(1:4, 0.1) * estimate(1, 0.1)
#> <estimate[4]>
#> [1] 1 ± 0.23 2 ± 0.37 3 ± 0.52 4 ± 0.68
```
