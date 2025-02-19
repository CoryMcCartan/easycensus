
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **easycensus** <a href="https://corymccartan.com/easycensus/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/easycensus)](https://CRAN.R-project.org/package=easycensus)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![R-CMD-check](https://github.com/CoryMcCartan/easycensus/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/CoryMcCartan/easycensus/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

Extracting desired data using the proper Census variable names can be
time-consuming. This package takes the pain out of that process.

The use case is best illustrated by an example. Suppose you want
age-by-race information at the tract level. Unfortunately, the Census
Bureau doesn’t publish a specific age-by-race table. You could build one
yourself from public-use microdata, but that lacks tract-level
geographic information, for privacy reasons. So you are left trying to
find an existing Census product that you can extract age-by-race
information from.

Unless you’re a Census pro, you won’t know what exactly what is off the
top of your head. But suppose you know you’d like to get the data from
the decennial census, since it covers the whole nation and asks about
age and race. `easycensus` provides the `cens_find_dec()` function to
help you locate exactly which decennial census table to use to get the
data you want.

``` r
library(easycensus)

cens_find_dec(age, race)
#> 
#> ── Top 2 matching tables ───────────────────────────────────────────────────────
#> 
#>  P12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 45 to 49 years / black or african american alone
#> • female / 20 years / two or more races
#> • male / 55 to 59 years / american indian and alaska native alone
#> 
#>  PCT12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 70 years / total
#> • female / 85 years / native hawaiian and other pacific islander alone
#> • female / 30 years / two or more races
```

We can see right away that our best bet is either table `P12` or table
`PCT12`, depending on whether we want age in 5-year groups or down to
individual years. Let’s say you’re OK with the five-year bins. Then all
you need to do to get your data is to call `cens_get_dec()`.

``` r
d_cens = cens_get_dec("P12", "tract", state="AK", county="Nome")
print(d_cens)
#> # A tibble: 960 × 7
#>    GEOID       NAME                    variable value sex   age   race_ethnicity
#>    <chr>       <chr>                   <chr>    <dbl> <fct> <fct> <fct>         
#>  1 02180000100 Census Tract 1, Nome C… P012002   3053 male  total total         
#>  2 02180000100 Census Tract 1, Nome C… P012003    359 male  unde… total         
#>  3 02180000100 Census Tract 1, Nome C… P012004    318 male  5 to… total         
#>  4 02180000100 Census Tract 1, Nome C… P012005    294 male  10 t… total         
#>  5 02180000100 Census Tract 1, Nome C… P012006    165 male  15 t… total         
#>  6 02180000100 Census Tract 1, Nome C… P012007    130 male  18 a… total         
#>  7 02180000100 Census Tract 1, Nome C… P012008     53 male  20 y… total         
#>  8 02180000100 Census Tract 1, Nome C… P012009     47 male  21 y… total         
#>  9 02180000100 Census Tract 1, Nome C… P012010    150 male  22 t… total         
#> 10 02180000100 Census Tract 1, Nome C… P012011    204 male  25 t… total         
#> # ℹ 950 more rows
```

Once you’ve gotten your labeled data, it’s easy to marginalize out the
unneeded `sex` variable. You can either use `group_by()` and
`summarize()` as usual, or you can use the `cens_margin_to()` function
in `easycensus`. This has the added advantage of automatically handling
margins of error for ACS data.

``` r
library(dplyr)

d_cens = d_cens %>%
    # Drop table margins. Can also use `drop_total=TRUE` in `get_dec_table()`
    filter(age != "total", race_ethnicity != "total") %>%
    cens_margin_to(age, race=race_ethnicity)
print(d_cens)
#> # A tibble: 414 × 5
#>    GEOID       NAME                                     age          race  value
#>    <chr>       <chr>                                    <fct>        <fct> <dbl>
#>  1 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… amer…   524
#>  2 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… asia…     1
#>  3 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… blac…     1
#>  4 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… hisp…     3
#>  5 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… nati…     0
#>  6 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… some…     0
#>  7 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… two …    23
#>  8 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… whit…    11
#>  9 02180000100 Census Tract 1, Nome Census Area, Alaska 10 to 14 ye… whit…    10
#> 10 02180000100 Census Tract 1, Nome Census Area, Alaska 15 to 17 ye… amer…   293
#> # ℹ 404 more rows
```

Finally, you might want to simplify the age and race labels, since they
are kind of verbose. `easycensus` provides a set of `tidy_*()` functions
to assist with this.

``` r
d_cens %>%
    mutate(race = tidy_race(race),
           tidy_age_bins(age))
#> # A tibble: 414 × 7
#>    GEOID       NAME                            age   race  value age_from age_to
#>    <chr>       <chr>                           <fct> <fct> <dbl>    <dbl>  <dbl>
#>  1 02180000100 Census Tract 1, Nome Census Ar… 10 t… aian    524       10     14
#>  2 02180000100 Census Tract 1, Nome Census Ar… 10 t… asian     1       10     14
#>  3 02180000100 Census Tract 1, Nome Census Ar… 10 t… black     1       10     14
#>  4 02180000100 Census Tract 1, Nome Census Ar… 10 t… hisp      3       10     14
#>  5 02180000100 Census Tract 1, Nome Census Ar… 10 t… nhpi      0       10     14
#>  6 02180000100 Census Tract 1, Nome Census Ar… 10 t… other     0       10     14
#>  7 02180000100 Census Tract 1, Nome Census Ar… 10 t… two      23       10     14
#>  8 02180000100 Census Tract 1, Nome Census Ar… 10 t… white    11       10     14
#>  9 02180000100 Census Tract 1, Nome Census Ar… 10 t… whit…    10       10     14
#> 10 02180000100 Census Tract 1, Nome Census Ar… 15 t… aian    293       15     17
#> # ℹ 404 more rows
```

Dive into the
[reference](https://corymccartan.com/easycensus/reference/) to learn
more!

## Installation

You can install the released version of easycensus from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("easycensus")
```

Or install the development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("CoryMcCartan/easycensus")
```
