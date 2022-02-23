
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **easycensus** <a href="https://corymccartan.github.io/easycensus/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/CoryMcCartan/easycensus/workflows/R-CMD-check/badge.svg)](https://github.com/CoryMcCartan/easycensus/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

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
age and race. `easycensus` provides the `find_dec_table()` function to
help you locate exactly which decennial census table to use to get the
data you want.

``` r
library(easycensus)

find_dec_table(age, race)
#> 
#> ── Top 2 matching tables ───────────────────────────────────────────────────────
#> 
#>  P012  - SEX BY AGE
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 45 to 49 years / black or african american alone
#> • female / 20 years / two or more races
#> • male / 55 to 59 years / american indian and alaska native alone
#> 
#>  PCT012  - SEX BY AGE
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 70 years / total
#> • female / 85 years / native hawaiian and other pacific islander alone
#> • female / 30 years / two or more races
```

We can see right away that our best bet is either table `P012` or table
`PCT012`, depending on whether we want age in 5-year groups or down to
individual years. Let’s say you’re OK with the five-year bins. Then all
you need to do to get your data is to call `get_dec_table()`.

``` r
d_cens = get_dec_table("tract", "P012", state="AK", county="Nome")
print(d_cens)
#> # A tibble: 9,600 × 7
#>    GEOID       NAME                    variable value sex   age   race_ethnicity
#>    <chr>       <chr>                   <chr>    <dbl> <fct> <fct> <fct>         
#>  1 02180000100 Census Tract 1, Nome C… P012002   3053 male  total total         
#>  2 02180000200 Census Tract 2, Nome C… P012002   2005 male  total total         
#>  3 02180000100 Census Tract 1, Nome C… P012003    359 male  unde… total         
#>  4 02180000200 Census Tract 2, Nome C… P012003    175 male  unde… total         
#>  5 02180000100 Census Tract 1, Nome C… P012004    318 male  5 to… total         
#>  6 02180000200 Census Tract 2, Nome C… P012004    132 male  5 to… total         
#>  7 02180000100 Census Tract 1, Nome C… P012005    294 male  10 t… total         
#>  8 02180000200 Census Tract 2, Nome C… P012005    161 male  10 t… total         
#>  9 02180000100 Census Tract 1, Nome C… P012006    165 male  15 t… total         
#> 10 02180000200 Census Tract 2, Nome C… P012006     90 male  15 t… total         
#> # … with 9,590 more rows
```

`easycensus` is built on top of the great
[`tidycensus`](https://walker-data.com/tidycensus/) package, so all of
the usual arguments to those functions (including the ability to get
shapefile information) work here, too.

Once you’ve gotten your labelled data, it’s easy to marginalize out the
unneeded `sex` variable.

``` r
library(dplyr)

d_cens %>%
    # Drop table margins. Can also use `drop_total=TRUE` in `get_dec_table()`
    filter(age != "total", race_ethnicity != "total") %>%
    group_by(GEOID, age, race_ethnicity) %>%
    summarize(value = sum(value))
#> # A tibble: 414 × 4
#> # Groups:   GEOID, age [46]
#>    GEOID       age            race_ethnicity                               value
#>    <chr>       <fct>          <fct>                                        <dbl>
#>  1 02180000100 10 to 14 years american indian and alaska native alone       5240
#>  2 02180000100 10 to 14 years asian alone                                     10
#>  3 02180000100 10 to 14 years black or african american alone                 10
#>  4 02180000100 10 to 14 years hispanic or latino                              30
#>  5 02180000100 10 to 14 years native hawaiian and other pacific islander …     0
#>  6 02180000100 10 to 14 years some other race alone                            0
#>  7 02180000100 10 to 14 years two or more races                              230
#>  8 02180000100 10 to 14 years white alone                                    110
#>  9 02180000100 10 to 14 years white alone, not hispanic or latino            100
#> 10 02180000100 15 to 17 years american indian and alaska native alone       2930
#> # … with 404 more rows
```

## Installation

<!-- 
You can install the released version of easycensus from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("easycensus")
```

And the development version from [GitHub](https://github.com/) with:
-->

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("CoryMcCartan/easycensus")
```
