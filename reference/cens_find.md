# Find a decennial or ACS census table with variables of interest

This function uses fuzzy matching to help identify tables from the
census which contain variables of interest. Matched table codes are
printed out, along with the Census-provided table description, the
parsed variable names, and example table cells. The website
<https://censusreporter.org/> may also be useful in finding variables.

## Usage

``` r
cens_find(tables, ..., show = 4)

cens_find_dec(..., show = 2)

cens_find_acs(..., show = 4)
```

## Arguments

- tables:

  A list of `cens_table` objects, such as is produced by
  [`cens_parse_tables()`](http://corymccartan.com/easycensus/reference/cens_parse_tables.md).

- ...:

  Variables to look for. These can be length-1 character vectors, or,
  for convenience, can be left unquoted (see examples).

- show:

  How many matching tables to show. Increase this to show more possible
  matches, at the cost of more output. Negative values will be converted
  to positive but will suppress any printing.

## Value

The codes for the top `show` tables, invisibly if `show` is positive.

## Examples

``` r
cens_find_dec("sex", "age")
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
#> • female / 10 to 14 years / white alone, not hispanic or latino
#> • male / 50 to 54 years / white alone
#> • male / 75 to 79 years / two or more races
#> 
#>  PCT12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 5 years / asian alone, not hispanic or latino
#> • male / 61 years / some other race alone, not hispanic or latino
#> • male / 18 years / two or more races
cens_find(tables_sf1, "sex", "age") # same as above
#> 
#> ── Top 4 matching tables ───────────────────────────────────────────────────────
#> 
#>  P12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • male / 22 to 24 years / asian alone
#> • male / 15 to 17 years / asian alone
#> • male / 15 to 17 years / total
#> 
#>  PCT12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • female / 21 years / hispanic or latino
#> • female / 14 years / black or african american alone
#> • female / 105 to 109 years / hispanic or latino
#> 
#>  PCO1  - GROUP QUARTERS POPULATION BY SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> Example values:
#> • male / 50 to 54 years
#> • male / total
#> • male / 10 to 14 years
#> 
#>  P14  - SEX BY AGE FOR THE POPULATION UNDER 20 YEARS
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age_for_the_population_under_20_years
#> Example values:
#> • male / 19 years
#> • female / 11 years
#> • male / 4 years
cens_find_dec(tenure, race)
#> 
#> ── Top 2 matching tables ───────────────────────────────────────────────────────
#> 
#>  H14  - TENURE BY RACE OF HOUSEHOLDER
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • tenure
#> • race_of_householder
#> Example values:
#> • renter occupied / householder who is some other race alone
#> • renter occupied / householder who is two or more races
#> • owner occupied / householder who is american indian and alaska native alone
#> 
#>  H16  - TENURE BY HOUSEHOLD SIZE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • tenure
#> • household_size
#> • race_ethnicity
#> Example values:
#> • renter occupied / 2-person household / black or african american alone
#> householder
#> • owner occupied / 6-person household / hispanic or latino householder
#> • renter occupied / 7-or-more-person household / white alone householder
cens_find_acs("income", "sex", show=3)
#> 
#> ── Top 3 matching tables ───────────────────────────────────────────────────────
#> 
#>  B17008  - AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS OF
#> UNRELATED INDIVIDUALS BY SEX
#> Surveys / Files:
#> ✔ ACS / 1-year Detailed
#> ✔ ACS / 5-year Detailed
#> Parsed variables:
#> • sex
#> Example values:
#> • total
#> • male (dollars)
#> • female (dollars)
#> 
#>  B19216  - AGGREGATE NONFAMILY HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019
#> INFLATION-ADJUSTED DOLLARS) BY SEX OF HOUSEHOLDER BY LIVING ALONE BY AGE OF
#> HOUSEHOLDER
#> Surveys / Files:
#> ✔ ACS / 1-year Detailed
#> ✔ ACS / 5-year Detailed
#> Parsed variables:
#> • sex_of_householder
#> • living_alone
#> • age_of_householder
#> Example values:
#> • male householder (dollars) / not living alone (dollars) / total
#> • female householder (dollars) / living alone (dollars) / total
#> • male householder (dollars) / living alone (dollars) / householder 65 years
#> and over (dollars)
#> 
#>  B21004  - MEDIAN INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED
#> DOLLARS) BY VETERAN STATUS BY SEX FOR THE CIVILIAN POPULATION 18 YEARS AND OVER
#> WITH INCOME
#> Surveys / Files:
#> ✔ ACS / 1-year Detailed
#> ✔ ACS / 5-year Detailed
#> Parsed variables:
#> • veteran_status
#> • sex_for_the_civilian_population_18_years_over_with_income
#> Example values:
#> • veteran / total
#> • total / total
#> • veteran / female
cens_find_acs("heath care", show=-1)
#> [1] "B27009"
```
