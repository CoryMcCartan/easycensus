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
#> • female / 15 to 17 years / american indian and alaska native alone
#> • male / 80 to 84 years / hispanic or latino
#> • female / 10 to 14 years / white alone, not hispanic or latino
#> 
#>  PCT12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • male / 29 years / black or african american alone, not hispanic or latino
#> • male / 67 years / black or african american alone
#> • male / 20 years / white alone, not hispanic or latino
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
#> • female / 20 years / two or more races
#> • female / 80 to 84 years / total
#> • female / 20 years / hispanic or latino
#> 
#>  PCT12  - SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> • race_ethnicity
#> Example values:
#> • male / 18 years / two or more races
#> • female / 47 years / two or more races, not hispanic or latino
#> • female / 59 years / black or african american alone, not hispanic or latino
#> 
#>  PCO1  - GROUP QUARTERS POPULATION BY SEX BY AGE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age
#> Example values:
#> • male / 10 to 14 years
#> • female / 10 to 14 years
#> • male / 60 to 64 years
#> 
#>  P14  - SEX BY AGE FOR THE POPULATION UNDER 20 YEARS
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • sex
#> • age_for_the_population_under_20_years
#> Example values:
#> • male / 11 years
#> • male / under 1 year
#> • male / 3 years
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
#> • owner occupied / householder who is asian alone
#> • renter occupied / householder who is american indian and alaska native alone
#> • renter occupied / householder who is asian alone
#> 
#>  H16  - TENURE BY HOUSEHOLD SIZE
#> Surveys / Files:
#> ✔ Decennial / Summary File 1
#> Parsed variables:
#> • tenure
#> • household_size
#> • race_ethnicity
#> Example values:
#> • owner occupied / 1-person household / black or african american alone
#> householder
#> • owner occupied / 5-person household / asian alone householder
#> • owner occupied / 3-person household / total
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
#> • male (dollars)
#> • total
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
#> • female householder (dollars) / living alone (dollars) / householder 15 to 64
#> years (dollars)
#> • male householder (dollars) / not living alone (dollars) / householder 15 to
#> 64 years (dollars)
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
#> • nonveteran / total
#> • nonveteran / male
#> • total / total
cens_find_acs("heath care", show=-1)
#> [1] "B27009"
```
