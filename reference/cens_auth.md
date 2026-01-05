# Authorize use of the Census API

Tries environment variables `CENSUS_API_KEY` and `CENSUS_KEY`, in that
order. If none is found and R is used in interactive mode, will prompt
the user for a key.

## Usage

``` r
cens_auth()
```

## Value

a Census API key
