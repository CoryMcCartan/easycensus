library(testthat)
library(easycensus)

check_api = function() {
    if (Sys.getenv("CENSUS_API_KEY") == "")
        skip("Census API not available")
}

test_check("easycensus")
