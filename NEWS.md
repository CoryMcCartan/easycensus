# easycensus 1.0.0

* Core functions renamed for a consistent `cens_` prefix
* Use `censusapi` instead of `tidycensus` for fewer dependencies
* New `estimate` vector type that tracks uncertainty through mathematical operations 
* Expose parsed tables to users with a new `cens_table` type
* More flexible geography options when downloading data. See `cens_geo()` for details.
* Improved tidiers, including new income bin and detailed race tidiers

# easycensus 0.2.0

* CRAN resubmission

# easycensus 0.2.0

* Availability information for 1-year and 5-year ACS
* Improve documentation and parameter organization
* New tests

# easycensus 0.1.0

* Initial release
* Functionality to search and download ACS and 2010 Census tables
* Helper functions for recoding common variables and for marginalizing out nuisance variables
