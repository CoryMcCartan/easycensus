## Test environments
* local R installation (macOS), R 4.1.2
* ubuntu-latest (on GitHub Actions), (release)
* windows-latest (on winbuilder), (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes

* Examples are \dontrun in `cens_get_*()`, `cens_parse_tables()`, and `cens_margin_to()` 
since these require an API key and may take more than a few seconds to download.
