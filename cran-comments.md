## Test environments
* local R installation (macOS), R 4.2.0
* windows-latest (on gh-actions), (release)
* macos-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (devel)
* ubuntu-latest (on gh-actions), (old-release)

## R CMD check results

0 errors | 0 warnings | 0 notes

* This submission fixes earlier invalid HTML5 flagged on a CRAN check, and also
updates package URLs.

* Examples are \dontrun in `cens_get_*()`, `cens_parse_tables()`,
`cens_margin_to()`, and one example in `tidiers` since these require an API key
and may take more than a few seconds to download.
