## Test environments
* local R installation (macOS), R 4.4.0
* windows-latest (on gh-actions), (release)
* macos-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (release)
* ubuntu-latest (on gh-actions), (devel)
* ubuntu-latest (on gh-actions), (old-release)

## R CMD check results

0 errors | 0 warnings | 1 note

* My email address has been updated; I don't have access to the old one to confirm.

* The 1 NOTE is for CRAN checks regarding non-ASCII characters in data.
This submission does not remove the non-ASCII characters as they are Spanish-language
and taken from the Census Bureau's data. It is important to maintain consistency
between the data here and the Census Bureau labels, which include non-ASCII characters.

* Examples are \dontrun in `cens_get_*()`, `cens_parse_tables()`,
`cens_margin_to()`, and one example in `tidiers` since these require an API key
and may take more than a few seconds to download.
