# Resubmission

In this resubmission, I have updated the DESCRIPTION field with a link to the
relevant webservices, and provided more detail about the package functionality.

## Test environments
* local R installation (macOS), R 4.1.2
* ubuntu-latest (on GitHub Actions), (release)
* windows-latest (on winbuilder), (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
* Examples are \dontrun in the `get_*_table()` and `marginalize()` functions
since these require an API key and may take more than a few seconds to download.
