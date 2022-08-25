#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
#' @import stringr
#' @import cli
#' @importFrom dplyr as_tibble if_else %>%
#' @importFrom rlang .data .env
#' @importFrom utils data adist
#' @importFrom stats qnorm rnorm
## usethis namespace: start
## usethis namespace: end
NULL

# load table names for autocomplete
.onLoad <- function(libname, pkgname) {
    data("tables_sf1", package=pkgname, envir=parent.env(environment()))
    data("tables_acs", package=pkgname, envir=parent.env(environment()))
}
