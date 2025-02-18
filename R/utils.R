#' Authorize use of the Census API
#'
#' Tries environment variables `CENSUS_API_KEY` and `CENSUS_KEY`, in that order.
#' If none is found and R is used in interactive mode, will prompt the user for
#' a key.
#'
#' @returns a Census API key
cens_auth = function() {
    if (!is.null(key <- cens_auth_env())) {
        key
    } else if (interactive()) {
        cli_alert_info("A Census API key is required to download Census data.")
        url <- "http://api.census.gov/data/key_signup.html"
        cli_text("Sign up at ", cli::style_hyperlink(text=str_c("<", url, ">"), url=url))
        key = readline("Enter your key here: ")
        if (nchar(key) != 40) {
            cli_abort("A valid Census API key must be provided.", .envir=parent.frame())
        } else {
            cli_inform(c(">"="Use the key automatically in the future by adding
                         the following line to your {.path .Renviron}:"))
            cli_text("CENSUS_API_KEY=\"{key}\"")
        }
        key
    } else {
        cli_abort("A valid Census API key must be provided.", .envir=parent.frame())
    }
}

# Internal getter. Returns NULL if no key
cens_auth_env = function() {
    if ((key <- Sys.getenv("CENSUS_API_KEY")) != "") {
        key
    } else if ((key <- Sys.getenv("CENSUS_KEY")) != "") {
        key
    } else {
        NULL
    }
}

#' Helper function to sum over nuisance variables
#'
#' For ACS data, margins of error will be updated appropriately, using
#' the functionality in [estimate()].
#'
#' @param data The output of [cens_get_dec()] or [cens_get_acs()]
#' @param ... The variables of interest, which will be kept. Remaining variables
#'   will be marginalized out.
#'
#' @return A new data frame that has had [dplyr::group_by()] and
#'   [dplyr::summarize()] applied.
#'
#' @examples \dontrun{
#' d_cens = cens_get_acs("state", "B25042")
#' cens_margin_to(d_cens, bedrooms)
#' }
#' @export
cens_margin_to = function(data, ...) {
    dplyr::summarize(
        dplyr::group_by(data, .data$GEOID, .data$NAME, ...),
        value = sum(.data$value),
        .groups = "drop")
}
