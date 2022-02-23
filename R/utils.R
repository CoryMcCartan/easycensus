#' Helper function to sum over nuisance variables
#'
#' For ACS data, margins of error will be updated appropriately, using
#' [tidycensus::moe_sum()].
#'
#' @param data The output of [get_dec_table()] or [get_acs_table()]
#' @param ... The variables of interest, which will be kept. Remaining variables
#'   will be marginalized out.
#'
#' @return A new data frame that has had [group_by()] and [summarize()] applied.
#'
#' @examples \dontrun{
#' d_cens = get_acs_table("state", "B25042")
#' marginalize(d_cens, bedrooms)
#' }
#' @export
marginalize = function(data, ...) {
    if (is_sf <- inherits(data, "sf")) {
        rlang::check_installed("sf")
        geom_d = dplyr::distinct(dplyr::select(data, .data$GEOID))
        requireNamespace("sf", quietly=TRUE)
        data = sf::st_drop_geometry(data)
    }

    if ("moe" %in% names(data)) { # ACS
        data = dplyr::summarize(
            dplyr::group_by(data, .data$GEOID, .data$NAME, ...),
            estimate = sum(.data$estimate),
            moe = moe_sum(.data$moe, .data$estimate))
    } else { # decennial
        data = dplyr::summarize(
            dplyr::group_by(data, .data$GEOID, .data$NAME, ...),
            value = sum(.data$value))
    }

    if (is_sf) {
        data = sf::st_as_sf(dplyr::left_join(data, geom_d, by="GEOID"))
    }

    data
}


#' @importFrom tidycensus moe_product
#' @export
tidycensus::moe_product

#' @importFrom tidycensus moe_prop
#' @export
tidycensus::moe_prop

#' @importFrom tidycensus moe_ratio
#' @export
tidycensus::moe_ratio

#' @importFrom tidycensus moe_sum
#' @export
tidycensus::moe_sum
