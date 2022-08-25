
#' Download data from a decennial census or ACS table
#'
#' Leverages [censusapi::getCensus()] to download tables of census data. Tables
#' are returned in tidy format, with variables given tidy, human-readable names.
#'
#' @param table The table to download, either as a character vector or a table
#'   object as produced by [cens_find_dec()],  [cens_find_acs()] or
#'   [cens_parse_tables()], or as included in `tables_dec` and `tables_acs`.
#'   Note: some tables are split into A/B/C/etc. versions by race; this function
#'   unifies all of these tables under one code. So, for example, use `P012`,
#'   not `P012A`.
#' @param geo The geographic level to return. One of the machine-readable or
#'   human-readable names listed in the "Details" section of [cens_geo()]. Will
#'   return all matching geographies of this level, as filtered by the further
#'   arguments to `...`.  For example, setting `geo="tract"` is equivalent to
#'   setting `tract="all"`.
#' @param ... Geographies to return, as supported by the Census API. Order
#'   matters here---the first argument will be the geographic level to return
#'   (i.e., it corresponds to the `geo` argument) and additional arguments will
#'   filter the results. Use `"all"`, `"*"`, `NA`, or `TRUE` to return all units
#'   of a particular geography.  See the examples of [cens_geo()] for details.
#' @param year For ACS data, the survey year to get data for.
#' @param survey For ACS data, whether to use the one-year or
#'   five-year survey (the default). Make sure to check availability using
#'   [cens_find_acs()].
#' @param check_geo If `TRUE`, validate the provided geographies against the
#'   available geographies from the relevant Census API.
#' @param drop_total Whether to filter out variables which are totals across
#'   another variable. Recommended only after inspection of the underlying
#'   table.
#' @param show_call Whether to show the actual call to the Census API. May be
#'   useful for debugging.
#'
#' @returns A tibble of census data in tidy format, with columns
#'   `GEOID`, `NAME`, `variable` (containing the Census variable code),
#'   `value` or `estiamte`, `moe` in the case of ACS tables,
#'   and additional factor columns specific to the table.
#'
#' @examples \dontrun{
#' cens_get_dec("P3", "state")
#' cens_get_dec(tables_sf1$H2, "state")
#' cens_get_dec("H2", "county", state="WA", drop_total=TRUE)
#'
#' cens_get_acs("B09001", county="King", state="WA")
#' }
#'
#' @name cens_get
NULL

#' @rdname cens_get
#' @export
cens_get_dec <- function(table, geo=NULL, ..., check_geo=FALSE, drop_total=FALSE, show_call=FALSE) {
    if (is.character(table) && length(table) == 1L) {
        if (!table %in% names(tables_sf1))
            cli_abort("Table {.field {table}} not found.")
        spec = tables_sf1[[table]]
    } else if (inherits(table, "cens_table")) {
        if (!table$tables[1] %in% names(tables_sf1))
            cli_abort("Table {.field {table$tables[1]}} not found for the decennial census.")
        spec = table
        table = spec$tables[1]
    } else {
        cli_abort("{.arg table} must be a character vector or {.cls cens_table}.")
    }

    geo_spec = cens_geo(geo, ..., check=check_geo, api="dec/sf1", year=2010)

    caller = rlang::current_call()
    suppressMessages({
    withCallingHandlers({
        d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
            load_dec("dec/sf1", 2010, tbl_code, geo_spec, show_call=show_call)
        }))
    },
    warning = function(w) {
        if (w$message != "NAs introduced by coercion") {
            rlang::warn(w$message)
        } else {
            invokeRestart("muffleWarning")
        }
    },
    error = function(e) {
        rlang::abort(e$message, call=caller)
    })
    })

    tbl_vars = spec$vars
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, dplyr::if_all(-1, function(x) x != "total"))
    dplyr::inner_join(d, tbl_vars, by="variable")
}

#' @rdname cens_get
#' @export
cens_get_acs <- function(table, geo=NULL, ..., year=2019, survey=c("acs5", "acs1"),
                         check_geo=FALSE, drop_total=FALSE, show_call=FALSE) {
    if (is.character(table) && length(table) == 1L) {
        if (!table %in% names(tables_acs))
            cli_abort("Table {.field {table}} not found.")
        spec = tables_acs[[table]]
    } else if (inherits(table, "cens_table")) {
        if (!table$tables[1] %in% names(tables_acs))
            cli_abort("Table {.field {table$tables[1]}} not found for the ACS.")
        spec = table
        table = spec$tables[1]
    } else {
        cli_abort("{.arg table} must be a character vector or {.cls cens_table}.")
    }

    api = str_c("acs/", match.arg(survey))
    if (!api %in% spec$surveys) {
        cli_abort("{.arg {table}} is not available for {.field {match_survey(api)}}")
    }

    geo_spec = cens_geo(geo, ..., check=check_geo, api=api, year=year)

    caller = rlang::current_call()
    suppressMessages({
        withCallingHandlers({
            d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
                load_acs(api, year, tbl_code, geo_spec, show_call=show_call)
            }))
        },
        warning = function(w) {
            if (w$message != "NAs introduced by coercion") {
                rlang::warn(w$message)
            } else {
                invokeRestart("muffleWarning")
            }
        },
        error = function(e) {
            rlang::abort(e$message, call=caller)
        })
    })

    tbl_vars = spec$vars
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, dplyr::if_all(-1, function(x) x != "total"))
    dplyr::inner_join(d, tbl_vars, by="variable")
}



# Internal: get Census data and tidy-format
load_dec <- function(api, year, tbl, geo_spec, show_call=FALSE) {
    d = censusapi::getCensus(api, year, key=cens_auth(),
                             vars=str_glue("group({tbl})"),
                             region=geo_spec$region, regionin=geo_spec$regionin,
                             show_call=show_call) %>%
        as_tibble()

    d = d[, str_starts(colnames(d), "[a-z]", negate=TRUE)]
    d$GEO_ID = str_sub(d$GEO_ID, 10)

    dplyr::select(d, GEOID=.data$GEO_ID, .data$NAME,
                  dplyr::everything(), -dplyr::ends_with("ERR")) %>%
        tidyr::pivot_longer(-c("GEOID", "NAME"),
                            names_to="variable", values_to="value")
}

# Internal: get Census data and tidy-format
load_acs <- function(api, year, tbl, geo_spec, show_call=FALSE) {
    d = censusapi::getCensus(api, year, key=cens_auth(),
                             vars=str_glue("group({tbl})"),
                             region=geo_spec$region, regionin=geo_spec$regionin,
                             show_call=show_call) %>%
        as_tibble()

    d = d[, str_starts(colnames(d), "[a-z]", negate=TRUE)]
    d$GEO_ID = str_sub(d$GEO_ID, 10)

    dplyr::select(d, GEOID=.data$GEO_ID, .data$NAME,
                  dplyr::everything(), -dplyr::ends_with("ERR")) %>%
        tidyr::pivot_longer(-c("GEOID", "NAME"),
                            names_pattern="([A-Z][0-9_]+)([A-Z]+)",
                            names_to=c("variable", ".value")) %>%
        dplyr::mutate(variable = str_c(.data$variable, "E"),
                      E = dplyr::case_when(.data$EA == "-" ~ NA_real_,
                                           .data$EA == "N" ~ NA_real_,
                                           .data$EA == "(X)" ~ NA_real_,
                                           is.na(.data$EA) ~ .data$E,
                                           TRUE ~ .data$E),
                      M = dplyr::case_when(.data$MA == "**" ~ NA_real_,
                                           .data$MA == "***" ~ NA_real_,
                                           .data$MA == "*****" ~ 0,
                                           .data$MA == "N" ~ NA_real_,
                                           .data$MA == "(X)" ~ NA_real_,
                                           is.na(.data$MA) ~ .data$M,
                                           TRUE ~ .data$M),
                      value = estimate(.data$E, .data$M)) %>%
        dplyr::select(.data$GEOID, .data$NAME, .data$variable, .data$value)
}
