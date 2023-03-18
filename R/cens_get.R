
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
#' @param sumfile For decennial data, the summary file to use. SF2 contains more
#'   detailed race and household info.
#' @param api A Census API programmatic name such as `"acs/acs5"`.
#' @param pop_group For decennial data using summary file SF2, the population
#'   group to filter to. See
#'   <https://www2.census.gov/programs-surveys/decennial/2010/technical-documentation/complete-tech-docs/summary-file/sf2.pdf#page=347>.
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
#'   `value` or `estimate` in the case of ACS tables,
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


#' @describeIn cens_get Get raw data from another Census Bureau API. Output will
#'   be minimally tidied but will likely require further manipulation.
#' @order 3
#' @export
cens_get_raw <- function(table, geo=NULL, ..., year=2010, api=NULL,
                         check_geo=FALSE, show_call=TRUE) {
    if (inherits(table, "cens_table")) {
        if (!is.null(api) && !api %in% table$surveys)
            cli_abort("Table {.field {table$tables[1]}} not found for the provided api {.val {api}}.")
        spec = table
        table = spec$tables[1]
    } else {
        cli_abort("For {.fn cens_get_raw}, {.arg table} must be a
                  {.cls cens_table} object.")
    }

    if (is.null(api)) {
        if (length(spec$surveys) > 1) {
            cli_abort(c("Table {.field {table}} exists for multiple Census products.",
                        ">"="Please specify the correct one with {.arg api}"))
        } else {
            api = spec$surveys[1]
        }
    }

    geo_spec = cens_geo(geo, ..., check=check_geo, api=api, year=year)

    caller = rlang::current_call()
    suppressMessages({
        withCallingHandlers({
            d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
                load_raw(api, year, tbl_code, geo_spec, show_call=show_call)
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
    dplyr::inner_join(d, tbl_vars, by="variable")
}

#' @describeIn cens_get Get decennial census data.
#' @order 1
#' @export
cens_get_dec <- function(table, geo=NULL, ..., sumfile="sf1", pop_group=NULL,
                         check_geo=FALSE, drop_total=FALSE, show_call=FALSE) {
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

    api = str_c("dec/", sumfile)
    geo_spec = cens_geo(geo, ..., check=check_geo, api=api, year=2010)

    caller = rlang::current_call()
    suppressMessages({
    withCallingHandlers({
        d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
            load_dec(api, 2010, tbl_code, geo_spec, pop_group=pop_group, show_call=show_call)
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

#' @describeIn cens_get Get American Community Survey (ACS) data.
#' @order 2
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
load_raw <- function(api, year, tbl, geo_spec, show_call=TRUE) {
    d = censusapi::getCensus(api, year, key=cens_auth(),
                             vars=str_glue("group({tbl})"),
                             region=geo_spec$region, regionin=geo_spec$regionin,
                             show_call=show_call) %>%
        as_tibble()

    d = d[, str_starts(colnames(d), "[a-z]", negate=TRUE)]
    d$GEO_ID = str_sub(d$GEO_ID, 10)

    rgx_tbl = str_replace_all(tbl, "\\d", "\\\\d") # make a regex which matches table variables
    pivot_ids = union( # the remaining other columns to use as ID cols in pivoting
        c("GEO_ID", "NAME"),
        colnames(d)[str_starts(colnames(d), rgx_tbl, negate=TRUE)]
    )

    dplyr::select(d, "GEO_ID", "NAME",
                  dplyr::everything(), -dplyr::ends_with("ERR")) %>%
        tidyr::pivot_longer(-dplyr::all_of(pivot_ids),
                            names_to="variable", values_to="value") %>%
        dplyr::rename(GEOID="GEO_ID", race_ethnicity=dplyr::any_of("POPGROUP_TTL"))
}

load_dec <- function(api, year, tbl, geo_spec, pop_group=NULL, show_call=FALSE) {
    d = censusapi::getCensus(api, year, key=cens_auth(),
                             vars=str_glue("group({tbl})"),
                             region=geo_spec$region, regionin=geo_spec$regionin,
                             show_call=show_call) %>%
        as_tibble()

    d = d[, str_starts(colnames(d), "[a-z]", negate=TRUE)]
    d$GEO_ID = str_sub(d$GEO_ID, 10)

    pivot_ids = c("GEOID", "NAME")
    if (api == "dec/sf2") {
        if (is.null(pop_group)) {
            pivot_ids = c("GEOID", "NAME", "race_ethnicity")
            d = dplyr::select(d, -.data$POPGROUP) %>%
                dplyr::rename(race_ethnicity = .data$POPGROUP_TTL)
        } else {
            if (!is.character(pop_group) || length(pop_group) != 1) {
                cli_abort("{.arg pop_group} must be a length-1 character vector, or NULL")
            }
            d = dplyr::filter(d, .data$POPGROUP == pop_group) %>%
                dplyr::select(-.data$POPGROUP, -.data$POPGROUP_TTL)
        }
    }

    dplyr::select(d, GEOID="GEO_ID", "NAME",
                  dplyr::everything(), -dplyr::ends_with("ERR")) %>%
        tidyr::pivot_longer(-dplyr::all_of(pivot_ids),
                            names_to="variable", values_to="value")
}

# Internal: get Census data and tidy-format
load_acs <- function(api, year, tbl, geo_spec, show_call=FALSE) {
    d = censusapi::getCensus(api, year, key=cens_auth(),
                             vars=str_glue("group({tbl})"),
                             region=geo_spec$region, regionin=geo_spec$regionin,
                             show_call=show_call) %>%
        # dplyr::mutate(dplyr::across(dplyr::where(~ all(is.na(.))), as.numeric)) %>%
        dplyr::mutate(dplyr::across(c(
            dplyr::ends_with("_EA"), dplyr::ends_with("_MA")
        ), as.character)) %>%
        as_tibble()

    d = d[, str_starts(colnames(d), "[a-z]", negate=TRUE)]
    d$GEO_ID = str_sub(d$GEO_ID, 10)

    dplyr::select(d, GEOID="GEO_ID", "NAME",
                  dplyr::everything(), -dplyr::ends_with("ERR")) %>%
        tidyr::pivot_longer(-c("GEOID", "NAME"),
                            names_pattern="([A-Z][0-9]+[A-Z]?[0-9_]+)([A-Z]+)",
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
        dplyr::select("GEOID", "NAME", "variable", estimate="value")
}
