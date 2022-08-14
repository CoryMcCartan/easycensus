
#' Download data from a decennial census or ACS table
#'
#' Leverages [censusapi::getCensus()] to download tables of census data. Tables
#' are returned in tidy format, with variables given tidy, human-readable names.
#'
#' @param geography The geography level to download data for. Usually one of
#'   `state`, `county`, `tract`, `block group`, `block`, `zcta`, etc. Consult
#'   <https://walker-data.com/tidycensus/articles/basic-usage.html#geography-in-tidycensus>
#'   for more information.
#' @param table The table code to download. See [cens_find_dec()] for help
#'   identifying a table of interest. Note: some tables are split into
#'   A/B/C/etc. versions by race; this function unifies all of these tables
#'   under one code. So, for example, use `P012`, not `P012A`.
#' @param state The state to get data for, if any.
#' @param county The state to get data for, if any.
#' @param year For ACS data, the survey year to get data for.
#' @param survey For ACS data, whether to use the one-year or
#'   five-year survey (the default). Make sure to check availability using
#'   [cens_find_acs()].
#' @param ... Further arguments passed to [tidycensus::get_decennial()] or
#'   [tidycensus::get_acs()], e.g. `year`, `state`, `county`, `geometry`.
#' @param drop_total Whether to filter out variables which are totals across
#'   another variable. Recommended only after inspection of the underlying
#'   table.
#'
#' @returns A tibble of census data in tidy format, with columns
#'   `GEOID`, `NAME`, `variable` (containing the Census variable code),
#'   `value` or `estiamte`, `moe` in the case of ACS tables,
#'   and additional factor columns specific to the table.
#'
#' @examples \dontrun{
#' cens_get_dec("state", "P003")
#' cens_get_dec("state", "H002")
#' cens_get_dec("county", "H002", state="WA", drop_total=TRUE)
#'
#' cens_get_acs("county subdivision", "B09001", state="WA", county="King")
#' }
#'
#' @name cens_get
NULL

#' @rdname cens_get
#' @export
cens_get_dec <- function(geography, table, state=NULL, county=NULL,
                          ..., drop_total=FALSE) {
    if (!table %in% names(tables_sf1))
        cli_abort("Table {.field {table}} not found.")

    spec = tables_sf1[[table]]
    suppressMessages({
    tryCatch({
        d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
            tidycensus::get_decennial(geography, variables=spec$vars$variable,
                                      sumfile="sf1", year=2010, state=state,
                                      county=county, output="tidy", ...)
        }))
    },
    error = function(e) {
        rlang::abort(e$message)
    })
    })

    tbl_vars = spec$vars
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, dplyr::if_all(-1, function(x) x != "total"))
    dplyr::inner_join(d, tbl_vars, by="variable")
}

#' @rdname cens_get
#' @export
cens_get_acs <- function(geography, table, year=2019, state=NULL, county=NULL,
                          survey=c("acs5", "acs1"), ..., drop_total=FALSE) {
    if (!table %in% names(tables_acs))
        cli_abort("Table {.field {table}} not found.")
    survey = match.arg(survey)

    spec = tables_acs[[table]]
    suppressMessages({
        tryCatch({
            vars = spec$vars$variable[spec$vars[[survey]]]
            d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
                tidycensus::get_acs(geography, variables=vars,
                                    survey=survey, year=year, state=state,
                                    county=county, output="tidy", ...)
            }))
        },
        error = function(e) {
            rlang::abort(e$message)
        })
    })

    d = dplyr::transmute(d,
                         GEOID = .data$GEOID,
                         NAME = .data$NAME,
                         variable = .data$variable,
                         value = .env$estimate(.data$estimate, moe=.data$moe))

    tbl_vars = dplyr::select(spec$vars, -dplyr::any_of(c("acs1", "acs5")))
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, dplyr::if_all(-1, function(x) x != "total"))
    dplyr::inner_join(d, tbl_vars, by="variable")
}
