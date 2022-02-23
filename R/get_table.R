
#' Download data from a decennial census table
#'
#' @param geography The geography level to download data for. Usually one of
#'   `state`, `county`, `tract`, `block group`, `block`, `zcta`, etc.
#' @param table The table code to download. See [find_dec_table()] for help
#'   identifying a table of interest. Note: some tables are split into
#'   A/B/C/etc. versions by race; this function unifies all of these tables
#'   under one code. So, for example, use `P012`, not `P012A`.
#' @param ... Arguments passed to [tidycensus::get_decennial()], e.g. `year`,
#'   `state`, `county`, `geometry`.
#' @param drop_total Whether to filter out variables which are totals across
#'   another variable. Recommended only after inspection of the underlying
#'   table.
#'
#' @returns A tibble of decennial Census data in tidy format, with columns
#'   `GEOID`, `NAME`, `variable` (containing the Census variable code), `value`,
#'   and additional factor columns specific to the table.
#'
#' @examples \dontrun{
#' get_dec_table("state", "P003")
#' get_dec_table("state", "H002")
#' get_dec_table("state", "H002", drop_total=TRUE)
#' }
#'
#' @export
get_dec_table <- function(geography, table, ..., drop_total=FALSE) {
    spec = tables_sf1[[table]]
    suppressMessages({
    tryCatch({
        d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
            tidycensus::get_decennial(geography, variables=spec$vars$variable,
                                      sumfile="sf1", output="tidy", ...)
        }))
    },
    error = function(e) {
        rlang::abort(e)
    })
    })

    tbl_vars = spec$vars
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, if_all(-1, function(x) x != "total"))
    dplyr::inner_join(d, tbl_vars, by="variable")
}
