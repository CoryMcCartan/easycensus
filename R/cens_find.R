#' Find a decennial or ACS census table with variables of interest
#'
#' This function uses fuzzy matching to help identify tables from the census
#' which contain variables of interest. Matched table codes are printed out,
#' along with the Census-provided table description, the parsed variable names,
#' and example table cells. The website <https://censusreporter.org/> may also
#' be useful in finding variables.
#'
#' @param ... Variables to look for. These can be length-1 character vectors,
#'   or, for convenience, can be left unquoted (see examples).
#' @param show How many matching tables to show. Increase this to show more
#'   possible matches, at the cost of more output. Negative values will be
#'   converted to positive but will suppress any printing.
#'
#' @returns The codes for the top `show` tables, invisibly if `show` is positive.
#'
#' @examples
#' cens_find_dec("sex", "age")
#' cens_find_dec(tenure, race)
#' cens_find_acs("income", "sex", show=3)
#' cens_find_acs("heath care", show=-1)
#'
#' @name cens_find
NULL


#' @rdname cens_find
#' @export
cens_find_dec <- function(..., show=2) {
    variables = str_to_upper(vapply(rlang::enquos(...), rlang::as_name, character(1)))
    best = utils::head(match_tables(variables, tables_sf1), abs(show))
    if (show > 0) {
        cli_h1("Top {show} matching table{?s}")
        lapply(tables_sf1[best], print, all=FALSE)
        invisible(best)
    } else {
        best
    }
}

#' @rdname cens_find
#' @export
cens_find_acs <- function(..., show=4) {
    variables = str_to_upper(vapply(rlang::enquos(...), rlang::as_name, character(1)))
    best = utils::head(match_tables(variables, tables_acs, complex_pen=0.1), abs(show))
    if (show > 0) {
        cli_h1("Top {show} matching table{?s}")
        lapply(tables_acs[best], print, all=FALSE)
        invisible(best)
    } else {
        best
    }
}


match_tables = function(variables, table_specs, complex_pen=0.5) {
    concepts = unlist(lapply(table_specs, function(tbl) {
            paste(tbl$concept, paste(str_to_upper(tbl$dims), collapse=" "))
        }))

    # fuzzy match
    avg_dists_1 = apply(adist(variables, concepts, costs=c(1, 1, 10), partial=TRUE), 2, max)
    avg_dists_2 = colMeans(adist(variables, concepts, costs=c(1, 1, 10), partial=FALSE))
    complexity = str_length(concepts)
    avg_dists = avg_dists_1/mean(avg_dists_1) +
        complex_pen*complexity/mean(complexity) +
        0.1*avg_dists_2/mean(avg_dists_2)

    names(sort(avg_dists))
}
