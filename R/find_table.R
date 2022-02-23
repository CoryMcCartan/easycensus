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
#' @returns The codes for the top `show` tables, invisibly.
#'
#' @examples
#' find_dec_table("sex", "age")
#' find_dec_table(tenure, race)
#' find_acs_table("income", "sex", show=3)
#' find_acs_table("heath care", show=-1)
#'
#' @name find_table
NULL


#' @rdname find_table
#' @export
find_dec_table <- function(..., show=2) {
    variables = str_to_upper(vapply(rlang::enquos(...), rlang::as_name, character(1)))
    best = utils::head(match_tables(variables, tables_sf1), abs(show))
    if (show > 0) output_matching_tables(best, tables_sf1)
    invisible(best)
}

#' @rdname find_table
#' @export
find_acs_table <- function(..., show=4) {
    variables = str_to_upper(vapply(rlang::enquos(...), rlang::as_name, character(1)))
    best = utils::head(match_tables(variables, tables_acs, complex_pen=0.1), abs(show))
    if (show > 0) output_matching_tables(best, tables_acs, acs=TRUE)
    invisible(best)
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


output_matching_tables = function(codes, table_specs, acs=FALSE) {
    cli_h1("Top {length(codes)} matching table{?s}")
    for (tbl in codes) {
        spec = table_specs[[tbl]]
        n_vars = nrow(spec$vars)
        cli_text("\n")
        cli_text(style_bold(style_inverse(c("\u00a0", tbl, "\u00a0"))), #nbsp
                 style_bold(c(" - ", spec$concept)))

        if (isTRUE(acs)) {
            cli_text("Availability:")
            acs1_avail = mean(spec$vars$acs1)
            acs5_avail = mean(spec$vars$acs5)
            if (acs5_avail == 0) {
                cli_alert_danger("5-year ACS")
            } else if (acs5_avail == 1) {
                cli_alert_success("5-year ACS")
            } else {
                cli_alert_warning("(Partial) 5-year ACS")
            }
            if (acs1_avail == 0) {
                cli_alert_danger("1-year ACS")
            } else if (acs1_avail == 1) {
                cli_alert_success("1-year ACS")
            } else {
                cli_alert_warning("(Partial) 1-year ACS")
            }
        }

        cli_text("Parsed variables:")
        cli_ul(items = str_c("{.field ", spec$dims, "}"))

        cli_text("Example values:")
        ex_items = as.matrix(dplyr::select(spec$vars, -dplyr::any_of(c("acs5", "acs1"))))
        if (ncol(ex_items) > 1) {
            ex_items = ex_items[sample.int(n_vars, min(n_vars, 3)), -1, drop=FALSE]
            ex_items = apply(ex_items, 1, function(x) paste(x, collapse=" / "))
        } else {
            ex_items = ex_items[1, -1]
        }
        cli_ul(items = ex_items)
        cli_end()
    }
}
