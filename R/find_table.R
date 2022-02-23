#' Find a decennial census table with variables of interest
#'
#' This function uses fuzzy matching to help identify SF1 tables from the census
#' which contain variables of interest. Matched table codes are printed out,
#' along with the Census-provided table description, the parsed variable names,
#' and example table cells.
#'
#' @param ... Variables to look for. These can be length-1 character vectors,
#'   or, for convenience, can be left unquoted (see examples).
#' @param show How many matching tables to show. Increase this to show more
#'   possible matches, at the cost of more output.
#'
#' @returns The codes for the top `show` tables, invisibly.
#'
#' @examples
#' find_dec_table("sex", "age")
#' find_dec_table(tenure, race, show=1)
#'
#' @export
find_dec_table <- function(..., show=2) {
    variables = str_to_upper(vapply(rlang::enquos(...), rlang::as_name, character(1)))
    concepts = unlist(lapply(tables_sf1, function(tbl) {
            paste(tbl$concept, paste(str_to_upper(tbl$dims), collapse=" "))
        }))

    # fuzzy match
    avg_dists_1 = apply(adist(variables, concepts, costs=c(1, 1, 10), partial=TRUE), 2, max)
    avg_dists_2 = colMeans(adist(variables, concepts, costs=c(1, 1, 10), partial=FALSE))
    complexity = str_length(concepts)
    avg_dists = avg_dists_1/mean(avg_dists_1) +
        0.5*complexity/mean(complexity) +
        0.1*avg_dists_2/mean(avg_dists_2)

    best = names(utils::head(sort(avg_dists), abs(show)))
    if (show > 0) output_matching_tables(best, tables_sf1)
    invisible(best)
}


output_matching_tables = function(codes, table_specs) {
    cli_h1("Top {length(codes)} matching table{?s}")
    for (tbl in codes) {
        spec = table_specs[[tbl]]
        n_vars = nrow(spec$vars)
        cli_text("\n")
        cli_text(style_bold(style_inverse(c("\u00a0", tbl, "\u00a0"))), #nbsp
                 style_bold(c(" - ", spec$concept)))

        cli_text("Parsed variables:")
        cli_ul(items = str_c("{.field ", spec$dims, "}"))

        cli_text("Example values:")
        ex_items = as.matrix(spec$vars)
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
