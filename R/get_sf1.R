find_table <- function(..., show=2) {
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

    best = names(head(sort(avg_dists), abs(show)))

    # print out
    if (show > 0) {
        cli_h2("Top {show} matching table{?s}")
        for (tbl in best) {
            spec = tables_sf1[[tbl]]
            n_vars = nrow(spec$vars)
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
            cat("\n")
        }
    }

    invisible(best)
}

get_sf1 <- function(geography, table, ..., drop_total=FALSE) {
    spec = tables_sf1[[table]]
    suppressMessages({
        d = do.call(dplyr::bind_rows, lapply(spec$tables, function(tbl_code) {
            tidycensus::get_decennial(geography, table=tbl_code,
                                      sumfile="sf1", output="tidy", ...)
        }))
    })

    tbl_vars = spec$vars
    if (isTRUE(drop_total))
        tbl_vars = dplyr::filter(tbl_vars, if_all(-1, function(x) x != "total"))
    inner_join(d, tbl_vars, by="variable")
}
