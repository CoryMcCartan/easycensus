
# Internal `cens_table` constructor
new_cens_table = function(x) {
    if (!all(c("concept", "tables", "surveys", "dims", "vars") %in% names(x))) {
        cli_abort("Invalid table.")
    }
    structure(x, class="cens_table")
}

match_survey = function(x) {
    dplyr::case_when(x == "acs/acs1" ~ "ACS / 1-year Detailed",
                     x == "acs/acs5" ~ "ACS / 5-year Detailed",
                     x == "dec/sf1" ~ "Decennial / Summary File 1",
                     x == "dec/pl" ~ "Decennial / P.L. 94-171",
                     TRUE ~ x)
}

#' @export
print.cens_table = function(x, ..., all=FALSE) {
    n_vars = nrow(x$vars)
    cli_text("\n")
    cli_text(style_bold(style_inverse(c("\u00a0", x$tables[1], "\u00a0"))), #nbsp
             style_bold(c(" - ", x$concept)))

    cli_text("Surveys / Files:")
    lapply(match_survey(x$surveys), cli_alert_success)

    cli_text("Parsed variables:")
    cli_ul(items = str_c("{.field ", x$dims, "}"))

    ex_items = as.matrix(x$vars)[, -1, drop=FALSE]
    if (isTRUE(all)) {
        cli_text("All values:")
    } else {
        cli_text("Example values:")
        ex_items = ex_items[sample.int(n_vars, min(n_vars, 3)), , drop=FALSE]
    }

    if (ncol(ex_items) > 1) {
        ex_items = apply(ex_items, 1, function(x) paste(x, collapse=" / "))
    } else {
        ex_items = ex_items[, 1]
    }

    cli_ul(items = ex_items)
    cli_end()
}
