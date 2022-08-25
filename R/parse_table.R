#' Parsed Census SF1 and ACS Tables
#'
#' Contains parsed table information for the 2010 Decennial Summary File 1 and
#' 2019 ACS 5-year and 1-year tables.
#' This parsed information is used internally in [cens_find_dec()],
#' [cens_find_acs()], [cens_get_dec()], and [cens_get_acs()].
#' For other sets of tables, try using [cens_parse_tables()].
#'
#' @format A list of `cens_table` objects, which are just lists with four elements:
#'   - `concept`, a human-readable name
#'   - `tables`, the constituent table codes
#'   - `surveys`, the supported surveys
#'   - `dims`, the parsed names of the dimensions of the tables
#'   - `vars`, a `tibble` with all of the parsed variable values
#'
#' @name tables
NULL

#' @rdname tables
"tables_sf1"

#' @rdname tables
"tables_acs"


#' Attempt to Parse Tables from a Census API
#'
#' Uses the same parsing code as that which generates [tables_sf1] and [tables_acs]
#' See <https://www.census.gov/data/developers/data-sets.html> for a list of
#' APIs and corresponding years, or use [censusapi::listCensusApis()].
#'
#' @param api A Census API programmatic name such as `"acs/acs5"`.
#' @param year The year for the data
#'
#' @returns A list of `cens_table` objects, which are just lists with four elements:
#'   - `concept`, a human-readable name
#'   - `tables`, the constituent table codes
#'   - `surveys`, the supported surveys
#'   - `dims`, the parsed names of the dimensions of the tables
#'   - `vars`, a `tibble` with all of the parsed variable values
#'
#' @examples \dontrun{
#' cens_parse_tables("dec/pl", 2020)
#' }
#'
cens_parse_tables = function(api, year) {
    vars = get_survey_vars(api, year)

    dplyr::group_map(vars, parse_table) %>%
        unlist(recursive=FALSE) %>%
        lapply(function(x) {
            x$surveys = api
            x
        }) %>%
        lapply(new_cens_table)
}

# Internal functions to parse Census tables with the API ------

get_survey_vars = function(api, year) {
    raw = censusapi::listCensusMetadata(api, vintage=year, type="variables") %>%
        as_tibble() %>%
        dplyr::mutate(label = str_trim(.data$label))
    if (str_starts(api, "dec")) {
        raw = dplyr::filter(raw,
                            str_starts(.data$label, "(!!)?Total:?!!") |
                                str_ends(.data$name, "001001"),
                            !str_detect(.data$label, "!!Not defined"))
    } else if (str_starts(api, "acs")) {
        raw = dplyr::filter(raw,
                            str_starts(.data$label, "Estimate!!"),
                            !str_detect(.data$label, "!!Not defined")) %>%
            dplyr::mutate(label = str_sub(.data$label, 11))
    }
    raw %>%
        dplyr::mutate(table = str_extract(.data$group, "[A-Z]*\\d+")) %>%
        dplyr::group_by(table) %>%
        dplyr::arrange(.data$group)
}

# Handle repeated tables by race
paren_race_re = "\\((WHITE|BLACK|AMERI|ASIAN|NATIVE|SOME OTHER|TWO OR|HISPANIC)[A-Z, ]+\\)"

tidy_names = function(x) {
    str_to_lower(x) %>%
        make.names(unique = TRUE) %>%
        str_replace_all("\\.", "_")
}

# table name to list of nice variables
parse_concept = function(concept) {
    parsed = concept %>%
        str_replace(paren_race_re, "BY RACE-ETHNICITY") %>%
        str_remove_all("AND ") %>%
        str_remove_all("\\(IN 20\\d\\d INFLATION-ADJUSTED DOLLARS\\) ") %>%
        unique() %>%
        str_split(" BY ") %>%
        lapply(tidy_names)
    parsed[[which.max(lengths(parsed))]]
}

# table column/row name to nice label
parse_label_group = function(lbl) {
    if_else(lbl == "", "total", lbl) %>%
        str_remove(":") %>%
        str_remove(" --") %>%
        str_trim() %>%
        str_to_lower()
}


parse_table = function(tbl, key) {
    dim_cats = str_split(tbl$label, "!!", simplify=TRUE)[, drop=FALSE]
    const_cats = apply(dim_cats, 2, dplyr::n_distinct) == 1
    if (nrow(tbl) > 1 && any(const_cats)) dim_cats = dim_cats[, !const_cats, drop=FALSE]
    paren_cats = str_extract(tbl$concept, paren_race_re)
    if (any(!is.na(paren_cats))) {
        new_col = dplyr::coalesce(paren_cats, "") %>%
            str_remove_all("[()]")
        dim_cats = cbind(dim_cats, new_col, deparse.level=0)
    }

    depth = ncol(dim_cats)
    if (depth == 0L) {
        dim_cats = matrix("")
        depth = 1L
    }

    dim_lbls = parse_concept(unique(tbl$concept))

    short_lbls = str_replace_all(dim_lbls, "_", " ") %>%
        str_remove_all("(or|and|of|the|for|in|,) ") %>%
        str_to_title() %>%
        abbreviate(5, named=FALSE) %>%
        str_to_lower() %>%
        paste(collapse="_")
    if (depth == length(dim_lbls) + 1L) {
        new_dim = paste0(short_lbls, "_sub")
        dim_lbls = c(dim_lbls, new_dim)
    } else if (depth > length(dim_lbls)) {
        dim_lbls = str_c(short_lbls, "_", seq_len(depth))
    } else if (depth < length(dim_lbls)) {
        dim_lbls = dim_lbls[-seq_len(length(dim_lbls) - depth)]
    }

    vars_tbl = dim_cats %>%
        `colnames<-`(dim_lbls) %>%
        as_tibble() %>%
        dplyr::mutate(variable=tbl$name, .before=1) %>%
        dplyr::mutate(dplyr::across(-.data$variable, function(x) as.factor(parse_label_group(x)))) %>%
        dplyr::arrange(.data$variable)

    out = list(list(concept = tbl$concept[1],
                    tables = sort(unique(tbl$group)),
                    dims = dim_lbls,
                    vars = vars_tbl))
    names(out) = key[[1]][1]
    out
}
