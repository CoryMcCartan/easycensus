## code to prepare variable datasets goes here
library(dplyr)
library(purrr)
library(stringr)

vars_sf1 = tidycensus::load_variables(2010, "sf1") %>%
    filter(str_starts(label, "Total!!") | str_ends(name, "001001"),
           !str_detect(label, "!!Not defined"))


# Handle repeated tables by race
paren_race_re = "\\((WHITE|BLACK|AMERI|ASIAN|NATIVE|SOME OTHER|TWO OR|HISPANIC)[A-Z, ]+\\)"

# table name to list of nice variables
parse_concept = function(concept) {
    parsed = concept %>%
        str_replace(paren_race_re, "BY RACE-ETHNICITY") %>%
        str_remove_all("AND ") %>%
        unique() %>%
        str_split(" BY ") %>%
        lapply(janitor::make_clean_names)
    parsed[[which.max(lengths(parsed))]]
}

# table column/row name to nice label
parse_label_group = function(lbl) {
    if_else(lbl == "", "total", lbl) %>%
        str_to_lower()
}

parse_table = function(tbl, key) {
    dim_cats = str_split(tbl$label, "!!", simplify=TRUE)[, -1, drop=FALSE]
    paren_cats = str_extract(tbl$concept, paren_race_re)
    if (any(!is.na(paren_cats))) {
        new_col = coalesce(paren_cats, "") %>%
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
        mutate(variable=tbl$name, .before=1) %>%
        mutate(across(-1, \(x) as.factor(parse_label_group(x))))

    out = list(list(concept = tbl$concept[1],
                    tables = unique(str_sub(tbl$name, 1, -4)),
                    dims = dim_lbls,
                    vars = vars_tbl))
    names(out) = key[[1]][1]
    out
}

tables_sf1 = vars_sf1 %>%
    mutate(table = str_extract(name, "[PH]C?[OT]?\\d\\d\\d")) %>%
    group_by(table) %>%
    group_map(parse_table) %>%
    flatten()

# then go make tables_acs
usethis::use_data(tables_sf1, tables_acs, overwrite=TRUE, compress="xz", internal=TRUE)
rm(tables_sf1, tables_acs)
