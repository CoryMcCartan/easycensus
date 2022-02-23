## code to prepare variable datasets goes here
library(dplyr)
library(purrr)
library(stringr)

# Get variable tables ------

vars_acs5 = tidycensus::load_variables(2019, "acs5") %>%
    filter(str_starts(label, "Estimate!!"),
           !str_detect(label, "!!Not defined")) %>%
    mutate(label = str_sub(label, 11))


# Handle repeated tables by race
paren_race_re = "\\((WHITE|BLACK|AMERI|ASIAN|NATIVE|SOME OTHER|TWO OR|HISPANIC)[A-Z, ]+\\)"

# table name to list of nice variables
parse_concept = function(concept) {
    parsed = concept %>%
        str_replace(paren_race_re, "BY RACE-ETHNICITY") %>%
        str_remove_all("AND ") %>%
        str_remove_all("\\(IN 20\\d\\d INFLATION-ADJUSTED DOLLARS\\) ") %>%
        unique() %>%
        str_split(" BY ") %>%
        lapply(janitor::make_clean_names)
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

tbl = filter(vars_acs5, str_starts(name, "C27009"))
parse_table = function(tbl, key) {
    dim_cats = str_split(tbl$label, "!!", simplify=TRUE)[, drop=FALSE]
    const_cats = apply(dim_cats, 2, n_distinct) == 1
    if (nrow(tbl) > 1 && any(const_cats)) dim_cats = dim_cats[, !const_cats, drop=FALSE]
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

    if (depth == length(dim_lbls) + 1L) {
        new_dim = paste0(paste(dim_lbls, collapse="."), "_sub")
        dim_lbls = c(dim_lbls, new_dim)
    } else if (depth > length(dim_lbls)) {
        dim_lbls = str_c(paste(dim_lbls, collapse="."), ".", seq_len(depth))
    } else if (depth < length(dim_lbls)) {
        dim_lbls = dim_lbls[-seq_len(length(dim_lbls) - depth)]
    }

    vars_tbl = dim_cats %>%
        `colnames<-`(dim_lbls) %>%
        as_tibble() %>%
        mutate(variable=tbl$name, .before=1) %>%
        mutate(across(-1, \(x) as.factor(parse_label_group(x))))

    out = list(list(concept = tbl$concept[1],
                    tables = unique(str_sub(tbl$name, 1, -5)),
                    dims = dim_lbls,
                    vars = vars_tbl))
    names(out) = key[[1]][1]
    out
}

tables_acs5 = vars_acs5 %>%
    mutate(table = str_extract(name, "[BC]\\d\\d\\d\\d\\d\\d?")) %>%
    group_by(table) %>%
    group_map(parse_table) %>%
    flatten()

