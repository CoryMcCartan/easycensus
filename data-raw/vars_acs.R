## code to prepare variable datasets goes here
library(dplyr)
library(purrr)
library(stringr)

# Get variable tables ------

vars_sf1 = tidycensus::load_variables(2010, "sf1") %>%
    filter(str_starts(label, "Total!!") | str_ends(name, "001001"),
           !str_detect(label, "!!Not defined"))
vars_acs5 = tidycensus::load_variables(2019, "acs5") %>%
    filter(str_starts(label, "Estimate!!Total:"),
           !str_detect(label, "!!Not defined")
    mutate(label = str_sub(label, 17))


# Handle repeated tables by race
paren_race_re = "\\((WHITE|BLACK|AMERI|ASIAN|NATIVE|SOME OTHER|TWO OR|HISPANIC)[A-Z, ]+\\)"
paren_race_names = c("(WHITE ALONE)"="white",
                     "(BLACK OR AFRICAN AMERICAN ALONE)"="black",
                     "(AMERICAN INDIAN AND ALASKA NATIVE ALONE)"="aian",
                     "(ASIAN ALONE)"="asian",
                     "(NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)"="nhpi",
                     "(SOME OTHER RACE ALONE)"="other",
                     "(TWO OR MORE RACES)"="two",
                     "(HISPANIC OR LATINO)"="hisp",
                     "(WHITE ALONE, NOT HISPANIC OR LATINO)"="white_nh")

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

tbl = filter(vars_sf1, str_starts(name, "P016"))
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

    list(table = key[[1]][1],
         concept = tbl$concept[1],
         dims = dim_lbls,
         vars = vars_tbl)
}

x = vars_sf1 %>%
    mutate(table = str_extract(name, "[PH]C?[OT]?\\d\\d\\d")) %>%
    group_by(table) %>%
    group_map(parse_table)




paren_total_re = "\\(?TOTAL RACES TALLIED\\)?( FOR HOUSEHOLDERS)?"
concepts_sf1 = vars_sf1$concept %>%
    unique() %>%
    subset(!str_detect(., paren_total_re)) %>%
    flatten_chr() %>%
    str_trim() %>%
    unique()

concepts_sf1[str_detect(concepts_sf1, "\\(")]
with(vars_sf1, concept[str_detect(concept, "AGE") & str_detect(concept, "SEX")]) %>% unique()
with(vars_sf1, concept[str_count(concept, "( BY )|( \\()") == 3]) %>% unique()
with(vars_sf1, concept[str_count(concept, "( BY )|( \\()") == 2]) %>% unique()











tidy_vars = function(v, acs=FALSE) {
    table_codes = str_sub(v$name, end=-4L - acs)
    split(v, table_codes) %>%
        lapply(function(x) {
            vars = x$label
            names(vars) = str_sub(x$name, start=-3L)
            list(concept = x$concept[1],
                 vars = vars)
        })
}
tables_sf1 = tidy_vars(vars_sf1)
tables_acs5 = tidy_vars(vars_acs5, acs=TRUE)

usethis::use_data(tables_sf1, tables_acs5, overwrite=TRUE, compress="xz", internal=TRUE)
