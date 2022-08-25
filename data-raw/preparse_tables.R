tables_sf1 = cens_parse_tables("dec/sf1", 2010)
usethis::use_data(tables_sf1, compress="xz", overwrite=TRUE)

tables_acs1 = cens_parse_tables("acs/acs1", 2019)
tables_acs5 = cens_parse_tables("acs/acs5", 2019)

all_vars = unique(c(names(tables_acs1), names(tables_acs5)))

tables_acs = lapply(all_vars, function(nm) {
    vars1 = tables_acs1[[nm]]$vars$variable
    vars5 = tables_acs5[[nm]]$vars$variable
    if (length(vars1) == length(vars5)) {
        out = tables_acs1[[nm]]
        out$surveys = c("acs/acs1", "acs/acs5")
    } else if (length(vars1) > length(vars5)) {
        out = tables_acs1[[nm]]
        out$surveys = "acs/acs1"
    } else {
        out = tables_acs5[[nm]]
        out$surveys = "acs/acs5"
    }
    out
})
names(tables_acs) = all_vars
usethis::use_data(tables_acs, compress="xz", overwrite=TRUE)
