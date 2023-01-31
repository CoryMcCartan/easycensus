#' Construct a Geography Specification for Census Data
#'
#' Currently used mostly internally.
#' Builds a Census API-formatted specification of which geographies to download
#' data for. State and county names (or postal abbreviations) are partially
#' matched to existing tables, for ease of use. Other geographies should be
#' specified with Census GEOIDs. The `usgazeteer` package, available with
#' `remotes::install_github("bhaskarvk/usgazetteer")`, may be useful in finding
#' GEOIDs for other geographies. Consult the "geography" sections of each API
#' at <https://www.census.gov/data/developers/data-sets.html> for information on
#' which geographic specifiers may be provided in combination with others.
#'
#' Supported geography arguments:
#'
#' ```{r geos, echo=FALSE, results="asis"}
#' code_names = names(geo)
#' cens_names = unlist(geo)
#' paren_labels = dplyr::if_else(
#'     cens_names == code_names, "",
#'     stringr::str_c(" (", str_to_title(cens_names), ")")
#'     )
#' cat(stringr::str_glue("* `{code_names}`{paren_labels}"), sep="\n")
#' ```
#'
#' @param geo The geographic level to return. One of the machine-readable or
#'   human-readable names listed in the "Details" section. Will return all
#'   matching geographies of this level, as filtered by the further arguments to
#'   `...`.  For example, setting `geo="tract"` is equivalent to setting
#'   `tract="all"`.
#' @param ... Geographies to return, as supported by the Census API. Order
#'   matters here---the first argument will be the geographic level to return
#'   (i.e., it corresponds to the `geo` argument) and additional arguments will
#'   filter the results. Use `"all"`, `"*"`, `NA`, or `TRUE` to return all units
#'   of a particular geography.  See the examples for details.
#' @param check If `TRUE`, validate the provided geographies against the
#'   available geographies from the relevant Census API. Requires the `api` and
#'   `year` arguments to be specified.
#' @param api A Census API programmatic name such as `"acs/acs5"`.
#' @param year The year for the data
#'
#' @returns A list with two elements, `region` and `regionin`, which together
#'   specify a valid Census API geography argument.
#'
#' @examples
#' cens_geo(state="WA")
#' cens_geo("county", state="WA") # equivalent to `cens_geo(county="all", state="WA")`
#' cens_geo(county="King", state="Wash")
#' cens_geo(zcta="02138", check=FALSE)
#' cens_geo(zcta=NA, state="WA", check=FALSE)
#' cens_geo("zcta", state="WA", check=FALSE)
#' cens_geo(cd="09", state="WA", check=FALSE)
#' cens_geo("county_part", state="WA", cd="09", check=FALSE)
#'
#' @export
cens_geo <- function(geo=NULL, ..., check=TRUE, api="acs/acs5", year=2019) {
    geo_in = list(...)
    geo_in = lapply(geo_in, regularize_wild)
    names(geo_in) = vapply(names(geo_in), match_geo, character(1))

    # parse state and county
    has_state = FALSE
    if ("state" %in% names(geo_in)) {
        has_state = TRUE
        geo_in$state = fips_state(geo_in$state)
    }
    idx_cty = which(names(geo_in) == "county" | names(geo_in) == "county (or part)")
    if (length(idx_cty) > 0 && !has_state)
        cli_abort("{.arg state} must be provided if {.arg county} is specified.")
    for (i in idx_cty) {
        geo_in[[i]] = fips_county(geo_in[[i]], geo_in$state)
    }

    # find the 'for' variable
    if (is.null(geo)) {
        if (length(geo_in) == 0) cli_abort("Must provide at least one geographic level.")
        geo_for = match_geo(names(geo_in)[[1]])
        geo_for_val = geo_in[[1]]
        geo_in = geo_in[-1] # remove first from list
    } else {
        if (!is.character(geo) && length(geo) == 1)
            cli_abort("{.arg geo} should be a string.")
        geo_for = match_geo(geo)
        geo_for_val = "*"
    }

    if (isTRUE(check)) {
        # match geographies to available
        d_geo = as_tibble(censusapi::listCensusMetadata(name=api, vintage=year, type="geographies"))
        idx_for = which(geo_for == d_geo$name)
        if (length(idx_for) == 0) cli_abort("Geography level {.val {geo_for}} not found.")
        matched = 0L

        req_test = paste0(sort(names(geo_in)), collapse="|")
        # look for perfect 'requires' match
        req = vapply(d_geo$requires, function(x) paste0(sort(x), collapse="|"), character(1))
        idx_in = which(req_test == req)
        idx_both = intersect(idx_for, idx_in)
        matched = length(idx_both)

        # look for match without wildcards
        if (matched == 0L) {
            req_soft = character(nrow(d_geo))
            for (i in seq_along(req_soft)) {
                req_soft[i] = paste0(setdiff(d_geo$requires[[i]], d_geo$wildcard[[i]]), collapse="|")
            }
            idx_in = which(req_test == req_soft)
            idx_both = intersect(idx_for, idx_in)
            matched = length(idx_both)
            if (matched == 1L) {
                match_geo = d_geo[idx_both, ]
                for (x in d_geo$wildcard[[idx_both]]) {
                    geo_in[[x]] = "*"
                }
            }
        }

        hyper = style_hyperlink("<https://www.census.gov/data/developers/data-sets.html>",
                                "https://www.census.gov/data/developers/data-sets.html")
        if (matched == 0L) {
            cli_abort(c("Geography combination {.val {c(geo_for, names(geo_in))}} not found.",
                        ">"="Check {hyper} and make sure you are using a valid combination
                        for your survey and file."))
        }
        if (matched > 1L) {
            cli_abort(c("Multiple geography combinations found for {.val {c(geo_for, names(geo_in))}}.",
                        ">"="Check {hyper} and make sure you are using a valid combination
                        for your survey and file."))
        }
    }

    if (length(geo_in) > 0) {
        list(region = str_c(geo_for, ":", geo_for_val),
             regionin = paste0(str_c(names(geo_in), ":", geo_in), collapse="+"))
    } else {
        list(region = str_c(geo_for, ":", geo_for_val),
             regionin = NULL)
    }
}

# Internal: match a geography level
match_geo <- function(g) {
    geo_levels = unlist(geo)
    match = dplyr::coalesce(pmatch(g, names(geo)),
                            pmatch(g, geo_levels))
    if (is.na(match)) {
        cli_abort("Geography level {.val {g}} not found.", call=parent.frame())
    } else {
        geo_levels[match]
    }
}

regularize_wild <- function(x) {
    if_else(x == "all" | x == "*" | isTRUE(x) | is.na(x), "*", as.character(x))
}

# Internal: match state to FIPS
fips_state <- function(state) {
    if (is.numeric(state) && state > 0 && state < 1e2) { # already a fips code
        state = str_pad(as.character(as.integer(state)), 2, pad="0")
    } else {
        state = str_to_lower(state)
    }

    match = dplyr::coalesce(pmatch(state, states$fips),
                            pmatch(state, str_to_lower(states$abbr)),
                            pmatch(state, str_to_lower(states$name)))

    if (is.na(match)) {
        cli_abort("No match found for {.val {state}}.", .envir=parent.frame())
    } else {
        states$fips[match]
    }
}

# Internal: match county to FIPS
fips_county <- function(county, state_fips) {
    if (is.numeric(county) && county > 0 && county < 1e5) { # already a fips code
        county = str_pad(as.character(as.integer(county)), 3, pad="0")
    } else {
        county = str_to_lower(county)
    }

    idx_st = which(counties$state == state_fips)
    names = str_to_lower(counties$name[idx_st])
    fips = counties$county[idx_st]

    match = dplyr::coalesce(pmatch(county, fips),
                            pmatch(county, names))

    if (is.na(match)) {
        # cli_abort("No match found for {.val {county}}.", .envir=parent.frame())
        cli_abort("No match found for {.val {county}}.")
    } else {
        fips[match]
    }
}

