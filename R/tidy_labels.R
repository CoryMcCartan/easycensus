#' Tidy labels in census tables
#'
#' Some table labels are quite verbose, and users will often want to shorten them.
#' These functions make tidying common types of labels easy.
#' Most produce straightforward output, but there are several more generic tidiers:
#' * [tidy_simplify()] attempts to simplify labels by removing words common to all labels.
#' * [tidy_parens()] attempts to simplify labels by removing all terms in parentheses.
#' * [tidy_race_detailed()] creates logical columns for each of the six racial categories.
#'
#' @param x A factor, which will be re-leveled. Character vectors will be converted to factors.
#' @param x2,x3 Additional character columns containing detailed information for certain variables (e.g. detailed race)
#'
#' @return A re-leveled factor, except for [tidy_age_bins()], which by default
#'   returns a data frame with columns `age_from` and `age_to` (inclusive).
#'
#' @examples
#' ex_race_long = c("american indian and alaska native alone", "asian alone",
#'     "black or african american alone", "hispanic or latino",
#'     "native hawaiian and other pacific islander alone",
#'     "some other race alone", "total", "two or more races",
#'     "white alone", "white alone, not hispanic or latino")
#' tidy_race(ex_race_long)
#'
#' tidy_age_bins(c("10 to 14 years", "21 years", "85 years and over"))
#'
#' tidy_parens(c("label one (fake)", "label two (fake)"))
#' tidy_simplify(c("label one (fake)", "label two (fake)"))
#'
#' \dontrun{ # requires API key
#' d = cens_get_acs("B02003", "us", year=2019, survey="acs1")
#' dplyr::mutate(d, tidy_race_detailed(dtldr_1, dtldr_2, dtldr_3))
#' }
#'
#' @name tidiers
NULL

check_fc = function(x) {
    if (is.character(x)) {
        x = as.factor(x)
    }
    if (!is.factor(x)) {
        cli_abort("{.arg x} must be a {.cls factor} or {.cls character}",
                  call=rlang::caller_env())
    }
    x
}

#' @rdname tidiers
#' @export
tidy_race = function(x) {
    x = check_fc(x)
    xlev = str_squish(str_remove(levels(x), "household(er)?"))
    levels(x) = labs_race_ethnicity[xlev]
    x
}

#' @rdname tidiers
#' @export
tidy_race_detailed = function(x, x2, x3) {
    x = check_fc(x)
    x2 = check_fc(x2)
    x3 = as.character(check_fc(x3))
    xlev = str_squish(str_remove(levels(x), "household(er)?"))
    xlev2 = str_squish(str_remove(levels(x2), "household(er)?"))
    x = xlev[as.integer(x)]
    x2 = xlev2[as.integer(x2)]

    drop_row = (x == "total" | x2 == "total" |
                    (str_starts(x2, "population of") & x3 == "total"))
    x[drop_row] = NA_character_
    x2[drop_row] = NA_character_
    x3[drop_row] = NA_character_
    idx_alone = which(x3 == "total")
    x3[idx_alone] = str_remove(x2[idx_alone], " alone")

    races = c(white="white", black="black or african american",
              aian="american indian and alaska native", asian="asian",
              nhpi="native hawaiian and other pacific islander", other="some other race")

    x3s = str_split(x3, "; ", simplify=FALSE)

    out = list()
    for (i in seq_along(races)) {
        out[[names(races)[i]]] = vapply(x3s, function(x) any(x == races[i]), logical(1))
    }

    as_tibble(out)
}

#' @rdname tidiers
#' @export
tidy_ethnicity = function(x) {
    x = check_fc(x)
    xlev = str_squish(str_remove(levels(x), "household(er)?"))
    levels(x) = labs_ethnicity[xlev]
    x
}

#' @rdname tidiers
#' @export
tidy_age = function(x) {
    x = check_fc(x)
    xlev = levels(x)
    xlev = str_replace(str_remove(xlev, "years?"), "under 1", "0")
    levels(x) = str_squish(xlev)
    x
}

#' @param as_factor if `TRUE`, return a factor with levels of the form `[10,14]`.
#' @rdname tidiers
#' @export
tidy_age_bins = function(x, as_factor=FALSE) {
    x = check_fc(x)
    xlev = str_remove(levels(x), "householder")
    xlev = str_squish(str_remove(xlev, "years?"))
    xlev = str_replace(xlev, "under", "0 to")
    xlev = str_replace(xlev, "total", "0 to Inf")
    xlev = str_replace(xlev, "over", "Inf")

    m = str_split(xlev, " (and|to) ", simplify=TRUE)
    if (ncol(m) != 2)
        cli_abort("Problem parsing age categories: {.code {levels(x)}}")
    m[m[, 2] == "", 2] = m[m[, 2] == "", 1]
    m = matrix(as.numeric(m), nrow=nrow(m))
    colnames(m) = c("age_from", "age_to")

    if (isFALSE(as_factor)) {
        as_tibble(m[as.integer(x), , drop=FALSE])
    } else {
        close_br = dplyr::if_else(m[, 2] == Inf, ")", "]")
        levels(x) = str_glue("[{m[,1]},{m[,2]}{close_br}")
        x
    }
}


#' @param as_factor if `TRUE`, return a factor with levels of the form `[35,40]`.
#' @rdname tidiers
# @export # TODO re-export for 0.3.0
tidy_income_bins = function(x, as_factor=FALSE) {
    x = check_fc(x)
    xlev = str_squish(str_remove_all(levels(x), "[$,]"))
    xlev = str_replace(xlev, "less than", "0 to")
    xlev = str_replace(xlev, "total", "0 to Inf")
    xlev = str_replace(xlev, "or more", "to Inf")

    m = str_split(xlev, " to ", simplify=TRUE)
    if (ncol(m) != 2)
        cli_abort("Problem parsing income categories: {.code {levels(x)}}")
    m[m[, 2] == "", 2] = m[m[, 2] == "", 1]
    nine_rows = str_ends(m[, 2], "999")
    m = matrix(as.numeric(m), nrow=nrow(m))
    m[nine_rows, 2] = m[nine_rows, 2] + 1
    m = m / 1e3
    colnames(m) = c("inc_from", "inc_to")

    if (isFALSE(as_factor)) {
        as_tibble(m[as.integer(x), , drop=FALSE])
    } else {
        levels(x) = str_glue("$[{m[,1]},{m[,2]})k")
        x
    }
}

#' @rdname tidiers
#' @export
tidy_simplify = function(x) {
    x = check_fc(x)
    xlev = str_squish(levels(x))
    common_words = Reduce(intersect, str_split(xlev, " "))
    common_regex = str_c("(", paste("\\Q", common_words, "\\E", sep="", collapse="|"), ")")
    xlev = str_squish(str_remove_all(xlev, common_regex))
    levels(x) = xlev
    x
}

#' @rdname tidiers
#' @export
tidy_parens = function(x) {
    x = check_fc(x)
    levels(x) = str_squish(str_remove_all(levels(x), "\\(.+\\)"))
    x
}



labs_race_ethnicity = c("total"="total",
                        "population of one race"="one",
                        "white alone"="white",
                        "black or african american alone"="black",
                        "american indian and alaska native alone"="aian",
                        "asian alone"="asian",
                        "native hawaiian and other pacific islander alone"="nhpi",
                        "some other race alone"="other",
                        "two or more races"="two",
                        "population of two or more races"="two",
                        "population of two races"="two",
                        "population of three races"="two",
                        "population of four races"="two",
                        "population of five races"="two",
                        "population of six races"="two",
                        "hispanic or latino"="hisp",
                        "white alone, not hispanic or latino"="white_nh")

labs_ethnicity = c("hispanic or latino"="hisp",
                   "not hispanic or latino"="nonhisp")
