#' Tidy labels in census tables
#'
#' Some table labels are quite verbose, and users will often want to shorten them.
#' These functions make tidying common types of labels easy.
#' Most produce straightforward output, but there are several more generic tidiers:
#' * [tidy_simplify()] attempts to simplify labels by removing words common to all labels.
#' * [tidy_parens()] attempts to simplify labels by removing all terms in parentheses.
#'
#' @param x A factor, which will be re-leveled. Character vectors will be converted to factors.
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
    xlev = str_squish(str_remove(levels(x), "years?"))
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
                        "hispanic or latino"="hisp",
                        "white alone, not hispanic or latino"="white_nh")

labs_ethnicity = c("hispanic or latino"="hisp",
                   "not hispanic or latino"="nonhisp")
