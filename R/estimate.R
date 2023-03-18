#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name estimate-vctrs
NULL

# Constructors and type check ---------

#' Estimate class
#'
#' A numeric vector that stores margin-of-error information along with it.
#' The margin of error will update through basic arithmetic operations, using a
#' first-order Taylor series approximation. The implicit assumption is that the
#' errors in each value are uncorrelated. If in fact there is correlation, the
#' margins of error could be wildly under- or over-estimated.
#'
#' @param x A numeric vector containing the estimate(s).
#' @param se A numeric vector containing the standard error(s) for the
#'   estimate(s). Users should supply either `se` or `moe` and `conf`.
#' @param moe A numeric vector containing the margin(s) of error. Users should
#'   supply either `se` or `moe` and `conf`.
#' @param conf The confidence level to use in converting the margin of error to
#'   a standard error. Defaults to 90%, which is what the Census Bureau uses for
#'   ACS estimates.
#'
#' @returns An `estimate` vector.
#'
#' @examples
#' estimate(5, 2) # 5 with std. error  2
#' estimate(15, moe=3) - estimate(5, moe=4)
#' estimate(1:4, 0.1) * estimate(1, 0.1)
#'
#' @export
estimate = function(x, se=NULL, moe=NULL, conf=0.9) {
    # length-0 case
    if (missing(x) || length(x) == 0) {
        return(new_estimate())
    }

    if (is.null(se) + is.null(moe) != 1) {
        cli_abort("Exactly one of  {.arg se} or {.arg moe} must be provided.")
    }

    if (!is.null(moe)) {
        if (any(moe < 0, na.rm=TRUE)) {
            cli_abort("{.arg moe} is a margin of error and must be nonnegative.")
        }
        if (conf <= 0 || conf >= 1) {
            cli_abort("{.arg conf} must be between 0 and 1.")
        }

        se = moe / conf_z(conf)
    }

    if (!is.numeric(x) || !is.numeric(se)) {
        cli_abort("All arguments must be numeric.")
    }

    if (any(se < 0, na.rm=TRUE)) {
        cli_abort("{.arg se} is a standard error and must be nonnegative.")
    }

    do.call(new_estimate, vctrs::vec_recycle_common(est=x, se=se))
}

# internal constructor
new_estimate = function(est=double(), se=double()) {
    vctrs::new_rcrd(list(est=est, se=se), class="estimate")
}

#' @export
#' @rdname estimate
is_estimate = function(x) {
    inherits(x, "estimate")
}

#' Extract estimates, standard errors, and margins of error
#'
#' Getter functions for [estimate()] vectors.
#'
#' @param x An [estimate] vector.
#' @param conf The confidence level to use in constructing the margin of error.
#'
#' @return An [estimate] vector.
#'
#' @examples
#' x = estimate(1, 0.1)
#' get_est(x)
#' get_moe(x)
#'
#' @rdname est_extract
#' @export
get_est = function(x) {
    if (!is_estimate(x)) cli_abort("{.arg x} must be an {.cls estimate}")
    field(x, "est")
}

#' @rdname est_extract
#' @export
get_se = function(x) {
    if (!is_estimate(x)) cli_abort("{.arg x} must be an {.cls estimate}")
    field(x, "se")
}

#' @rdname est_extract
#' @export
get_moe = function(x, conf = 0.9) {
    if (!is_estimate(x)) cli_abort("{.arg x} must be an {.cls estimate}")
    field(x, "se") * conf_z(conf)
}

#' Convert an [estimate] to an `rvar`
#'
#' The [posterior::rvar] class may be useful in handling standard errors for
#' more complicated mathematical expressions. This function assumes a Normal
#' distribution centered on the estimate, with standard deviation equal to the
#' standard error of the estimate. The `posterior` package is required for this
#' function.
#'
#' @param x An [estimate] vector.
#' @param n How many samples to draw.
#'
#' @return A [posterior::rvar] vector.
#'
#' @examples
#' x = estimate(1, 0.1)
#' if (requireNamespace("posterior", quietly=TRUE)) {
#'     rv_x = to_rvar(x)
#'     (rv_x^2 / rv_x) - rv_x # std. errors zero (correct)
#'     x^2 / x - x # std. errors not zero
#' }
#'
#' @rdname est_extract
#' @export
to_rvar = function(x, n=500) {
    rlang::check_installed("posterior")
    posterior::rvar_rng(rnorm, n=length(x),
                        mean=field(x, "est"), sd=field(x, "se"))
}

# helper throughout
conf_z = function(p) -qnorm((1 - p) / 2)


# Coercion / casting --------

#' @export
vec_ptype2.estimate.estimate = function(x, y, ...) new_estimate()

#' @export
vec_ptype2.double.estimate = function(x, y, ...) new_estimate()
#' @export
vec_ptype2.estimate.double = function(x, y, ...) new_estimate()

#' @export
vec_ptype2.integer.estimate = function(x, y, ...) new_estimate()
#' @export
vec_ptype2.estimate.integer = function(x, y, ...) new_estimate()


#' @export
vec_cast.estimate.estimate = function(x, to, ...) x

#' @export
vec_cast.double.estimate = function(x, to, ...) field(x, "est")
#' @export
vec_cast.estimate.double = function(x, to, ...) estimate(x, se=0.0)

#' @export
vec_cast.estimate.integer = function(x, to, ...) estimate(as.double(x), se=0.0)

#' @rdname estimate
#' @export
as_estimate <- function(x) {
    vec_cast(x, new_estimate())
}


# Equality / comparison -------

#' @export
vec_proxy_compare.estimate = function(x, ...) field(x, "est")


# Math -------

#' @method vec_arith estimate
#' @export
vec_arith.estimate = function(op, x, y, ...) {
    UseMethod("vec_arith.estimate", y)
}
#' @method vec_arith.estimate default
#' @export
vec_arith.estimate.default = function(op, x, y, ...) {
    stop_incompatible_op(op, x, y)
}

#' @method vec_arith.estimate estimate
#' @export
vec_arith.estimate.estimate = function(op, x, y, ...) {
    est_x = field(x, "est")
    est_y = field(y, "est")
    se_x = field(x, "se")
    se_y = field(y, "se")

    switch(
        op,
        "+" = new_estimate(est_x + est_y,
                           sqrt(se_x^2 + se_y^2)),
        "-" = new_estimate(est_x - est_y,
                          sqrt(se_x^2 + se_y^2)),
        "*" = new_estimate(est_x * est_y,
                          sqrt(est_x^2 * se_x^2 + est_y^2 * se_y^2)),
        "/" = new_estimate(est_x / est_y,
                           sqrt(se_x^2 + (est_x / est_y)^2 * se_y^2)),
        stop_incompatible_op(op, x, y)
    )
}

#' @method vec_arith.estimate numeric
#' @export
vec_arith.estimate.numeric = function(op, x, y, ...) {
    recyc = vec_recycle_common(x=x, y=y)
    est_x = field(recyc$x, "est")
    se_x = field(recyc$x, "se")
    y = recyc$y
    n = length(y)

    switch(
        op,
        "+" = new_estimate(est_x + y, se_x),
        "-" = new_estimate(est_x - y, se_x),
        "*" = new_estimate(est_x * y, se_x * y),
        "/" = new_estimate(est_x / y, se_x / y),
        "^" = if_else(y == 0,
                      new_estimate(rep(1, n), rep(0, n)),
                      if_else(y == 1, recyc$x,
                              new_estimate(est_x ^ y, y * est_x^(y-1) * se_x)
                      )
        ),
        stop_incompatible_op(op, x, y)
    )
}
#' @method vec_arith.numeric estimate
#' @export
vec_arith.numeric.estimate = function(op, x, y, ...) {
    recyc = vec_recycle_common(x=x, y=y)
    est_y = field(recyc$y, "est")
    se_y = field(recyc$y, "se")
    x = recyc$x
    n = length(x)

    switch(
        op,
        "+" = new_estimate(est_y + x, se_y),
        "-" = new_estimate(est_y - x, se_y),
        "*" = new_estimate(est_y * x, se_y * x),
        "/" = new_estimate(est_y / x, se_y / x),
        "^" = if_else(x == 0,
                      new_estimate(rep(0, n), rep(0, n)),
                      new_estimate(est_y^x, est_y^x * log(x) * se_y)
        ),
        stop_incompatible_op(op, x, y)
    )
}

#' @method vec_arith.estimate MISSING
#' @export
vec_arith.estimate.MISSING <- function(op, x, y, ...) {
    switch(op,
           `-` = new_estimate(-field(x, "est"), field(x, "se")),
           `+` = x,
           stop_incompatible_op(op, x, y)
    )
}

#' @export
vec_math.estimate <- function(.fn, .x, ...) {
    est = field(.x, "est")
    se = field(.x, "se")
    n = length(est)

    switch(.fn,
           sum = new_estimate(sum(est), sqrt(sum(se^2))),
           mean = new_estimate(mean(est), sqrt(sum(se^2)) / n),
           abs = {
               abs_est = abs(est)
               if (all(abs_est >= 3*se, na.rm=T)) {
                   new_estimate(abs_est, se)
               } else {
                   cli_warn("Some values too close to zero to calculate absolute value errors.")
                   abs_est
               }
           },
           sign = {
               if (all(abs(est) >= 3*se, na.rm=T)) {
                   new_estimate(sign(est), rep(0, n))
               } else {
                   cli_warn("Some values too close to zero to calculate absolute value errors.")
                   abs_est
               }
           },
           sqrt = new_estimate(sqrt(est), 0.5 * se / sqrt(est)),
           exp = new_estimate(exp(est), exp(est) * se),
           expm1 = new_estimate(expm1(est), exp(est) * se),
           log = new_estimate(log(est), se / est),
           log1p = new_estimate(log1p(est), se / (1 + est)),
           cos = new_estimate(cos(est), -sin(est) * se),
           sin = new_estimate(sin(est), cos(est) * se),
           tan = new_estimate(cos(est), se / cos(est)^2),
           {
               cli_warn(c("Function {.fn {.fn}} not supported.",
                          ">"="Coerce to numeric to suppress this warning."))
               vec_math_base(.fn, est)
           }
    )
}


#' Specialized margin-of-error calculations
#'
#' Proportions and percent-change-over-time calculations require different
#' standard error calculations.
#'
#' @param x,y An [estimate] vector. For `est_pct_chg()`, calculates the % change
#'   from `x` to `y` (i.e., \eqn{(y-x)/x})
#'
#' @return An [estimate] vector.
#'
#' @examples
#' x = estimate(1, 0.1)
#' y = estimate(1.5, 0.1)
#' est_prop(x, y)
#' est_pct_chg(x, y)
#'
#' @rdname est_special
#' @export
est_prop = function(x, y) {
    est_x = field(x, "est")
    est_y = field(y, "est")
    se_x = field(x, "se")
    se_y = field(y, "se")

    new_var = se_x^2 - (est_x / est_y)^2 * se_y^2
    new_var = if_else(new_var >= 0, new_var,
                      se_x^2 + (est_x / est_y)^2 * se_y^2)
    new_estimate(est_x / est_y, sqrt(new_var))
}

#' @rdname est_special
#' @export
est_pct_chg = function(x, y) {
    y/x - 1
}


# Formatting ---------

#' Format an estimate
#'
#' Format an estimate for pretty printing
#'
#' @param x An [estimate] vector
#' @param conf The confidence level to use in converting the margin of error to
#'   a standard error. Defaults to 90%, which is what the Census Bureau uses for
#'   ACS estimates.
#' @param digits The number of dig
#' @param trim logical; if `FALSE`, logical, numeric and complex values are
#'   right-justified to a common width: if `TRUE` the leading blanks for
#'   justification are suppressed.
#' @param ... Ignored.
#' @param formatter the formatting function to use internally
#'
#' @export
format.estimate = function(x, conf = 0.9, digits = 2, trim = FALSE, ..., formatter=fmt_plain) {
    out = formatter(x, conf=conf, digits=digits, trim=trim)
    out[is.na(x)] = NA_character_
    out
}

fmt_plain = function(x, conf=0.9, digits=2, trim=FALSE) {
    est = fmt_est(field(x, "est"), digits=digits, trim=trim)
    moe = fmt_moe(field(x, "se"), conf=conf, digits=digits, trim=trim)
    str_c(est, " \u00B1 ", moe)
}

fmt_color = function(x, conf=0.9, digits=2, trim=FALSE) {
    est = fmt_est(field(x, "est"), digits=digits, trim=trim)
    moe = fmt_moe(field(x, "se"), conf=conf, digits=digits, trim=trim)
    str_c(est, pillar::style_subtle(str_c(" \u00B1 ", moe)))
}

fmt_est = function(x, digits=2, trim=FALSE) {
    format(x, justify="right",
           digits=digits, scientific=2, trim=trim)
}

fmt_moe = function(x, conf = 0.9, digits=2, trim=FALSE) {
    format(conf_z(conf) * x,
           justify="left", digits=digits, scientific=2, trim=trim)
}


#' @export
vec_ptype_abbr.estimate = function(x, ..., prefix_named = FALSE, suffix_shape = TRUE) "est"

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.estimate <- function(x, ...) {
    out <- format(x, formatter=fmt_color)
    pillar::new_pillar_shaft_simple(out, align="right", min_width=9)
}

