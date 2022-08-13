test_that("constructor works", {
    x1 = estimate(1:3, 2)
    expect_s3_class(x1, "estimate")

    expect_s3_class(estimate(), "estimate")

    expect_error(estimate(1:3, -2), "nonnegative")
    expect_error(estimate(1:3, se=1, moe=1.96), "Exactly one")
    expect_error(estimate(1:3, se=double()), "recycle")

    x2 = estimate(1, moe=1, conf=0.95)
    expect_equal(vctrs::field(x2, "se"), 1/qnorm(0.975))
})

test_that("printing works", {
    x1 = estimate(pi, exp(-1))
    expect_equal(format(x1), "3.1 ± 0.61")
    expect_equal(format(x1, digits=6), "3.14159 ± 0.605108")
    expect_equal(format(x1, conf=0.5), "3.1 ± 0.25")
})

test_that("coercion and comparison work", {
    x1 = estimate(1, 2)
    x2 = estimate(-1, 5)
    expect_true(x2 < x1)
    expect_s3_class(c(x1, x2), "estimate")
    expect_length(c(x1, x2), 2)
    expect_s3_class(vctrs::vec_c(x1, 5.0), "estimate")
    expect_s3_class(vctrs::vec_c(5.0, x1), "estimate")
    expect_s3_class(as_estimate(5.0), "estimate")
    expect_equal(x1, estimate(1, 2))
    expect_error(vctrs::vec_c(x1, "a"), class = "vctrs_error_incompatible_type")
})

test_that("math works", {
    x1 = estimate(1, 0.2)
    x2 = estimate(1.5, 0.1)

    expect_equal(x1+x2, estimate(2.5, sqrt(0.05)))
    expect_equal(x1-x2, estimate(-0.5, sqrt(0.05)))

    r1 = x1 / x2
    r2 = est_prop(x1, x2)
    expect_gt(field(r1, "se"), field(r2, "se"))

    expect_equal(field(est_pct_chg(x1, x2), "est"), 0.5)

    expect_equal(field(mean(c(x1, x2)), "est"), 1.25)

    r = x1^(0:2)
    expect_equal(r[1], estimate(1, 0))

    r = (0:2)^x1
    expect_equal(r[1:2], estimate(0:1, 0))

    x = estimate(0:5, 0.5)
    r1 = sum(x)
    r2 = mean(x)
    expect_gt(field(r1, "se"), field(r2, "se"))
    expect_equal(field(r2, "est"), 2.5)
})
