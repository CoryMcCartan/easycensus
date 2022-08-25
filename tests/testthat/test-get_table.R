test_that("decennial tables can be downloaded", {
    skip_if(is.null(cens_auth_env()), "Census API not available")

    d = cens_get_dec("P4", "county", state="RI")
    expect_s3_class(d, "data.frame")
    expect_equal(length(unique(d$GEOID)), 5)
    expect_true(any(grepl("hispanic", names(d))))
    expect_equal(sum(d$value), 1052567)
})

test_that("ACS tables can be downloaded", {
    skip_if(is.null(cens_auth_env()), "Census API not available")

    d = cens_get_acs("B09001", "county", state="RI")
    expect_s3_class(d, "data.frame")
    expect_equal(length(unique(d$GEOID)), 5)
    expect_true(any(grepl("age", names(d))))
    expect_s3_class(d$value, "estimate")
    x = sum(d$value)
    expect_equal(get_est(x), 621325)
    expect_equal(get_se(x), 2515, tolerance=0.01)
})
