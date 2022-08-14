test_that("decennial tables can be downloaded", {
    skip_if(is.null(cens_auth_env()), "Census API not available")

    d = cens_get_dec("county", "P4", state="RI")
    expect_s3_class(d, "data.frame")
    expect_equal(length(unique(d$GEOID)), 5)
    expect_true(any(grepl("hispanic", names(d))))
    expect_equal(sum(d$value), 1052567)
})
