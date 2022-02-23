test_that("decennial tables can be downloaded", {
    check_api()

    d = get_dec_table("county", "P004", state="RI")
    expect_s3_class(d, "data.frame")
    expect_equal(length(unique(d$GEOID)), 5)
    expect_true(any(grepl("hispanic", names(d))))
    expect_equal(sum(d$value), 1052567)
})
