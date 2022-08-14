test_that("tables are found for common use cases", {
    expect_equal(cens_find_dec("sex", "age", show=-1), "P12")
    expect_equal(cens_find_dec("hisp", show=-1), "P4")
})
