test_that("tables are found for common use cases", {
    expect_equal(find_dec_table("sex", "age", show=-1), "P012")
    expect_equal(find_dec_table("hisp", show=-1), "P004")
})
