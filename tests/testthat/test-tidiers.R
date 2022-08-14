test_that("tidiers produce no warnings", {
  expect_warning({
      tidy_age(tables_sf1$P12$vars$age)
      tidy_age_bins(tables_sf1$P12$vars$age)
      tidy_age_bins(tables_sf1$P12$vars$age, as_factor=TRUE)
      tidy_race(tables_sf1$P12$vars$race_ethnicity)
      tidy_race(tables_sf1$P8$vars$race_1)
  }, regex=NA) # aka expect _no_ warning
})

test_that("generic tidiers work as expected", {
    test_labs = c("label one (fake)", "label two (fake)")
    expect_equal(levels(tidy_simplify(test_labs)), c("one", "two"))
    expect_equal(levels(tidy_parens(test_labs)), c("label one", "label two"))
})
