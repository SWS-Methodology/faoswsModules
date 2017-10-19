context("PublishPackage")

test_that("FilterIgnored filters ignored values correctly",{
  # Correct filtering of comments
  expect_identical(FilterIgnored("x.csv", "#"), "x.csv")
  # Correct filtering of the normal case
  expect_identical(FilterIgnored("x.csv", "\\.csv$"), character())
})
