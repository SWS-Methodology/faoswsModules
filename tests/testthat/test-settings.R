context("Settings file")
test_that("Handles missing settings file",{
          expect_error(ReadSettings("badfile"), "doesn't exist")
  })
