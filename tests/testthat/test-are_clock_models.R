context("test-are_clock_models")

test_that("use", {

  testthat::expect_true(are_clock_models(create_strict_clock_models(1)))
  testthat::expect_true(are_clock_models(create_strict_clock_models(2)))

})
