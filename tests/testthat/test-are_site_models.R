context("test-are_site_models")

test_that("use", {

  testthat::expect_true(are_site_models(create_jc69_site_models(1)))
  testthat::expect_true(are_site_models(create_jc69_site_models(2)))

})
