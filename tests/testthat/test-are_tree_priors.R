context("test-are_tree_priors")

test_that("use", {

  expect_true(
    are_tree_priors(
      create_yule_tree_priors(ids = c("a", "b"))
    )
  )
})
