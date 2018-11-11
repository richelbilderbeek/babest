context("test-create_beast2_input_operators")

test_that("Multiple fixed_crown_ages, interface", {
  input_filenames <- get_beautier_paths(c("anthus_aco.fas", "anthus_nd2.fas"))
  ids <- get_alignment_ids(input_filenames)

  testthat::expect_silent(
    beautier:::create_beast2_input_operators(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      fixed_crown_ages = c(TRUE, TRUE)
    )
  )

})
