context("test-create_beast2_input_1_12")


test_that("abuse: two alignments", {

  input_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- get_alignment_ids(input_filenames)

  # Two filenames, one site model
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    ),
    "Must supply as much input_filenames as site_models"
  )

  # Two filenames, one clock model
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      clock_models = create_strict_clock_models(ids = ids[1])
    ),
    "Must supply as much input_filenames as clock_models"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    ),
    "Must supply as much input_filenames as tree priors"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      fixed_crown_ages = TRUE
    ),
    "Must supply as much input_filenames as fixed crown ages"
  )

  # Two filenames, one phylogeny
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      initial_phylogenies = c(ape::rcoal(4))
    ),
    "Must supply as much input_filenames as initial_phylogenies"
  )


  # Two filenames, two RLN clock models
  testthat::expect_error(
    create_beast2_input_1_12(
      input_filenames = input_filenames,
      clock_models = list(
        create_rln_clock_model(id = ids[1]),
        create_rln_clock_model(id = ids[1])
      )
    ),
    "Cannot have shared Relaxed Log-Normal clock models"
  )
})
