context("create_beast2_input")
# Does
# * check the interface
# * check if XML created is valid with minimal tests
# Does not
# * check if valid XML files are reproduced.
#   'test-create_beast2_input_by_reproducing_files.R' does that
# * check if XML created is valid with thorough tests.
#   'test-create_beast2_input_file.R' does that

test_that("abuse: two alignments", {

  input_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- get_alignment_ids(input_filenames)

  # Two filenames, one site model
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      site_models = create_jc69_site_models(ids = "only_one")
    ),
    "Must supply as much input_filenames as site_models"
  )

  # Two filenames, one clock model
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      clock_models = create_strict_clock_models(ids = ids[1])
    ),
    "Must supply as much input_filenames as clock_models"
  )

  # Two filenames, one tree prior
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      tree_priors = create_yule_tree_priors(ids = ids[1])
    ),
    "Must supply as much input_filenames as tree priors"
  )

  # Two filenames, two RLN clock models
  testthat::expect_error(
    create_beast2_input(
      input_filenames = input_filenames,
      clock_models = list(
        create_rln_clock_model(id = ids[1]),
        create_rln_clock_model(id = ids[1])
      )
    ),
    "Cannot have shared Relaxed Log-Normal clock models"
  )
})
