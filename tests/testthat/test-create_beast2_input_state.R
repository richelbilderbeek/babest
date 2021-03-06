context("test-create_beast2_input_state")

test_that("abuse", {

  ids <- c("a", "b")

  # Two ids OK
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = beautier:::init_tree_priors(
        create_yule_tree_priors(ids = ids), ids = ids),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one site model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = "only_one"),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one clock model
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(
        ids = "only_one"), # One too little
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one tree prior
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = "only_one")),
      initial_phylogenies = rep(NA, 2)
    )
  )

  # Two ids, one phylogeny
  testthat::expect_error(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = init_tree_priors(create_yule_tree_priors(ids = ids)),
      initial_phylogenies = rep(NA, 1)
    )
  )

})

test_that("two phylogenies", {

  ids <- c("Anthus_nd2", "Anthus_aco")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = c(NA, NA)
    )
  )

})

test_that("three phylogenies", {

  ids <- c("aco", "nd2", "nd3")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
    )
  )

})

test_that("four phylogenies", {

  ids <- c("aco", "nd2", "nd3", "nd4")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
    )
  )

})

test_that("four phylogenies, shared clock", {

  ids <- c("aco", "nd2", "nd3", "nd4")
  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids[1]),
      tree_priors = create_yule_tree_priors(ids = ids),
      initial_phylogenies = rep(NA, length(ids))
    )
  )

})

test_that("two alignments, two initial phylogenies", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_nd2.fas")
  ids <- get_alignment_ids(c(fasta_filename_1, fasta_filename_2))
  phylo1 <- fasta_to_phylo(fasta_filename_1, crown_age = 314)
  phylo2 <- fasta_to_phylo(fasta_filename_2, crown_age = 42)
  initial_phylogenies <- c(phylo1, phylo2)

  testthat::expect_silent(
    beautier:::create_beast2_input_state(
      site_models = create_jc69_site_models(ids = ids),
      clock_models = create_strict_clock_models(ids = ids),
      tree_priors = beautier:::init_tree_priors(
        create_yule_tree_priors(ids = ids), ids = ids
      ),
      initial_phylogenies = initial_phylogenies
    )
  )

})
