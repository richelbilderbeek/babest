context("test-create_beast2_input_run")


test_that("usage, two alignments", {

  fasta_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- get_alignment_ids(fasta_filenames)

  testthat::expect_silent(
    beautier:::create_beast2_input_run(
      ids = ids,
      tree_priors = list(
        create_yule_tree_prior(
          id = ids[1],
          birth_rate_distr = create_uniform_distr(id = 1)
        ),
        create_yule_tree_prior(
          id = ids[2],
          birth_rate_distr = create_uniform_distr(id = 2)
        )
      ),
      clock_models = list(
        create_strict_clock_model(
          id = ids[1],
          clock_rate_distr = create_uniform_distr(id = 3)
        ),
        create_strict_clock_model(
          id = ids[2],
          clock_rate_distr = create_uniform_distr(id = 4)
        )
      )
    )
  )
})

test_that("usage, two alignments, fixed crown ages", {

  fasta_filenames <- beautier::get_beautier_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  ids <- get_alignment_ids(fasta_filenames)

  testthat::expect_silent(
    beautier:::create_beast2_input_run(
      ids = ids,
      tree_priors = list(
        create_yule_tree_prior(
          id = ids[1],
          birth_rate_distr = create_uniform_distr(id = 1)
        ),
        create_yule_tree_prior(
          id = ids[2],
          birth_rate_distr = create_uniform_distr(id = 2)
        )
      ),
      clock_models = list(
        create_strict_clock_model(
          id = ids[1],
          clock_rate_distr = create_uniform_distr(id = 3)
        ),
        create_strict_clock_model(
          id = ids[2],
          clock_rate_distr = create_uniform_distr(id = 4)
        )
      ),
      fixed_crown_ages = c(TRUE, TRUE)
    )
  )
})
