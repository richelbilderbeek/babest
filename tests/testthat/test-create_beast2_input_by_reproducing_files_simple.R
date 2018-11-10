context(
  paste(
    "create_beast2_input by reproducing files,",
    "simple and single alignments"
  )
)

test_that("anthus_aco_sub_two_mrca_priors.xml", {

  fasta_filename <- get_beautier_path("anthus_aco_sub.fas")

  created <- create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mcmc = create_mcmc(chain_length = 10000),
    mrca_priors = list(
      create_mrca_prior(
        name = "first_two",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[1:2],
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "last_three",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[3:5],
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = TRUE)
  )

  expected <- readLines(get_beautier_path(
    "anthus_aco_sub_two_mrca_priors.xml")
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("test_0_two_mrca_priors.xml, #30", {

  fasta_filename <- get_beautier_path("test_output_0.fas")

  created <- create_beast2_input(
    input_filenames = fasta_filename,
    clock_models = create_rln_clock_model(
      ucldstdev_distr = create_gamma_distr(
        id = 0,
        alpha = create_alpha_param(id = 2, value = "0.5396"),
        beta = create_beta_param(id = 3, value = "0.3819")
      ),
      mparam_id = 1
    ),
    tree_priors = create_cep_tree_prior(
      pop_size_distr = create_one_div_x_distr(id = 1),
      growth_rate_distr = create_laplace_distr(
        id = 0,
        mu = create_mu_param(id = 4, value = "0.001"),
        scale = create_scale_param(id = 5, value = "30.701135")
      )
    ),
    mrca_priors = list(
      create_mrca_prior(
        name = "all",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "one_and_three",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = get_taxa_names(fasta_filename)[c(2, 5)],
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = FALSE)
  )

  expected <- readLines(get_beautier_path(
    "test_output_0_two_mrca_priors.xml")
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})

test_that("issue_30.xml, #30", {

  fasta_filename <- get_beautier_path("test_output_0.fas")

  created <- create_beast2_input(
    input_filenames = fasta_filename,
    tree_priors = create_yule_tree_prior(
      birth_rate_distr = create_uniform_distr(id = 1)
    ),
    mrca_priors = list(
      create_mrca_prior(
        name = "most",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = c(paste0("t", seq(1, 4))),
        is_monophyletic = FALSE,
        clock_prior_distr_id = 0
      ),
      create_mrca_prior(
        name = "some_mono",
        alignment_id = get_alignment_id(fasta_filename),
        taxa_names = c(paste0("t", seq(2, 3))),
        is_monophyletic = TRUE,
        clock_prior_distr_id = 0
      )
    ),
    misc_options = create_misc_options(nucleotides_uppercase = FALSE)
  )

  expected <- readLines(get_beautier_path(
    "issue_30.xml")
  )
  expect_true(are_equivalent_xml_lines(created, expected))
})
