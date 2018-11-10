context("bbt_run")

test_that("use, two alignments, estimated crown ages", {
  testit::assert(beastier::is_beast2_installed())

  out <- NA

  testthat::expect_silent(
    out <- bbt_run(
      fasta_filenames = get_babette_paths(
        c("anthus_aco.fas", "anthus_nd2.fas")
      ),
      mcmc = create_mcmc(chain_length = 1000, store_every = 1000)
    )
  )
  testthat::expect_true("estimates" %in% names(out))
  testthat::expect_true("anthus_aco_trees" %in% names(out))
  testthat::expect_true("anthus_nd2_trees" %in% names(out))
  testthat::expect_true("operators" %in% names(out))
  testthat::expect_equal(class(out$anthus_aco_trees[[1]]), "phylo")
  testthat::expect_equal(class(out$anthus_nd2_trees[[1]]), "phylo")
  testthat::expect_equal(length(out$anthus_aco_trees), 2)
  testthat::expect_equal(length(out$anthus_nd2_trees), 2)

  testthat::expect_true("Sample" %in% names(out$estimates))
  testthat::expect_true("posterior" %in% names(out$estimates))
  testthat::expect_true("likelihood" %in% names(out$estimates))
  testthat::expect_true("prior" %in% names(out$estimates))
  testthat::expect_true("treeLikelihood.aco" %in% names(out$estimates))
  testthat::expect_true("treeLikelihood.nd2" %in% names(out$estimates))
  testthat::expect_true("TreeHeight.aco" %in% names(out$estimates))
  testthat::expect_true("TreeHeight.nd2" %in% names(out$estimates))
  testthat::expect_true("YuleModel.aco" %in% names(out$estimates))
  testthat::expect_true("YuleModel.nd2" %in% names(out$estimates))
  testthat::expect_true("birthRate.aco" %in% names(out$estimates))
  testthat::expect_true("birthRate.nd2" %in% names(out$estimates))

  testthat::expect_true("operator" %in% names(out$operators))
  testthat::expect_true("p" %in% names(out$operators))
  testthat::expect_true("accept" %in% names(out$operators))
  testthat::expect_true("reject" %in% names(out$operators))
  testthat::expect_true("acceptFC" %in% names(out$operators))
  testthat::expect_true("rejectFC" %in% names(out$operators))
  testthat::expect_true("rejectIv" %in% names(out$operators))
  testthat::expect_true("rejectOp" %in% names(out$operators))
})

################################################################################
# Initial phylogenies
################################################################################

test_that("JC69 JC69 strict strict coalescent_exp_population", {

  input_fasta_filename_1 <- get_babette_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_babette_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_cep_tree_prior()
  lines <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

test_that("TN93 TN93 strict strict yule", {

  input_fasta_filename_1 <- get_babette_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_babette_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_tn93_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})



test_that("GTR GTR strict strict yule", {

  input_fasta_filename_1 <- get_babette_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_babette_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_gtr_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})


test_that("GTR TN93 strict strict yule", {

  input_fasta_filename_1 <- get_babette_path("anthus_aco.fas")
  input_fasta_filename_2 <- get_babette_path("anthus_nd2.fas")
  input_filenames <- c(input_fasta_filename_1, input_fasta_filename_2)
  site_model_1 <- create_gtr_site_model()
  site_model_2 <- create_tn93_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_strict_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(are_beast2_input_lines(lines))
})

test_that("JC69 JC69 strict relaxed_log_normal Yule", {

  input_filenames <- get_babette_paths(
    c("anthus_aco.fas", "anthus_nd2.fas")
  )
  site_model_1 <- create_jc69_site_model()
  site_model_2 <- create_jc69_site_model()
  clock_model_1 <- create_strict_clock_model()
  clock_model_2 <- create_rln_clock_model()
  tree_prior <- create_yule_tree_prior()
  lines <- create_beast2_input(
    input_filenames = input_filenames,
    site_models = list(site_model_1, site_model_2),
    clock_models = list(clock_model_1, clock_model_2),
    tree_priors = list(tree_prior, tree_prior)
  )
  testthat::expect_true(
    are_beast2_input_lines(lines)
  )
})
