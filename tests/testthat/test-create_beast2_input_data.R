context("create_beast2_input_data")

test_that("two alignments", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_nd2.fas")

  testthat::expect_silent(
    create_beast2_input_data(
      input_filenames = c(fasta_filename_1, fasta_filename_2)
    )
  )
})

test_that("alignments start with a capital", {

  fasta_filename_1 <- beautier::get_beautier_path("anthus_aco.fas")
  fasta_filename_2 <- beautier::get_beautier_path("anthus_nd2.fas")

  lines <- create_beast2_input_data(
    input_filenames = c(fasta_filename_1, fasta_filename_2),
    create_misc_options(
      capitalize_first_char_id = TRUE
    )
  )
  testthat::expect_equal(lines[2], "id=\"Anthus_aco\"")

})
