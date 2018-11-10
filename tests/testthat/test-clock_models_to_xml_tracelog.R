context("clock_models_to_xml_tracelog")

################################################################################
# Two alignments with unlinked clock models
################################################################################

test_that("RLN RLN ", {

  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_nd2\"/>", # nolint XML
    "<log id=\"rate.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("RLN strict", {

  expected <- c(
    "<log idref=\"clockRate.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict RLN", {

  expected <- c(
    "<log idref=\"ucldMean.c:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ucldStdev.c:anthus_nd2\"/>", # nolint XML
    "<log id=\"rate.c:anthus_nd2\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("strict strict", {

  expected <- c(
    "<log idref=\"clockRate.c:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments with shared clock models
################################################################################

test_that("shared RLN", {

  expected <- c(
    "<log idref=\"ucldStdev.c:anthus_aco\"/>", # nolint XML
    "<log id=\"rate.c:anthus_aco\" spec=\"beast.evolution.branchratemodel.RateStatistic\" branchratemodel=\"@RelaxedClock.c:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>" # nolint XML
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_rln_clock_model(id = "anthus_aco"),
      create_rln_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("shared strict", {

  expected <- c(
    # Nothing
  )
  created <- beautier:::clock_models_to_xml_tracelog(
    list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(id = "anthus_aco")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
