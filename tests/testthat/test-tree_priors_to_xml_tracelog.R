context("test-tree_priors_to_xml_tracelog")

################################################################################
# Two alignments with unlinked clock models
################################################################################

test_that("BD BD ", {

  expected <- c(
    "<log idref=\"BirthDeath.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"BDBirthRate.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"BDDeathRate.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"BirthDeath.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"BDBirthRate.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"BDDeathRate.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_tracelog(
    list(
      create_bd_tree_prior(id = "anthus_aco"),
      create_bd_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CBS CBS", {

  expected <- c(
    "<log idref=\"BayesianSkyline.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"bPopSizes.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"bGroupSizes.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"BayesianSkyline.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"bPopSizes.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"bGroupSizes.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_tracelog(
    list(
      create_cbs_tree_prior(id = "anthus_aco"),
      create_cbs_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CCP CCP", {

  expected <- c(
    "<log idref=\"popSize.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"CoalescentConstant.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"popSize.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"CoalescentConstant.t:anthus_nd2\"/>" # nolint XML

  )
  created <- beautier:::tree_priors_to_xml_tracelog(
    list(
      create_ccp_tree_prior(id = "anthus_aco"),
      create_ccp_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("CEP CEP", {

  expected <- c(
    "<log idref=\"CoalescentExponential.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"ePopSize.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"growthRate.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"CoalescentExponential.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"ePopSize.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"growthRate.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_tracelog(
    list(
      create_cep_tree_prior(id = "anthus_aco"),
      create_cep_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("Yule Yule", {

  expected <- c(
    "<log idref=\"YuleModel.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"birthRate.t:anthus_aco\"/>", # nolint XML
    "<log idref=\"YuleModel.t:anthus_nd2\"/>", # nolint XML
    "<log idref=\"birthRate.t:anthus_nd2\"/>" # nolint XML
  )
  created <- beautier:::tree_priors_to_xml_tracelog(
    list(
      create_yule_tree_prior(id = "anthus_aco"),
      create_yule_tree_prior(id = "anthus_nd2")
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

################################################################################
# Two alignments with shared tree priors
################################################################################

test_that("shared BD", {

  testthat::expect_error(
    beautier:::tree_priors_to_xml_tracelog(
      list(
        create_bd_tree_prior(id = "anthus_aco"),
        create_bd_tree_prior(id = "anthus_aco")
      )
    ),
    "Cannot have linked tree priors"
  )

})

test_that("shared CBS", {

  testthat::expect_error(
    beautier:::tree_priors_to_xml_tracelog(
      list(
        create_cbs_tree_prior(id = "anthus_aco"),
        create_cbs_tree_prior(id = "anthus_aco")
      )
    ),
    "Cannot have linked tree priors"
  )
})

test_that("shared CCP", {

  testthat::expect_error(
    beautier:::tree_priors_to_xml_tracelog(
      list(
        create_ccp_tree_prior(id = "anthus_aco"),
        create_ccp_tree_prior(id = "anthus_aco")
      )
    ),
    "Cannot have linked tree priors"
  )
})

test_that("shared CEP", {

  testthat::expect_error(
    beautier:::tree_priors_to_xml_tracelog(
      list(
        create_cep_tree_prior(id = "anthus_aco"),
        create_cep_tree_prior(id = "anthus_aco")
      )
    ),
    "Cannot have linked tree priors"
  )
})

test_that("shared Yule", {

  testthat::expect_error(
    beautier:::tree_priors_to_xml_tracelog(
      list(
        create_yule_tree_prior(id = "anthus_aco"),
        create_yule_tree_prior(id = "anthus_aco")
      )
    ),
    "Cannot have linked tree priors"
  )
})
