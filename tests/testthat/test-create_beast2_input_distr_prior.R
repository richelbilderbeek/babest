context("create_beast2_input_distr_prior")

test_that("Yule Yule", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_aco\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_nd2\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_aco\" name=\"distribution\" x=\"@birthRate.t:anthus_aco\">", # nolint XML
    "        <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_nd2\" name=\"distribution\" x=\"@birthRate.t:anthus_nd2\">", # nolint XML
    "        <Uniform id=\"Uniform.4\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "        <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::create_beast2_input_distr_prior(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_nd2")
    ),
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "anthus_aco",
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      create_yule_tree_prior(
        id = "anthus_nd2",
        birth_rate_distr = create_uniform_distr(id = 4)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})

test_that("Yule Yule Yule", {

  expected <- c(
    "<distribution id=\"prior\" spec=\"util.CompoundDistribution\">", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_aco\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_aco\" tree=\"@Tree.t:anthus_aco\"/>", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_nd2\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_nd2\" tree=\"@Tree.t:anthus_nd2\"/>", # nolint XML
    "    <distribution id=\"YuleModel.t:anthus_nd3\" spec=\"beast.evolution.speciation.YuleModel\" birthDiffRate=\"@birthRate.t:anthus_nd3\" tree=\"@Tree.t:anthus_nd3\"/>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_aco\" name=\"distribution\" x=\"@birthRate.t:anthus_aco\">", # nolint XML
    "        <Uniform id=\"Uniform.1\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_nd2\" name=\"distribution\" x=\"@birthRate.t:anthus_nd2\">", # nolint XML
    "        <Uniform id=\"Uniform.4\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"YuleBirthRatePrior.t:anthus_nd3\" name=\"distribution\" x=\"@birthRate.t:anthus_nd3\">", # nolint XML
    "        <Uniform id=\"Uniform.7\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"ClockPrior.c:anthus_nd2\" name=\"distribution\" x=\"@clockRate.c:anthus_nd2\">", # nolint XML
    "        <Uniform id=\"Uniform.3\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "    <prior id=\"ClockPrior.c:anthus_nd3\" name=\"distribution\" x=\"@clockRate.c:anthus_nd3\">", # nolint XML
    "        <Uniform id=\"Uniform.6\" name=\"distr\" upper=\"Infinity\"/>", # nolint XML
    "    </prior>", # nolint XML
    "</distribution>" # nolint XML
  )
  created <- beautier:::create_beast2_input_distr_prior(
    site_models = list(
      create_jc69_site_model(id = "anthus_aco"),
      create_jc69_site_model(id = "anthus_nd2"),
      create_jc69_site_model(id = "anthus_nd3")
    ),
    clock_models = list(
      create_strict_clock_model(id = "anthus_aco"),
      create_strict_clock_model(
        id = "anthus_nd2",
        clock_rate_distr = create_uniform_distr(id = 3)
      ),
      create_strict_clock_model(
        id = "anthus_nd3",
        clock_rate_distr = create_uniform_distr(id = 6)
      )
    ),
    tree_priors = list(
      create_yule_tree_prior(
        id = "anthus_aco",
        birth_rate_distr = create_uniform_distr(id = 1)
      ),
      create_yule_tree_prior(
        id = "anthus_nd2",
        birth_rate_distr = create_uniform_distr(id = 4)
      ),
      create_yule_tree_prior(
        id = "anthus_nd3",
        birth_rate_distr = create_uniform_distr(id = 7)
      )
    )
  )
  testthat::expect_true(beautier:::are_equivalent_xml_lines(created, expected))
})
