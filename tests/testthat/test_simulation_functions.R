library(TestDesign)

test_that("simExaminee works", {

  set.seed(1)

  assessment_structure <- createAssessmentStructure(
    n_test  = 3,
    n_phase = 2,
    route_limit_below = 1,
    route_limit_above = 2
  )

  examinee_list <- simExaminees(
    N             = 100,
    mean_v        = c(0, 0, 0),
    sd_v          = c(1, 1, 1),
    cor_v         = diag(1, 3),
    assessment_structure = assessment_structure
  )

  expect_error(
    simExaminees(
      N             = 100,
      mean_v        = c(0, 0),
      sd_v          = c(1, 1),
      cor_v         = diag(1, 2),
      assessment_structure = assessment_structure
    )
  )

})

conditions <- expand.grid(
  overlap_control_policy = c(
    "all", "within_test", "none"
  ),
  transition_policy = c(
    "CI",
    "pool_difficulty_percentile",
    "pool_difficulty_percentile_exclude_administered",
    "on_grade"
  ),
  combine_policy = c(
    "conditional",
    "always",
    "never"
  ),
  estimation_method = c(
    "MLE",
    "MLEF",
    "EAP"
  ),
  stringsAsFactors = FALSE
)

for (condition in 1:dim(conditions)[1]) {

  test_that(
    sprintf(
      "maat() works (%s, %s, %s, %s)",
      conditions$overlap_control_policy[condition],
      conditions$transition_policy[condition],
      conditions$combine_policy[condition],
      conditions$estimation_method[condition]
    ),
    {

      if (condition != 1) {
        skip_on_cran()
        skip_on_ci()
      }

      config <- createShadowTestConfig(
        final_theta = list(
          method = conditions$estimation_method[condition]
        ),
        exclude_policy = list(
          method = "SOFT",
          M = 100
        )
      )

      set.seed(1)
      examinee_list <- examinee_list_math[1:2]

      solution <- maat(
        examinee_list          = examinee_list,
        assessment_structure   = assessment_structure_math,
        module_list            = module_list_math,
        config                 = config,
        cut_scores             = cut_scores_math,
        overlap_control_policy = conditions$overlap_control_policy[condition],
        transition_policy      = conditions$transition_policy[condition],
        combine_policy         = conditions$combine_policy[condition],
        transition_percentile_lower = 0.05,
        transition_percentile_upper = 0.95,
        transition_CI_alpha         = 0.05,
        verbose = FALSE
      )

      o <- getRMSE(solution)
      o <- getBias(solution)
      o <- getSE(solution)
      o <- getAdaptivityIndex(solution)
      o <- getAdministeredItemsPerTest(solution)
      o <- getItemExposureRate(solution)

      plot(solution, type = "route")
      plot(solution, type = "correlation")
      plot(solution, type = "audit")

      expect_true(TRUE)

    }
  )

}
