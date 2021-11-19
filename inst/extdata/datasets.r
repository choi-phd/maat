library(maat)

assessment_structure_math <- createAssessmentStructure(
  n_test = 3,
  n_phase = 2,
  route_limit_below = 1,
  route_limit_above = 2
)

cor_v <- matrix(.8, 3, 3)
diag(cor_v) <- 1

set.seed(1)
examinee_list_math <- simExaminees(
  N = 10,
  mean_v = c(0, 0, 0),
  sd_v   = c(1, 1, 1),
  cor_v  = cor_v,
  assessment_structure = assessment_structure_math,
  initial_grade = "G4",
  initial_phase = "P1",
  initial_test  = "T1"
)

module_list_math <- loadModules(
  "inst/extdata/module_definition_MATH_normal_N500_flexible.csv",
  base_path = "inst",
  assessment_structure = assessment_structure_math,
  examinee_list = examinee_list_math
)

cut_scores_math <- list(
  G3 = c(-1.47, -0.55, 0.48),
  G4 = c(-1.07, -0.15, 0.88),
  G5 = c(-0.67,  0.25, 1.28),
  G6 = c(-0.27,  0.65, 1.68),
  G7 = c( 0.13,  1.05, 2.08),
  G8 = c( 0.53,  1.45, 2.48)
)

usethis::use_data(assessment_structure_math, overwrite = TRUE)
usethis::use_data(examinee_list_math, overwrite = TRUE)
usethis::use_data(module_list_math, overwrite = TRUE)
usethis::use_data(cut_scores_math, overwrite = TRUE)
