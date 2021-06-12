assessment_structure <- createAssessmentStructure(
  n_test  = 3,
  n_phase = 2,
  route_limit_below = 1,
  route_limit_above = 2
)

cor_v <- matrix(.8, 3, 3)
diag(cor_v) <- 1

examinee_list <- simExaminees(
  N = 10,
  mean_v = c(0, 0, 0),
  sd_v   = c(1, 1, 1),
  cor_v  = cor_v,
  assessment_structure = assessment_structure,
  initial_grade = "G4",
  initial_phase = "P1",
  initial_test  = "T1"
)

fn <- system.file("extdata", "module_definition_MATH_normal_N500.csv", package = "maat")
module_list <- loadModules(
  fn = fn,
  assessment_structure = assessment_structure,
  examinee_list = examinee_list,
  base_path = system.file(package = "maat")
)

cut_scores <- list(
  G3 = c(-1.47, -0.55, 0.48),
  G4 = c(-1.07, -0.15, 0.88),
  G5 = c(-0.67,  0.25, 1.28),
  G6 = c(-0.27,  0.65, 1.68),
  G7 = c( 0.13,  1.05, 2.08),
  G8 = c( 0.53,  1.45, 2.48)
)

library(TestDesign)
config <- createShadowTestConfig(
  interim_theta = list(method = "EAP"),
  final_theta = list(method = "EAP")
)

examinee_list_output <- maat(
  examinee_list          = examinee_list,
  assessment_structure   = assessment_structure,
  module_list            = module_list,
  config                 = config,
  cut_scores             = cut_scores,
  overlap_control_policy = "within_test",
  transition_policy      = "CI",
  combine_policy         = "conditional",
  transition_CI_alpha         = 0.05,
  transition_percentile_lower = 0.05,
  transition_percentile_upper = 0.95,
  initial_theta_list = NULL,
  prior_mean_policy = "mean_difficulty",
  prior_mean_user = NULL,
  prior_sd = 1,
  verbose = TRUE
)

