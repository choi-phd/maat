#' @include module_class.R
NULL

#' Class 'examinee': a single examinee
#'
#' \code{\linkS4class{examinee}} is an S4 class to represent a single examinee.
#'
#' @slot examinee_id the ID of examinee.
#' @slot current_grade the current grade the examinee is in.
#' @slot current_phase the current phase the examinee is in.
#' @slot current_test the current test the examinee is in.
#' @slot current_module the current module the examinee is in.
#' @slot grade_log grades that the examinee belonged at each module position.
#' @slot phase_log phases that the examinee belonged at each module position.
#' @slot test_log tests that the examinee belonged at each module position.
#' @slot module_log modules that the examinee belonged at each module position.
#' @slot n_module the number of modules the examinee received. This is the number of module positions.
#' @slot true_theta a vector containing the true theta (if simulated) of the examinee, for each module position.
#' @slot initial_theta_in_module a vector containing initial thetas used in each module.
#' @slot prior_par_by_module a list containing prior parameters used for each module.
#' @slot estimated_theta_by_phase a list containing estimated thetas and SEs using items in each phase.
#' @slot estimated_theta_by_test a list containing estimated thetas and SEs using combined items in each test.
#' @slot estimated_theta_for_routing a list containing estimated thetas and SEs that were used for routing.
#' @slot alpha the alpha value used to compute lower and upper bounds.
#' @slot selection_theta a list containing selection thetas in each module position.
#' @slot interim_theta a list containing interim thetas and SEs in each module position.
#' @slot administered_items a list containing administered items in each module position.
#' @slot administered_stimuli a list containing administered stimuli in each module position.
#' @slot response a list containing the examinee response in each module position.
#' @slot item_data a list containing \code{\linkS4class{item_pool}} of administered items.
#' @slot routing_based_on a vector containing the routing was based on \code{estimated_theta_by_phase} or {estimated_theta_by_test} at each module position.
#'
#' @export
setClass("examinee",
  slots = c(
    examinee_id                 = "character",
    current_grade               = "character",
    current_phase               = "character",
    current_test                = "character",
    current_module              = "character",
    grade_log                   = "character",
    phase_log                   = "character",
    test_log                    = "character",
    module_log                  = "character",
    n_module                    = "numeric",
    true_theta                  = "numeric",
    initial_theta_in_module     = "numeric",
    prior_par_by_module         = "list",
    estimated_theta_by_phase    = "list",
    estimated_theta_by_test     = "list",
    estimated_theta_for_routing = "list",
    alpha                       = "numeric",
    selection_theta             = "list",
    interim_theta               = "list",
    administered_items          = "list",
    administered_stimuli        = "list",
    response                    = "list",
    item_data                   = "list",
    routing_based_on            = "character"
  ),
  prototype = list(
    examinee_id                 = character(0),
    current_grade               = character(0),
    current_phase               = character(0),
    current_test                = character(0),
    current_module              = character(0),
    grade_log                   = character(0),
    phase_log                   = character(0),
    test_log                    = character(0),
    module_log                  = character(0),
    n_module                    = numeric(0),
    true_theta                  = numeric(0),
    initial_theta_in_module     = numeric(0),
    estimated_theta_by_phase    = list(),
    estimated_theta_by_test     = list(),
    estimated_theta_for_routing = list(),
    alpha                       = numeric(0),
    selection_theta             = list(),
    interim_theta               = list(),
    administered_items          = list(),
    administered_stimuli        = list(),
    response                    = list(),
    item_data                   = list(),
    routing_based_on            = character(0)
  ),
  validity = function(object) {
    if (object@n_module <= 0) {
      stop("@n_module must be > 0")
    }
    if (length(object@true_theta) != object@n_module) {
      stop("length(@true_theta) must be equal to @n_module")
    }
    if (length(object@estimated_theta_by_phase) != object@n_module) {
      stop("length(@estimated_theta_by_phase) must be equal to @n_module")
    }
    if (length(object@estimated_theta_by_test) != object@n_module) {
      stop("length(@estimated_theta_by_test) must be equal to @n_module")
    }
    if (length(object@estimated_theta_for_routing) != object@n_module) {
      stop("length(@estimated_theta_for_routing) must be equal to @n_module")
    }
    if (length(object@grade_log) != object@n_module) {
      stop("length(@grade_log) must be equal to @n_module")
    }
    if (length(object@phase_log) != object@n_module) {
      stop("length(@phase_log) must be equal to @n_module")
    }
    if (length(object@test_log) != object@n_module) {
      stop("length(@test_log) must be equal to @n_module")
    }
    if (length(object@module_log) != object@n_module) {
      stop("length(@module_log) must be equal to @n_module")
    }
    if (length(object@response) != object@n_module) {
      stop("length(@response) must be equal to @n_module")
    }
    if (length(object@routing_based_on) != object@n_module) {
      stop("length(@routing_based_on) must be equal to @n_module")
    }
    return(TRUE)
  }
)
