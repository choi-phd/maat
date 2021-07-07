#' @include module_functions.R
NULL

#' Bound grades within a specified range
#'
#' \code{\link{boundGrade}} is a function for keeping the grade within a specified range.
#' \code{\link{boundGrade}} checks the relative grade compared to the grade of record. If the current grade is outside the allowed bound, the grade that is within the bound in the same direction is returned.
#'
#' @param current_grade the current grade. This must be formatted as \code{G?}, where \code{?} is a number.
#' @param grade_of_record the grade of record. This must be formatted as \code{G?}, where \code{?} is a number.
#' @param route_limit_below the number of grades to allow routing below, relative to the grade of record. If the grade of record is G4 and this is 1, then routing to G3 is allowed but not to G2.
#' @param route_limit_above the number of grades to allow routing above, relative to the grade of record. If the grade of record is G4 and this is 2, then routing to G6 is allowed but not to G7.
#' @return the grade after the range limit is applied
#'
#' @examples
#' boundGrade("G2", "G1", 0, 2) # G2
#' boundGrade("G3", "G1", 0, 2) # G3
#' boundGrade("G4", "G1", 0, 2) # G3
#' boundGrade("G5", "G1", 0, 2) # G3
#'
#' @export
boundGrade <- function(current_grade, grade_of_record, route_limit_below, route_limit_above) {

  delta <- getRelativeGrade(current_grade, grade_of_record)
  if (delta < -route_limit_below) {
    g <- changeGrade(grade_of_record, -route_limit_below)
    return(g)
  }
  if (delta > route_limit_above) {
    g <- changeGrade(grade_of_record, route_limit_above)
    return(g)
  }
  return(current_grade)

}

#' Update theta estimates using combined responses from a test
#'
#' \code{\link{updateThetaUsingCombined}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#' \code{\link{updateThetaUsingCombined}} adds final theta estimates using all administered items in the test. A test may consist of multiple phases.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param current_module_position the current module position.
#' @param config a \code{\linkS4class{config_Shadow}} object. The config for obtaining final estimates is used.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{estimated_theta_by_test} slot updated.
#'
#' @export
updateThetaUsingCombined <- function(examinee_object, current_module_position, config) {

  if (current_module_position %% 2 == 0) {

    item_data <-
      examinee_object@item_data[(current_module_position - 1):current_module_position]
    combined_response <-
      examinee_object@response[(current_module_position - 1):current_module_position]

    item_data[[1]]@raw$ID <- paste0("temp1", 1:length(item_data[[1]]@id))
    item_data[[2]]@raw$ID <- paste0("temp2", 1:length(item_data[[2]]@id))

    # combine two modules
    combined_item_data <- item_data[[1]] + item_data[[2]]
    combined_response <- unlist(combined_response)

    # calculate MLE or MLEF

    if (config@final_theta$method == "MLEF") {
      res_tmp <- mlef(
        object           = combined_item_data,
        resp             = combined_response,
        fence_slope      = config@final_theta$fence_slope,
        fence_difficulty = config@final_theta$fence_difficulty,
        max_iter         = config@final_theta$max_iter,
        crit             = config@final_theta$crit,
        theta_range      = config@final_theta$bound_ML,
        truncate         = config@final_theta$truncate_ML,
        max_change       = config@final_theta$max_change,
        do_Fisher        = config@final_theta$do_Fisher
      )
    }

    if (config@final_theta$method == "MLE") {
      res_tmp <- mle(
        object      = combined_item_data,
        resp        = combined_response,
        max_iter    = config@final_theta$max_iter,
        crit        = config@final_theta$crit,
        theta_range = config@final_theta$bound_ML,
        truncate    = config@final_theta$truncate_ML,
        max_change  = config@final_theta$max_change,
        do_Fisher   = config@final_theta$do_Fisher
      )
    }

    if (config@final_theta$method == "EAP") {
      ## Extract the prior parameters from the examinee list for each examinee for each module
      prior_par <-  examinee_object@prior_par_by_module[[current_module_position -1]]
      ## Generate the distribution according to the given parameters
      prior_dist <- genPriorDist(
        dist_type  = "normal",
        prior_par  = prior_par,
        theta_grid = config@theta_grid,
        nj         = 1)
      ## EAP estimation
      res_tmp <- eap(
        object      = combined_item_data,
        resp        = combined_response,
        theta_grid  = config@theta_grid,
        prior       = prior_dist
      )
    }

    # store the estimated theta and SE
    o <- list()
    o$theta    <- res_tmp$th
    o$theta_se <- res_tmp$se
    examinee_object@estimated_theta_by_test[[current_module_position - 1]] <- o
    examinee_object@estimated_theta_by_test[[current_module_position    ]] <- o

    return(examinee_object)

  } else {

    return(examinee_object)

  }

}

#' Update the theta used for routing of an examinee object
#'
#' \code{\link{updateThetaForRouting}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#' \code{\link{updateThetaForRouting}} determines what type of theta estimate is used to perform routing.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param current_module_position the current module position.
#' @param combine_policy
#' \itemize{
#'   \item{} This is only applied when \code{module_position \%\% 2 == 0} (at Phase 2, which is the end of each test).
#'   \item{\code{conditional}} uses the combined theta (using items from the previous module combined with the current module), if the examinee was in the same grade in Phases 1 and 2. If the examinee was in different grades in Phases 1 and 2, then the theta estimate from Phase 2 is used.
#'   \item{\code{always}} uses the combined theta.
#'   \item{\code{never}} uses the theta estimate from Phase 2.
#'   \item{} (default = \code{conditional})
#' }
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{estimated_theta_for_routing} slot updated.
#'
#' @export
updateThetaForRouting <- function(examinee_object, current_module_position, combine_policy) {

  if (current_module_position %% 2 == 1) {

    examinee_object@routing_based_on[current_module_position] <- "estimated_theta_by_phase"
    examinee_object@estimated_theta_for_routing[[current_module_position]] <-
      examinee_object@estimated_theta_by_phase[[current_module_position]]

    return(examinee_object)

  }

  if (current_module_position %% 2 == 0) {

    if (combine_policy == "always") {

      examinee_object@routing_based_on[current_module_position] <- "estimated_theta_by_test"
      examinee_object@estimated_theta_for_routing[[current_module_position]] <-
        examinee_object@estimated_theta_by_test[[current_module_position]]

      return(examinee_object)

    }

    if (combine_policy == "never") {

      examinee_object@routing_based_on[current_module_position] <- "estimated_theta_by_phase"
      examinee_object@estimated_theta_for_routing[[current_module_position]] <-
        examinee_object@estimated_theta_by_phase[[current_module_position]]

      return(examinee_object)

    }

    if (combine_policy == "conditional") {

      grade_is_same <- getRelativeGrade(
        examinee_object@grade_log[current_module_position],
        examinee_object@grade_log[current_module_position - 1]
      ) == 0

      if (grade_is_same) {

        examinee_object@routing_based_on[current_module_position] <- "estimated_theta_by_test"
        examinee_object@estimated_theta_for_routing[[current_module_position]] <-
          examinee_object@estimated_theta_by_test[[current_module_position]]

        return(examinee_object)

      } else {

        examinee_object@routing_based_on[current_module_position] <- "estimated_theta_by_phase"
        examinee_object@estimated_theta_for_routing[[current_module_position]] <-
          examinee_object@estimated_theta_by_phase[[current_module_position]]

        return(examinee_object)

      }

    }

    stop(sprintf("unexpected combine_policy: '%s'", combine_policy))

  }

}

#' Update the grade slot of an examinee object
#'
#' \code{\link{updateGrade}} is a function for determining the grade an examinee is routed to.
#'
#' Currently the routing rules are hard-coded in the function. See the vignette for a description of routing rules.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#' @param module_position the current module position, ranging from \code{1} to \code{6}.
#' @param cut_scores a named list containing cut scores to be used in each grade. Each element must be named in the form \code{G?}, where \code{?} is a number.
#' @param transition_policy
#' \itemize{
#' \item{\code{CI}} uses the confidence interval to perform routing.
#' \item{\code{pool_difficulty_percentile}} uses item difficulty percentiles of all items in the \code{item_pool} argument to perform routing.
#' \item{\code{pool_difficulty_percentile_exclude_administered}} uses item difficulty percentiles of all items in the \code{item_pool} argument to perform routing, excluding all previous items administered to the examinee.
#' \item{\code{on_grade}} does not permit any transition.
#' \item{} (default = \code{CI})
#' }
#' @param transition_CI_alpha the alpha level used when \code{transition_policy == "CI"}.
#' @param transition_percentile_lower the percentile value (between 0 and 1) used for the lower routing in percentile-based transition policies.
#' @param transition_percentile_upper the percentile value (between 0 and 1) used for the upper routing in percentile-based transition policies.
#' @param item_pool the \code{\linkS4class{item_pool}} object to determine difficulty range in percentile-based transition policies.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{current_grade} slot updated.
#'
#' @export
updateGrade <- function(
  examinee_object, assessment_structure, module_position, cut_scores, transition_policy = "CI",
  transition_CI_alpha,
  transition_percentile_lower,
  transition_percentile_upper,
  item_pool) {

  theta    <- examinee_object@estimated_theta_for_routing[[module_position]]$theta
  theta_se <- examinee_object@estimated_theta_for_routing[[module_position]]$theta_se

  if (tolower(transition_policy) %in% c(
    "pool_difficulty_percentile",
    "pool_difficulty_percentile_exclude_administered",
    "ci")
  ) {

    if (tolower(transition_policy) == "pool_difficulty_percentile") {
      theta_L <- theta
      theta_U <- theta
      item_b  <- na.omit(as.vector(item_pool@ipar))
      lower_b <- quantile(item_b, transition_percentile_lower)
      upper_b <- quantile(item_b, transition_percentile_upper)
      cut_scores_thisgrade <- c(lower_b, 0, upper_b)
    } else if (tolower(transition_policy) == "pool_difficulty_percentile_exclude_administered") {
      theta_L <- theta
      theta_U <- theta
      administered_item_pool <- suppressWarnings(do.call(c, examinee_object@item_data))
      pool    <- item_pool - administered_item_pool
      item_b  <- na.omit(as.vector(pool@ipar))
      lower_b <- quantile(item_b, transition_percentile_lower)
      upper_b <- quantile(item_b, transition_percentile_upper)
      cut_scores_thisgrade <- c(lower_b, 0, upper_b)
    } else if (tolower(transition_policy) == "ci") {
      theta_L <- theta - qnorm((1 - transition_CI_alpha / 2)) * theta_se
      theta_U <- theta + qnorm((1 - transition_CI_alpha / 2)) * theta_se
      cut_scores_thisgrade <- cut_scores[[examinee_object@current_grade]]
    }

    # only use two cut scores
    if (length(cut_scores_thisgrade) > 2) {
      cut_scores_thisgrade <- c(
        head(cut_scores_thisgrade, 1),
        tail(cut_scores_thisgrade, 1)
      )
    }

    relative_grade <- getRelativeGrade(
      examinee_object@current_grade,
      examinee_object@grade_log[1] # the grade of record
    )

    # Exception E1: If grade is G-1 in the last phase of any administration,
    # ignore achievement level and always change grade by +1.
    if (module_position %% assessment_structure@n_phase == 0) {
      if (
        relative_grade == -1 &&
        "R1" %in%  assessment_structure@test_routing_restrictions
      ) {
        # if route_limit_below = 2,
        # this has to be changed to reset to the grade of record
        examinee_object@current_grade <- changeGrade(examinee_object@current_grade, 1)
        examinee_object@current_grade <- boundGrade(
          examinee_object@current_grade,
          examinee_object@grade_log[1],
          assessment_structure@route_limit_below,
          assessment_structure@route_limit_above
        )
        return(examinee_object)
      }
    }

    if (theta_U < cut_scores_thisgrade[1]) { # Achievement level is Beginning

      # Exception E2: If grade is G in the last phase of any administration:
      # If achievement level is Beginning, do not decrease grade.
      if (module_position %% assessment_structure@n_phase == 0) {
        if (
          relative_grade == 0 &&
          "R2" %in% assessment_structure@test_routing_restrictions
        ) {
          examinee_object@current_grade <- changeGrade(examinee_object@current_grade, 0)
          return(examinee_object)
        }
      }

      examinee_object@current_grade <- changeGrade(examinee_object@current_grade, -1)
      examinee_object@current_grade <- boundGrade(
        examinee_object@current_grade,
        examinee_object@grade_log[1],
        assessment_structure@route_limit_below,
        assessment_structure@route_limit_above
      )
      return(examinee_object)

    } else if (theta_L > cut_scores_thisgrade[2]) { # Achievement level is Advanced

      # Exception E3: If grade is G+1 in the last phase of Administration 1:
      # If achievement level is Advanced, do not increase grade.
      # Only one grade increase is allowed in a single administration.

      test_position <- module_position %/% assessment_structure@n_phase + 1
      if (module_position %% assessment_structure@n_phase == 0) {
        if (
          relative_grade >= (test_position - 1) &&
          "R3" %in% assessment_structure@test_routing_restrictions
        ) {
          examinee_object@current_grade <- changeGrade(examinee_object@current_grade, 0)
          return(examinee_object)
        }
      }

      examinee_object@current_grade <- changeGrade(examinee_object@current_grade, 1)
      examinee_object@current_grade <- boundGrade(
        examinee_object@current_grade,
        examinee_object@grade_log[1],
        assessment_structure@route_limit_below,
        assessment_structure@route_limit_above
      )
      return(examinee_object)

    } else {

      return(examinee_object)

    }

  } else if (tolower(transition_policy) == "on_grade") {

    return(examinee_object)

  }

  stop(sprintf(
    "module position %s: cannot route module for examinee '%s' with relative grade: %s, estimated theta = %s (%s), cut scores = %s, transition policy = %s",
    module_position,
    examinee_object@examinee_id,
    relative_grade,
    examinee_object@estimated_theta_for_routing[[module_position]]$theta,
    examinee_object@estimated_theta_for_routing[[module_position]]$theta_se,
    paste0(cut_scores_thisgrade, collapse = " "),
    transition_policy
  ))

}
