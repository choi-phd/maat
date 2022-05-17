#' @include module_functions.R
NULL

#' Class 'output_maat': a simulation output
#'
#' \code{\linkS4class{output_maat}} is an S4 class to represent a simulation output.
#'
#' @slot examinee_list a list of \code{\linkS4class{examinee}} objects.
#' @slot assessment_structure an \code{\linkS4class{assessment_structure}} object.
#' @slot module_list a module list from \code{\link{loadModules}}.
#' @slot config the list of \code{\linkS4class{config_Shadow}} objects used in the simulation for each module.
#' @slot cut_scores the cut scores used in the simulation.
#' @slot overlap_control_policy the policy used in the simulation.
#' @slot transition_policy the policy used in the simulation.
#' @slot combine_policy the policy used in the simulation.
#' @slot transition_CI_alpha the transition parameter used in the simulation.
#' @slot transition_percentile_lower the transition parameter used in the simulation.
#' @slot transition_percentile_upper the transition parameter used in the simulation.
#' @slot initial_theta_list the starting theta values used in the simulation.
#' @slot prior_mean_policy the policy used in the simulation.
#' @slot prior_mean_user the prior parameters used in the simulation.
#' @slot prior_sd the prior parameters used in the simulation.
#'
#' @export
setClass("output_maat",
  slots = c(
    examinee_list        = "list",
    assessment_structure = "assessment_structure",
    module_list          = "list",
    config               = "list_or_config_Shadow",
    cut_scores           = "list",
    overlap_control_policy = "character",
    transition_policy      = "character",
    combine_policy         = "character",
    transition_CI_alpha    = "numeric",
    transition_percentile_lower = "numeric_or_null",
    transition_percentile_upper = "numeric_or_null",
    initial_theta_list = "list_or_null",
    prior_mean_policy  = "character",
    prior_mean_user    = "numeric_or_null",
    prior_sd           = "numeric"
  ),
  prototype = list(
    examinee_list        = list(),
    assessment_structure = new("assessment_structure"),
    module_list          = list(),
    config               = list(),
    cut_scores           = list(),
    overlap_control_policy = character(0),
    transition_policy      = character(0),
    combine_policy         = character(0),
    transition_CI_alpha    = numeric(0),
    transition_percentile_lower = numeric(0),
    transition_percentile_upper = numeric(0),
    initial_theta_list = list(),
    prior_mean_policy  = character(0),
    prior_mean_user    = numeric(0),
    prior_sd           = numeric(0)
  ),
  validity = function(object) {
    return(TRUE)
  }
)

#' Simulate theta values
#'
#' \code{\link{simTheta}} is a function for generating a theta matrix based on the given
#' sample size, mean, standard deviation, and correlation matrix.
#'
#' \code{\link{simTheta}} calls \code{\link{mvrnorm}} internally.
#'
#' @param N the number of examinees.
#' @param mean_v a vector containing the mean of each dimension.
#' @param sd_v a vector containing the standard deviation of each dimension.
#' @param cor_v a correlation matrix.
#' @return a theta matrix.
#'
#' @examples
#' o <- simTheta(
#'   N      = 100,
#'   mean_v = c(0, 0, 0),
#'   sd_v   = c(1, 1, 1),
#'   cor_v  = diag(1, 3)
#' )
#'
#' @export
simTheta <- function(N, mean_v, sd_v, cor_v) {

  sigma <- diag(sd_v) %*% cor_v %*% diag(sd_v)
  theta <- mvrnorm(n = N, mu = mean_v, Sigma = sigma)
  theta <- matrix(theta, nrow = N)
  return(theta)

}

#' Simulate an examinee list
#'
#' \code{\link{simExaminees}} is a function for generating a list of \code{\linkS4class{examinee}} objects.
#'
#' Each dimension of \code{mean_v}, \code{sd_v}, \code{cor_v} represents a test level. For example in a three-test structure (see the \code{assessment_structure_math} example data), these arguments must have three dimensions.
#'
#' @param N the number of examinees.
#' @param mean_v a vector containing the mean of each dimension.
#' @param sd_v a vector containing the standard deviation of each dimension.
#' @param cor_v a correlation matrix.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object. This can be created using \code{\link{createAssessmentStructure}}.
#' @param initial_grade the initial grade for all examinees. The grade must exist in \code{module_list}. Also used as the grade of record when the initial phase and test points to a module position greater than 1. (default = \code{G4})
#' @param initial_phase the initial phase for all examinees. The phase must exist in \code{module_list}. (default = \code{P1})
#' @param initial_test the initial test for all examinees. (default = \code{T1})
#' @return a list of \code{\linkS4class{examinee}} objects.
#'
#' @examples
#' assessment_structure <- createAssessmentStructure(
#'   n_test  = 3,
#'   n_phase = 2,
#'   route_limit_below = 1,
#'   route_limit_above = 2
#' )
#' examinee_list <- simExaminees(
#'   N      = 100,
#'   mean_v = c(0, 0, 0),
#'   sd_v   = c(1, 1, 1),
#'   cor_v  = diag(1, 3),
#'   assessment_structure = assessment_structure
#' )
#'
#' @export
simExaminees <- function(N, mean_v, sd_v, cor_v, assessment_structure,
  initial_grade = "G4", initial_test = "T1", initial_phase = "P1") {

  isGrade(initial_grade)
  isTest(initial_test)
  isPhase(initial_phase)

  true_theta <- simTheta(N, mean_v, sd_v, cor_v)

  if (dim(true_theta)[2] != assessment_structure@n_test) {
    stop(sprintf("unexpected number of dimensions: expecting %s dimensions (for %s tests)", assessment_structure@n_test, assessment_structure@n_test))
  }

  examinee_list <- list()
  n_examinee <- dim(true_theta)[1]

  for (i in 1:n_examinee) {
    x <- new("examinee")
    x@examinee_id <- sprintf("examinee_%s", i)
    x@n_module    <- assessment_structure@n_test * assessment_structure@n_phase
    x@true_theta  <- rep(true_theta[i, ], each = assessment_structure@n_phase)
    x@current_grade <- initial_grade
    x@current_phase <- initial_phase
    x@current_test  <- initial_test
    x@grade_log     <- rep(NA_character_, x@n_module)
    x@phase_log     <- rep(NA_character_, x@n_module)
    x@test_log      <- rep(NA_character_, x@n_module)
    x@grade_log[1]  <- initial_grade
    examinee_list[[i]] <- x
    names(examinee_list)[i] <- x@examinee_id
  }

  return(examinee_list)

}

#' Simulate multi-stage multi-administration adaptive test
#'
#' \code{\link{maat}} is the main function for simulating a multi-stage multi-administration adaptive test.
#'
#' @param examinee_list an examinee list from \code{\link{simExaminees}}.
#' @param assessment_structure a \code{\linkS4class{assessment_structure}} object.
#' @param module_list a module list from \code{\link{loadModules}}.
#' @param config a \code{\linkS4class{config_Shadow}} object. Also accepts a list of \code{\linkS4class{config_Shadow}} objects to use separate configurations for each module. Must be from 'TestDesign' 1.3.3 or newer, and its \code{exclude_policy$method} slot must be \code{SOFT}.
#' @param cut_scores a named list containing cut scores to be used in each grade. Each element must be named in the form \code{G?}, where \code{?} is a number.
#' @param overlap_control_policy overlap control is performed by excluding administered items from being administered again within the same examinee.
#' \itemize{
#' \item{\code{all}} performs overlap control at all module positions.
#' \item{\code{within_test}} performs overlap control only within each test.
#' \item{\code{none}} does not perform overlap control.
#' }
#' @param transition_policy
#' \itemize{
#' \item{\code{CI}} uses the confidence interval to perform routing.
#' \item{\code{pool_difficulty_percentile}} uses item difficulty percentiles of all items in the \code{item_pool} argument to perform routing.
#' \item{\code{pool_difficulty_percentile_exclude_administered}} uses item difficulty percentiles of all items in the \code{item_pool} argument to perform routing, excluding all previous items administered to the examinee.
#' \item{\code{on_grade}} does not permit any transition.
#' \item{} (default = \code{CI})
#' }
#' @param combine_policy
#' \itemize{
#'   \item{} This is only applied when \code{module_position \%\% 2 == 0} (at Phase 2, which is the end of each test).
#'   \item{\code{conditional}} uses the combined theta (using items from the previous module combined with the current module), if the examinee was in the same grade in Phases 1 and 2. If the examinee was in different grades in Phases 1 and 2, then the theta estimate from Phase 2 is used.
#'   \item{\code{always}} uses the combined theta.
#'   \item{\code{never}} uses the theta estimate from Phase 2.
#'   \item{} (default = \code{conditional})
#' }
#' @param transition_CI_alpha the alpha level to use when \code{transition_policy == "CI"}.
#' @param transition_percentile_lower the percentile value (between 0 and 1) to use for the lower routing when \code{transition_policy == "difficulty_percentile"}.
#' @param transition_percentile_upper the percentile value (between 0 and 1) to use for the upper routing when \code{transition_policy == "difficulty_percentile"}.
#' @param initial_theta_list (optional) a list containing initial thetas to use in each module position.
#' @param prior_mean_policy
#' \itemize{
#'   \item{} This is only effective at the beginning of each test. This determines what value is used as the prior mean.
#'   \item{\code{mean_difficulty}} uses the mean item difficulty of the current item pool.
#'   \item{\code{carryover}} uses the routing theta from the previous module. For Phase 1 of the first test, user supplied values are used if available. Otherwise, the mean item difficulty of the current item pool is used.
#'   \item{\code{user}} uses user-supplied values in the \code{prior_mean_user} argument.
#'   \item{} (default = \code{mean_difficulty})
#' }
#' @param prior_mean_user (optional) user-supplied values for the prior mean. Must be a single value, or a vector for each grade.
#' @param prior_sd user-supplied values for the prior standard deviation. This is only effective at the beginning of each test. This is utilized regardless of \code{prior_mean_policy}. Must be a single value, or a vector for each grade. (default = \code{1})
#' @param verbose if \code{TRUE}, print status messages. (default = \code{TRUE})
#'
#' @return an \code{\linkS4class{output_maat}} object from the simulation.
#'
#' @examples
#' \donttest{
#' library(TestDesign) # >= 1.3.3
#' config <- createShadowTestConfig(
#'   final_theta = list(method = "MLE"),
#'   exclude_policy = list(method = "SOFT", M = 100)
#' )
#' # exclude_policy must be SOFT
#'
#' examinee_list <- maat(
#'   examinee_list          = examinee_list_math,
#'   assessment_structure   = assessment_structure_math,
#'   module_list            = module_list_math,
#'   overlap_control_policy = "all",
#'   transition_CI_alpha    = 0.05,
#'   config                 = config,
#'   cut_scores             = cut_scores_math
#' )
#' }
#' @export
maat <- function(
  examinee_list = examinee_list,
  assessment_structure = NULL,
  module_list = NULL,
  config = NULL,
  cut_scores = NULL,
  overlap_control_policy = NULL,
  transition_policy = "CI",
  combine_policy = "conditional",
  transition_CI_alpha = NULL,
  transition_percentile_lower = NULL,
  transition_percentile_upper = NULL,
  initial_theta_list = NULL,
  prior_mean_policy = "mean_difficulty",
  prior_mean_user = NULL,
  prior_sd = 1,
  verbose = TRUE) {

  if (is.null(assessment_structure)) {
    stop("'assessment_structure' is required but was not supplied. See ?maat for details.")
  }
  if (is.null(module_list)) {
    stop("'module_list' is required but was not supplied. See ?maat for details.")
  }
  if (is.null(config)) {
    stop("'config' is required but was not supplied. See ?maat for details.")
  }
  if (is.null(cut_scores)) {
    stop("'cut_scores' is required but was not supplied. See ?maat for details.")
  }
  if (is.null(overlap_control_policy)) {
    stop("'overlap_control_policy' is required but was not supplied. See ?maat for details.")
  }
  if (!"exclude_policy" %in% slotNames(config)) {
    stop("'config' does not have '@exclude_policy' slot: 'config' from createShadowTestConfig() in TestDesign >= 1.3.3 is required.")
  }
  if ("exclude_policy" %in% slotNames(config)) {
    if (tolower(config@exclude_policy$method) != "soft") {
      stop(sprintf(
        "unrecognized 'config@exclude_policy$method': %s (must be SOFT)",
        config@exclude_policy$method
      ))
    }
    if (is.null(config@exclude_policy$M)) {
      stop(sprintf(
        "unrecognized 'config@exclude_policy$M': NULL (must be a numeric value)"
      ))
    }
  }

  if (!overlap_control_policy %in% c("all", "within_test", "none")) {
    stop(sprintf("unrecognized 'overlap_control_policy': %s", overlap_control_policy))
  }
  if (!tolower(transition_policy) %in% c(
    "ci",
    "pool_difficulty_percentile",
    "pool_difficulty_percentile_exclude_administered",
    "on_grade")
  ) {
    stop(sprintf("unrecognized 'transition_policy': %s", transition_policy))
  }
  if (!combine_policy %in% c("conditional", "always", "never")) {
    stop(sprintf("unrecognized 'combine_policy': %s", combine_policy))
  }
  if (tolower(transition_policy) == "ci") {
    if (is.null(transition_CI_alpha)) {
      stop(sprintf("transition_policy '%s' requires the argument 'transition_CI_alpha'", transition_policy))
    }
  }
  if (transition_policy %in% c(
    "pool_difficulty_percentile",
    "pool_difficulty_percentile_exclude_administered"
  )) {
    if (is.null(transition_percentile_lower) | is.null(transition_percentile_upper)) {
      stop(sprintf("transition_policy '%s' requires arguments 'transition_percentile_lower' and 'transition_percentile_upper'", transition_policy))
    }
  }
  if (!prior_mean_policy %in% c(
    "mean_difficulty",
    "carryover",
    "user"
  )) {
    stop(sprintf("unrecognized 'prior_mean_policy': %s", prior_mean_policy))
  }
  if (prior_mean_policy %in% c(
    "user"
  )) {
    if (is.null(prior_mean_user)) {
      stop(sprintf("prior_mean_policy '%s' requires the argument 'prior_mean_user'", prior_mean_policy))
    }
  }
  if (!is.null(prior_mean_user)) {
    if (length(prior_mean_user) == 1) {
      prior_mean_user <- rep(prior_mean_user, length(module_list))
    }
    if (length(prior_mean_user) != length(module_list)) {
      stop("length(prior_mean_user) must match length(module_list)")
    }
    names(prior_mean_user) <- names(module_list)
  }
  if (length(prior_sd) == 1) {
    prior_sd <- rep(prior_sd, length(module_list))
  }
  if (length(prior_sd) != length(module_list)) {
    stop("length(prior_sd) must match length(module_list)")
  }
  names(prior_sd) <- names(module_list)

  # Module Information -------------

  n_modules <-
    assessment_structure@n_test *
    assessment_structure@n_phase

  module_list_by_name <- unlist(module_list)
  module_names <- unlist(lapply(
    module_list_by_name,
    function(x) {
      x@module_id
    }
  ))
  names(module_list_by_name) <- module_names

  # Expand config to list
  if (inherits(config, "config_Shadow")) {
    config_list <- vector("list", n_modules)
    for (m in 1:n_modules) {
      config_list[[m]] <- config
    }
  }
  if (inherits(config, "list")) {
    if (length(config) != n_modules) {
      stop(sprintf("unexpected 'config' length: %s (must be %s)", length(config), n_modules))
    }
    config_list <- config
  }
  config <- NULL

  # Determine the module
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      updateModule(x, module_list)
    }
  )

  # Determine the module position (assuming everyone shares the same initial phase/test)
  module_position_list <- lapply(
    examinee_list,
    function(x) {
      getModulePosition(
        x@current_phase, x@current_test, assessment_structure
      )
    }
  )
  starting_module_position <- unique(unlist(module_position_list))

  # Repeat through all module positions

  for (current_module_position in starting_module_position:n_modules) {

    examinee_current_module <- lapply(examinee_list, function(x) {
      x@current_module
    })

    examinee_current_module <- unlist(examinee_current_module)
    unique_modules <- unique(examinee_current_module)


    # Theta Estimation ---------------

    for (module_for_thisgroup in unique_modules) {

      examinee_in_thisgroup <- which(examinee_current_module == module_for_thisgroup)
      examinee_in_thisgroup <- names(examinee_in_thisgroup)

      if (verbose) {
        cat(sprintf(
          "Module position %s: %s examinees in module %s\n",
          current_module_position,
          length(examinee_in_thisgroup),
          module_for_thisgroup
        ))
      }

      constraints_thisgroup <- module_list_by_name[[module_for_thisgroup]]@constraints

      # run simulation for this group

      config_thisgroup             <- config_list[[current_module_position]]
      administered_entry           <- NULL
      prior_par                    <- NULL
      include_items_for_estimation <- NULL

      if (current_module_position %% assessment_structure@n_phase == 1) {
        if (prior_mean_policy == "mean_difficulty") {
          # at the beginning of each test
          # use mean difficulty of the current item pool
          examinee_list[examinee_in_thisgroup] <- getPriorUsingMeanDifficulty(
            examinee_list[examinee_in_thisgroup],
            current_module_position,
            module_list_by_name, module_for_thisgroup,
            prior_sd
          )
        }
        if (prior_mean_policy == "carryover") {
          # at the beginning of each test
          # carryover previous theta
          if (current_module_position > 1) {
            examinee_list[examinee_in_thisgroup] <- getPriorUsingCarryoverMeans(
              examinee_list[examinee_in_thisgroup],
              current_module_position,
              prior_sd
            )
          }
          # if cannot carryover, fallback
          if (current_module_position == 1) {
            if (!is.null(prior_mean_user)) {
              # use user values
              examinee_list[examinee_in_thisgroup] <- getPriorUsingUserMeans(
                examinee_list[examinee_in_thisgroup],
                current_module_position,
                prior_mean_user,
                prior_sd
              )
            } else {
              # use mean difficulty of the current item pool
              examinee_list[examinee_in_thisgroup] <- getPriorUsingMeanDifficulty(
                examinee_list[examinee_in_thisgroup],
                current_module_position,
                module_list_by_name, module_for_thisgroup,
                prior_sd
              )
            }
          }
        }
        if (prior_mean_policy == "user") {
          # at the beginning of each test
          # use user values, because we expect true theta to change after each test
          examinee_list[examinee_in_thisgroup] <- getPriorUsingUserMeans(
            examinee_list[examinee_in_thisgroup],
            current_module_position,
            prior_mean_user,
            prior_sd
          )
        }
      } else {
        # within each test, after Phase 1, reuse the prior used for Phase 1
        # this should be uninformative, we are already carrying over response data to reconstruct posterior
        examinee_list[examinee_in_thisgroup] <- getPriorUsingReuse(
          examinee_list[examinee_in_thisgroup],
          current_module_position
        )
      }

      prior_par <- extractPrior(
        examinee_list[examinee_in_thisgroup],
        current_module_position
      )

      if (current_module_position > starting_module_position) {

        # use the theta estimate from the previous routing
        config_thisgroup@item_selection$initial_theta <- unlist(lapply(
          examinee_list[examinee_in_thisgroup],
          function(x) {
            x@estimated_theta_for_routing[[current_module_position - 1]]$theta
          }
        ))

        # exclude administered items
        if (overlap_control_policy == "all") {
          if (verbose) {
            cat(sprintf(
              "Module position %s: overlap control (all administered items & stimuli)\n",
              current_module_position
            ))
          }
          administered_items <- lapply(
            examinee_list[examinee_in_thisgroup],
            function(x) {
              unlist(x@administered_items)
            }
          )
          administered_stimuli <- lapply(
            examinee_list[examinee_in_thisgroup],
            function(x) {
              if (length(x@administered_stimuli) == 0) {
                return(NULL)
              } else {
                unlist(x@administered_stimuli)
              }
            }
          )
          administered_entry <- mapply(
            function(i, s) {
              x <- list()
              x$i <- i
              x$s <- s
              return(x)
            },
            administered_items,
            administered_stimuli,
            SIMPLIFY = FALSE
          )
        }
        if (overlap_control_policy == "within_test") {
          if (current_module_position %% assessment_structure@n_phase == 0) {
            if (verbose) {
              cat(sprintf(
                "Module position %s: overlap control (within test)\n",
                current_module_position
              ))
            }
            administered_items <- lapply(
              examinee_list[examinee_in_thisgroup],
              function(x) {
                unlist(x@administered_items[[current_module_position - 1]])
              }
            )
            administered_stimuli <- lapply(
              examinee_list[examinee_in_thisgroup],
              function(x) {
                if (length(x@administered_stimuli) == 0) {
                  return(NULL)
                } else {
                  unlist(x@administered_stimuli[[current_module_position - 1]])
                }
              }
            )
            administered_entry <- mapply(
              function(i, s) {
                x <- list()
                x$i <- i
                x$s <- s
                return(x)
              },
              administered_items,
              administered_stimuli,
              SIMPLIFY = FALSE
            )
          } else {
            administered_entry <- NULL
          }
        }
        if (overlap_control_policy == "none") {
          if (verbose) {
            cat(sprintf(
              "Module position %s: no overlap control\n",
              current_module_position
            ))
          }
          administered_entry <- NULL
        }

        if (current_module_position %% assessment_structure@n_phase == 0) {

          include_items_for_estimation <- lapply(
            examinee_list[examinee_in_thisgroup],
            function(x) {
              o <- list()
              o$administered_item_pool <- x@item_data[[current_module_position - 1]]
              o$administered_item_resp <- x@response[[current_module_position - 1]]
              return(o)
            }
          )

        }

      }

      if (!is.null(initial_theta_list[[current_module_position]])) {
        if (verbose) {
          cat(sprintf(
            "Module position %s: overriding initial thetas\n",
            current_module_position
          ))
        }
        config_thisgroup@item_selection$initial_theta <-
          unlist(initial_theta_list[[current_module_position]][examinee_in_thisgroup])
      }

      theta_thisgroup <- unlist(lapply(
        examinee_list[examinee_in_thisgroup],
        function(x) {
          x@true_theta[current_module_position]
        }
      ))

      solution <- Shadow(
        config_thisgroup,
        constraints_thisgroup,
        true_theta                   = theta_thisgroup,
        prior_par                    = prior_par,
        exclude                      = administered_entry,
        include_items_for_estimation = include_items_for_estimation
      )
      names(solution@output) <- examinee_in_thisgroup

      for (examinee in examinee_in_thisgroup) {

        # store initial thetas to each examinee object
        initial_theta <- config_thisgroup@item_selection$initial_theta[examinee]
        if (is.null(initial_theta)) {
          initial_theta <- 0
        }
        examinee_list[[examinee]]@initial_theta_in_module[current_module_position] <- initial_theta

        # store theta estimates to each examinee object
        o <- list()
        o$theta    <- solution@output[[examinee]]@final_theta_est
        o$theta_se <- solution@output[[examinee]]@final_se_est
        examinee_list[[examinee]]@estimated_theta_by_phase[[current_module_position]] <- o

        examinee_list[[examinee]]@alpha <- transition_CI_alpha

        o <- list()
        o$theta    <- solution@output[[examinee]]@interim_theta_est
        o$theta_se <- solution@output[[examinee]]@interim_se_est
        examinee_list[[examinee]]@interim_theta[[current_module_position]] <- o

        # store selection thetas to each examinee object
        selection_theta <- c(
          initial_theta,
          solution@output[[examinee]]@interim_theta_est[
            -length(solution@output[[examinee]]@interim_theta_est)
          ]
        )
        examinee_list[[examinee]]@selection_theta[[current_module_position]] <- selection_theta

        # store administered items and stimuli to each examinee object
        examinee_list[[examinee]]@administered_items[[current_module_position]] <-
          solution@pool@id[
            solution@output[[examinee]]@administered_item_index
          ]
        if (solution@constraints@set_based) {
          examinee_list[[examinee]]@administered_stimuli[[current_module_position]] <-
            solution@constraints@st_attrib@data$STID[
              solution@output[[examinee]]@administered_stimulus_index
            ]
          examinee_list[[examinee]]@administered_stimuli[[current_module_position]] <-
            unique(na.omit(
              examinee_list[[examinee]]@administered_stimuli[[current_module_position]]
            ))
        }

        # store response to each examinee object
        examinee_list[[examinee]]@response[[current_module_position]] <-
          solution@output[[examinee]]@administered_item_resp

        examinee_list[[examinee]] <-
          updateItemData(examinee_list[[examinee]], current_module_position, solution)
      }

    }

    # Theta Choice for Routing -----------------

    # combine with the previous module to estimate test-level theta
    # this is stored in @estimated_theta_by_test
    examinee_list <- lapply(
      examinee_list,
      function(x) {
        x <- updateThetaUsingCombined(x, current_module_position, config_list[[current_module_position]])
      }
    )

    # update grade / phase / module logs
    examinee_list <- lapply(
      examinee_list,
      function(x) {
        x <- updateLog(x, current_module_position)
      }
    )

    # determine which theta to use for routing
    examinee_list <- lapply(
      examinee_list,
      function(x) {
        x <- updateThetaForRouting(x, current_module_position, combine_policy)
      }
    )


    # Selection of Next Module ------------------

    if (current_module_position < n_modules) {
      examinee_list <- lapply(
        examinee_list,
        function(x) {
          module <- module_list[[x@current_grade]][[x@current_test]][[x@current_phase]]
          item_pool_for_this_examinee <- module@constraints@pool
          x <- updateGrade(
            x, assessment_structure, current_module_position, cut_scores, transition_policy,
            transition_CI_alpha,
            transition_percentile_lower,
            transition_percentile_upper,
            item_pool_for_this_examinee
          )
          x <- updateTest(x, assessment_structure)
          x <- updatePhase(x, assessment_structure)
          x <- updateModule(x, module_list)
        }
      )
    }

  }

  # Update assessment-level theta
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      x <- updateAssessmentLevelTheta(x, config_list[[current_module_position]])
    }
  )

  o <- new("output_maat")
  o@examinee_list <- examinee_list
  o@assessment_structure <- assessment_structure
  o@module_list <- module_list
  o@config <- config_list
  o@cut_scores <- cut_scores
  o@overlap_control_policy <- overlap_control_policy
  o@transition_policy      <- transition_policy
  o@combine_policy         <- combine_policy
  o@transition_CI_alpha    <- transition_CI_alpha
  o@transition_percentile_lower <- transition_percentile_lower
  o@transition_percentile_upper <- transition_percentile_upper
  o@initial_theta_list          <- initial_theta_list
  o@prior_mean_policy           <- prior_mean_policy
  o@prior_mean_user             <- prior_mean_user
  o@prior_sd                    <- prior_sd

  return(o)

}

#' Format the output of maat
#'
#' \code{\link{formatOutput}} is a function for formatting the output \code{\linkS4class{examinee}} object
#' of the function \code{\link{maat}} for analysis.
#'
#' @param examinee_list the output from \code{\link{maat}}.
#' @param digits digits to round theta values. (default = 3)
#'
#' @return a data frame containing:
#' \itemize{
#'   \item{\code{p_ID}}: the person ID.
#'   \item{\code{test_phase_ID}}: the module position. If we have 3 tests with 2 phases in each test then the range of test_phase_ID is 1 to 6.
#'   \item{\code{initial_grade}}: the initial grade of the person.
#'   \item{\code{final_grade}}: the final grade of the person after completing all modules.
#'   \item{\code{grade_ID}}: the grade at the module position.
#'   \item{\code{phase_ID}}: the phase at the module position.
#'   \item{\code{test_ID}}: the test at the module position.
#'   \item{\code{module_ID}}: the module ID at the module position.
#'   \item{\code{final_theta_est}}: the grand final estimated \eqn{\theta} after completing all tests.
#'   \item{\code{final_SE_est}}: the standard error of grand final estimated \eqn{\theta} after completing all tests.
#'   \item{\code{theta_by_phase}}: the final estimated \eqn{\theta} after completing each phase.
#'   \item{\code{SE_by_phase}}: the standard error of final estimated \eqn{\theta} after completing each phase.
#'   \item{\code{combined}}: whether items were combined with the previous phase to obtain the theta estimate.
#'   \item{\code{true_theta}}: the true \eqn{\theta} in each module position.
#'   \item{\code{item_ID}}: the item IDs of administered items.
#'   \item{\code{ncat}}: the number of categories of administered items.
#'   \item{\code{IRT_model}}: the IRT models of administered items.
#'   \item{\code{item_par_1}}: the first item parameter of each administered item (e.g., for 1PL, this is item difficulty)
#'   \item{\code{item_par_2}}: the second item parameter of each administered item (e.g., for 1PL, this is `NA`)
#'   \item{\code{item_resp}}: the item response on each administered item.
#'   \item{\code{momentary_theta}}: the momentary (interim) \eqn{\theta} estimate obtained after each item administration in CAT engine.
#'   \item{\code{momentary_SE}}: the standard error of momentary (interim) \eqn{\theta} estimate obtained after each item administration in CAT engine.
#' }
#' @export
formatOutput <- function(examinee_list, digits = 3) {

  examinee_list <- removeItemData(examinee_list)

  o <- NULL

  for (examinee in examinee_list) {
    for (m in 1:examinee@n_module) {

      max_m <- examinee@n_module

      nrow <- length(examinee@response[[m]])

      x <- data.frame(
        p_ID             = examinee@examinee_id,
        test_phase_ID    = m,
        initial_grade    = examinee@grade_log[1],
        final_grade      = examinee@grade_log[max_m],
        grade_ID         = examinee@grade_log[m],
        phase_ID         = examinee@phase_log[m],
        test_ID          = examinee@test_log[m],
        module_ID        = examinee@module_log[m],
        final_theta_est  = examinee@estimated_theta_for_routing[[max_m]]$theta,
        final_SE_est     = examinee@estimated_theta_for_routing[[max_m]]$theta_se,
        initial_theta_in_module = examinee@initial_theta_in_module[m],
        theta_by_test    = examinee@estimated_theta_by_test[[m]]$theta,
        SE_by_test       = examinee@estimated_theta_by_test[[m]]$theta_se,
        theta_by_phase   = examinee@estimated_theta_by_phase[[m]]$theta,
        SE_by_phase      = examinee@estimated_theta_by_phase[[m]]$theta_se,
        routing_based_on = examinee@routing_based_on[[m]],
        routing_theta    = NA,
        routing_SE       = NA,
        routing_L        = NA,
        routing_U        = NA,
        alpha            = examinee@alpha,
        true_theta       = examinee@true_theta[m],
        item_sequence    = 1:length(examinee@administered_items[[m]]),
        item_ID          = examinee@administered_items[[m]],
        ncat             = examinee@item_data[[m]]$NCAT,
        IRT_model        = examinee@item_data[[m]]$model,
        item_par_1       = examinee@item_data[[m]]$ipar_1,
        item_par_2       = examinee@item_data[[m]]$ipar_2,
        item_resp        = examinee@response[[m]],
        selection_theta  = examinee@selection_theta[[m]],
        momentary_theta  = examinee@interim_theta[[m]]$theta,
        momentary_SE     = examinee@interim_theta[[m]]$theta_se
      )

      for (i in 1:dim(x)[1]) {

        x$routing_theta[i] <- switch(
          x$routing_based_on[i],
          estimated_theta_by_phase = x$theta_by_phase[i],
          estimated_theta_by_test  = x$theta_by_test[i],
        )
        x$routing_SE[i] <- switch(
          x$routing_based_on[i],
          estimated_theta_by_phase = x$SE_by_phase[i],
          estimated_theta_by_test  = x$SE_by_test[i],
        )

      }

      x$routing_L <- x$routing_theta - qnorm((1 - x$alpha / 2)) * x$routing_SE
      x$routing_U <- x$routing_theta + qnorm((1 - x$alpha / 2)) * x$routing_SE

      columns_to_round <- c(
        "final_theta_est",
        "final_SE_est",
        "initial_theta_in_module",
        "theta_by_test",
        "SE_by_test",
        "theta_by_phase",
        "SE_by_phase",
        "routing_theta",
        "routing_SE",
        "true_theta",
        "selection_theta",
        "momentary_theta",
        "momentary_SE"
      )

      x[, columns_to_round] <- round(x[, columns_to_round], digits)

      o <- rbind(o, x)

    }
  }

  return(o)

}

#' Calculate RMSE from an examinee list object
#'
#' \code{\link{getRMSE}} is a function for calculating root mean square error (RMSE)
#' for the simulation results.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#'
#' @return a list containing RMSE by test and also for all tests combined.
#'
#' @export
getRMSE <- function(x) {

  o <- list()

  RMSE <- numeric(6)
  for (p in c(2, 4, 6)) {
    d <- lapply(x@examinee_list,
      function(xx) {
        xx@estimated_theta_by_test[[p]]$theta - xx@true_theta[p]
      }
    )
    RMSE[p] <- sqrt(mean(unlist(d) ** 2))
  }

  o$RMSE_by_test <- RMSE[c(2, 4, 6)]

  d <- lapply(x@examinee_list,
    function(xx) {
      estimated_theta_by_test <-
        lapply(
          xx@estimated_theta_by_test,
          function(xxx) {
            xxx$theta
          }
        )
      estimated_theta_by_test <- unlist(estimated_theta_by_test)
      diff <- estimated_theta_by_test - xx@true_theta
      diff <- diff[c(2, 4, 6)]
      return(diff)
    }
  )

  o$RMSE_all_tests <- sqrt(mean(unlist(d) ** 2))

  return(o)

}

#' Calculate bias from an examinee list object
#'
#' \code{\link{getBias}} is a function for calculating the bias of ability estimates of the simulation results.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#'
#' @return a list containing bias by test and also for all tests combined.
#'
#' @export
getBias <- function(x) {

  o <- list()

  Bias <- numeric(6)
  for (p in c(2, 4, 6)) {
    d <- lapply(x@examinee_list,
      function(xx) {
        xx@estimated_theta_by_test[[p]]$theta - xx@true_theta[p]
      }
    )
    Bias[p] <- mean(unlist(d))
  }

  o$Bias_by_test <- Bias[c(2, 4, 6)]
  o$Bias_all_tests <- mean(o$Bias_by_test)

  return(o)
}

#' Calculate standard error from an examinee list object
#'
#' \code{\link{getSE}} is a function for calculating the standard error of the estimates.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#'
#' @return a list containing SE by test and also for all tests combined.
#'
#' @export
getSE <- function(x) {

  o <- list()

  SE <- numeric(6)
  for (p in c(2, 4, 6)) {
    estimated_theta_by_test <-
      unlist(lapply(x@examinee_list, function(xx){
        xx@estimated_theta_by_test[[p]]$theta
    }))

    mean_est_theta <- mean(estimated_theta_by_test)

    SE[p] <- mean((estimated_theta_by_test - mean_est_theta) ** 2)
  }

  o$SE_by_test <- SE[c(2, 4, 6)]
  o$SE_all_tests <- mean(o$SE_by_test)

  return(o)
}

#' Calculate adaptivity indices from an examinee list object
#'
#' \code{\link{getAdaptivityIndex}} is a function for calculating adaptivity indices from the output of \code{\link{maat}}.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#'
#' @return a data frame containing adaptivity indices by test and also for all tests combined.
#'
#' @export
getAdaptivityIndex <- function(x) {

  theta_t1  <- vector(length = length(x@examinee_list))
  theta_t2  <- vector(length = length(x@examinee_list))
  theta_t3  <- vector(length = length(x@examinee_list))

  mean_difficulty_t1    <- vector(length = length(x@examinee_list))
  mean_difficulty_t2    <- vector(length = length(x@examinee_list))
  mean_difficulty_t3    <- vector(length = length(x@examinee_list))
  mean_difficulty_total <- vector(length = length(x@examinee_list))

  for (i in 1:length(x@examinee_list)) {
    theta_t1[i] <- x@examinee_list[[i]]@estimated_theta_by_test[[2]]$theta
    theta_t2[i] <- x@examinee_list[[i]]@estimated_theta_by_test[[4]]$theta
    theta_t3[i] <- x@examinee_list[[i]]@estimated_theta_by_test[[6]]$theta

    mean_difficulty_t1[i] <- mean(c(
      mean(as.vector(x@examinee_list[[i]]@item_data[[1]]@ipar), na.rm = TRUE),
      mean(as.vector(x@examinee_list[[i]]@item_data[[2]]@ipar), na.rm = TRUE)
    ))

    mean_difficulty_t2[i] <- mean(c(
      mean(as.vector(x@examinee_list[[i]]@item_data[[3]]@ipar), na.rm = TRUE),
      mean(as.vector(x@examinee_list[[i]]@item_data[[4]]@ipar), na.rm = TRUE)
    ))

    mean_difficulty_t3[i] <- mean(c(
      mean(as.vector(x@examinee_list[[i]]@item_data[[5]]@ipar), na.rm = TRUE),
      mean(as.vector(x@examinee_list[[i]]@item_data[[6]]@ipar), na.rm = TRUE)
    ))

    mean_difficulty_total[i] <- mean(c(
      mean_difficulty_t1[i],
      mean_difficulty_t2[i],
      mean_difficulty_t3[i]
    ))

  }

  cor_t1    <- cor(theta_t1, mean_difficulty_t1)
  cor_t2    <- cor(theta_t2, mean_difficulty_t2)
  cor_t3    <- cor(theta_t3, mean_difficulty_t3)
  cor_total <- cor(theta_t3, mean_difficulty_total)

  ratio_t1    <- sd(mean_difficulty_t1)    / sd(theta_t1)
  ratio_t2    <- sd(mean_difficulty_t2)    / sd(theta_t2)
  ratio_t3    <- sd(mean_difficulty_t3)    / sd(theta_t3)
  ratio_total <- sd(mean_difficulty_total) / sd(theta_t3)

  adaptivity_index <- data.frame(
    cor   = c(cor_t1  , cor_t2  , cor_t3  , cor_total),
    ratio = c(ratio_t1, ratio_t2, ratio_t3, ratio_total)
  )
  rownames(adaptivity_index) <- c("T1", "T2", "T3", "ALL")

  return(adaptivity_index)

}

#' Get administered items per test
#'
#' \code{\link{getAdministeredItemsPerTest}} is a function for extracting the administered items stored in the
#' \code{\linkS4class{examinee}} objects.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#'
#' @return a list containing administered items in each test and also for all tests combined.
#'
#' @export
getAdministeredItemsPerTest <- function(x) {
  items_used <- list()
  for (i in 1:length(x@examinee_list)) {
    for (m in 1:x@examinee_list[[i]]@n_module) {
      test_idx <- x@examinee_list[[i]]@test_log[m]
      items_used[[test_idx]] <- c(
        items_used[[test_idx]],
        x@examinee_list[[i]]@administered_items[[m]]
      )
    }
  }
  items_used$ALL <- do.call(c, items_used)
  return(items_used)
}

#' Get item names per grade
#'
#' \code{\link{getItemNamesPerGrade}} is a function for extracting item names from a module list.
#'
#' @param module_list a module list from \code{\link{loadModules}}.
#' @return item names per grade.
#'
#' @examples
#' getItemNamesPerGrade(module_list_math)
#'
#' @export
getItemNamesPerGrade <- function(module_list) {

  items_per_grade <- lapply(
    module_list,
    function(x) {
      x$T1$P1@constraints@pool@id
    }
  )
  return(items_per_grade)

}

#' Get item exposure rates from an examinee list
#'
#' \code{\link{getItemExposureRate}} is a function for building an item exposure rate table.
#'
#' @param x an \code{\linkS4class{output_maat}} object from \code{\link{maat}}.
#' @return the table of item exposure rate.
#'
#' @export
getItemExposureRate <- function(x) {

  n_examinee           <- length(x@examinee_list)
  items_per_grade      <- getItemNamesPerGrade(x@module_list)
  administered_items   <- getAdministeredItemsPerTest(x)

  grades <- names(items_per_grade)
  tests  <- names(administered_items)

  administered_items_list <- list()
  pool_usage              <- list()
  exposure_rates          <- list()
  exposure_rate_stats     <- list()

  for (test in tests) {

    pool_usage[[test]] <- list()

    for (grade in grades) {

      items_in_this_grade <- items_per_grade[[grade]]

      administered_items_list[[test]][[grade]] <- administered_items[[test]][
        administered_items[[test]] %in% items_in_this_grade
      ]

      pool_usage[[test]][[grade]] <-
        length(unique(administered_items_list[[test]][[grade]])) /
        length(items_in_this_grade)

      exposure_rates[[test]][[grade]] <-
        table(administered_items_list[[test]][[grade]]) /
        n_examinee

      unexposed_items <- setdiff(
        items_in_this_grade,
        unique(administered_items_list[[test]][[grade]])
      )

      exposure_rates[[test]][[grade]][unexposed_items] <- 0

      exposure_rate_stats[[test]][[grade]] <- c(
        summary(as.vector(exposure_rates[[test]][[grade]])),
        sd = sd(exposure_rates[[test]][[grade]])
      )
    }
  }

  exposure_rate_tables <- lapply(
    exposure_rate_stats,
    function(x) {
      o <- as.data.frame(do.call(rbind, x))
    }
  )

  exposure_rate_tables <- mapply(
    function(x, y) {
      cbind(
        Grade = rownames(x),
        Utilization = do.call(c, y),
        x
      )
    },
    exposure_rate_tables,
    pool_usage,
    SIMPLIFY = FALSE
  )

  return(exposure_rate_tables)

}
