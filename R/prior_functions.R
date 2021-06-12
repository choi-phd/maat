#' @noRd
getPriorUsingMeanDifficulty <- function(examinee_list, current_module_position, module_list_by_name, module_for_thisgroup, prior_sd) {
  itempool_thisgroup <- module_list_by_name[[module_for_thisgroup]]@constraints@pool
  mean_difficulty <- mean(itempool_thisgroup@ipar, na.rm = TRUE)
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      x@prior_par_by_module[[current_module_position]] <-
        c(
          mean_difficulty,
          prior_sd[[x@current_grade]]
        )
      return(x)
    }
  )
  return(examinee_list)
}

#' @noRd
getPriorUsingUserMeans <- function(examinee_list, current_module_position, prior_mean_user, prior_sd) {
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      x@prior_par_by_module[[current_module_position]] <-
        c(
          prior_mean_user[[x@current_grade]],
          prior_sd[[x@current_grade]]
        )
      return(x)
    }
  )
  return(examinee_list)
}

#' @noRd
getPriorUsingCarryoverMeans <- function(examinee_list, current_module_position, prior_sd) {
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      x@prior_par_by_module[[current_module_position]] <-
        c(
          x@estimated_theta_for_routing[[current_module_position - 1]]$theta,
          prior_sd[[x@current_grade]]
        )
      return(x)
    }
  )
  return(examinee_list)
}

#' @noRd
getPriorUsingReuse <- function(examinee_list, current_module_position) {
  examinee_list <- lapply(
    examinee_list,
    function(x) {
      x@prior_par_by_module[[current_module_position]] <-
      x@prior_par_by_module[[current_module_position - 1]]
      return(x)
    }
  )
  return(examinee_list)
}

#' @noRd
extractPrior <- function(examinee_list, current_module_position) {
  prior_par <- lapply(
    examinee_list,
    function(x) {
      x@prior_par_by_module[[current_module_position]]
    }
  )
  prior_par <- do.call("rbind", prior_par)
  return(prior_par)
}

#' @noRd
genPriorDist <- function(dist_type, prior_par, theta_grid, nj) {

  nq <- length(theta_grid)
  m  <- NULL

  if (tolower(dist_type) == "normal" && is.vector(prior_par) && length(prior_par) == 2) {
    x <- dnorm(theta_grid, mean = prior_par[1], sd = prior_par[2])
    m <- matrix(x, nj, nq, byrow = TRUE)
  }
  if (tolower(dist_type) == "normal" && is.matrix(prior_par) && all(dim(prior_par) == c(nj, 2))) {
    m <- matrix(NA, nj, nq, byrow = TRUE)
    for (j in 1:nj) {
      m[j, ] <- dnorm(theta_grid, mean = prior_par[j, 1], sd = prior_par[j, 2])
    }
  }
  if (tolower(dist_type) == "uniform") {
    x <- 1
    m <- matrix(x, nj, nq, byrow = TRUE)
  }
  if (is.null(m)) {
    stop("unrecognized 'prior_par': must be a vector c(mean, sd), or a nj * 2 matrix")
  }

  return(m)

}
