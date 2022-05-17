#' @include sim_functions.R
NULL

#' @noRd
countModuleRoutes <- function(examinee_list, assessment_structure) {

  if (!inherits(assessment_structure, "assessment_structure")) {
    stop(sprintf("unexpected objected class: expecting 'assessment_structure'"))
  }

  starting_grade <- lapply(
    examinee_list,
    function(x) {
      x@grade_log[1]
    }
  )
  starting_grade <- unique(unlist(starting_grade))
  starting_grade_value <- valueOf(starting_grade)

  n_test  <- assessment_structure@n_test
  n_phase <- assessment_structure@n_phase
  route_limit_below <- assessment_structure@route_limit_below
  route_limit_above <- assessment_structure@route_limit_above

  max_grade <- starting_grade_value + route_limit_above
  min_grade <- starting_grade_value - route_limit_below

  module_map <- vector("list", n_test * n_phase)
  module_map[1] <- list(starting_grade_value)

  for (i in 1:(n_test * n_phase - 1)) {
    temp_max <- max(unlist(module_map[i]))
    temp_min <- min(unlist(module_map[i]))
    next_max_grade <- ifelse((temp_max + 1) <  max_grade, starting_grade_value + 1, max_grade)
    next_min_grade <- ifelse((temp_min - 1) >= min_grade, starting_grade_value - 1, min_grade)
    module_map[(i + 1)] <- list(next_min_grade:next_max_grade)
  }

  module_path <- vector("list", (n_test * n_phase - 1))
  for (i in 1:(n_test * n_phase - 1)) {
    temp_path <- expand.grid(
      sort(module_map[[i]], decreasing = TRUE),
      sort(module_map[[(i+1)]], decreasing = TRUE)
    )
    names(temp_path) <- sprintf("p%s", c(i, i + 1))
    temp_path <- temp_path[!(temp_path[[1]] - temp_path[[2]] > 1 | temp_path[[1]] - temp_path[[2]] < -1), ]
    temp_path <- temp_path[order(temp_path[,1], decreasing = TRUE), ]
    module_path[i] <- list(temp_path)
  }

  test_routing_restrictions <- assessment_structure@test_routing_restrictions

  if ("R1" %in% test_routing_restrictions) {
    min_grade <- min(module_path[[2]][, 1])
    module_path[[2]][module_path[[2]][, 1] == min_grade, 2] <-
      ifelse(
        starting_grade_value == min_grade,
        min_grade, min_grade + 1
      )
    min_grade <- min(module_path[[4]][, 1])
    module_path[[4]][module_path[[4]][, 1] == min_grade, 2] <-
      ifelse(
        starting_grade_value == min_grade,
        min_grade, min_grade + 1
      )
  }

  if ("R2" %in% test_routing_restrictions) {
    sgn <- starting_grade_value
    module_path[[2]][module_path[[2]][, 1] == sgn & module_path[[2]][, 2] == (sgn - 1), 2] <- sgn
    min_grade <- min(module_path[[4]][, 1])
    module_path[[4]][module_path[[4]][, 1] == sgn & module_path[[4]][, 2] == (sgn - 1), 2] <- sgn
  }

  if ("R3" %in% test_routing_restrictions) {
    max_grade <- max(module_path[[2]][, 1])
    module_path[[2]][module_path[[2]][, 1] == max_grade & module_path[[2]][, 2] >= max_grade, 2] <- max_grade
    max_grade <- max(module_path[[4]][, 1])
    module_path[[4]][module_path[[4]][, 1] == max_grade & module_path[[4]][, 2] >= max_grade, 2] <- max_grade
  }

  module_path <- lapply(module_path, function(x) unique(x))

  for (i in c(2, 4)) {
    available_module <- unique(module_path[[i]][, 2])
    idx <- module_path[[i + 1]][, 1] %in% available_module
    module_path[[i + 1]] <- module_path[[i + 1]][idx, ]
  }

  # final module_map
  module_map <- append(
    starting_grade,
    lapply(
      module_path,
      function(x) {
        sprintf("G%s", unique(x[, 2]))
      }
    )
  )

  ##########################
  #### calculate counts ####
  ##########################
  grade_log <- lapply(
    examinee_list,
    function(x) {
      x@grade_log
    }
  )
  grade_log <- do.call("rbind", grade_log)

  prop_data <- grade_log
  prop_data <- data.frame(prop_data)
  prop_data$X2 <- factor(prop_data$X2, levels = module_map[[2]])
  prop_data$X3 <- factor(prop_data$X3, levels = module_map[[3]])
  prop_data$X4 <- factor(prop_data$X4, levels = module_map[[4]])
  prop_data$X5 <- factor(prop_data$X5, levels = module_map[[5]])
  prop_data$X6 <- factor(prop_data$X6, levels = module_map[[6]])

  counts <- c()
  for (i in 1:5) {
    a1 <- sprintf("G%s", module_path[[i]][, 2])
    names(a1) <- sprintf("G%s", module_path[[i]][, 1])
    a2 <- split(a1, f = names(a1))
    prop0 <- table(prop_data[, (i + 1):i])
    for (j in length(names(a2)):1) {
      col_idx <- which(colnames(prop0) == names(a2)[j])
      row_idx <- which(rownames(prop0) %in% a2[[names(a2)[j]]])
      counts  <- append(counts, prop0[row_idx, col_idx])
    }
  }

  o <- list(
    starting_grade     = starting_grade,
    starting_grade_value = starting_grade_value,
    module_arrow       = module_path,
    module_map         = module_map,
    module_names       = unlist(module_map),
    counts             = counts,
    individual_log     = grade_log,
    n_test  = n_test,
    n_phase = n_phase,
    route_limit_below = route_limit_below,
    route_limit_above = route_limit_above,
    max_grade = max_grade,
    min_grade = min_grade,
    test_routing_restrictions = test_routing_restrictions
  )
  return(o)
}

#' plot
#'
#' @param x x
#' @param y y
#' @param type the type of plot. \code{route} plots the number of examinees routed to each path across the course of entire assessment. \code{correlation} produces a scatterplot of thetas across administrations. \code{audit} plots interim thetas over modules for a single examinee.
#' @param examinee_id the examinee ID to plot.
#' @param cut_scores (optional) a named list containing cut scores for each grade.
#' @param theta_range the theta range to use in scatter plots when \code{x} is an examinee list.
#' @param main the figure title to use in scatter plots when \code{x} is an examinee list.
#' @param box_color the cell color to use when \code{type} is \code{route}. (default = \code{PaleTurquoise})
#'
#' @return the route plot.
#'
#' @examples
#' \donttest{
#' library(TestDesign)
#' config <- createShadowTestConfig(
#'   final_theta = list(method = "MLE"),
#'   exclude_policy = list(method = "SOFT", M = 100)
#' )
#' examinee_list <- maat(
#'   examinee_list          = examinee_list_math,
#'   assessment_structure   = assessment_structure_math,
#'   module_list            = module_list_math,
#'   overlap_control_policy = "all",
#'   transition_CI_alpha    = 0.05,
#'   config                 = config,
#'   cut_scores             = cut_scores_math
#' )
#'
#' plot(examinee_list, type = "route")
#' plot(examinee_list, type = "correlation")
#' plot(examinee_list, type = "audit", examinee_id = 1)
#'
#' }
#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "output_maat",
  definition = function(
    x, y, type, examinee_id = 1, cut_scores = NULL, theta_range = c(-4, 4), main = NULL, box_color = "PaleTurquoise") {

    if (type == "correlation") {

      tests <- lapply(
        x@examinee_list,
        function(o) {
          o@test_log
        }
      )
      n_tests <- length(unique(unlist(tests)))

      true_theta <- lapply(
        x@examinee_list,
        function(o) {
          theta <- c()
          for (test in unique(o@test_log)) {
            if (is.na(test)) {
              theta <- c(theta, NA)
            }
            if (!is.na(test)) {
              idx <- max(which(o@test_log == test))
              theta <- c(theta, o@true_theta[idx])
            }
          }
          return(theta)
        }
      )
      true_theta <- matrix(unlist(true_theta), length(true_theta), byrow = TRUE)

      final_theta <- lapply(
        x@examinee_list,
        function(o) {
          theta <- c()
          for (test in unique(o@test_log)) {
            if (is.na(test)) {
              theta <- c(theta, NA)
            }
            if (!is.na(test)) {
              idx <- max(which(o@test_log == test))
              theta <- c(theta, o@estimated_theta_for_routing[[idx]]$theta)
            }
          }
          return(theta)
        }
      )
      final_theta <- matrix(unlist(final_theta), length(final_theta), byrow = TRUE)

      old_par <- par(no.readonly = TRUE)
      on.exit({
        par(old_par)
      })
      par(mfrow = c(1, n_tests))

      if (is.null(main)) {
        main <- sprintf("Test %s", 1:n_tests)
      }
      for (test in 1:n_tests) {
        plot(
          0, 0,
          type = "n",
          xlim = theta_range,
          ylim = theta_range,
          xlab = "True theta",
          ylab = "Estimated theta",
          main = main[test]
        )
        lines(
          theta_range * 2,
          theta_range * 2,
          lty = 2,
          col = "gray"
        )
        points(
          true_theta[, test],
          final_theta[, test],
          col = "blue"
        )
        r <- cor(true_theta[, test], final_theta[, test])
        text(
          x = min(theta_range),
          y = max(theta_range),
          labels = sprintf("r = %1.3f", r),
          adj = 0
        )
        box(lwd = 1)
      }
    }

    if (type == "audit") {

      examinee <- x@examinee_list[[examinee_id]]

      estimated_theta_for_routing <- unlist(lapply(
        examinee@estimated_theta_for_routing,
        function(xx) {
          if (is.null(xx)) return(NA)
          xx$theta
        }
      ))

      interim_theta <- unlist(lapply(
        examinee@interim_theta,
        function(xx) {
          if (is.null(xx)) return(NA)
          xx$theta
        }
      ))

      n_items <- unlist(lapply(
        examinee@interim_theta,
        function(xx) {
          if (is.null(xx)) return(1)
          length(xx$theta)
        }
      ))
      true_theta <- rep(examinee@true_theta, times = n_items)

      x_idx <- 1:length(interim_theta)
      plot(
        x_idx, interim_theta, type = 'n',
        xlim = range(x_idx), ylim = c(-5, 5),
        main = sprintf("Examinee ID: %s", examinee@examinee_id),
        xlab = "Item position",
        ylab = "Interim theta")

      # number of phases in a test is assumed to be 2
      v <- c(0, cumsum(n_items)) + 0.5
      abline(v = v[seq(1, length(v), 2)], col = "grey", lty = 1)
      abline(v = v[seq(2, length(v), 2)], col = "grey", lty = 2)

      for (m in 1:6) {
        x_from <- c(0, cumsum(n_items))[m]
        x_to   <- c(0, cumsum(n_items))[m + 1]

        for (idx_cut in c(1, 3)) {
          lines(
            c(x_from, x_to) + 0.5,
            rep(x@cut_scores[[examinee@grade_log[m]]][idx_cut], 2),
            col = "grey"
          )
        }
      }

      lines(x_idx, true_theta, col = "red")
      lines(x_idx, interim_theta, col = "blue", lty = 2)
      points(x_idx, interim_theta, pch = 21, col = "blue", bg = "blue")
      text(
        cumsum(n_items) - n_items / 2,
        rep(3, 6),
        examinee@grade_log
      )

      module_list_by_name <- unlist(x@module_list)
      module_names <- unlist(lapply(
        module_list_by_name,
        function(xx) {
          xx@module_id
        }
      ))
      names(module_list_by_name) <- module_names

      response <- unlist(lapply(
        examinee@response,
        function(xx) {
          if (is.null(xx)) return(-1)
          xx
        }
      ))

      n_category <- list()
      for (m in 1:6) {
        module <- module_list_by_name[[examinee@module_log[m]]]
        if (is.null(module)) next
        administered_items <- examinee@administered_items[[m]]
        n_category[[m]] <- sapply(
          1:length(administered_items),
          function(xx) {
            idx_in_pool <- which(module@constraints@pool@id == administered_items[xx])
            module@constraints@pool@NCAT[idx_in_pool]
          }
        )
      }

      n_category <- unlist(lapply(
        n_category,
        function(xx) {
          if (is.null(xx)) return(NA)
          xx
        }
      ))

      response_color <- sapply(
        1:length(response),
        function(xx) {
          if (is.na(n_category[xx])) return("white")
          if (n_category[xx] == 2) {
            if (response[xx] == 0) return("red")
            if (response[xx] == 1) return("lime green")
          }
          if (n_category[xx] > 2) {
            return("cyan")
          }
        }
      )

      for (i in x_idx) {
        rect(
          i - 0.5, -5, i + 0.5, -5 + (response[i] + 1) * 0.2,
          col = response_color[i])
      }

    }

    if (type == "route") {

      route_counts <- countModuleRoutes(x@examinee_list, x@assessment_structure)

      route_range <-
        -x@assessment_structure@route_limit_below:
        x@assessment_structure@route_limit_above
      n_grades <- length(route_range)
      n_modules <-
        x@assessment_structure@n_test *
        x@assessment_structure@n_phase
      n_cells <- n_grades * n_modules

      M <- matrix(NA, n_cells, n_cells)
      cell_names <- sprintf(
        "G%sP%s",
        rep(route_counts$max_grade:route_counts$min_grade, each = n_modules),
        rep(1:n_modules, n_grades)
      )
      colnames(M) <- cell_names
      rownames(M) <- cell_names

      for (m in 1:length(route_counts$module_arrow)) {
        o <- route_counts$module_arrow[[m]]
        for (i in 1:dim(o)[1]) {
          cell_source <- sprintf("G%sP%s", o[i, 1], m)
          cell_target <- sprintf("G%sP%s", o[i, 2], m + 1)
          n <-
            (route_counts$individual_log[, m    ] == sprintf("G%s", o[i, 1])) &
            (route_counts$individual_log[, m + 1] == sprintf("G%s", o[i, 2]))
          n <- sum(n)
          M[cell_target, cell_source] <- sprintf("%s", n)
        }
      }

      cell_names_visible <- c()
      for (p in 1:length(route_counts$module_map)) {
        cell_names_visible <- c(
          cell_names_visible,
          sprintf("%sP%s", route_counts$module_map[[p]], p)
        )
      }

      visibility <- cell_names %in% cell_names_visible

      box_type <- rep("rect", n_cells)
      box_type[!visibility] <- FALSE
      box_name <- colnames(M)
      box_name[!visibility] <- ""
      box_name <- substr(box_name, 1, 2)

      cell_layout <- rep(n_modules, n_grades)

      old_par <- par(no.readonly = TRUE)
      on.exit({
        par(old_par)
      })
      par(mar = c(1, 1, 1, 1))
      plotmat(
        M,
        pos = cell_layout,
        absent   = -1,
        name     = box_name,
        box.type = box_type,
        box.size = 0.04,
        arr.type = "triangle",
        arr.length = 0.2,
        arr.width  = 0.2,
        curve = 0,
        box.col = box_color,
        shadow.size = 0,
        dtext = c(0, 1),
        arr.pos = 0.4
      )

    }

  }
)
