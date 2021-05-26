#' @include loadModules.R
NULL

#' Plot module routes
#'
#' \code{\link{plotModuleRoutes}} is a function for building a chart for module routes.
#'
#' @param examinee_list the output from \code{\link{maat}}.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#' @param examinee_id the examinee ID to plot the module route. \code{all} plots the count that each routing node was used. (default = \code{all})
#' @param font_size the font size for the count labels. (default = \code{15})
#' @param box_colour the colour for the boxes. (default = \code{PaleTurquoise})
#'
#' @export
plotModuleRoutes <-  function(examinee_list, assessment_structure, examinee_id = 'all', font_size = 15, box_colour = "PaleTurquoise") {

  ###############################
  ##### Module Information ######
  ###############################
  route_counts <- countModuleRoutes(examinee_list, assessment_structure)

  ###############################
  ##### Data for DiagrammeR #####
  ###############################

  for(i in 1:(route_counts$n_test * route_counts$n_phase - 1)) {
    route_counts$module_arrow[[i]][,1] <- paste0(names(route_counts$module_arrow[[i]])[1],"_",route_counts$module_arrow[[i]][,1])

    route_counts$module_arrow[[i]][,2] <- paste0(names(route_counts$module_arrow[[i]])[2],"_",route_counts$module_arrow[[i]][,2])
  }

  arrow_IDs <- unlist(lapply(route_counts$module_arrow, function(x) paste0(x[,1], " -> ",x[,2])))

  if (examinee_id == "all") {
    arrow_labels  <- route_counts$counts
    color_arrfont <- rep("black", length(arrow_labels))
    module_flag   <- rep(TRUE, length(unlist(route_counts$module_map)))
  } else {
    arrow_labels  <- rep("0", length(route_counts$counts))
    color_arrfont <- rep("white", length(arrow_labels))
    module_log    <- route_counts$individual_log[examinee_id, ]
    module_flag <- c(
      route_counts$module_map[[1]] %in% module_log[1],
      route_counts$module_map[[2]] %in% module_log[2],
      route_counts$module_map[[3]] %in% module_log[3],
      route_counts$module_map[[4]] %in% module_log[4],
      route_counts$module_map[[5]] %in% module_log[5],
      route_counts$module_map[[6]] %in% module_log[6])
  }

  # cell_ID should be matched to arrow IDs
  cell_ID <- route_counts$module_map
  for(i in 1:(route_counts$n_test * route_counts$n_phase)) { # i = 1
    cell_ID[[i]]  <- as.numeric(gsub("[^\\d]+", "", route_counts$module_map[[i]], perl=TRUE))
    cell_ID[[i]] <- paste0("p", i, "_", cell_ID[[i]])
  }

  a1 <-
    data.frame(
      cell_ID     = unlist(cell_ID),
      cell_names  = route_counts$module_names,
      cell_flag   = module_flag,
      colour_box  = rep("white", length(unlist(route_counts$module_map))),
      colour_arr  = rep("grey", length(unlist(route_counts$module_map))),
      colour_font = rep("black", length(unlist(route_counts$module_map)))
    )

  a1$colour_box[a1$cell_flag]   <- box_colour
  a1$colour_font[!a1$cell_flag] <- "grey"
  colour_box <- a1$colour_box
  colour_font<- a1$colour_font
  colour_arr <- rep("grey90", length(arrow_IDs))

  if (examinee_id != "all") {
    temp <- a1$cell_ID[a1$cell_flag]
    arrow_IDs_draw <-
      sapply(2:length(temp), function(ei) {
        fi = ei - 1; paste0(temp[fi], " -> ",temp[ei])})

    colour_arr[arrow_IDs %in% arrow_IDs_draw] <- "black"
  }

  if(examinee_id == "all") {
    temp_arrowid <- do.call("rbind",strsplit(arrow_IDs, " -> "))

    start <- as.numeric(do.call('rbind', strsplit(temp_arrowid[,1], "_"))[,2])
    end <- as.numeric(do.call('rbind', strsplit(temp_arrowid[,2], "_"))[,2])

    arrow_dir_index <- sapply(1:length(start), function(i) {

      if(start[i] > end[i]) {
        "down"
      } else if(start[i] < end[i]) {
        "up"
      } else {
        "stay"
      }
    })

    colour_arr[arrow_dir_index == "up"] <- "Blue"
    colour_arr[arrow_dir_index == "stay"] <- "DarkGreen"
    colour_arr[arrow_dir_index == "down"] <- "Red"

    color_arrfont[arrow_dir_index == "up"] <- "Blue"
    color_arrfont[arrow_dir_index == "stay"] <- "DarkGreen"
    color_arrfont[arrow_dir_index == "down"] <- "Red"

  }
  ######################
  ##### DiagrammeR #####
  ######################

  # open syntax
  plot_syntax <- glue("digraph maat {{
    // comment
    graph[rankdir = LR, overlap = true, fontsize = 10]
    splines='false';
    node[shape = rectangle, style = filled, color = DimGrey, margin = 0.2]

    ")
  # module generation
  for(i in 1:dim(a1)[1]) {
     module_parts <- glue("
       {a1$cell_ID[i]}[label = {a1$cell_names[i]},
       fillcolor = {a1$colour_box[i]},
       fontcolor = {a1$colour_font[i]}]
       ")

     plot_syntax <- glue("{plot_syntax}
                         {module_parts}")
  }

  # arrow generation
  plot_syntax <- glue("{plot_syntax}

                      edge[arrowhead = vee, arrowsize = .5, fontsize = {font_size}, penwidth = 1,
       minlen = 1]")

  weights <- rep("", length(arrow_IDs))
  for(i in 1:length(arrow_IDs)) {
    temp1 <- strsplit(arrow_IDs[i], split=" -> ")[[1]]
    temp2 <- unlist(strsplit(temp1, split="_"))[c(2,4)]

    if(temp2[1] == temp2[2]) {
      weights[i] <- ", weight = 5"
    }
  }

  if(sum(c("R1","R2","R3") %in% route_counts$test_routing_restrictions) == 3){
    if(route_counts$route_limit_below == 0 && route_counts$route_limit_above == 2 ) {
      # if(starting_grade_num == 3) {
      weights[c(1,4,8)] <- ", constraint=false"
    } else if(route_counts$route_limit_below == 1 && route_counts$route_limit_above == 1 ) {
        # } else if(starting_grade_num == 7) {
      weights[c(1,16,21)] <- ", constraint=false"
    } else {
      weights[1] <- ", constraint=false"
      }
  }

  if(sum(c("R1","R2") %in% route_counts$test_routing_restrictions) == 2){
    if(route_counts$route_limit_below == 1 && route_counts$route_limit_above == 1 ) {
      weights[c(1, 11, 21)] <- ", constraint=false"
    }
  }


  if(sum(c("R2","R3") %in% route_counts$test_routing_restrictions) == 2){
    weights[1] <- ", constraint=false"
  }

  if(route_counts$route_limit_below == 1 && route_counts$route_limit_above == 0 ) {
    # } else if(starting_grade_num == 8) {
    weights[1] <- ", weight = 5"
  }

  arrow_syntax <- ""
  for(i in 1:length(arrow_IDs)) {
    arrow_parts <- glue("{arrow_IDs[i]} [label = {arrow_labels[i]}, color = {colour_arr[i]}, fontcolor = {color_arrfont[i]} {weights[i]}]")
    arrow_syntax <- glue("{arrow_syntax}
                         {arrow_parts}")
  }

  plot_syntax <- glue("{plot_syntax}
                      {arrow_syntax}")

  if(route_counts$route_limit_below == 1 && route_counts$route_limit_above == 0) {
    if((length(route_counts$test_routing_restrictions) == 1 && route_counts$test_routing_restrictions == "R1") | (length(route_counts$test_routing_restrictions) == 2 && sum(route_counts$test_routing_restrictions %in% c("R1","R3"))==2) ) {
    # } else if(starting_grade_num == 8) {

    limit_below <- route_counts$starting_grade_num - 1
    plot_syntax <- glue("
       {plot_syntax}

p2_{limit_below} -> p3_{limit_below} [label = 0, color = white, fontcolor = white , weight = 5]
p4_{limit_below} -> p5_{limit_below} [label = 0, color = white, fontcolor = white , weight = 5]

")
  }
}
  if((route_counts$route_limit_below == 1 && route_counts$route_limit_above == 2) | (route_counts$route_limit_below == 1 && route_counts$route_limit_above == 1) ) {
    if(
      (length(route_counts$test_routing_restrictions) == 1 && route_counts$test_routing_restrictions == "R1") |
     (length(route_counts$test_routing_restrictions) == 2 && sum(route_counts$test_routing_restrictions %in% c("R1","R3"))==2) ) {
    # } else if(starting_grade_num == 8) {

    limit_below <- route_counts$starting_grade_num - 1
    plot_syntax <- glue("
       {plot_syntax}

p2_{limit_below} -> p3_{limit_below} [label = 0, color = white, fontcolor = white , weight = 5]
p4_{limit_below} -> p5_{limit_below} [label = 0, color = white, fontcolor = white , weight = 5]

")
  }
  }

  # close plot syntax
  plot_syntax <- glue("{plot_syntax}
                        }}")

  grViz(plot_syntax)
}

#' @noRd
countModuleRoutes <-  function(examinee_list, assessment_structure){

  ###############################
  ##### Generate Module map #####
  ###############################
  starting_grade <- lapply(
    examinee_list,
    function(x) {
      x@grade_log[1]
    }
  )
  starting_grade <- unique(unlist(starting_grade))
  starting_grade_num <- as.numeric(gsub("[^\\d]+", "", starting_grade, perl=TRUE))

  n_test <- assessment_structure@n_test
  n_phase <- assessment_structure@n_phase
  route_limit_below <- assessment_structure@route_limit_below
  route_limit_above <- assessment_structure@route_limit_above

  max_grade <- starting_grade_num + route_limit_above
  min_grade <- starting_grade_num - route_limit_below

  module_map <- vector("list", n_test * n_phase)
  module_map[1] <- list(starting_grade_num)

  for(i in 1:(n_test * n_phase -1)) { # i = 2
    temp_max <- max(unlist(module_map[i]))
    temp_min <- min(unlist(module_map[i]))

    next_max_grade <- ifelse((temp_max + 1) < max_grade, starting_grade_num + 1, max_grade)
    next_min_grade <- ifelse((temp_min - 1) >= min_grade, starting_grade_num - 1, min_grade)

    module_map[(i+1)] <- list(next_min_grade:next_max_grade)
  }

  module_path <- vector("list", (n_test * n_phase - 1))
  for(i in 1:(n_test * n_phase -1)) {
    temp_path <- expand.grid(sort(module_map[[i]],decreasing = T), sort(module_map[[(i+1)]],decreasing = T))

    names(temp_path) <- c(paste0("p", i),paste0("p", i+1))

    temp_path <- temp_path[!(temp_path[[1]] - temp_path[[2]] > 1 | temp_path[[1]] - temp_path[[2]] < -1) ,]
    temp_path <- temp_path[order(temp_path[,1], decreasing = T),]

    module_path[i] <- list(temp_path)
  }

  test_routing_restrictions <-  assessment_structure@test_routing_restrictions
  # c("R1","R2","R3")
  if("R1" %in% test_routing_restrictions) {

    min_grade <- min(module_path[[2]][,1])

    module_path[[2]][module_path[[2]][,1] == min_grade, 2] <- ifelse(starting_grade_num == min_grade, min_grade, min_grade + 1)

    min_grade <- min(module_path[[4]][,1])
    module_path[[4]][module_path[[4]][,1] == min_grade, 2] <- ifelse(starting_grade_num == min_grade, min_grade, min_grade + 1)
  }

  if("R2" %in% test_routing_restrictions) {

    sgn <- starting_grade_num
    module_path[[2]][module_path[[2]][,1] == sgn & module_path[[2]][,2] == (sgn-1), 2] <- sgn

    min_grade <- min(module_path[[4]][,1])
    module_path[[4]][module_path[[4]][,1] == sgn & module_path[[4]][,2] == (sgn-1), 2] <- sgn
  }

  if("R3" %in% test_routing_restrictions) {

    max_grade <- max(module_path[[2]][,1])
    module_path[[2]][module_path[[2]][,1] == max_grade & module_path[[2]][,2] >= max_grade, 2] <- max_grade

    max_grade <- max(module_path[[4]][,1])
    module_path[[4]][module_path[[4]][,1] == max_grade & module_path[[4]][,2] >= max_grade, 2] <- max_grade
  }

  module_path <- lapply(module_path, function(x) unique( x ))
  for(i in c(2,4)) {
    available_module <- unique(module_path[[i]][,2])
    module_path[[i+1]] <- module_path[[i+1]][module_path[[i+1]][,1] %in% available_module , ]
  }

  # final module_map
  module_map <- append(starting_grade, lapply(module_path, function(x) paste0("G",unique(x[,2]))))

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
  for(i in 1:5) {
    # i = 1
    a1 <- paste0("G",module_path[[i]][,2])
    names(a1) <- paste0("G",module_path[[i]][,1])
    a2 <- split(a1, f = names(a1))

    prop0 <- table(prop_data[,(i+1):i])

    for(j in length(names(a2)):1) {
      # j = 1
      col_idx <- which(colnames(prop0) == names(a2)[j])
      row_idx <- which(rownames(prop0) %in% a2[[names(a2)[j]]])

      counts <- append(counts, prop0[row_idx ,col_idx])
    }
  }

  o <- list(
    starting_grade    = starting_grade,
    starting_grade_num=starting_grade_num,
    module_arrow      = module_path,
    module_map        = module_map,
    module_names      = unlist(module_map),
    counts            = counts,
    individual_log    = grade_log,

    n_test = n_test,
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
#' @param cut_scores a named list containing cut scores for each grade.
#'
#' @examples
#' library(TestDesign)
#' config <- createShadowTestConfig(
#'   final_theta = list(
#'     method = "MLE"
#'   )
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
#' examinee <- examinee_list[[1]]
#' plot(examinee, cut_scores = cut_scores_math)
#'
#' @docType methods
#' @rdname plot-methods
#' @export
setMethod(
  f = "plot",
  signature = "examinee",
  definition = function(
    x, y, cut_scores) {

    o <- x

    estimated_theta_for_routing <- unlist(lapply(
      o@estimated_theta_for_routing,
      function(x) {
        x$theta
      }
    ))

    interim_theta <- unlist(lapply(
      o@interim_theta,
      function(x) {
        x$theta
      }
    ))

    n_items <- unlist(lapply(
      o@interim_theta,
      function(x) {
        length(x$theta)
      }
    ))
    true_theta <- rep(o@true_theta, times = n_items)

    x_idx <- 1:length(interim_theta)
    plot(
      x_idx, interim_theta, type = 'n',
      xlim = range(x_idx), ylim = c(-5, 5),
      main = sprintf("Examinee ID: %s", o@examinee_id))
    abline(v = c(0, cumsum(n_items)) + 0.5, col = "grey", lty = 2)

    for (m in 1:6) {
      x_from <- c(0, cumsum(n_items))[m]
      x_to   <- c(0, cumsum(n_items))[m + 1]

      for (idx_cut in c(1, 3)) {
        lines(
          c(x_from, x_to) + 0.5,
          rep(cut_scores[[o@grade_log[m]]][idx_cut], 2),
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
      o@grade_log
    )

    response <- unlist(o@response)
    response_color <- factor(response)
    levels(response_color) <-
      c("red", "lime green", "cyan")
    response_color <- as.character(response_color)

    for (i in x_idx) {
      rect(
        i - 0.5, -5, i + 0.5, -5 + (response[i] + 1) * 0.2,
        col = response_color[i])
    }

  })
