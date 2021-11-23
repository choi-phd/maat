#' @include module_functions.R
NULL

#' Update the current phase of an examinee object
#'
#' \code{\link{updatePhase}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#' \code{\link{updatePhase}} updates the phase by calling \code{\link{changePhase}}.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{current_phase} slot updated.
#'
#' @examples
#' ## assessment uses two phases
#'
#' examinee <- examinee_list_math[[1]]
#' examinee@current_phase ## P1
#'
#' examinee <- updatePhase(examinee, assessment_structure_math)
#' examinee@current_phase ## P2
#'
#' examinee <- updatePhase(examinee, assessment_structure_math)
#' examinee@current_phase ## P1
#'
#' @export
updatePhase <- function(examinee_object, assessment_structure) {
  isPhase(examinee_object@current_phase)
  examinee_object@current_phase <- changePhase(
    examinee_object@current_phase,
    assessment_structure
  )
  return(examinee_object)
}

#' Update the current test of an examinee object
#'
#' \code{\link{updateTest}} is the function for updating the new test ID in
#' an \code{\linkS4class{examinee}} object.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{current_test} slot updated.
#'
#' @examples
#' ## assessment uses two phases
#'
#' examinee <- examinee_list_math[[1]]
#' examinee@current_test  ## T1
#' examinee@current_phase ## P1
#'
#' examinee <- updateTest(examinee, assessment_structure_math)
#' examinee <- updatePhase(examinee, assessment_structure_math)
#' examinee@current_test  ## T1
#' examinee@current_phase ## P2
#'
#' examinee <- updateTest(examinee, assessment_structure_math)
#' examinee <- updatePhase(examinee, assessment_structure_math)
#' examinee@current_test  ## T2
#' examinee@current_phase ## P1
#'
#' @export
updateTest <- function(examinee_object, assessment_structure) {
  isTest(examinee_object@current_test)
  isPhase(examinee_object@current_phase)
  examinee_object@current_test <- changeTest(
    examinee_object@current_test,
    examinee_object@current_phase,
    assessment_structure
  )
  return(examinee_object)
}

#' Update the current module of an examinee object
#'
#' \code{\link{updateModule}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#' \code{\link{updateModule}} assigns an \code{\linkS4class{module}} object from the supplied list to match the grade and the phase the \code{\linkS4class{examinee}} is in.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param module_list a module list from \code{\link{loadModules}}.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{current_module} slot updated.
#'
#' @export
updateModule <- function(examinee_object, module_list) {

  grade <- examinee_object@current_grade
  test  <- examinee_object@current_test
  phase <- examinee_object@current_phase
  module <- module_list[[grade]][[test]][[phase]]
  module_name <- module@module_id
  examinee_object@current_module <- module_name

  return(examinee_object)

}

#' Update the routing log of an examinee object
#'
#' \code{\link{updateLog}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#' \code{\link{updateLog}} updates logs with grades, phases, tests and modules.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param current_module_position the current module position.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{grade_log}, \code{phase_log}, \code{test_log}, and \code{module_log} slots updated.
#'
#' @export
updateLog <- function(examinee_object, current_module_position) {
  examinee_object@grade_log[current_module_position] <-
    examinee_object@current_grade
  examinee_object@phase_log[current_module_position] <-
    examinee_object@current_phase
  examinee_object@test_log[current_module_position] <-
    examinee_object@current_test
  examinee_object@module_log[current_module_position] <-
    examinee_object@current_module
  return(examinee_object)
}

#' Update the item data slot of an examinee object
#'
#' \code{\link{updateItemData}} is a function for updating \code{\linkS4class{examinee}} objects after completing a module.
#'
#' \code{\link{updateItemData}} updates the \code{item_data} slot with an \code{\linkS4class{item_pool}} object
#' that contains administered items in the module.
#'
#' @param examinee_object an \code{\linkS4class{examinee}} object.
#' @param module_position the current module position.
#' @param solution an \code{\linkS4class{output_Shadow_all}} object.
#'
#' @return an \code{\linkS4class{examinee}} object with its \code{item_data} slot updated.
#'
#' @export
updateItemData <- function(examinee_object, module_position, solution) {

  examinee_id <- examinee_object@examinee_id
  output      <- solution@output[[examinee_id]]
  pool        <- solution@pool

  index_new <- output@administered_item_index

  administered_item_data         <- pool[index_new]
  administered_item_data@ni      <- output@test_length_constraints
  administered_item_data@max_cat <- output@max_cat_pool
  administered_item_data@index   <- 1:administered_item_data@ni

  examinee_object@item_data[[module_position]] <- administered_item_data

  return(examinee_object)

}
