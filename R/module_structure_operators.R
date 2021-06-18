#' @include module_class.R
NULL

#' Phase operator: move to next phase
#'
#' \code{\link{changePhase}} is an operator for phase values.
#'
#' @param phase a string containing the current phase in the format \code{P?}, where \code{?} is a number.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#'
#' @return a string containing the new phase.
#'
#' @examples
#' ## assessment uses two phases
#' changePhase("P1", assessment_structure_math) ## P2
#' changePhase("P2", assessment_structure_math) ## P1
#'
#' @export
changePhase <- function(phase, assessment_structure) {
  phase <- substr(phase, 2, 100)
  phase <- as.numeric(phase)
  phase <- phase + 1
  if (phase > assessment_structure@n_phase) {
    phase <- 1
  }
  phase <- sprintf("P%s", phase)
  return(phase)
}

#' Grade operator: add or subtract
#'
#' \code{\link{changeGrade}} is an operator for grade values.
#'
#' @param grade a string containing the current grade in the form \code{G?}, where \code{?} is a number.
#' @param delta a number containing the relative change in grade to apply. \code{0} retains the current grade as-is.
#'
#' @return a string containing the new grade.
#'
#' @examples
#' changeGrade("G4",  0) ## G4
#' changeGrade("G4",  1) ## G5
#' changeGrade("G4", -1) ## G3
#' changeGrade("G10", 1) ## G11
#'
#' @export
changeGrade <- function(grade, delta) {
  grade <- substr(grade, 2, 100)
  grade <- as.numeric(grade)
  grade <- grade + delta
  grade <- sprintf("G%s", grade)
  return(grade)
}

#' Grade operator: difference between two grades
#'
#' \code{\link{getRelativeGrade}} is an operator for grade values.
#'
#' @param current_grade a string containing the current grade in the form \code{G?}, where \code{?} is a number.
#' @param initial_grade a string containing the initial grade in the form \code{G?}, where \code{?} is a number.
#'
#' @return the grade difference of the current grade relative to the initial grade.
#'
#' @examples
#' getRelativeGrade("G4", "G3") ## 1
#' getRelativeGrade("G5", "G3") ## 2
#' getRelativeGrade("G2", "G3") ## -1
#'
#' @export
getRelativeGrade <- function(current_grade, initial_grade) {
  current_grade <- substr(current_grade, 2, 100)
  initial_grade <- substr(initial_grade, 2, 100)
  current_grade <- as.numeric(current_grade)
  initial_grade <- as.numeric(initial_grade)
  return(current_grade - initial_grade)
}

#' Test operator: move to next phase
#'
#' \code{\link{changeTest}} is an operator for test values.
#'
#' @param test a string containing the current test in the format \code{T?}, where \code{?} is a number.
#' @param phase a string containing the current phase in the format \code{P?}, where \code{?} is a number.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#'
#' @return a string containing the new test.
#'
#' @examples
#' ## assessment uses two phases
#' changeTest("T1", "P1", assessment_structure_math) ## T1
#' changeTest("T1", "P2", assessment_structure_math) ## T2
#'
#' @export
changeTest <- function(test, phase, assessment_structure) {
  last_phase <- sprintf("P%s", assessment_structure@n_phase)
  if (phase == last_phase) {
    test <- substr(test, 2, 100)
    test <- as.numeric(test)
    test <- test + 1
    test <- sprintf("T%s", test)
    return(test)
  } else {
    return(test)
  }

}

#' Create an assessment structure
#'
#' \code{\link{createAssessmentStructure}} is a function for creating an
#' \code{\linkS4class{assessment_structure}} object that defines the structure of the assessment.
#'
#' @param n_test a numeric, the number of test administrations.
#' @param n_phase a numeric, the number of phases within each test.
#' @param route_limit_below the number of grades to allow routing below, relative to the grade of record. If the grade of record is G4 and this is 1, then routing to G3 is allowed but not to G2.
#' @param route_limit_above the number of grades to allow routing above, relative to the grade of record. If the grade of record is G4 and this is 2, then routing to G6 is allowed but not to G7.
#' @param test_routing_restrictions the restrictions for between-test routing. (default = \code{c("R1", "R2", "R3")})
#'
#' @return an \code{\linkS4class{assessment_structure}} object.
#'
#' @examples
#' assessment_structure <- createAssessmentStructure(
#'   n_test  = 3,
#'   n_phase = 2,
#'   route_limit_below = 1,
#'   route_limit_above = 2
#' )
#'
#' @export
createAssessmentStructure <- function(
  n_test, n_phase, route_limit_below, route_limit_above,
  test_routing_restrictions = c("R1", "R2", "R3")
) {

  o <- new("assessment_structure")
  o@n_test  <- n_test
  o@n_phase <- n_phase
  o@route_limit_below <- route_limit_below
  o@route_limit_above <- route_limit_above
  o@test_routing_restrictions <- test_routing_restrictions

  validObject(o)

  return(o)

}
