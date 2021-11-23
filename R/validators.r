#' @include module_functions.R
NULL

#' @noRd
isGrade <- function(x) {

  if (substr(x, 0, 1) != "G") {
    stop(sprintf("%s is not a grade (must be G? where ? is a positive number)", x))
  }

  typecast_to_number <- suppressWarnings(
    as.numeric(substr(x, 2, nchar(x)))
  )
  if (is.na(typecast_to_number)) {
    stop(sprintf("%s is not a grade (must be G? where ? is a positive number)", x))
  }

  if (typecast_to_number <= 0) {
    stop(sprintf("%s is not a grade (must be G? where ? is a positive number)", x))
  }

  return(TRUE)

}

#' @noRd
isTest <- function(x) {

  if (substr(x, 0, 1) != "T") {
    stop(sprintf("%s is not a test (must be T? where ? is a positive number)", x))
  }

  typecast_to_number <- suppressWarnings(
    as.numeric(substr(x, 2, nchar(x)))
  )
  if (is.na(typecast_to_number)) {
    stop(sprintf("%s is not a test (must be T? where ? is a positive number)", x))
  }

  if (typecast_to_number <= 0) {
    stop(sprintf("%s is not a test (must be T? where ? is a positive number)", x))
  }

  return(TRUE)

}

#' @noRd
isPhase <- function(x) {

  if (substr(x, 0, 1) != "P") {
    stop(sprintf("%s is not a phase (must be P? where ? is a positive number)", x))
  }

  typecast_to_number <- suppressWarnings(
    as.numeric(substr(x, 2, nchar(x)))
  )
  if (is.na(typecast_to_number)) {
    stop(sprintf("%s is not a phase (must be P? where ? is a positive number)", x))
  }

  if (typecast_to_number <= 0) {
    stop(sprintf("%s is not a phase (must be P? where ? is a positive number)", x))
  }

  return(TRUE)

}

#' @noRd
valueOf <- function(x) {
  typecast_to_number <- suppressWarnings(
    as.numeric(substr(x, 2, nchar(x)))
  )
  return(typecast_to_number)
}
