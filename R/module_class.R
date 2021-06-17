#' @include import.R
NULL

#' Class 'module': a module
#'
#' \code{\linkS4class{module}} is an S4 class to represent a module.
#'
#' @slot module_id the ID of the module.
#' @slot constraints a \code{\linkS4class{constraints}} object.
#'
#' @export
setClass("module",
  slots = c(
    module_id   = "character",
    constraints = "constraints"
  ),
  prototype = list(
    module_id   = character(0),
    constraints = new("constraints")
  ),
  validity = function(object) {
    if (!validObject(object@constraints)) {
      stop("@constraints is not a valid constraints object")
    }
    return(TRUE)
  }
)

#' Class 'assessment_structure': assessment structure
#'
#' \code{\linkS4class{assessment_structure}} is an S4 class to represent an assessment structure.
#'
#' @slot n_test a numeric, the number of test administrations.
#' @slot n_phase a numeric, the number of phases within each test.
#' @slot route_limit_below the number of grades to allow routing below, relative to the grade of record. If the grade of record is G4 and this is 1, then routing to G3 is allowed but not to G2.
#' @slot route_limit_above the number of grades to allow routing above, relative to the grade of record. If the grade of record is G4 and this is 2, then routing to G6 is allowed but not to G7.
#' @slot test_routing_restrictions R1: If grade is G-1 in the last phase of any administration, ignore achievement level and always change grade by +1.
#' R2: If grade is G in the last phase of any administration: If achievement level is Beginning, do not decrease grade.
#' R3: If grade is G+k in the last phase of Administration k: If achievement level is Advanced, do not increase grade.
#'
#' @export
setClass("assessment_structure",
  slots = c(
    n_test                    = "numeric",
    n_phase                   = "numeric",
    route_limit_below         = "numeric",
    route_limit_above         = "numeric",
    test_routing_restrictions = "character"
  ),
  prototype = list(
    n_test                    = numeric(0),
    n_phase                   = numeric(0),
    route_limit_below         = numeric(0),
    route_limit_above         = numeric(0),
    test_routing_restrictions = character(0)
  ),
  validity = function(object) {
    x <- NULL
    if (object@route_limit_below > 1) {
      m <- sprintf("currently maat package only supports route_limit_below not larger than 1")
      x <- c(x, m)
    }
    if (object@route_limit_above > 2) {
      m <- sprintf("currently maat package only supports route_limit_above not larger than 2")
      x <- c(x, m)
    }
    if (object@route_limit_below < 0) {
      m <- sprintf("route_limit_below must be nonnegative")
      x <- c(x, m)
    }
    if (object@route_limit_above < 0) {
      m <- sprintf("route_limit_above must be nonnegative")
      x <- c(x, m)
    }
    if (object@n_phase != 2) {
      m <- sprintf("currently maat package only supports n_phase == 2")
      x <- c(x, m)
    }
    if (object@n_test != 3) {
      m <- sprintf("currently maat package only supports n_test == 3")
      x <- c(x, m)
    }
    if (length(x) == 0) {
      return(TRUE)
    } else {
      return(x)
    }
  }
)
