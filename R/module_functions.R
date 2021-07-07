#' @include module_class.R
NULL

#' Create a single module
#'
#' \code{\link{createModule}} is a function for creating a \code{\linkS4class{module}} object
#' based on the item pool, attribute, and constraints.
#'
#' @param constraints constraints data. A \code{\link{data.frame}} or a csv file name to be used in \code{\link{loadConstraints}}.
#' @param item_pool item pool data. A \code{\link{data.frame}} or a csv file name to be used in \code{\link{loadItemPool}}.
#' @param item_attrib item attribute data. A \code{\link{data.frame}} or a csv file name to be used in \code{\link{loadItemAttrib}}.
#' @param passage_attrib passage attribute data. A \code{\link{data.frame}} or a csv file name to be used in \code{\link{loadStAttrib}}.
#'
#' @returns a \code{\linkS4class{module}} object.
#'
#' @export
createModule <- function(constraints, item_pool, item_attrib, passage_attrib) {
  module_item_pool   <- loadItemPool(item_pool)
  module_item_attrib <- loadItemAttrib(item_attrib, module_item_pool)
  if (!is.na(passage_attrib)) {
    module_passage_attrib <- loadStAttrib(passage_attrib, module_item_attrib)
    module_constraints <- loadConstraints(constraints, module_item_pool, module_item_attrib, module_passage_attrib)
  } else {
    module_passage_attrib <- NULL
    module_constraints <- loadConstraints(constraints, module_item_pool, module_item_attrib)
  }
  o <- new("module")
  o@constraints <- module_constraints
  return(o)
}

#' Load multiple modules
#'
#' \code{\link{loadModules}} is a function for creating multiple \code{\linkS4class{module}} objects
#' from a specification sheet.
#'
#' @param fn the name of a csv file containing module specifications.
#' @param base_path (optional) the base path to append before the file paths contained in module specs.
#' @param assessment_structure an \code{\linkS4class{assessment_structure}} object.
#' @param examinee_list an examinee list from \code{\link{simExaminees}}. Used to determine the range of required modules.
#'
#' @details
#' The module specification file is expected to have the following columns:
#' \itemize{
#'   \item{\code{Grade} a string containing the grade in the form \code{G?}, where \code{?} is a number.}
#'   \item{\code{Phase} a string containing the phase in the form \code{P?}, where \code{?} is a number.}
#'   \item{\code{ItemPool} the file path of a file that contains item pool data. This must be readable with \code{\link[TestDesign]{loadItemPool}}.}
#'   \item{\code{ItemAttrib} the file path of a file that contains item attribute data. This must be readable with \code{\link[TestDesign]{loadItemAttrib}}.}
#'   \item{\code{PassageAttrib} the file path of a file that contains passage attribute data. This must be readable with \code{\link[TestDesign]{loadStAttrib}}.}
#'   \item{\code{Constraints} the file path of a file that contains constraints data. This must be readable with \code{\link[TestDesign]{loadConstraints}}.}
#' }
#'
#' @returns a module list containing \code{\linkS4class{module}} objects.
#' Each module can be accessed using \code{module_list[[grade]][[phase]]}.
#'
#' @examples
#' assessment_structure <- createAssessmentStructure(
#'   n_test  = 3,
#'   n_phase = 2,
#'   route_limit_below = 0,
#'   route_limit_above = 2
#' )
#' examinee_list <- simExaminees(
#'   N             = 5,
#'   mean_v        = c(0, 0, 0),
#'   sd_v          = c(1, 1, 1),
#'   cor_v         = diag(1, 3),
#'   assessment_structure = assessment_structure
#' )
#'
#' fn <- system.file("extdata", "module_definition_MATH_normal_N500.csv", package = "maat")
#' pkg_path <- system.file(package = "maat")
#' module_list <- loadModules(
#'   fn,
#'   base_path = pkg_path,
#'   assessment_structure = assessment_structure,
#'   examinee_list = examinee_list
#' )
#'
#' @export
loadModules <- function(fn, base_path = NULL, assessment_structure, examinee_list) {

  # Determine required grade range
  starting_grades <- lapply(examinee_list,
    function(x) {
      if (is.na(x@grade_log[1])) {
        return(x@current_grade)
      } else {
        return(x@grade_log[1])
      }
    }
  )
  starting_grades <- unique(unlist(starting_grades))

  v <- seq(-assessment_structure@route_limit_below, assessment_structure@route_limit_above)
  required_grades <- lapply(starting_grades,
    function(x) {
      changeGrade(x, v)
    }
  )
  required_grades <- unique(unlist(required_grades))

  required_phases <- sprintf("P%s", 1:assessment_structure@n_phase)

  required_modules <- expand.grid(
    grade = required_grades,
    phase = required_phases,
    stringsAsFactors = FALSE
  )
  n_required_modules <- dim(required_modules)[1]

  cat(sprintf("Required modules: %s\n", n_required_modules))

  # Read module sheet and validate whether all required modules exist
  df <- read.csv(fn, stringsAsFactors = FALSE)

  for (i in 1:n_required_modules) {
    idx <- which(
      required_modules$grade[i] == df$Grade &
      required_modules$phase[i] == df$Phase
    )
    if (length(idx) != 1) {
      stop(
        sprintf(
          "cannot find Grade %s Phase %s in %s",
          required_modules$grade[i],
          required_modules$phase[i],
          fn
        )
      )
    }
  }

  # Now load modules

  cat(sprintf("Using base path: %s\n", base_path))
  cat(sprintf("Loading %s modules\n", n_required_modules))

  module_list <- list()

  for (i in 1:n_required_modules) {

    idx <- which(
      required_modules$grade[i] == df$Grade &
      required_modules$phase[i] == df$Phase
    )

    cat(sprintf(
      "Grade %s Phase %s : Module %s\n",
      df$Grade[idx],
      df$Phase[idx],
      df$Module[idx]
    ))

    if (!is.null(base_path)) {
      df$Constraints[idx]   <- file.path(base_path, df$Constraints[idx])
      df$ItemPool[idx]      <- file.path(base_path, df$ItemPool[idx])
      df$ItemAttrib[idx]    <- file.path(base_path, df$ItemAttrib[idx])
      if (!is.na(df$PassageAttrib[idx])) {
        df$PassageAttrib[idx] <- file.path(base_path, df$PassageAttrib[idx])
      }
    }

    o <- createModule(
      df$Constraints[idx],
      df$ItemPool[idx],
      df$ItemAttrib[idx],
      df$PassageAttrib[idx]
    )

    o@module_id <- df$Module[idx]

    if (!df$Grade[idx] %in% names(module_list)) {
      module_list[[df$Grade[idx]]] <- list()
    }
    module_list[[df$Grade[idx]]][[df$Phase[idx]]] <- o

  }

  return(module_list)

}
