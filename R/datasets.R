#' Example item pools
#'
#' Example data for a 6-module assessment.
#'
#' \itemize{
#'   \item \code{assessment_structure_math} an \code{\linkS4class{assessment_structure}} object defining 3 tests with 2 phases in each test. Also defines routing limits as \code{G - 1} and \code{G + 2}, where \code{G} is the starting grade.
#'   \item \code{examinee_list_math} a list of \code{\linkS4class{examinee}} objects. The number of examinees is 10. This can be created using \code{\link{simExaminees}}.
#'   \item \code{module_list_math} a list of \code{\linkS4class{module}} objects. This can be created using \code{\link{loadModules}}.
#'   \item \code{cut_scores_math} a list of theta cut scores. This is used in the \code{cut_scores} argument of the \code{\link{maat}} function.
#' }
#'
#' @aliases assessment_structure_math examinee_list_math module_list_math cut_scores_math
#' @docType data
#' @keywords datasets
#' @name module_list_math
#' @rdname module_list_math
NULL
