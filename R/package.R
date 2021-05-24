#' Through-Year Computerized Adaptive Testing
#'
#' **maat** package is based on the Through-Year DMST design, a longitudinal
#' extension of the Dynamic Multi-Stage Test (DMST) design which presents each
#' module as an adaptive test assembled dynamically or on the fly based on test
#' blueprint constraints and interim ability estimates in real time. The
#' Through-Year DMST design also maintains adaptivity across administrations of
#' an assessment throughout the year.
#'
#' The DMST design in **maat** package allows three administrations (Fall, Winter,
#' and Spring) with two phases within each administration (Phase 1 and Phase 2),
#' so that a student takes six modules in total.
#'
#' Within each administration, students begin Phase 1 at the grade of record.
#' One exception to this is that if a student's final \eqn{\theta} from the
#' previous administration was above the 'advanced achievement' cut score of
#' the grade of record, then the student begins Phase 1 of the following
#' administration in an above-grade item pool. For example, if a Grade 3
#' student's final \eqn{\theta} from the Fall administration was
#' \eqn{\theta = 1.1} and the 'advanced achievement' cut score for Grade 3 was
#' \eqn{\theta = 1.0}, then the student begins Phase 1 of the Winter
#' administration in a Grade 4 item pool.
#'
#' Within each administration, at the completion of Phase 1, business rules are
#' used to determine whether a student is routed to an on-grade or off-grade
#' item pool in Phase 2.
#'
#' Detailed descriptions of the DMST design are available in the vignette.
#'
#' @name maat-package
#' @docType package
#' @title Through-Year Computerized Adaptive Testing
#' @keywords package
NULL
