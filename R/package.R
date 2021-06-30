#' Multiple Administrations Adaptive Testing
#'
#' \bold{maat} package is based on the assessment framework involving multiple
#' tests administered throughout the year using multiple item pools vertically
#' scaled and multiple phases (stages) of computerized adaptive testing (CAT) 
#' within each test allowing for transitioning from one item pool (and associated 
#' constraints) to another between phases as determined necessary by a selected 
#' transition policy to enhance the quality of measurement.
#'
#' The current version of \bold{maat} supports three administrations (Fall, Winter,
#' and Spring) with two phases within each administration (Phase 1 and Phase 2),
#' for six modules in total administered over the course of a year.
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
#' Detailed descriptions of the assessment design are available in the vignette.
#'
#' @name maat-package
#' @docType package
#' @title Multiple Administrations Adaptive Testing
#' @keywords package
NULL
