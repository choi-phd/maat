% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim_functions.R
\name{simExaminees}
\alias{simExaminees}
\title{Simulate an examinee list}
\usage{
simExaminees(
  N,
  mean_v,
  sd_v,
  cor_v,
  assessment_structure,
  initial_grade = "G4",
  initial_test = "T1",
  initial_phase = "P1"
)
}
\arguments{
\item{N}{the number of examinees.}

\item{mean_v}{a vector containing the mean of each dimension.}

\item{sd_v}{a vector containing the standard deviation of each dimension.}

\item{cor_v}{a correlation matrix.}

\item{assessment_structure}{an \code{\linkS4class{assessment_structure}} object. This can be created using \code{\link{createAssessmentStructure}}.}

\item{initial_grade}{the initial grade for all examinees. The grade must exist in \code{module_list}. Also used as the grade of record when the initial phase and test points to a module position greater than 1. (default = \code{G4})}

\item{initial_test}{the initial test for all examinees. (default = \code{T1})}

\item{initial_phase}{the initial phase for all examinees. The phase must exist in \code{module_list}. (default = \code{P1})}
}
\value{
a list of \code{\linkS4class{examinee}} objects.
}
\description{
\code{\link{simExaminees}} is a function for generating a list of \code{\linkS4class{examinee}} objects.
}
\details{
Each dimension of \code{mean_v}, \code{sd_v}, \code{cor_v} represents a test level. For example in a three-test structure (see the \code{assessment_structure_math} example data), these arguments must have three dimensions.
}
\examples{
assessment_structure <- createAssessmentStructure(
  n_test  = 3,
  n_phase = 2,
  route_limit_below = 1,
  route_limit_above = 2
)
examinee_list <- simExaminees(
  N      = 100,
  mean_v = c(0, 0, 0),
  sd_v   = c(1, 1, 1),
  cor_v  = diag(1, 3),
  assessment_structure = assessment_structure
)

}
