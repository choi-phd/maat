% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extensions.R, R/plot_functions.R
\docType{methods}
\name{plot}
\alias{plot}
\alias{plot,output_maat-method}
\title{Extension of plot()}
\usage{
\S4method{plot}{output_maat}(
  x,
  y,
  type,
  examinee_id = 1,
  cut_scores = NULL,
  theta_range = c(-4, 4),
  main = NULL,
  box_color = "PaleTurquoise"
)
}
\arguments{
\item{x}{x}

\item{y}{y}

\item{type}{the type of plot. \code{route} plots the number of examinees routed to each path across the course of entire assessment. \code{correlation} produces a scatterplot of thetas across administrations. \code{audit} plots interim thetas over modules for a single examinee.}

\item{examinee_id}{the examinee ID to plot.}

\item{cut_scores}{(optional) a named list containing cut scores for each grade.}

\item{theta_range}{the theta range to use in scatter plots when \code{x} is an examinee list.}

\item{main}{the figure title to use in scatter plots when \code{x} is an examinee list.}

\item{box_color}{the cell color to use when \code{type} is \code{route}. (default = \code{PaleTurquoise})}
}
\value{
the route plot.
}
\description{
Extension of plot()
}
\examples{
\donttest{
library(TestDesign)
config <- createShadowTestConfig(
  final_theta = list(method = "MLE"),
  exclude_policy = list(method = "SOFT", M = 100)
)
examinee_list <- maat(
  examinee_list          = examinee_list_math,
  assessment_structure   = assessment_structure_math,
  module_list            = module_list_math,
  overlap_control_policy = "all",
  transition_CI_alpha    = 0.05,
  config                 = config,
  cut_scores             = cut_scores_math
)

plot(examinee_list, type = "route")
plot(examinee_list, type = "correlation")
plot(examinee_list, type = "audit", examinee_id = 1)

}
}
