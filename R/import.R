#' @import TestDesign
#' @import readxl
#' @import methods
#' @importFrom DiagrammeR grViz
#' @importFrom MASS mvrnorm
#' @importFrom stats qnorm cor sd na.omit quantile
#' @importFrom graphics abline lines points rect text
#' @importFrom glue glue
#' @importFrom utils read.csv packageVersion head tail
#' @importFrom TestDesign Shadow createShadowTestConfig
NULL

.onAttach <- function(libname, pkgname) {
  if (packageVersion("TestDesign") < "1.1.3.9003") {
    stop("Version requirement not met: 'TestDesign' needs to be at least 1.1.3.9003")
  }
}
