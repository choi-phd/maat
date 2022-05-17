#' @import TestDesign
#' @import readxl
#' @import methods
#' @importFrom MASS mvrnorm
#' @importFrom diagram plotmat
#' @importFrom stats qnorm cor sd na.omit quantile dnorm
#' @importFrom graphics abline lines points rect text box par
#' @importFrom utils read.csv packageVersion head tail
#' @importFrom TestDesign Shadow createShadowTestConfig
#' @importClassesFrom TestDesign config_Shadow
NULL

setClassUnion("numeric_or_null", c("numeric", "NULL"))
setClassUnion("list_or_null"   , c("list", "NULL"))
setClassUnion("list_or_config_Shadow", c("list", "config_Shadow"))
