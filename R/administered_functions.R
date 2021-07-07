#' @include module_functions.R
NULL

#' Update a constraints object to exclude administered items
#'
#' The function \code{\link{excludeAdministeredItems}} produces a new \code{\linkS4class{constraints}} object
#' that excludes administered items from being selected.
#'
#' @param constraints a \code{\linkS4class{constraints}} object.
#' @param administered_items item names of previously administered items.
#'
#' @return a \code{\linkS4class{constraints}} object that also constrains the administered items to be excluded.
#'
#' @examples
#' \dontrun{
#' require(TestDesign)
#'
#' cfg <- createShadowTestConfig(
#'   MIP = list(solver = "lpsymphony")
#' )
#' constraints <- constraints_reading
#' solution <- Shadow(cfg, constraints, true_theta = 0)
#' administered_items <- solution@output[[1]]@administered_item_index
#' administered_items <- solution@constraints@pool@id[administered_items]
#' administered_items
#'
#' updated_constraints <- excludeAdministeredItems(constraints, administered_items)
#'
#' solution <- Shadow(cfg, updated_constraints, true_theta = 0)
#' administered_items <- solution@output[[1]]@administered_item_index
#' administered_items <- solution@constraints@pool@id[administered_items]
#' administered_items ## entirely different from above
#' }
#' @export
excludeAdministeredItems <- function(constraints, administered_items) {

  if (length(administered_items) == 0) {
    return(constraints)
  }

  item_pool <- constraints@pool
  item_index <- which(item_pool@id %in% administered_items)
  if (length(item_index) != length(administered_items)) {
    stop("'administered_items' has item names not present in the pool")
  }

  tmp <- sprintf('"%s"', administered_items)
  tmp <- paste0(tmp, collapse = ", ")
  tmp <- sprintf("ID %%in%% c(%s)", tmp)

  new_constraint <- data.frame(
    CONSTRAINT = dim(constraints@constraints)[1] + 1,
    TYPE = "EXCLUDE",
    WHAT = "ITEM",
    CONDITION = tmp,
    LB = NA,
    UB = NA,
    ONOFF = NA
  )

  new_constraints <- rbind(constraints@constraints[, 1:7], new_constraint)

  new_constraints <- loadConstraints(
    new_constraints,
    constraints@pool,
    constraints@item_attrib,
    constraints@st_attrib)

  return(new_constraints)

}

#' Remove item data from examinee list
#'
#' \code{\link{removeItemData}} is a function to remove the item data from
#' the \code{\linkS4class{examinee}} objects for the reduction of file size.
#'
#' @param examinee_list a list containing \code{\linkS4class{examinee}} objects.
#'
#' @return a list containing \code{\linkS4class{examinee}} objects,
#' with \code{item_data} data stripped for compact storage.
#'
#' @export
removeItemData <- function(examinee_list) {

  examinee_list <-
    lapply(
      examinee_list,
      function(x) {
        n_module  <- x@n_module
        item_data <- vector("list", n_module)
        for (module in 1:n_module) {
          item_data[[module]] <- list(
            NCAT   = x@item_data[[module]]@NCAT,
            model  = x@item_data[[module]]@model,
            ipar_1 = x@item_data[[module]]@ipar[, 1],
            ipar_2 = x@item_data[[module]]@ipar[, 2]
          )
        }
        x@item_data[1:x@n_module] <- NULL
        x@item_data <- item_data
        return(x)
      }
    )

  return(examinee_list)

}
