#' @include module_class.R
# Show functions should return NULL invisibly.
# Always call print() internally.
NULL

#' @aliases show,module-method
#' @docType methods
#' @rdname show-methods
setMethod("show", "module", function(object) {
  print(object)
  return(invisible(NULL))
})
