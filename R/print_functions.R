#' @include module_class.R
# Print functions should return the argument 'x' invisibly.
NULL

#' @aliases print,module-method
#' @docType methods
#' @rdname print-methods
setMethod("print", "module", function(x) {
  cat(sprintf("Module ID: %s\n", x@module_id))
  return(invisible(x))
})
