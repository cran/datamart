#' Buildable target
#' 
#'
#' @name Target-class
#' @rdname Target-class
#' @exportClass Target
setClass(Class="Target", representation=representation(name="character"))

#' Show method for Target
#'
#' @param object    an Target object
#'
#' @docType methods
#' @name show
#' @rdname show-methods
#' @aliases show,Target-method
#' @export
setMethod(
  f="show",
  signature="Target",
  definition=function(object) cat(sprintf("<object of class %s>\n", class(object)))
)

