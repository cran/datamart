#' S4 base class to represent resources
#'
#' This class is used as based class when defining new resources
#' for the \code{query} method. The \code{resource} function takes
#' a character and defines a S4 class derived from Resource. Thus
#' dispatching on the signature is possible, which is the main reason
#' for using this class.
#'
#' Usually, you do not use the class directly. The exception is
#' to use the \code{resource} function in the signature argument
#' of setMethod when defining your own queries. 
#' 
#' @examples
#' getSlots("Resource")
#' 
#' @name Resource-class
#' @rdname Resource-class
#' @exportClass Resource
setClass(Class="Resource", representation=representation())

#' Generate Resource classes on the fly
#'
#' The \code{resource} function takes a character as input. If no class with this
#' name exists, the function calls setClass and creates one, derived
#' from Resource. The function returns its input, i.e. its use lies in
#' its side effect.
#' 
#' @param id    character, identifier of the resource
#' @param verbose  if TRUE, diagnostic messages. Defaults to getOption("verbose").
#' 
#' @rdname Resource-class
#' @export
resource <- function(id, verbose=getOption("verbose")) {
  cl <- try(getClass(id), silent=TRUE)
  if(inherits(cl, "try-error")) {
    if(verbose) message("defining ", id, "\n")
    setClass(Class=id, representation(), contains="Resource", where=parent.frame())
  }
  return(id)
}

#' The as.character for Xdata/Resource
#'
#' The \code{as.character} method has been defined for various classes, such as
#' the \code{Resource} class and the \code{DirectoryLocation} class.
#'
#' @rdname as.character-methods
#' @name as.character
#' @export
#' @docType methods
#' @aliases as.character as.character,Resource-method
setMethod(
  f="as.character",
  signature="Resource",
  definition=function(x) class(x)
)
