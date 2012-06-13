#' S4 base class to represent resources
#'
#' This class is used as based class when defining new resources
#' for the \code{query} method. The \code{resource} function takes
#' a character and defines a S4 class derived from Resource. Thus
#' dispatching on the signature is possible, which is the main reason
#' for using this class.
#' 
#' @name Resource-class
#' @rdname Resource-class
#' @exportClass Resource
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(Class="Resource", representation=representation())

#' Generate Resource classes on the fly
#'
#' This function takes a character as input. If no class with this
#' name exists, the function calls setClass and creates one, derived
#' from Resource. The function returns its input, i.e. its use lies in
#' its side effect.
#' 
#' The function is useful for defining \code{query} methods for
#' data classes derived from xdata. It enables dispatching on the
#' signature, which is the main reason for the function.
#' 
#' @param id    character, identifier of the resource
#' @param verbose  if TRUE, diagnostic messages. Defaults to getOption("verbose").
#' 
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
resource <- function(id, verbose=getOption("verbose")) {
  cl <- try(getClass(id), silent=TRUE)
  if(inherits(cl, "try-error")) {
    if(verbose) message("defining ", id, "\n")
    setClass(Class=id, representation(), contains="Resource", where=parent.frame())
  }
  return(id)
}

#' as.character method for Resource
#'
#' This method returns the class of itself.
#' It is just the identifier of the resource.
#'
#' @param a Resource object
#'
#' @docType methods
#' @name as.character
#' @rdname as.character-methods
#' @aliases as.character,Resource-method
#' @export
setMethod(
  f="as.character",
  signature="Resource",
  definition=function(x) class(x)
)
