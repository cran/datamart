#' A class representing a data source
#' 
#' Most methods of the class are abstract,
#' however the \code{show}, \code{print},
#' \code{queries} methods will usually not
#' need to be redefined.
#'
#' The \code{query} method is defined for
#' character resource arguments. It is tried
#' to transform the argument to an object; if
#' that succeeds, \code{query} is called again.
#' Derived methods that are also interpreting
#' resource as character should first call
#' this method via \code{callNextMethod}.
#'
#' @name Xdata-class
#' @rdname Xdata-class
#' @exportClass Xdata
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(Class="Xdata", representation=representation())

#' Request data from data source
#'
#' This generic function is the main interface to
#' the data behind the xdata layer. The first argument
#' is the data object, the second argument is an identifier
#' of the resource requested. For the end-user this is
#' usually a character, but when building derived
#' classes use resource("your_id") as signature. 
#' 
#' Depending on the data object, additional parameter can be
#' provided.
#'
#' @param self        an Xdata object
#' @param resource    an identifier of the resource requested. End-user usually provide character,
#'                    developer use \code{resource} and dispatch on the type.
#' @param ...         additional parameter
#'
#' @export
#' @docType methods
#' @rdname query-methods
setGeneric(
  name="query",
  def=function(self, resource, ...){standardGeneric("query")}
)

#' @rdname query-methods
#' @aliases query,Xdata,character-method
setMethod(
  f="query",
  signature=c(self="Xdata", resource="character"),
  definition=function(self, resource, ...) {
      r <- try(new(resource), silent=TRUE)
      if(!inherits(r, "try-error"))
        query(self, r, ...)
      else
        stop(sprintf("Invalid resource '%s' specified for data object '%s'", resource, class(self)))
  }
)

#' List resources
#'
#' The \code{queries} method returns a named character vector of all
#' defined resources for the given data object. Note that this
#' method will not detect when there is a query method which
#' accepts other types than from Resource derived.
#'
#' If your class has a query method that works with 
#' character arguments (e.g. an SQL statement), this method
#' will be ignored by \code{queries}.
#'
#' @param self    an Xdata object
#'
#' @export
#' @docType methods
#' @rdname queries-methods
setGeneric(
  name="queries",
  def=function(self){standardGeneric("queries")}
)

#' @param self    an Xdata object
#'
#' @rdname queries-methods
#' @aliases queries,Xdata-method
setMethod(
  f="queries",
  signature="Xdata",
  definition=function(self) {
    proc_one <- function(md) tryCatch(attr(md, "target")[["resource"]], error=function(e) NULL) # md is of class MethodDefinition
    md_list <- findMethods("query", classes=c(class(self), names(getClass(class(self))@contains)))
    res <- sapply(md_list, proc_one)
    res <- res[names(res)!="Xdata#character"]
    return(res)
  }
)

#' Show method for Xdata
#'
#' @param object    an Xdata object
#'
#' @docType methods
#' @name show
#' @rdname show-methods
#' @aliases show,Xdata-method
#' @export
setMethod(
  f="show",
  signature="Xdata",
  definition=function(object) cat(sprintf("<object of class %s>\n", class(object)))
)

