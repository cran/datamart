#' Request data from data source
#'
#' This generic function is the main interface to
#' the data behind the Xdata layer. The first argument
#' is the data object, the second argument is an identifier,
#' usually a string, of the resource requested.  
#'
#' Depending on the data object, additional parameter can be
#' provided.
#'
#' The slot resource should be filled with the help of the \code{resource} function.
#' The default (Xdata) implementation with resource as character creates a
#' Resource object of that name and calls itself again with this parameter.
#' Thus dispatching on the resource is possible.
#' 
#' @param self        an Xdata object
#' @param resource    an identifier of the resource requested. End-user usually provide character,
#'                    developer use \code{resource} and dispatch on the type.
#' @param ...         additional parameter
#'
#' @export
#' @docType methods
#' @name query
#' @rdname query-methods
setGeneric(
  name="query",
  def=function(self, resource, ...){standardGeneric("query")}
)
