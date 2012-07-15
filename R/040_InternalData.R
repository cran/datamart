#' A class for querying data()sets
#' 
#' This class allows to query datasets that can be
#' loaded with data(). Only read-only access.
#' 
#' @name InternalData-class
#' @rdname InternalData-class
#' @exportClass InternalData
setClass(
  Class="InternalData", 
  representation=representation(name="character", package="character", data_env="environment"), 
  contains="Xdata",
  validity=function(object) if(!exists(object@name, object@data_env)) stop("error loading data")
)

#' Constructor for InternalData objects
#'
#' @param name  name of the dataset. Required.
#' @param package name of the package where the dataset is located. Default NULL.
#' @param clss  name of the class to create. Default InternalData, must be inherited from this class.
#'
#' @export
internalData <- function(name, package=NULL, clss="InternalData") {
    e <- new.env()
    do.call("data", list(name=name, package=package, envir=e))
    new(clss, name=name, package=package, data_env=e)
}

#' For the InternalData class, one resource with the same name as the dataset is defined.
#'
#' @rdname query-methods
#' @aliases query,InternalData,character,missing-method
setMethod(
  f="query",
  signature=c(self="InternalData", resource="character", dbconn="missing"),
  definition=function(self, resource, ...) {
    if(resource==self@name)
      self@data_env[[self@name]]
    else
      callNextMethod()
  }
)

