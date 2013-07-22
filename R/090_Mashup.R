#' Mashup -- A class for combining Xdata objects
#'
#' This class administers a list of Xdata objects and 
#' delegates \code{query} and
#' other methods to them. The aim of the class is to 
#' support the operation of combining different data sources
#' into one data object.
#'
#' Instances of this class also maintain a database connection for storing/
#' collecting data. The database connection is shared among the administered Xdata
#' instances.
#'
#' Usually it is not necessary for inherited classes to
#' redefine the \code{queries}
#' method. Rather, it is recommended to add \code{query}
#' methods with new signatures in the resource slot.
#' 
#' @examples
#' getSlots("Mashup")
#'
#' @exportClass Mashup
#' @name Mashup-class
#' @rdname Mashup-class
setClass(
  Class="Mashup", 
  representation=representation(data.lst="list"),
  contains="Xdata"
)

#' Constructor for Mashup objects
#'
#' @param ...   named arguments of Xdata objects
#' @param clss  name of the class to create. Default Mashup, must be inherited from this class.
#'
#' @export
#' @rdname Mashup-class
mashup <- function(..., clss="Mashup") {
    data.lst <- list(...)
    
    # some checks
    if(length(data.lst)==0) stop("Mashup must consists of at least one Xdata object.")
    qrs <- lapply(data.lst, queries)
    qrs.zeros <- which(sapply(qrs, length)==0)
    if(length(qrs.zeros)>0) warning("Data objects with no resources passed at position(s) ", paste(qrs.zeros, collapse=", "))
    qrs <-  unlist(Filter(function(x) !is.null(x), qrs)) # sapply does not simplify in case of NULL
    qrs.dups <- which(duplicated(qrs))
    if(length(qrs.dups)) stop("Duplicated resources: ", paste(qrs[qrs.dups], collapse=", "))
    
    # build object
    new(clss, data.lst=data.lst)
}

#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,Mashup,character-method
setMethod(
  f="query",
  signature=c(self="Mashup", resource="character"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    ## first, look for queries in this class 
    if(verbose) cat("Mashup calls inherited method..\n")
    ret <- try(callNextMethod(), silent=TRUE) 
    if(!inherits(ret, "try-error")) return(ret)
    
    ## then try the mashed data objects 
    if(verbose) cat("Mashup calls query on its components...\n")
    for(d in self@data.lst) {
      ret <- try(query(d, resource=resource, verbose=verbose, ...), silent=TRUE)
      if(!inherits(ret, "try-error")) return(ret)
    }
    stop(sprintf("Invalid resource '%s' specified for mashup data object '%s'", resource, class(self)))
  }
)

  
#' @rdname queries-methods
#' @name queries
#' @export
#' @docType methods
#' @aliases queries queries,Mashup-method
setMethod(
  f="queries",
  signature="Mashup",
  definition=function(self) { 
    qrs <- lapply(self@data.lst, queries)
    qrs <-  unlist(Filter(function(x) !is.null(x), qrs)) # sapply does not simplify in case of NULL

    ret <- c(callNextMethod(), qrs)
    names(ret) <- NULL
    ret <- ret[ret!="character"]
    return(ret)
  }
)
