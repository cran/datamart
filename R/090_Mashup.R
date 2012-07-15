#' Mashup -- A class for combining Xdata objects
#'
#' This class administers a list of Xdata objects and 
#' delegates \code{query} and \code{scrape} and
#' other methods to them. The aim of the class is to 
#' support the operation of combining different data sources
#' into one data object.
#'
#' Instances of this class also maintain a database connection for storing/
#' collecting data. The database connection is shared among the administered Xdata
#' instances.
#'
#' It is possible to suppress \code{scrape} operation by setting the slot \code{read.only=TRUE}.
#' This is useful if the database is part of a package and is not to be altered (except for
#' package updates).
#'
#' Usually it is not necessary for inherited classes to
#' redefine the \code{queries, scrape} or \code{scrapes}
#' methods. Rather, it is recommended to add \code{query}
#' methods with new signatures in the resource slot.
#'
#' @exportClass Mashup
#' @name Mashup-class
#' @rdname Mashup-class
setClass(
  Class="Mashup", 
  representation=representation(data.lst="list", res.vec="character", dbname="character", dbconn="SQLiteDataStore", read.only="logical"),
  contains="Xdata"
)

#' Constructor for Mashup objects
#'
#' @param ...   named arguments of Xdata objects
#' @param dbname the filename of the database. Defaults to ":memory:"
#' @param read.only disable \code{scrape} method. Default FALSE.
#' @param clss  name of the class to create. Default Mashup, must be inherited from this class.
#'
#' @export
mashup <- function(..., dbname=":memory:", read.only=FALSE, clss="Mashup") {
    data.lst <- list(...)
    
    # some checks
    if(length(data.lst)==0) stop("mashup must consists of at least one Xdata object.")
    idx <- which(is.null(names(data.lst)))
    if(length(idx) > 0) stop("named arguments required.")
    
    # build res.vec
    qrs <- lapply(data.lst, queries)
    qrs <-  unlist(Filter(function(x) !is.null(x), qrs)) # sapply does not simplify in case of NULL
    idx <- which(table(qrs)>1)
    if(length(idx)>0) stop("duplicate resources defined: '", names(idx), "'.")
    res.vec <- names(qrs)
    names(res.vec) <- qrs

    # build dbconn and object
    dbconn <- datastore(dbname)
    new(clss, data.lst=data.lst, res.vec=res.vec, dbname=dbname, dbconn=dbconn, read.only=read.only)
}

#' @rdname query-methods
#' @aliases query,Mashup,character,SQLiteDataStore-method
setMethod(
  f="query",
  signature=c(self="Mashup", resource="character", dbconn="SQLiteDataStore"),
  definition=function(self, resource, dbconn, verbose=getOption("verbose"), ...) {
    ## first, look for queries in this class (and maybe in the datastore, if not ignore.db was passed)
    if(verbose) cat("Mashup calls inherited method..\n")
    ret <- try(callNextMethod(), silent=TRUE) 
    if(!inherits(ret, "try-error")) return(ret)
    
    ## then try the mashed data objects 
    if(verbose) cat("Mashup calls query on its Xdata elements in the data.lst slot..\n")
    src <- try(self@res.vec[[resource]], silent=TRUE)
    if(inherits(src, "try-error")) stop("Invalid resource '", resource, "'.")
    ret <- query(self@data.lst[[src]], resource=resource, dbconn=dbconn, verbose=verbose, ...) 
    
    return(ret)  
  }
)

#' @rdname query-methods
#' @aliases query,Mashup,character,missing-method
setMethod(
  f="query",
  signature=c(self="Mashup", resource="character", dbconn="missing"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) 
    query(
      self=self, 
      resource=resource,
      dbconn=self@dbconn,
      verbose=verbose,
      ...
    )
)
  
#' @rdname queries-methods
#' @aliases queries,Mashup-method
setMethod(
  f="queries",
  signature="Mashup",
  definition=function(self) { 
    ret <- c(callNextMethod(), names(self@res.vec))
    names(ret) <- NULL
    ret <- ret[ret!="character"]
    return(ret)
  }
)

#' @rdname scrapes-methods
#' @aliases scrapes,Mashup-method
setMethod(
  f="scrapes",
  signature=c("Mashup"),
  definition=function(self) {
    res <- list()
    if(self@read.only) return(res)
    for (d in self@data.lst) {
      add <- scrapes(d)
      for (r in names(add)) res[[r]] <- add[[r]]
    }
    return(res)
  }
)

#' @rdname scrape-methods
#' @aliases scrape,Mashup,missing-method
setMethod(
  f="scrape",
  signature=c(self="Mashup", dbconn="missing"),
  definition=function(self, resource="", verbose=getOption("verbose"), ...) scrape(self, dbconn=self@dbconn, resource=resource, verbose=verbose, ...)
)
