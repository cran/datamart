#' UrlData -- A class for static web pages
#' 
#' 
#' @name UrlData-class
#' @rdname UrlData-class
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
  Class="UrlData", 
  representation=representation(
    template="character", 
    map.lst="list", 
    map.fct="function", 
    extract.fct="function", 
    transform.fct="function",
    scrape.lst="list"
  ),
  contains="Xdata"
)

#' Constructor for UrlData objects
#'
#' @param template   a pattern for the url. Must contain %s for substitution. Required.
#' @param clss  name of the class to create. Default UrlData, must be inherited from this class.
#'
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
urldata <- function(template, map.lst=list(), scrape.lst=list(), map.fct=NULL, extract.fct=readLines, transform.fct=identity, ns="", clss="UrlData") {
    if(is.null(map.fct)) map.fct <- function(self, x) if(length(self@map.lst)>0) self@map.lst[[x]] else x
    new(clss, template=template, map.lst=map.lst, map.fct=map.fct, extract.fct=extract.fct, transform.fct=transform.fct, scrape.lst=scrape.lst, ns=ns)
}

#' Query an UrlData source
#'
#' @param self reference of the UrlData object
#' @param resource a string to use as template
#'
#' @return a data.frame
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#'
setMethod(
  f="query",
  signature=c(self="UrlData", resource="character"),
  definition=function(self, resource, dbconn=NULL, verbose=getOption("verbose"), ...) {
    if(!is.null(dbconn)) {
      if(verbose) cat("Loading from internal datastore..\n")
      si <- scrapes(self)[[resource]]
      params <- list(conn=dbconn, name=paste(self@ns, resource, sep=""), verbose=verbose)
      if(!is.null(si[["dates"]])) params[["dates"]] <- si[["dates"]]
      if(!is.null(si[["timestamps"]])) params[["timestamps"]] <- si[["timestamps"]]
      ret <- do.call("dbReadTable", params)
      return(ret)
    }
    mapped <- self@map.fct(self, resource)
    if(is.null(mapped)) stop("invalid 'resource'. Use queries() to get a vector of available resources.")
    if(!inherits(mapped, "list")) mapped <- list(mapped)
    uri <- do.call(function(...) sprintf(self@template, ...), mapped) # allow for several arguments.
    
    if(verbose) cat("Downloading ", uri,"...\n")
    res <- try(self@extract.fct(uri), silent=TRUE)
    if(inherits(res, "try-error")) return(res)
    
    if(verbose) cat("Transforming data ...\n")
    res <- try(self@transform.fct(res), silent=TRUE)
    return(res)
  }
)

#' @param self    an UrlData object
#'
#' @rdname queries-methods
#' @aliases queries,UrlData-method
setMethod(
  f="queries",
  signature="UrlData",
  definition=function(self) names(self@map.lst)
)

#' Scrapes method for Xdata
#'
#' Which resources should be scraped? Default list().
#'
#' @param object    an Xdata object
#'
#' @docType methods
#' @name scrapes
#' @rdname scrapes-methods
#' @aliases scrapes,Xdata-method
#' @export
setMethod(
  f="scrapes",
  signature=c("UrlData"),
  definition=function(self) self@scrape.lst
)
