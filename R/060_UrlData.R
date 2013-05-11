#' UrlData -- unified access to WWW resources
#' 
#' This class provides the infrastructure to
#' scrape the web with a Extract, Transform, Load (ETL)
#' approach.
#'
#' The slots \code{template, map.lst, map.fct} are used
#' to map resources to URL addresses. The \code{extract.fct} slot
#' downloads the data, the \code{transform.fct} slot transforms it.
#' Using the \code{scrape} mechanism inherited from Xdata it
#' is possible to store the data in a local database. The slot
#' \code{scrape.lst} serves to defines the resources and storage parameters.
#'
#' In most cases, it is not necessary to subclass \code{UrlData}.
#' The slots can be set by the \code{urldata} function and allow
#' to customize each step of the process.
#' 
#' @exportClass UrlData
#' @name UrlData-class
#' @rdname UrlData-class
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
#' @param map.lst    A named list that maps the resource to parameters for the template url. If the template requires more than one
#'                   parameter, use a list. Default is an empty list.
#' @param map.fct    a function that maps the resource to parameters for the template url. Use this parameter if map.lst is not
#'                   flexible enough. The default behaviour if this parameter is not provided is a lookup on map.lst
#' @param scrape.lst a named list (name=resource, value=list of options) that can be stored in a local database. 
#' @param extract.fct a function that takes an URI and returns the raw data. Default readLines.
#' @param transform.fct a function that takes the raw data and returns the cleaned/transformed data. Default identity.
#' @param clss        name of the class to create. Default UrlData, must be inherited from this class.
#'
#' @export
urldata <- function(template, map.lst=list(), scrape.lst=list(), map.fct=NULL, extract.fct=readLines, transform.fct=identity, clss="UrlData") {
    if(is.null(map.fct)) map.fct <- function(self, x) if(length(self@map.lst)>0) self@map.lst[[x]] else x
    new(clss, template=template, map.lst=map.lst, map.fct=map.fct, extract.fct=extract.fct, transform.fct=transform.fct, scrape.lst=scrape.lst)
}

#' @rdname query-methods
#' @aliases query,UrlData,character,missing-method
setMethod(
  f="query",
  signature=c(self="UrlData", resource="character", dbconn="missing"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    ## first, look for queries in this class (and maybe in the datastore, if not ignore.db was passed)
    if(verbose) cat("trying inherited method..\n")
    ret <- try(callNextMethod(), silent=TRUE) 
    if(!inherits(ret, "try-error")) return(ret)
    
    # if that went wrong, query the web
    if(verbose) cat("trying to construct URL..\n")
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

#' @rdname queries-methods
#' @aliases queries,UrlData-method
setMethod(
  f="queries",
  signature="UrlData",
  definition=function(self) { 
    ret <- c(callNextMethod(), names(self@map.lst))
    names(ret) <- NULL
    ret <- ret[ret!="character"]
    return(ret)
  }
)

#' @rdname scrapes-methods
#' @aliases scrapes,UrlData-method
setMethod(
  f="scrapes",
  signature=c("UrlData"),
  definition=function(self) self@scrape.lst
)
