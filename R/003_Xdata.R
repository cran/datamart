#' Xdata -- A class representing a data source
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
setClass(Class="Xdata", representation=representation())
setClass(Class="EmptySet", representation=representation(), contains="Xdata")

#' Request data from data source
#'
#' This generic function is the main interface to
#' the data behind the Xdata layer. The first argument
#' is the data object, the second argument is an identifier,
#' usually a string, of the resource requested.  
#'
#' A optional third parameter is dbconn, a SqliteDataStore object.
#' If provided, it tries to locate the resource in the database first and
#' thus complements the \code{scrape} method.
#'
#' Depending on the data object, additional parameter can be
#' provided.
#'
#' Inherited classes should override the slot with dbconn="missing".
#' The slot resource should be filled with the help of the \code{resource} function.
#' The default (Xdata) implementation with resource as character creates a
#' Resource object of that name and calls itself again with this parameter.
#' Thus dispatching on the resource is possible.
#' 
#' @param self        an Xdata object
#' @param resource    an identifier of the resource requested. End-user usually provide character,
#'                    developer use \code{resource} and dispatch on the type.
#' @param dbconn      an optional SQLiteDataStoreConnection. See details
#' @param ...         additional parameter
#'
#' @export
#' @docType methods
#' @rdname query-methods
setGeneric(
  name="query",
  def=function(self, resource, dbconn, ...){standardGeneric("query")}
)

#' List resources
#'
#' The \code{queries} method returns a character vector of all
#' defined resources for the given data object. 
#'
#' The default (XData) implementation inspects definitions of the
#' \code{query} method. Inherited classes should override this method
#' if necessary.
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

#' Scrape external data source
#'
#' The \code{scrape} method extracts external data and stores it
#' in a SqliteDataStore.
#'
#' By default (Xdata), the method \code{scrapes} is used to
#' determine which resources are "scrapeable" (as opposed to
#' to "refined" resources). For each of the resources,
#' \code{query} is called (with ignore.db=TRUE) and thus the
#' data extraction and transformation is performed. The result
#' of the query is stored in the datastore.
#'
#' Usually the method needs not be overwritten by inherited classes. 
#'
#' @param self    an Xdata object
#' @param dbconn  a SQLiteDataStore object (which is a DBIConnection)
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Data_scraping}
#' @export
#' @docType methods
#' @rdname scrape-methods
setGeneric(
  name="scrape",
  def=function(self, dbconn, ...){standardGeneric("scrape")}
)

#' List scrapes
#'
#' The \code{scrapes} method returns a list of all
#' defined resources for the given data object that are
#' scrapeable i.e. that can be extracted from some external
#' data source. 
#'
#' The names of this list are suitable parameters to the
#' \code{scrape} method. The elements of the list are themselves
#' lists that contain parameters for \code{dbWriteTable}: 
#' savemode ("w" for replacing, "u" for updating), uniq (columns names which may appear only once)
#' dates and timestamps (column names
#' that are converted to character before writing to and after loading from the datastore).
#'
#' By default (XData), \code{scrapes} returns an empty list. Inherited classes should override this method
#' if necessary.
#'
#' @param self    an Xdata object
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/Data_scraping}
#' @export
#' @docType methods
#' @rdname scrapes-methods
setGeneric(
  name="scrapes",
  def=function(self){standardGeneric("scrapes")}
)


#' @rdname query-methods
#' @aliases query,Xdata,character,missing-method
setMethod(
  f="query",
  signature=c(self="Xdata", resource="character", dbconn="missing"),
  definition=function(self, resource, ...) {
      r <- try(new(resource), silent=TRUE)
      if(!inherits(r, "try-error"))
        query(self, r, ...)
      else
        stop(sprintf("Invalid resource '%s' specified for data object '%s'", resource, class(self)))
  }
)

#' @rdname query-methods
#' @aliases query,Xdata,character,SQLiteDataStore-method
setMethod(
  f="query",
  signature=c(self="Xdata", resource="character", dbconn="SQLiteDataStore"),
  definition=function(self, resource, dbconn, ignore.db=FALSE, verbose=getOption("verbose"), ...) {
    search.db <- (!ignore.db && (!is.null(si <- scrapes(self)[[resource]])))
    if(search.db) {
      if(verbose) cat("Loading from local datastore..\n")
      params <- list(conn=dbconn, name=resource, verbose=verbose)
      if(!is.null(si[["dates"]])) params[["dates"]] <- si[["dates"]]
      if(!is.null(si[["timestamps"]])) params[["timestamps"]] <- si[["timestamps"]]
      ret <- try(do.call("dbReadTable", params), silent=!verbose)
      if(!inherits(ret, "try-error")) return(ret)
    }
    ret <- query(self, resource, verbose=verbose, ...) # call without dbconn
    return(ret)
  }
)
  
#' @rdname queries-methods
#' @aliases queries,Xdata-method
setMethod(
  f="queries",
  signature="Xdata",
  definition=function(self) {
    proc_one <- function(md) tryCatch(attr(md, "target")[["resource"]], error=function(e) NULL) # md is of class MethodDefinition
    md_list <- findMethods("query", classes=c(class(self), names(getClass(class(self))@contains)))
    res <- sapply(md_list, proc_one)
    res <- res[res!="character"]
    names(res) <- NULL
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

#' @rdname scrape-methods
#' @aliases scrape,Xdata,DBIConnection-method
setMethod(
  f="scrape",
  signature=c(self="Xdata", dbconn="DBIConnection"),
  definition=function(self, dbconn, resource="", verbose=getOption("verbose"), ...) {
    si <- scrapes(self)
    if(resource=="") resource <- names(si)

    for (s in resource) {
      if(verbose) cat("scraping '", s,"'\n")
      dat <- try(query(self, s, ignore.db=TRUE, ...))
      if(inherits(dat, "try-error")) 
        warning("error scraping '", s, "', skipping.")
      else {
        params <- si[[s]]
        params[["conn"]]=dbconn
        params[["value"]]=dat
        params[["name"]]=s
        params[["verbose"]]=verbose
        if(verbose) cat("saving '", s,"'\n")
        res <- do.call("dbWriteTable", params)
        if(!res) warning("error storing data.")
      }
    }
  }
)

#' @rdname scrapes-methods
#' @aliases scrapes,Xdata-method
setMethod(
  f="scrapes",
  signature=c("Xdata"),
  definition=function(self) list() 
)

