#' Gscholar -- A class for querying Google Scholar
#' 
#' This class uses Google Scholar to determine how many
#' publications contain a given search term in a given time 
#' span (resolution: year). The idea for the class comes from
#' a blog post on librestats, see references.
#' This class is likely to change in future versions.
#' 
#' @references
#' \url{http://librestats.com/2012/04/12/statistical-software-popularity-on-google-scholar/}
#' @name Gscholar-class
#' @rdname Gscholar-class
setClass(
  Class="GScholar", 
  contains="UrlData"
)

#' Constructor for Gscholar objects
#'
#' @param map.lst    A named list that maps the resource to search terms for Google Scholar.
#'
#' @export
gscholar <- function(map.lst) urldata(
  clss="GScholar",
  map.lst=map.lst,
  template="http://scholar.google.com/scholar?hl=en?&num=1&q=%s&btnG=Search&as_sdt=1%%2C43&as_ylo=%g&as_yhi=%g&as_vis=1",
  transform.fct=function(x) {
    if(any(grepl("No pages were found", x))) return(0)
    tmp <- strparse("([0-9,]+) results \\(", x)
    idx <- which(tmp!="")
    if(length(idx)>0) return(as.numeric(gsub(",", "", tmp[idx[[1]]])))
    return(NA)
  }
)

#' For the GScholar class additional required parameters are "from" and "to", integers (years) that 
#' specify the search time span.
#'
#' @rdname query-methods
#' @aliases query,GScholar,character,missing-method
setMethod(
  f="query",
  signature=c(self="GScholar", resource="character", dbconn="missing"),
  definition=function(self, resource, from, to, verbose=getOption("verbose"), ...) {
    mapped <- self@map.fct(self, resource)
    if(is.null(mapped)) stop("invalid 'resource'. Use queries() to get a vector of available resources.")
    thisYear <- as.numeric(strftime(Sys.time(), "%Y"))
    if(missing(to) || !is.numeric(to) || to > thisYear) stop("missing/invalid 'to' parameter.")
    if(missing(from) || !is.numeric(from) || from > to) stop("missing/invalid 'from' parameter,")

    if(from < to) {
      # recursion
      res <- rep(NA, to-from+1)
      names(res) <- from:to
      for(i in from:to) {
        tmp <- query(self, resource=resource, from=i, to=i, verbose=verbose, ...)
        res[[as.character(i)]] <- if(inherits(tmp, "try-error")) NA else tmp
      }
      return(res)
    } else {
      # as inherited, but slightly adapted..
      mapped <- list(mapped=mapped, from=from, to=to)
      uri <- do.call(function(...) sprintf(self@template, ...), mapped) 
      
      if(verbose) cat("Downloading ", uri,"...\n")
      res <- try(self@extract.fct(uri), silent=TRUE)
      if(inherits(res, "try-error")) return(res)
      
      if(verbose) cat("Transforming data ...\n")
      res <- try(self@transform.fct(res), silent=TRUE)
      return(res)
    }
})








