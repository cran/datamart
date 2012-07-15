#' A class for querying SPARQL end points
#' 
#' This class allows to run SELECT statement on SPARQL endpoints.
#' The resource parameter is interpreted as SPARQL statement.
#'
#' See \code{Dbpedia} for examples.
#' 
#' @name Xsparql-class
#' @rdname Xsparql-class
#' @exportClass Xsparql
setClass(
  Class="Xsparql", 
  representation=representation(url="character", nspace="character"), 
  contains="Xdata",
  validity=function(object) {
    if((object@nspace != "") && length(object@nspace) %% 2 != 0)
      stop("invalid nspace parameter, character vector of even length or '' expected.")
  }
)

#' Constructor for Xsparql
#'
#' @param url       sparql end point
#' @param nspace        character vector with short name / namespace expansions
#'
#' @return a xsparql object
#' @export
xsparql <- function(url, nspace="") {
  res <- new("Xsparql", url=url, nspace=nspace)
  return(res)
}

#' internal query method for SPARQL end points
#'
#' Internal function, use query(xsparql(), ...) instead.
#'
#' @param url        URL of SPARQL end point
#' @param query      SPARQL statement
#' @param typeconv   if TRUE (default), converts numbers and dates to R types
#' @param verbose    if TRUE, print diagnostic messages. Defaults to getOption("verbose")
#'
#' @return a data.frame object
#' @author see SPARQL package
mySPARQL <- function (url, query, typeconv=TRUE, verbose=getOption("verbose")) {
  if (url=="") stop("missing url")
  if (query=="") stop("missing query")
  if(verbose) cat(paste(url, "?query=", URLencode(query), sep = ""),"\n")
    
  js <- fromJSON(getURL(
          paste(url, "?query=", URLencode(query), sep = ""), 
          httpheader = c(Accept = "application/sparql-results+json")
        )
  )
  if(length(js$results$bindings)==0) return(data.frame())
  attrs <- js$head$vars
  empty_row <- as.data.frame(
      matrix(rep(NA, length(attrs)),
      ncol=length(attrs),
      dimnames=list(NULL, attrs)
      )
  )
  
  one_row <- function(l) {
    row <- empty_row
    one_value <- function(name) {
      if (typeconv && "datatype" %in% names(l[[name]]))  
        if (grepl(".*integer$|.*float$|.*double$|.*int$", l[[name]][["datatype"]]))
          row[[name]] <<- as.numeric(l[[name]][["value"]])
        else if(grepl(".*date$", l[[name]][["datatype"]]))
          row[[name]] <<- as.Date(l[[name]][["value"]])
        else 
          row[[name]] <<- l[[name]][["value"]]
      else
        row[[name]] <<- l[[name]][["value"]]
    }
    sapply(intersect(attrs, names(l)), one_value)
    return(row)
  }
  
  res <- Reduce(rbind, lapply(js$results$bindings, one_row))
  rownames(res) <- NULL
  return(data.frame(res))
}

#' For the Xsparql class the query method relies on code of the authors of the SPARQL package.
#' The resource parameter is interpreted als SPARQL statement, optional parameters are: 
#' maxrows (numeric, default=NULL) for limit the rows to fetch, 
#' interactive (logical, default FALSE) asks for user input before fetching next rows,
#' typeconv (logical, default TRUE) to convert numbers and dates to R types
#'
#' @rdname query-methods
#' @aliases query,Xsparql,character,missing-method
setMethod(
  f="query",
  signature=c(self="Xsparql", resource="character", dbconn="missing"),
  definition=function(self, resource, maxrows=NULL, interactive=FALSE, typeconv=TRUE, verbose=getOption("verbose")) {
    if(verbose) cat("query Xsparql#res=", resource, "\n")
    r <- try(new(resource), silent=TRUE)
    if(!inherits(r, "try-error")) {
      if(verbose) cat("calling query again but with Resource object\n")
      return(query(self, r, maxrows=maxrows, interactive=interactive, typeconv=typeconv, verbose=verbose))
    }
    prefix <- c()
    if(length(self@nspace) %% 2 != 0) 
      for(i in seq(1,length(self@nspace)-1,2)) 
        prefix <- c(prefix, paste("PREFIX ", self@nspace[[i]], ": ", self@nspace[[i+1]], sep=""))
    prefix <- paste(prefix, collapse="\n")
    query <- paste(prefix, resource, sep="")
    if(is.null(maxrows)) {
      d <- mySPARQL(url=self@url, query=query, typeconv=typeconv, verbose=verbose)
    } else {
      offset <- 0
      limit <- maxrows
      d <- NULL
      repeat {
        chunk <- mySPARQL(url=self@url, query=paste(query, "LIMIT", limit, "OFFSET", offset), typeconv=typeconv, verbose=verbose)
        d <- if(is.null(d)) chunk$results else rbind(d,chunk$results)
        offset <- offset + limit
        if(is.null(chunk$results) || nrow(chunk$results) < limit) break
        if(interactive) {
          print(chunk$results)
          input <- readline("hit <c> to continue, <enter> to exit.. ")
          if(input=="") break
        } else if(nrow(d) >= maxrows) break
      }
    }
    return(d)
  }
)

# getinfo.d.sparql <- function(x, tag, param=NULL, maxrows=100, interactive=TRUE) {
  # if(tag=="classes") {
    # if(is.null(param)) 
      # stmt <- "SELECT DISTINCT ?txt, ?class WHERE {?s a ?class . ?class rdfs:label ?txt .}"
    # else
      # stmt <- sprintf("SELECT DISTINCT ?txt, ?class WHERE {?s a ?class . ?class rdfs:label ?txt . ?txt bif:contains '%s'.}", param)
    # return(query(x,stmt, maxrows, interactive))
  # } else if(tag=="properties") {
    # if(is.null(param)) 
      # stmt <- "SELECT DISTINCT ?property WHERE {?s ?property ?o .}"
    # else
      # stmt <- sprintf("SELECT DISTINCT ?property WHERE {?s a %s; ?property ?o .}", param)
    # return(query(x,stmt, maxrows, interactive))  
  # }
# }
