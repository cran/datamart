#' A class for querying twitter
#' 
#' This class allows to query user timelines.
#' Other resources have not been implemented yet.
#' The class is subject to change in further versions.
#' 
#' @name Twttr-class
#' @rdname Twttr-class
#' @exportClass Twttr
#' @examples
#' \dontrun{
#'   tw <- twttr(user="wagezudenken")
#'   query(tw, "User_timeline")
#' }
setClass(Class="Twttr", representation=representation(user="character"), contains="Xdata")

#' Constructor for Twttr objects
#'
#' Use twttr() to initialize. If no user parameter are provided, then the global options
#' pft.user is used.
#' 
#' @param user  character, twitter screenname
#'
#' @export
twttr <- function(user="") {
  res <- new("Twttr", user=user)
  return(res)
}

#' For the Twttr class, the query method provides additional optional arguments:
#' user (twitter screenname, default self@@user), since_id (character, only tweets with newer id, default NULL),
#' count (numerical, maximum tweets, default NULL)
#'
#' @rdname query-methods
#' @aliases query,Twttr,User_timeline,missing-method
setMethod(
  f="query",
  signature=c(self="Twttr", resource=resource("User_timeline"), dbconn="missing"),
  definition=function(self, resource, user=NULL, since_id=NULL, count=NULL, verbose=getOption("verbose")) {
    if(is.null(user))  user <- self@user
    
    url_str <- paste("http://twitter.com/statuses/user_timeline/", user, ".json", sep="")
    
    opts <- NULL
    if(!is.null(since_id)) opts <- paste("since_id=",since_id, sep="")
    if(!is.null(count)) opts <- c(opts, paste("count=", count, sep=""))
    opts <- paste(opts, collapse="&")
    if(opts!="") url_str <- paste(url_str, "?", opts, sep="")
      
    if(verbose) cat("fetching from '", url_str, "'..\n")
    json_str <- readLines(url_str, encoding="UTF-8")
    json_lst <- fromJSON(json_str)
    if(length(json_lst)==0) return(data.frame())
    
    null.as.na <- function(x) if(is.null(x)) NA else x
    extr_tw <- function(l) c(l[["id_str"]], null.as.na(l[["location"]]), l[["created_at"]], l[["text"]])
    res <- lapply(json_lst, extr_tw)
    res <- Reduce(rbind, res)
    if(length(json_lst)==1)
      res <- data.frame(id_str=res[1], location=res[2], created_at=res[3], text=res[4])
    else {
      res <- as.data.frame(res, stringsAsFactors=FALSE)
      rownames(res) <- NULL
      colnames(res) <- c("id_str", "location", "created_at", "text")
    }
    
    # transform created_at column to date
    lct <- Sys.getlocale("LC_TIME"); 
    on.exit(Sys.setlocale("LC_TIME", lct), add=TRUE)
    Sys.setlocale("LC_TIME", "C")
    time_str <- res[,"created_at"]
    res[,"created_at"] <- as.POSIXct(strptime(
         paste(substr(time_str,5,19), substr(time_str,27,30)), 
         format="%b %d %H:%M:%S %Y"
    ))

    return(res)
  }
)

