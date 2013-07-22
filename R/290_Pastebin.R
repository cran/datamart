#' Pastebin
#' 
#' This class exposes partially the Web API to the pastebin service.
#' 
#' @examples
#' getSlots("Pastebin")
#'
#' @name Pastebin-class
#' @rdname Pastebin-class
#' @exportClass Pastebin
setClass(
    Class="Pastebin", 
    representation=representation(
      api_dev_key = "character",
      api_user_key = "character",
      api_user_name= "character",
      curl.handle="CURLHandle"
    ),
    contains=c("Location", "Xdata")
)

#' Constructor for Pastebin Location object
#'
#' see Pastebin class for more information
#'
#' @param api_dev_key    API Dev Key, default getOption("pastebin.api_dev_key")
#' @param api_user_name  API User Name, default getOption("pastebin.api_user_name")
#' @param api_user_password API User password, default getOption("pastebin.api_user_password")
#' @param clss           Class name to initiate, default "Pastebin"
#'
#' @rdname Pastebin-class
#' @export
pastebin <- function(
  api_dev_key = getOption("pastebin.api_dev_key"),
  api_user_name = getOption("pastebin.api_user_name"),
  api_user_password = getOption("pastebin.api_user_password"),
  clss="Pastebin"
) {
  curlHandle = getCurlHandle()
  api_user_key = postForm("http://pastebin.com/api/api_login.php",
    api_dev_key = api_dev_key,
    api_user_name = api_user_name,
    api_user_password = api_user_password,
    curl = curlHandle
  )
  new(clss, api_user_key=api_user_key, api_dev_key=api_dev_key, api_user_name=api_user_name, curl.handle=curlHandle)
}


#' @rdname show-methods
#' @name show
#' @export
#' @docType methods
#' @aliases show show,Pastebin-method
setMethod(
  f="show",
  signature="Pastebin",
  definition=function(object) cat(sprintf("<%s @ Pastebin.com>\n", object@api_user_name))
)

#' @rdname meta-methods
#' @name meta
#' @export
#' @docType methods
#' @aliases meta meta,Pastebin-method
setMethod(
  f="meta",
  signature="Pastebin",
  definition=function(self) {
    ans <- postForm("http://pastebin.com/api/api_post.php",
      api_option="list",
      api_dev_key = self@api_dev_key,
      api_user_key = self@api_user_key,
      api_results_limit = 999,
      curl = self@curl.handle
    )
    ans <- htmlParse(ans)
    ans <- getNodeSet(ans, "//paste")
    ans <- sapply(ans, function(n) c(
      xmlValue(n[["paste_key"]]),
      xmlValue(n[["paste_date"]]),
      xmlValue(n[["paste_title"]]),
      xmlValue(n[["paste_size"]]),
      xmlValue(n[["paste_expire_date"]]),
      xmlValue(n[["paste_private"]]),
      xmlValue(n[["paste_format_long"]]),
      xmlValue(n[["paste_format_short"]]),
      xmlValue(n[["paste_hits"]])
    ))
    ans <- as.data.frame(t(ans), stringsAsFactors=FALSE)
    colnames(ans) <- c("key", "date", "title", "size", "expire_date", "private", "format_long", "format_short", "hits")
    rownames(ans) <- ans[,"key"]
    ans <- ans[,-1]
    ans <- transform(ans, date=as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "GMT")) #maybe off one hour
    return(ans)
  }
)


#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,Pastebin,character-method
setMethod(
  f="query",
  signature=c(self="Pastebin", resource="character"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
      dl <- getURL(paste("http://pastebin.com/raw.php?i=", resource, sep=""))
      return(dl)
      if(resource %in% ls(envir=self@data_env))
        self@data_env[[resource]]
      else
        callNextMethod()
  }
)

