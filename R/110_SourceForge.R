#' SourceForge -- query stats for sourceforge projects
#'
#' @param proj  name of the project
#' @param from  when did the project start? Default "2008-01-01".
#' @param clss  which clss to instantiate, default "UrlData3"
#'
#' @references
#' \url{http://sourceforge.net/p/forge/documentation/}
#' @export
sourceforge <- function(proj, from="2008-01-01", clss="UrlData3") {
  if(missing(proj)) stop("'proj' parameter missing.")
  urldata3(
    clss=clss,
    resource="SourceforgeStats",
    template=paste("http://sourceforge.net/projects/", proj, "/files/stats/json?start_date=$(from)&end_date=$(to)", sep=""),
    from=function(x=from) strftime(as.Date(x), "%Y-%m-%d"),
    to=function(x=NULL) if(is.null(x)) strftime(Sys.time(), "%Y-%m-%d") else strftime(as.Date(x), "%Y-%m-%d"),
    extract.fct=fromJSON, 
    transform.fct=function(x) {
      tbl <- x[["downloads"]]
      nm <- c("Timestamp", "Downloads")
      dat <- as.data.frame(matrix(NA, length(tbl), length(nm)))
      colnames(dat) <- nm
      for(i in 1:length(tbl)) dat[i,] <- tbl[[i]][c(1,2)]
      dat$Timestamp <- as.Date(dat$Timestamp)
      return(dat)
    }
  )
}


