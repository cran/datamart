#' SourceForge -- query stats for sourceforge projects
#'
#' @param proj  name of the project
#' @param from  when did the project start? Default "2008-01-01".
#'
#' @references
#' \url{http://sourceforge.net/p/forge/documentation/}
#' @export
sourceforge <- function(proj, from="2008-01-01") {
  if(missing(proj)) stop("'proj' parameter missing.")
  now <- as.Date(Sys.time())
  urldata(
    template=paste("http://sourceforge.net/projects/", proj, "/files/stats/json?start_date=%s&end_date=%s", sep=""),
    map.lst=list(
      LastWeek=list(now - 7, now),
      LastMonth=list(now - 30, now),
      YearToDate=list(paste(strftime(now, "%Y"), 1,1, sep="-0"), now),
      AllTime=list(from, now)
    ), 
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


