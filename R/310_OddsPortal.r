#' Query bet quotes from oddsportal
#' 
#' This class implements a "HTML API" to the oddsportal. 
#' The \code{queries} function returns the available API commands (currently only
#' the German premier soccer league).
#' 
#' The \code{query} function constructs the URL(s), downloads and parses the data,
#' and returns a data.frame of the result.
#' 
#' @examples
#' getSlots("OddsPortal")
#'
#' @name OddsPortal-class
#' @rdname OddsPortal-class
#' @exportClass OddsPortal
setClass(
    Class="OddsPortal", 
    contains=c("UrlData3")
)


#' Constructor for OddsPortal class
#'
#' @param clss Class name for the object, default OddsPortal
#'
#' @return OddsPortal
#' @rdname OddsPortal-class
#' @export
oddsportal <- function(clss="OddsPortal") urldata3(
  resource="SoccerGermanyFirstBundesliga",
  clss=clss,
  template="http://www.oddsportal.com/soccer/germany/bundesliga-$(saison)/results/page/$(page)/",
  saison=function(x=NA) gsub("/", "-", x), # required
  page=NA, # required
  extract.fct=getURI,
  transform.fct=function(x) {
      nodes <- getNodeSet(htmlParse(x), '//tr[@class=" deactivate" or @class="odd deactivate"]')
      #as.POSIXct("1970-01-01")+1282329000
      dat <- as.data.frame(t(sapply(nodes, function(n) c(
        timestamp=xmlAttrs(n[[1]], TRUE, TRUE)[["class"]], 
        players=xmlValue(n[[2]]),
        result=xmlValue(n[[3]]),
        q1=xmlAttrs(n[[4]], TRUE, TRUE)[["xodd"]],
        q2=xmlAttrs(n[[5]], TRUE, TRUE)[["xodd"]],
        q3=xmlAttrs(n[[6]], TRUE, TRUE)[["xodd"]],
        bmakers=xmlValue(n[[7]])
      ))), stringsAsFactors=FALSE)
      decode_quotes <- function(x) {
          res <- gsub("^[^f]*f", "", x)
          res <- gsub("a", "1", res, fixed=TRUE)
          res <- gsub("x", "2", res, fixed=TRUE)
          res <- gsub("c", "3", res, fixed=TRUE)
          res <- gsub("t", "4", res, fixed=TRUE)
          res <- gsub("e", "5", res, fixed=TRUE)
          res <- gsub("o", "6", res, fixed=TRUE)
          res <- gsub("p", "7", res, fixed=TRUE)
          res <- gsub("z", ".", res, fixed=TRUE)
          return(as.numeric(res))
      }
      dat$timestamp <- as.Date(as.numeric(substr(dat$timestamp,19,28))+as.POSIXct("1970-01-01"))
      dat$q1 <- decode_quotes(dat$q1)
      dat$q2 <- decode_quotes(dat$q2)
      dat$q3 <- decode_quotes(dat$q3)
      dat$bmakers <- as.numeric(dat$bmakers)
      goals <- Reduce(rbind,strsplit(dat$result, ":"), c())
      teams <- Reduce(rbind,strsplit(dat$players, " - "), c())
      dat$g1 <- goals[,1]
      dat$g2 <- goals[,2]
      dat$t1 <- teams[,1]
      dat$t2 <- teams[,2]
      dat$players <- NULL
      dat$result <- NULL
      return(dat)
  }
)

