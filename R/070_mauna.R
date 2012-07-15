#' CO2 data from Mauna Loa
#'
#' Access to monthly and annual mean CO2 concentration.
#'
#' @references 
#' \url{http://www.esrl.noaa.gov/gmd/ccgg/trends/co2_data_mlo.html}
#' \url{http://tamino.wordpress.com/2012/06/07/co2-sanity/}
#' @docType data
#' @return UrlData
#' @export
mauna_loa <- function() urldata(
  template="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_%s_mlo.txt",
  map.lst=list(
    MonthlyMean="mm",
    AnnualMean="annmean",
    AnnualGrowth="gr"
  ),  
  extract.fct=readLines,
  transform.fct=function(x) {
    i <- max(which(x=="#"))
    nm <- x[i+1]
    x <- x[(i+1):length(x)]
    x <- gsub("^[ \t]+", "", x)
    x <- gsub("[ \t]+", ",", x)
    dat <- read.csv(textConnection(x), header=FALSE, comment.char="#", sep=",")
    nm <- gsub("^#[ \t]+", "", nm)
    nm <- gsub("ann inc", "ann.inc", nm)
    nm <- strsplit(nm, "[ \t]+")[[1]]
    if(ncol(dat)>length(nm)) nm <- c(paste("V", 1:(ncol(dat)-length(nm)), sep=""), nm)
    colnames(dat) <- nm
    return(dat)
  }
)
