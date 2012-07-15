#' Tour de France data
#'
#' Dataset collected by Martin Theusrus.
#'
#' @return an object of class UrlData
#' @references 
#' \url{http://www.theusrus.de/blog/category/sports/tour-de-france/}
#' @export
#' @docType data
tourdefrance <- function() urldata(
  template="http://www.theusrus.de/Blog-files/TDF%s.txt",
  map.lst=list("2010"="2010", "2011"="2011", "2012"="2012"),
  extract.fct=function(uri) readLines(uri, encoding="utf-8"), #encoding does not work
  transform.fct=function(x) read.csv(textConnection(x), header=TRUE, sep="\t")
)


