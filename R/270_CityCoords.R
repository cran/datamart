#' Longitude and Latitude for Cities
#'
#' Based on Stackoverflow solution by Jochem Donkers.
#'
#' @param country   two-character country code, default 'DE'
#'
#' @return an object of class UrlData
#' @references 
#' \url{http://stackoverflow.com/questions/13905098/how-to-get-the-longitude-and-latitude-coordinates-from-a-city-name-and-country-i}
#' @export
cityCoords <- function(country="DE") urldata(
  template=paste("http://nominatim.openstreetmap.org/search?city=%s&countrycodes=",country,"&limit=9&format=json", sep=""),
  map.fct=function(self, x) gsub(' ','%20',x), #remove space for URLs
  extract.fct=fromJSON,
  transform.fct=function(x) if(is.vector(x)) c(lat=as.numeric(x[[1]]$lat), lon=as.numeric(x[[1]]$lon)) else c(NA, NA)
)
