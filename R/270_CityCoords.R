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
city_coords <- function(country="DE") urldata3(
  resource="CityCoordinates",
  template="http://nominatim.openstreetmap.org/search?city=$(city)&countrycodes=$(country)&limit=9&format=json",
  country=country,
  city=NA,
  extract.fct=fromJSON,
  transform.fct=function(x) if(is.vector(x)) c(lat=as.numeric(x[[1]]$lat), lon=as.numeric(x[[1]]$lon)) else c(NA, NA)
)
