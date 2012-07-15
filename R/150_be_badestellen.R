#' Water quality at bathing spots in Berlin
#'
#' E.coli pro 100 ml, Intestinale Enterokokken pro 100 ml, Sichttiefe in cm
#'
#' @references 
#' \url{http://www.berlin.de/badegewaesser/baden-details/index.php/index/all.json?q=}
#' \url{http://support.berlin.de/wiki/index.php/SimpleSearch_JSON}
#'
#' @docType data
#' @export
be_bathing_water_quality <- function() urldata(
  template="http://www.berlin.de/badegewaesser/baden-details/index.php/index/all.json?q=%s",
  map.lst=list(BathingWaterQuality=""),
  extract.fct=fromJSON,
  transform.fct=function(x) {
    tbl <- x[["index"]]
    nm <- names(tbl[[1]])
    dat <- as.data.frame(matrix(NA, length(tbl), length(nm)))
    colnames(dat) <- nm
    for(i in 1:length(tbl)) dat[i,nm] <- tbl[[i]][nm]
    
    dat$farbe <- gsub("\\.jpg$", "", dat$farbe)
    idx <- grepl("_ka$", dat$farbe)
    dat[idx, "farbe"] <- NA
    dat$farbe <- factor(dat$farbe, levels=c("gruen", "gelb", "rot"))
    
    dat$badestellelink <- NULL
    dat$profillink <- NULL
    
    idx <- grepl("^[<>].*", dat$eco)
    dat[idx, "eco"] <- substring(dat[idx, "eco"], 2)
    dat$eco <- as.numeric(dat$eco)
    
    idx <- grepl("^[<>].*", dat$ente)
    dat[idx, "ente"] <- substring(dat[idx, "ente"], 2)
    dat$ente <- as.numeric(dat$ente)
    
    idx <- grepl("^[<>].*", dat$sicht)
    dat[idx, "sicht"] <- substring(dat[idx, "sicht"], 2)
    dat$sicht <- as.numeric(dat$sicht)

    dat$dat <- strptime(dat$dat, "%Y-%m-%d")
    dat <- transform(dat, algen=FALSE)
    
    return(dat)
  },
  scrape.lst=list(BathingWaterQuality=list(uniq=c("id", "badname", "dat"), saveMode="u", dates="dat"))
)

#' be_bathing_areas -- geocoordinates of bathing areas around Berlin
#'
#' @references 
#' \url{http://www.berlin.de/badegewaesser/baden-details/index.php/index/all.kml?q=}
#' \url{http://code.google.com/intl/de-DE/apis/kml/documentation/kmlreference.html}
#'
#' @docType data
#' @export
be_bathing_areas <- function() urldata(
  template="http://www.berlin.de/badegewaesser/baden-details/index.php/index/all.kml?q=%s",
  map.lst=list(BathingAreas=""),
  extract.fct=getURL,
  transform.fct=function(x) {
   dat <- xmlParse(x)
   
   nodes <- getNodeSet(dat, "//k:Placemark", namespaces=c(k="http://www.opengis.net/kml/2.2"))
   # or: nodes <- getNodeSet(doc, "//*[local-name() = 'Placemark']")
   worker <- function(n) c(xmlValue(n[["name"]]), xmlValue(n[["Point"]][["coordinates"]]))
   dat <- t(sapply(nodes, worker))
   
   coords <- sapply(strsplit(dat[,2], ","), identity)
   lng <- as.numeric(coords[1,])
   lat <- as.numeric(coords[2,])
   
   dat <- data.frame(rss_name=as.character(dat[,1]), lat=lat, lng=lng)
   
   return(dat)
  },
  scrape.lst=list(BathingAreas=list(saveMode="w"))
)





