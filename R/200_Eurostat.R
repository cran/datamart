#' eurostat_data -- creates UrlData object for accessing Eurostat datasets.
#'
#' @rdname datamart-internal
eurostat_data <- function() urldata(
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?file=data%%2F%s.tsv.gz",
  extract.fct= function(uri) readLines(gzcon(url(uri))),
  transform.fct=function(x) {
    res <- read.csv(textConnection(x), na.strings=c(": ", "p"), stringsAsFactor=FALSE, sep="\t")
    code <- strhead(colnames(res)[[1]], -5)
    code <- strsplit(code, "\\.")[[1]]
    tm <- as.Date(paste(substring(tail(colnames(res),-1),2),1,1,sep="-0"))
    nm <- gsub(",", ".", res[,1])
    res <- res[,2:ncol(res)]
    
    res <- as.numeric(gsub("( s|,)", "", as.matrix(res)))
    res <- matrix(res, nrow=length(tm), ncol=length(nm), byrow=TRUE)
    res <- xts(res, tm)
    names(res) <- nm
    attr(res, "code") <- code
    return(res)
  }
)

#' eurostat_toc -- creates UrlData object for accessing Eurostat "table of contents", i.e. a description of datasets.
#'
#' @rdname datamart-internal
eurostat_toc <- function() urldata(
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?file=table_of_contents.xml",
  #template="C:\\Dokumente und Einstellungen\\TravelMate\\Desktop\\table_of_contents.xml",
  map.lst=list(Toc=""),
  transform.fct=function(x) {
    dat <- xmlParse(x)
    namespaces <- c(
      nt="urn:eu.europa.ec.eurostat.navtree",
      xsi="http://www.w3.org/2001/XMLSchema-instance" 
    )
    nodes <- getNodeSet(dat, "//nt:leaf[@type='dataset']", namespaces=namespaces)
    dat <- sapply(nodes, function(n) c(xmlValue(n[["code"]]), xmlValue(n[["title"]]), xmlValue(n[["dataStart"]]))) # this gets ignored: , xmlValue(n[["unit"]]))))
    dat <- data.frame(code=dat[1,], title=dat[2,], dataStart=dat[3,], stringsAsFactors=FALSE)
    return(dat)
  },
  scrape.lst=list(Toc=list(saveMode="w"))
)

#' eurostat_dicts -- creates UrlData object for accessing Eurostat "dictionaries", i.e. a description of indicators.
#'
#' @rdname datamart-internal
eurostat_dicts <- function() urldata(
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%%2Fall_dic.zip",
  #template="file://C|\\Dokumente und Einstellungen\\TravelMate\\Desktop\\all_dic.zip",
  map.lst=list(Dicts=""),
  extract.fct=function(uri) {p <- file.path(tempdir(), "all_dic.zip"); if(download.file(uri, p)==0) p else ""}, 
  transform.fct=function(x) {
    if(x=="") return(NULL)
    d <- dirname(x)
    fl <- unzip(x, exdir=d)
    fl <- fl[grepl("/en/", fl)]
    dat <- lapply(fl, function(x) {
        res <- read.csv(x, sep="\t", header=FALSE, stringsAsFactors=FALSE)
        colnames(res) <- c("code", "descr")
        res[,"fn"] <- strhead(basename(x),-4)
        return(res)
      }
    )
    dat <- Reduce(rbind, dat) # this is slow
    return(dat)
  },
  scrape.lst=list(Dicts=list(saveMode="w"))
)

#' Eurostat data object.
#'
#' From Eurostat's webpage (see references):
#' You can download individual datasets or the complete database by using the bulk download facility. 
#' On the bulk download you will find:
#' - all information updated twice a day, at 11:00 and 23:00, (at this time the service is usually not available, K.W.)
#' - the datasets in tsv (tab separated values) (this is what is used by this R code, K.W.), 
#'   dft and sdmx format, which can be easily used to import the data in a tool of your choice,
#' - a manual containing all detailed information on the bulkdownload facility,
#' - the table of contents that includes the list of the datasets available,
#' - the "dictionaries" of all the coding systems used in the datasets.
#'
#' @docType data
#' @return an object of class Mashup
#' @references 
#' \url{http://epp.eurostat.ec.europa.eu/}
#' @export
eurostat <- function() mashup(
  dat=eurostat_data(),
  toc=eurostat_toc(),
  dic=eurostat_dicts(),
  dbname=file.path(tempdir(), "eurostat.db")
)
