#' Eurostat web object.
#'
#' From Eurostat's webpage (see references):
#' You can download individual datasets or the complete database by using the bulk download facility. 
#' On the bulk download you will find:
#' all information updated twice a day, at 11:00 and 23:00, (at this time the service is usually not available, K.W.)
#' the datasets in tsv (tab separated values) (this is what is used by this R code, K.W.), 
#' dft and sdmx format, which can be easily used to import the data in a tool of your choice,
#' a manual containing all detailed information on the bulkdownload facility,
#' the table of contents that includes the list of the datasets available,
#' the "dictionaries" of all the coding systems used in the datasets.
#'
#' The eurostat_web() function returns a data object with three methods,
#' "EurostatToc" which returns a data.frame of available datasets,
#' "EurostatDicts" which returns a large data.frame of codes,
#' "EurostatData" which returns the dataset for a given code.
#'
#' @return an object of class Mashup
#' @references 
#' \url{http://epp.eurostat.ec.europa.eu/}
#'
#' @examples
#' getSlots("Eurostat")
#' 
#' @name Eurostat-class
#' @rdname Eurostat-class
#' @exportClass Eurostat
setClass(
  Class="Eurostat", 
  contains="Mashup"
)

#' eurostat_data -- creates UrlData object for accessing Eurostat datasets.
#'
#' @rdname Eurostat-class
eurostat_data <- function() urldata3(
  resource="EurostatData",
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?file=data%2F$(code).tsv.gz",
  code=NA, # required
  extract.fct= function(uri) {
    tf <- tempfile()
    content <- getBinaryURL(uri)
    writeBin(content, tf)
    return(tf)
  },
  transform.fct=function(x) {
    res <- read.csv(gzfile(x), na.strings=c(": ", "p"), stringsAsFactor=FALSE, sep="\t")
    unlink(x)
    for(i in 2:ncol(res)) res[,i] <- as.numeric(gsub("( s|,)", "", res[,i]))
    return(res)
  }
)


#' eurostat_dicts -- creates UrlData object for accessing Eurostat "dictionaries", i.e. a description of indicators.
#'
#' @rdname Eurostat-class
eurostat_dicts <- function() urldata3(
  resource="EurostatDicts",
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=dic%2Fall_dic.zip",
  #template="C:\\Users\\weinert\\Desktop\\all_dic.zip",
  extract.fct=function(uri) {
    tf <- tempfile()
    content <- getBinaryURL(uri)
    writeBin(content, tf)
    return(tf)
  },
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
  }
)

#' eurostat_toc -- creates UrlData object for accessing Eurostat "table of contents", i.e. a description of datasets.
#'
#' @rdname Eurostat-class
eurostat_toc <- function() urldata3(
  resource="EurostatToc",
  template="http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?file=table_of_contents.xml",
  #template="C:\\Dokumente und Einstellungen\\TravelMate\\Desktop\\table_of_contents.xml",
  extract.fct = xmlParse,
  transform.fct=function(dat) {
    namespaces <- c(
      nt="urn:eu.europa.ec.eurostat.navtree",
      xsi="http://www.w3.org/2001/XMLSchema-instance" 
    )
    nodes <- getNodeSet(dat, "//nt:leaf[@type='dataset']", namespaces=namespaces)
    dat <- sapply(nodes, function(n) c(xmlValue(n[["code"]]), xmlValue(n[["title"]]), xmlValue(n[["dataStart"]]))) # this gets ignored: , xmlValue(n[["unit"]]))))
    dat <- data.frame(code=dat[1,], title=dat[2,], dataStart=dat[3,], stringsAsFactors=FALSE)
    return(dat)
  }
)

#' @export
#' @rdname Eurostat-class
eurostat_web <- function() mashup(
  dat=eurostat_data(),
  toc=eurostat_toc(),
  dic=eurostat_dicts(),
  clss="Eurostat"
)
