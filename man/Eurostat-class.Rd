\name{Eurostat-class}
\alias{Eurostat-class}
\alias{eurostat_data}
\alias{eurostat_dicts}
\alias{eurostat_toc}
\alias{eurostat_web}
\title{Eurostat web object.}
\usage{
  eurostat_data()

  eurostat_dicts()

  eurostat_toc()

  eurostat_web()
}
\value{
  an object of class Mashup
}
\description{
  From Eurostat's webpage (see references): You can
  download individual datasets or the complete database by
  using the bulk download facility. On the bulk download
  you will find: all information updated twice a day, at
  11:00 and 23:00, (at this time the service is usually not
  available, K.W.) the datasets in tsv (tab separated
  values) (this is what is used by this R code, K.W.), dft
  and sdmx format, which can be easily used to import the
  data in a tool of your choice, a manual containing all
  detailed information on the bulkdownload facility, the
  table of contents that includes the list of the datasets
  available, the "dictionaries" of all the coding systems
  used in the datasets.

  eurostat_data -- creates UrlData object for accessing
  Eurostat datasets.

  eurostat_dicts -- creates UrlData object for accessing
  Eurostat "dictionaries", i.e. a description of
  indicators.

  eurostat_toc -- creates UrlData object for accessing
  Eurostat "table of contents", i.e. a description of
  datasets.
}
\details{
  The eurostat_web() function returns a data object with
  three methods, "EurostatToc" which returns a data.frame
  of available datasets, "EurostatDicts" which returns a
  large data.frame of codes, "EurostatData" which returns
  the dataset for a given code.
}
\examples{
getSlots("Eurostat")
}
\references{
  \url{http://epp.eurostat.ec.europa.eu/}
}

