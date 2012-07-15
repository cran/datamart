\docType{methods}
\name{scrapes}
\alias{scrapes}
\alias{scrapes,Mashup-method}
\alias{scrapes,UrlData-method}
\alias{scrapes,Xdata-method}
\title{List scrapes}
\arguments{
  \item{self}{an Xdata object}
}
\description{
  The \code{scrapes} method returns a list of all defined
  resources for the given data object that are scrapeable
  i.e. that can be extracted from some external data
  source.
}
\details{
  The names of this list are suitable parameters to the
  \code{scrape} method. The elements of the list are
  themselves lists that contain parameters for
  \code{dbWriteTable}: savemode ("w" for replacing, "u" for
  updating), uniq (columns names which may appear only
  once) dates and timestamps (column names that are
  converted to character before writing to and after
  loading from the datastore).

  By default (XData), \code{scrapes} returns an empty list.
  Inherited classes should override this method if
  necessary.
}
\references{
  \url{http://en.wikipedia.org/wiki/Data_scraping}
}

