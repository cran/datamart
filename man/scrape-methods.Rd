\docType{methods}
\name{scrape}
\alias{scrape}
\alias{scrape,Mashup,missing-method}
\alias{scrape,Xdata,DBIConnection-method}
\title{Scrape external data source}
\arguments{
  \item{self}{an Xdata object}

  \item{dbconn}{a SQLiteDataStore object (which is a
  DBIConnection)}
}
\description{
  The \code{scrape} method extracts external data and
  stores it in a SqliteDataStore.
}
\details{
  By default (Xdata), the method \code{scrapes} is used to
  determine which resources are "scrapeable" (as opposed to
  to "refined" resources). For each of the resources,
  \code{query} is called (with ignore.db=TRUE) and thus the
  data extraction and transformation is performed. The
  result of the query is stored in the datastore.

  Usually the method needs not be overwritten by inherited
  classes.
}
\references{
  \url{http://en.wikipedia.org/wiki/Data_scraping}
}

