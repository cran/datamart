% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{class}
\name{Dbpedia-class}
\alias{Dbpedia-class}
\alias{dbpedia}
\title{A class for querying Dbpedia.org}
\usage{
dbpedia(lang = "")
}
\arguments{
\item{lang}{two-character language code for the dbpedia, default ''}
}
\value{
a Dbpedia object, inherited from Xsparql
}
\description{
This class defines some resources at dbpedia.
See \code{queries(dbpedia())} for a list of resources.

Constructor for Dbpedia objects
}
\examples{
\dontrun{
  dbp <- dbpedia()
  queries(dbpedia)
  query(dbp, "Nuts1")
}
}
\seealso{
\code{\link{dbpedia}}, \code{\link{xsparql}}
}

