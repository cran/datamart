\name{Dbpedia-class}
\alias{dbpedia}
\alias{Dbpedia-class}
\title{A class for querying Dbpedia.org}
\usage{
  dbpedia(lang = "")
}
\arguments{
  \item{lang}{two-character language code for the dbpedia,
  default ''}
}
\value{
  a Dbpedia object, inherited from Xsparql
}
\description{
  This class defines some resources at dbpedia. See
  \code{queries(dbpedia())} for a list of resources.

  Constructor for Dbpedia objects
}
\examples{
\dontrun{
  dbp <- dbpedia()
  queries(dbpedia)
  query(dbp, "Nuts1")
}
}

