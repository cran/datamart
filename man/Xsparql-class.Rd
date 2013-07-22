\name{Xsparql-class}
\alias{xsparql}
\alias{Xsparql-class}
\title{A class for querying SPARQL end points}
\usage{
  xsparql(url, nspace = "")
}
\arguments{
  \item{url}{sparql end point}

  \item{nspace}{character vector with short name /
  namespace expansions}
}
\value{
  a xsparql object
}
\description{
  This class allows to run SELECT statement on SPARQL
  endpoints. The resource parameter is interpreted as
  SPARQL statement.

  The function \code{xsparql} constructs a Xsparql object.
}
\details{
  See \code{Dbpedia} for examples.
}
\examples{
getSlots("Xsparql")
}

