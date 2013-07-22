\name{mySPARQL}
\alias{mySPARQL}
\title{Internal query method for SPARQL end points}
\usage{
  mySPARQL(url, query, typeconv = TRUE,
    verbose = getOption("verbose"))
}
\arguments{
  \item{url}{URL of SPARQL end point}

  \item{query}{SPARQL statement}

  \item{typeconv}{if TRUE (default), converts numbers and
  dates to R types}

  \item{verbose}{if TRUE, print diagnostic messages.
  Defaults to getOption("verbose")}
}
\value{
  data.frame object
}
\description{
  Internal function, use query(xsparql(), ...) instead.
}
\author{
  see SPARQL package
}

