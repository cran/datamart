\name{UrlData3-class}
\alias{urldata3}
\alias{UrlData3-class}
\title{UrlData3 -- unified access to WWW resources}
\usage{
  urldata3(resource, template, extract.fct = readLines,
    transform.fct = identity, clss = "UrlData3", ...)
}
\arguments{
  \item{resource}{the name of the resource. Required.}

  \item{template}{a pattern for the url. Must contain %s
  for substitution. Required.}

  \item{extract.fct}{a function that takes an URI and
  returns the raw data. Default readLines.}

  \item{transform.fct}{a function that takes the raw data
  and returns the cleaned/transformed data. Default
  identity.}

  \item{clss}{name of the class to create. Default UrlData,
  must be inherited from this class.}

  \item{...}{parameters for the query. Must be named
  arguments, values can be characters (for defaults), NULL,
  or functions.}
}
\description{
  This class provides the infrastructure to scrape the web
  with a Extract, Transform, Load (ETL) approach.

  Constructor for UrlData3 objects
}
\details{
  In most cases, it is not necessary to subclass
  \code{UrlData}. The slots can be set by the
  \code{urldata} function and allow to customize each step
  of the process.
}
\examples{
getSlots("UrlData3")
}

