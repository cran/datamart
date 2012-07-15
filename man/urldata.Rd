\name{urldata}
\alias{urldata}
\title{Constructor for UrlData objects}
\usage{
  urldata(template, map.lst = list(), scrape.lst = list(),
    map.fct = NULL, extract.fct = readLines,
    transform.fct = identity, clss = "UrlData")
}
\arguments{
  \item{template}{a pattern for the url. Must contain %s
  for substitution. Required.}

  \item{map.lst}{A named list that maps the resource to
  parameters for the template url. If the template requires
  more than one parameter, use a list. Default is an empty
  list.}

  \item{map.fct}{a function that maps the resource to
  parameters for the template url. Use this parameter if
  map.lst is not flexible enough. The default behaviour if
  this parameter is not provided is a lookup on map.lst}

  \item{scrape.lst}{a named list (name=resource, value=list
  of options) that can be stored in a local database.}

  \item{extract.fct}{a function that takes an URI and
  returns the raw data. Default readLines.}

  \item{transform.fct}{a function that takes the raw data
  and returns the cleaned/transformed data. Default
  identity.}

  \item{clss}{name of the class to create. Default UrlData,
  must be inherited from this class.}
}
\description{
  Constructor for UrlData objects
}

