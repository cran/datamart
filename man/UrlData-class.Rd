\name{UrlData-class}
\alias{UrlData-class}
\title{UrlData -- unified access to WWW resources}
\description{
  This class provides the infrastructure to scrape the web
  with a Extract, Transform, Load (ETL) approach.
}
\details{
  The slots \code{template, map.lst, map.fct} are used to
  map resources to URL addresses. The \code{extract.fct}
  slot downloads the data, the \code{transform.fct} slot
  transforms it. Using the \code{scrape} mechanism
  inherited from Xdata it is possible to store the data in
  a local database. The slot \code{scrape.lst} serves to
  defines the resources and storage parameters.

  In most cases, it is not necessary to subclass
  \code{UrlData}. The slots can be set by the
  \code{urldata} function and allow to customize each step
  of the process.
}

