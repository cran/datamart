\docType{package}
\name{datamart}
\alias{datamart}
\alias{datamart-package}
\title{Common interface to various data sources.}
\description{
  datamart provides several S4 classes to access and cache
  data sources on the internet or elsewhere. Its aim is to
  extent the functionality of the data() function by
  enabling parametrized data requests and offering a data
  update process.
}
\details{
  At the heart of datamart are two new methods: With
  query() you actually use your data source and request
  some data. With scrape() you curate your data, that is,
  you may provide a mechanism to akquire new data.

  The datamart package provides basic infrastructure for
  the data collection, i.e. the generic methods, and some
  examples to prove the concept. The package is inspired by
  the
  \href{https://bitbucket.org/ScraperWiki/scraperwiki}{scraperwiki
  project}, which provides a webbased service for data
  collection. Also inspiring are
  \href{http://reference.wolfram.com/mathematica/ref/CountryData.html}{Mathematica's
  xxxData functions}, which provide in-built parametrizable
  datasets.
}
\references{
  Karsten W.,
  \href{http://factbased.blogspot.com/search/label/datamart}{factbased
  blogspot.}
}

