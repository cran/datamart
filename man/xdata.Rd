\docType{package}
\name{xdata}
\alias{xdata}
\alias{xdata-package}
\title{Common interface to various data sources.}
\description{
  xdata provides several S4 classes to access and cache
  data sources on the internet or elsewhere. Its aim is to
  extent the functionality of the data() function by
  enabling parametrized data requests and offering a data
  update process.
}
\details{
  At the heart of xdata are two new methods: With query()
  you actually use your data source and request some data.
  With scrape() you curate your data, that is, you may
  provide a mechanism to akquire new data or thin out old
  data. A common usage is as follows.

  You create an instance of your data object, e.g. \code{tw
  <- twttr(user='wagezudenken', dbi=sqlite("my.db")} for a
  twitter client. Before you start working, you update your
  local cache via \code{scrape(tw)}. Now you can query your
  data for various time frames e.g. \code{query(tw,
  "User_timeline", from=as.POSIXct("2011-06-01"),
  to=as.POSIXct("2011-12-31"))} Of course, you can define
  additional resources (such as "User_timeline") to mine
  your data. A list of all defined resources is available
  via \code{queries(tw)}.

  The xdata package provides basic infrastructure for the
  data collection, i.e. the generic methods, and some
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
  Karsten W., factbased blogspot.
  \url{http://factbased.blogspot.com/search/label/data-to-info}
}

