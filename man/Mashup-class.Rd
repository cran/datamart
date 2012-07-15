\name{Mashup-class}
\alias{Mashup-class}
\title{Mashup -- A class for combining Xdata objects}
\description{
  This class administers a list of Xdata objects and
  delegates \code{query} and \code{scrape} and other
  methods to them. The aim of the class is to support the
  operation of combining different data sources into one
  data object.
}
\details{
  Instances of this class also maintain a database
  connection for storing/ collecting data. The database
  connection is shared among the administered Xdata
  instances.

  It is possible to suppress \code{scrape} operation by
  setting the slot \code{read.only=TRUE}. This is useful if
  the database is part of a package and is not to be
  altered (except for package updates).

  Usually it is not necessary for inherited classes to
  redefine the \code{queries, scrape} or \code{scrapes}
  methods. Rather, it is recommended to add \code{query}
  methods with new signatures in the resource slot.
}

