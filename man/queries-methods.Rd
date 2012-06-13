\docType{methods}
\name{queries}
\alias{queries}
\alias{queries,Xdata-method}
\title{List resources}
\arguments{
  \item{self}{an Xdata object}

  \item{self}{an Xdata object}
}
\description{
  The \code{queries} method returns a named character
  vector of all defined resources for the given data
  object. Note that this method will not detect when there
  is a query method which accepts other types than from
  Resource derived.
}
\details{
  If your class has a query method that works with
  character arguments (e.g. an SQL statement), this method
  will be ignored by \code{queries}.
}

