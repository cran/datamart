\docType{methods}
\name{queries}
\alias{queries}
\alias{queries,InternalData-method}
\alias{queries,Mashup-method}
\alias{queries,UrlData-method}
\alias{queries,Xdata-method}
\title{List resources}
\arguments{
  \item{self}{an Xdata object}
}
\description{
  The \code{queries} method returns a character vector of
  all defined resources for the given data object.
}
\details{
  The default (XData) implementation inspects definitions
  of the \code{query} method. Inherited classes should
  override this method if necessary.
}

