\name{Mashup-class}
\alias{mashup}
\alias{Mashup-class}
\title{Mashup -- A class for combining Xdata objects}
\usage{
  mashup(..., clss = "Mashup")
}
\arguments{
  \item{...}{named arguments of Xdata objects}

  \item{clss}{name of the class to create. Default Mashup,
  must be inherited from this class.}
}
\description{
  This class administers a list of Xdata objects and
  delegates \code{query} and other methods to them. The aim
  of the class is to support the operation of combining
  different data sources into one data object.

  Constructor for Mashup objects
}
\details{
  Instances of this class also maintain a database
  connection for storing/ collecting data. The database
  connection is shared among the administered Xdata
  instances.

  Usually it is not necessary for inherited classes to
  redefine the \code{queries} method. Rather, it is
  recommended to add \code{query} methods with new
  signatures in the resource slot.
}
\examples{
getSlots("Mashup")
}

