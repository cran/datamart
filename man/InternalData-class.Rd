\name{InternalData-class}
\alias{internalData}
\alias{InternalData-class}
\title{A class for querying data()sets}
\usage{
  internalData(name, package = NULL, clss = "InternalData")
}
\arguments{
  \item{name}{name of the dataset. Required.}

  \item{package}{name of the package where the dataset is
  located. Default NULL.}

  \item{clss}{name of the class to create. Default
  InternalData, must be inherited from this class.}
}
\description{
  This class allows to query datasets that can be loaded
  with data(). Only read-only access.

  Constructor for InternalData objects
}
\examples{
getSlots("InternalData")
}

