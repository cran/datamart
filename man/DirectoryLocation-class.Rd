% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{class}
\name{DirectoryLocation-class}
\alias{DirectoryLocation-class}
\alias{dirloc}
\title{Directory location}
\usage{
dirloc(path, clss = "DirectoryLocation")
}
\arguments{
\item{path}{character, pointing to an existing directory. Required.}

\item{clss}{character, optional class name. Default is "DirectoryLocation".}
}
\description{
The show method for the DirectoryLocation class has been adapted to display the path.

The dirloc function creates a DirectoryLocation object.
}
\details{
The as.character method for the DirectoryLocation class returns the path to the directory
it represents.

The meta method for the DirectoryLocation class returns the output of file.info of the folder.
}
\examples{
getSlots("DirectoryLocation")
}
\seealso{
\code{\link{dirloc}}
}

