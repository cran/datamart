\name{FileTarget-class}
\alias{filetarget}
\alias{FileTarget-class}
\title{FileTarget}
\usage{
  filetarget(name, filename, clss = "FileTarget")
}
\arguments{
  \item{name}{name of the Report, default ''}

  \item{filename}{name of original file}

  \item{clss}{class name, default 'FileTarget'}
}
\value{
  generic
}
\description{
  This class is a decorator for a physical existing file. A
  common workflow for other targets is to create a
  temporary file and then call put again with a FileTarget.

  see class FileTarget for details.
}
\examples{
getSlots("FileTarget")
}

