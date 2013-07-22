\name{MdFigure-class}
\alias{mdfigure}
\alias{MdFigure-class}
\title{MdFigure}
\usage{
  mdfigure(name, xdata, resource = name, clss = "MdFigure")
}
\arguments{
  \item{name}{name of the figure, default ''}

  \item{xdata}{call for creating the figure}

  \item{resource}{name of the resource}

  \item{clss}{class name, default 'MdFigure'}
}
\value{
  MdFigure
}
\description{
  Internal Class representing figures in MdReport

  Internal function to create an MdFigure object.
}
\examples{
getSlots("MdReport")
}

