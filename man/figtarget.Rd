\name{figtarget}
\alias{figtarget}
\title{Constructor for UnitSetManager objects}
\usage{
  figtarget(name = "", viz.call = viz(resource = name),
    dev = "png", clss = "FigureTarget", ...)
}
\arguments{
  \item{name}{name of the figure, default ''}

  \item{viz.call}{call for creating the figure}

  \item{dev}{character for the device, default 'png'}

  \item{clss}{class name, default 'FigureTarget'}

  \item{...}{additional parameters passed to the device}
}
\value{
  generic
}
\description{
  Internal function to create an UnitSetManager object.
}

