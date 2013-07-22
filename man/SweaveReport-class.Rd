\name{SweaveReport-class}
\alias{SweaveReport-class}
\alias{swvreport}
\title{Wrapper for Sweave and pdf}
\usage{
  swvreport(tpl, name = NULL, clss = "SweaveReport",
    verbose = getOption("verbose"), ...)
}
\arguments{
  \item{tpl}{path to markdown template file}

  \item{name}{name of the Report, default ''}

  \item{verbose}{diagnostic messages T/F}

  \item{clss}{class of the constructed object, default
  'SweaveReport'}

  \item{...}{number of targets}
}
\value{
  generic
}
\description{
  The main S4 class in this framework is
  \code{SweaveReport}. You can create a report with
  \code{swvreport}, which takes a sweave file name, The
  generic method \code{put} can then be used to actually
  produce the report in pdf format.

  see class SweaveReport for details.
}
\examples{
getSlots("SweaveReport")
}
\author{
  Karsten Weinert \email{k.weinert@gmx.net}
}

