\name{OddsPortal-class}
\alias{oddsportal}
\alias{OddsPortal-class}
\title{Query bet quotes from oddsportal}
\usage{
  oddsportal(clss = "OddsPortal")
}
\arguments{
  \item{clss}{Class name for the object, default
  OddsPortal}
}
\value{
  OddsPortal
}
\description{
  This class implements a "HTML API" to the oddsportal. The
  \code{queries} function returns the available API
  commands (currently only the German premier soccer
  league).

  Constructor for OddsPortal class
}
\details{
  The \code{query} function constructs the URL(s),
  downloads and parses the data, and returns a data.frame
  of the result.
}
\examples{
getSlots("OddsPortal")
}

