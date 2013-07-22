\name{siunit}
\alias{siunit}
\title{Generate numeric vector with SI prefixes}
\usage{
  siunit(uname, value = 1, extended = FALSE)
}
\arguments{
  \item{uname}{the name of the unit to define, e.g. "g"}

  \item{value}{scaling factor, see details.}

  \item{extended}{logical (default=FALSE), add not so
  common prefixes?}
}
\value{
  numeric
}
\description{
  Given a basename for the unit, e.g. "m", the function
  returns a named vector of most or all (extended=TRUE)
  prefixes defined by SI, i.e. "km", "Mm", "Gm", ... and
  "mm", "nm", ...
}
\details{
  By default, the prefixes "h", "d", "da" and "c" are not
  generated. Use extended=TRUE to include these.

  Use \code{value} to define a conversion factor between
  the unit without prefix and the base unit of the unitset
  you are defining. \code{value} is how many units of
  \code{uname} form one unit of the base unit. Default is
  1.

  The function is internal.
}
\references{
  \url{http://en.wikipedia.org/wiki/International_System_of_Units}
}

