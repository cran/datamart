\docType{methods}
\name{uconv}
\alias{uconv}
\alias{uconv,numeric,character,character,character-method}
\alias{uconv,numeric,character,character,missing-method}
\title{convert between numerical units}
\arguments{
  \item{x}{numerical vector}

  \item{from}{character, unit to convert from. Default
  'Unit(x)'}

  \item{to}{character, unit to convert to}

  \item{uset}{optional, character, unit set to use.}
}
\description{
  This function converts between numerical units. It works
  similar to the \code{iconv} function: You provide vector
  \code{x} and a \code{from} and a \code{to} unit name and
  the function converts. If you omit the \code{from}
  parameter, it is tried to lookup the current unit name by
  \code{Unit(x)}.
}
\details{
  Additionally, you may provide a unitset name. Here, the
  analogy to \code{iconv} ceases. Think of unitset as a
  dimension of units, or a context for units. Predefined
  unitsets are "Length", "Mass", "Energy", which are
  dimensional unitsets and "GaseousFuel", "CrudeOil", which
  provide the right conversion factors for the given
  contexts. It is recommened to provide the unitset name.

  A list of available unitsets and the units defined by
  them can be obtained with \code{uconvlist()}. You may add
  your own unitset(s) using \code{add_unitsets}.
}
\examples{
uconv(1, "horse length", "m")
uconv(1:10, "t LNG", "GJ", "GaseousFuel")
}

