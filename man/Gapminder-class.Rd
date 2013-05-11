\name{Gapminder-class}
\alias{Gapminder-class}
\title{Gapminder data source.}
\description{
  Gapminder describes itself as a "fact tank" that promotes
  a fact based world view. On their website they provide a
  service that allows to create animated charts for various
  indicators, differentiated by country. They also provide
  the underlying datasets for download. This S3 class
  serves as a wrapper for easy access to a subset of these
  data.
}
\details{
  Please note that neither Gapminder nor the package
  developer/maintainer are the data provider, except for a
  few cases. Therefore you will have to go to the source to
  find out the terms of use for the specific indicator.

  This class defines some resources of the Gapminder
  Project. See \code{queries(gapminder())} for a list of
  resources.
}
\examples{
\dontrun{
  gm <- gapminder()
  queries(gm)
  query(gm, "ReligionAndBabies")
}
}

