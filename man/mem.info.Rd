\name{mem.info}
\alias{mem.info}
\title{Improved list of objects}
\usage{
  mem.info(env = .GlobalEnv, sortBy = "Size")
}
\arguments{
  \item{env}{the environment to inspect, default is
  .GlobalEnv}

  \item{sortBy}{the result will be decreasingly sorted by
  this column. Possible values "Type", "Size" (default),
  "Rows", "Columns"}
}
\value{
  data.frame with object information
}
\description{
  Improved list of objects
}
\author{
  Petr Pikal, David Hinds and Dirk Eddelbuettel
}
\references{
  \url{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}
}

