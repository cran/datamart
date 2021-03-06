% Generated by roxygen2 (4.0.2): do not edit by hand
\name{mem.info}
\alias{mem.info}
\title{Information on objects in R environment}
\usage{
mem.info(envir = .GlobalEnv, sortBy = "Size")
}
\arguments{
\item{envir}{the environment to inspect, default is .GlobalEnv}

\item{sortBy}{the result will be decreasingly sorted by this column. Possible values "Type", "Size" (default), "Rows", "Columns"}
}
\value{
data.frame with object information
}
\description{
This function creates a list of objects that are currently in a given R environment (default the global workspace).
Hence it is an extended version of \code{ls}.
}
\author{
Petr Pikal, David Hinds and Dirk Eddelbuettel
}
\references{
\href{http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session}{Stackoverflow}
}

