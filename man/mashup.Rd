\name{mashup}
\alias{mashup}
\title{Constructor for Mashup objects}
\usage{
  mashup(..., dbname = ":memory:", read.only = FALSE,
    clss = "Mashup")
}
\arguments{
  \item{...}{named arguments of Xdata objects}

  \item{dbname}{the filename of the database. Defaults to
  ":memory:"}

  \item{read.only}{disable \code{scrape} method. Default
  FALSE.}

  \item{clss}{name of the class to create. Default Mashup,
  must be inherited from this class.}
}
\description{
  Constructor for Mashup objects
}

