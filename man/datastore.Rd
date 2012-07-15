\name{datastore}
\alias{datastore}
\title{datastore -- Connect to local database}
\usage{
  datastore(fn = ":memory:")
}
\arguments{
  \item{fn}{file name of the sqlite database. Default
  :memory:}
}
\description{
  Use this function to connect to a local sqlite database.
  It is essentially a call to \code{dbConnect}.
}

