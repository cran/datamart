\name{data_store_driver}
\alias{data_store_driver}
\title{data_store_driver -- a constructor for DataStoreDriver object.}
\usage{
  data_store_driver(max.con = 200L,
    fetch.default.rec = 500, force.reload = FALSE,
    shared.cache = FALSE)
}
\arguments{
  \item{max.con}{default 200L}

  \item{fetch.default.rec}{default 500}

  \item{force.reload}{default FALSE}

  \item{shared.cache}{default FALSE}
}
\description{
  Internal function. Basically, a call to sqliteInitDriver,
  but returns a DataStoreDriver instead a SQLiteDriver
  object.
}

