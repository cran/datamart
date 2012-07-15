\docType{methods}
\name{dbConnect}
\alias{dbConnect}
\alias{dbConnect,DataStoreDriver-method}
\alias{dbConnect,SQLiteDataStore-method}
\title{dbConnect method that creates a SQLiteDataStore}
\arguments{
  \item{drv}{an DataStoreDriver object}
}
\description{
  The method is almost identical with its ancestor. The
  only difference is that is returns a SQLiteDataStore
  object instead of a SQLiteConnection.
}

