\docType{methods}
\name{dbReadTable}
\alias{dbReadTable}
\alias{dbReadTable,SQLiteDataStore,character-method}
\alias{dbReadTable,SQLiteDataStore,character,ANY-method}
\title{dbReadTable method with update and conversion options.}
\arguments{
  \item{conn}{an SQLiteDataStore object}

  \item{name}{the name of the data table to create}

  \item{value}{the data object}

  \item{dates}{optional - column names that are to
  interpreted as dates. Converts them back and forth.}

  \item{timestamps}{optional - column names that are
  intepreted as timestamps. Converts them back and forth.}

  \item{verbose}{optional - print diagnostic messages}

  \item{...}{additional arguments, passed to ancestor}
}
\description{
  Counterpart to dbWriteTable. Ensures conversion of dates
  and timestamps.
}

