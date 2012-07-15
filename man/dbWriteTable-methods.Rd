\docType{methods}
\name{dbWriteTable}
\alias{dbWriteTable}
\alias{dbWriteTable,SQLiteDataStore,character,data.frame-method}
\title{dbWriteTable method with update and conversion options.}
\arguments{
  \item{conn}{an SQLiteDataStore object}

  \item{name}{the name of the data table to create}

  \item{value}{the data object}

  \item{savemode}{optional write mode "u" for update
  (default), "w" for replacement of the table}

  \item{uniq}{optional - column names that must be unique
  combinations. Used in "u" savemode.}

  \item{dates}{optional - column names that are to
  interpreted as dates. Converts them back and forth.}

  \item{timestamps}{optional - column names that are
  intepreted as timestamps. Converts them back and forth.}

  \item{field.types}{optional - class list of fields. See
  RSQLite package.}

  \item{verbose}{optional - print diagnostic messages}
}
\description{
  The method is almost identical with its ancestor. The
  only difference is that supports updates instead of
  appends/replacements. It also provides a conversion
  mechanism for temporal attributes.
}

