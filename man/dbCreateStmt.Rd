\name{dbCreateStmt}
\alias{dbCreateStmt}
\title{dbCreateStmt -- create a SQL DDL statement with UNIQUE constraints}
\usage{
  dbCreateStmt(dbObj, name, value, uniq = NULL,
    field.types = NULL)
}
\arguments{
  \item{dbObj}{DBIConnection object}

  \item{name}{table name}

  \item{value}{data.frame to store}

  \item{uniq}{column names for which to build a UNIQUE
  constraint, default=NULL}

  \item{field.types}{use this types. default NULL}
}
\description{
  Internal function. Builds SQL DDL statement. Code taken
  from RSQLite package and slightly adapted.
}

