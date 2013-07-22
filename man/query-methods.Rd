\docType{methods}
\name{query}
\alias{query}
\alias{query,Dbpedia,Nuts1-method}
\alias{query,Dbpedia,PlzAgs-method}
\alias{query,Evs,Categories-method}
\alias{query,Evs,Elasticities-method}
\alias{query,Evs,Elasticity-method}
\alias{query,InternalData,character-method}
\alias{query,Mashup,character-method}
\alias{query,Pastebin,character-method}
\alias{query,Twttr,User_timeline-method}
\alias{query,UnitSetManager,Energy-method}
\alias{query,UnitSetManager,Length-method}
\alias{query,UnitSetManager,Mass-method}
\alias{query,UnitSetManager,Temperature-method}
\alias{query,UrlData3,character-method}
\alias{query,Xdata,character-method}
\alias{query,Xsparql,character-method}
\title{Request data from data source}
\arguments{
  \item{self}{an Xdata object}

  \item{resource}{an identifier of the resource requested.
  End-user usually provide character, developer use
  \code{resource} and dispatch on the type.}

  \item{...}{additional parameter}
}
\description{
  This generic function is the main interface to the data
  behind the Xdata layer. The first argument is the data
  object, the second argument is an identifier, usually a
  string, of the resource requested.
}
\details{
  Depending on the data object, additional parameter can be
  provided.

  The slot resource should be filled with the help of the
  \code{resource} function. The default (Xdata)
  implementation with resource as character creates a
  Resource object of that name and calls itself again with
  this parameter. Thus dispatching on the resource is
  possible.
}

