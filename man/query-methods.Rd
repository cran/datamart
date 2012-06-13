\docType{methods}
\name{query}
\alias{query}
\alias{query,Dbpedia,Nuts1-method}
\alias{query,Evs,Categories-method}
\alias{query,Evs,Elasticities-method}
\alias{query,Evs,Elasticity-method}
\alias{query,InternalData,Raw-method}
\alias{query,Twttr,User_timeline-method}
\alias{query,Xdata,character-method}
\alias{query,Xsparql,character-method}
\title{Request data from data source}
\arguments{
  \item{self}{an Xdata object}

  \item{resource}{an identifier of the resource requested.
  End-user usually provide character, developer use
  \code{resource} and dispatch on the type.}

  \item{...}{additional parameter}

  \item{self}{a Twttr object}

  \item{resource}{an object of class User_timeline, usually
  created by query.xdata}

  \item{user}{twitter screenname, default self@user}

  \item{since_id}{character, only tweets with newer id,
  default NULL}

  \item{count}{numerical, maximum tweets, default NULL}

  \item{verbose}{logical, diagnostic messages? Default is
  getOption("verbose")}

  \item{self}{xsparql object}

  \item{resource}{SPARQL statement}

  \item{opts}{optional parameters as a list, see details}

  \item{maxrows}{optional (numeric, default=NULL) for limit
  the rows to fetch}

  \item{interactive}{optional (logical, default FALSE) asks
  for user input before fetching next rows}

  \item{typeconv}{optional (logical, default TRUE) to
  convert numbers and dates to R types}

  \item{self}{data object}

  \item{resource}{character describing the resource
  requested}

  \item{verbose}{if TRUE, diagnostic messages (default
  getOption("verbose"))}

  \item{self}{a InternalData object}

  \item{resource}{an object of class Raw, usually created
  by query.xdata}

  \item{...}{additional arguments, ignored.}

  \item{self}{data object}

  \item{resource}{\code{Categories} object identifying the
  resource requested}

  \item{income}{income level, default "(all)"}

  \item{hhtype}{household type, default "(all)"}

  \item{relative}{if TRUE (default), return percentages,
  otherwise Euro}

  \item{...}{other parameters, ignored}

  \item{self}{data object}

  \item{resource}{\code{Elasticity} object identifying the
  resource requested}

  \item{categ}{category for which to plot income
  elasticity.}

  \item{...}{other parameters, passed to beeswarm}

  \item{self}{data object}

  \item{resource}{\code{Elasticities} object identifying
  the resource requested}

  \item{...}{other parameters, passed to beeswarm}
}
\value{
  a data.frame object

  a data.frame object

  vector

  vector

  vector
}
\description{
  This generic function is the main interface to the data
  behind the xdata layer. The first argument is the data
  object, the second argument is an identifier of the
  resource requested. For the end-user this is usually a
  character, but when building derived classes use
  resource("your_id") as signature.

  Query user timeline

  Query method for SPARQL end points

  Internal function, use query(dbpedia(), "Nuts1", ...)
  instead.

  Get the Raw data.

  Returns typical expenses by Coicop2 categories.
  Specification of income group and household type is
  possible. Return value either in percent (relative=TRUE,
  default) or in Euro.

  Plots expenditures by income group for a given category.

  Plots expenditures by income group for two categories.
}
\details{
  Depending on the data object, additional parameter can be
  provided.
}
\author{
  see SPARQL package
}

