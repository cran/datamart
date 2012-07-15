\docType{methods}
\name{query}
\alias{query}
\alias{query,Dbpedia,Nuts1,missing-method}
\alias{query,Evs,Categories,missing-method}
\alias{query,Evs,Elasticities,missing-method}
\alias{query,Evs,Elasticity,missing-method}
\alias{query,GScholar,character,missing-method}
\alias{query,InternalData,character,missing-method}
\alias{query,Mashup,character,missing-method}
\alias{query,Mashup,character,SQLiteDataStore-method}
\alias{query,Twttr,User_timeline,missing-method}
\alias{query,UrlData,character,missing-method}
\alias{query,Xdata,character,missing-method}
\alias{query,Xdata,character,SQLiteDataStore-method}
\alias{query,Xsparql,character,missing-method}
\title{Request data from data source}
\arguments{
  \item{self}{an Xdata object}

  \item{resource}{an identifier of the resource requested.
  End-user usually provide character, developer use
  \code{resource} and dispatch on the type.}

  \item{dbconn}{an optional SQLiteDataStoreConnection. See
  details}

  \item{...}{additional parameter}
}
\description{
  This generic function is the main interface to the data
  behind the Xdata layer. The first argument is the data
  object, the second argument is an identifier, usually a
  string, of the resource requested.

  For the Twttr class, the query method provides additional
  optional arguments: user (twitter screenname, default
  self@user), since_id (character, only tweets with newer
  id, default NULL), count (numerical, maximum tweets,
  default NULL)

  For the Xsparql class the query method relies on code of
  the authors of the SPARQL package. The resource parameter
  is interpreted als SPARQL statement, optional parameters
  are: maxrows (numeric, default=NULL) for limit the rows
  to fetch, interactive (logical, default FALSE) asks for
  user input before fetching next rows, typeconv (logical,
  default TRUE) to convert numbers and dates to R types

  For the InternalData class, one resource with the same
  name as the dataset is defined.

  For the Evs class, the query for the Categories resource
  (typical expenses by Coicop2 categories) provides
  additional optional parameters: income (income level,
  default "(all)"), hhtype (household type, default
  "(all)"), relative (if TRUE (default), return
  percentages, otherwise Euro).

  For the Evs class, the query for the Elasticity resource
  (Plots expenditures by income group for a given
  category), an additional parameters categ (character,
  category for which to plot income elasticity), xlab and
  ylab (axis titles) are supported. Other parameters will
  be passed to boxplot.

  For the GScholar class additional required parameters are
  "from" and "to", integers (years) that specify the search
  time span.
}
\details{
  A optional third parameter is dbconn, a SqliteDataStore
  object. If provided, it tries to locate the resource in
  the database first and thus complements the \code{scrape}
  method.

  Depending on the data object, additional parameter can be
  provided.

  Inherited classes should override the slot with
  dbconn="missing". The slot resource should be filled with
  the help of the \code{resource} function. The default
  (Xdata) implementation with resource as character creates
  a Resource object of that name and calls itself again
  with this parameter. Thus dispatching on the resource is
  possible.
}

