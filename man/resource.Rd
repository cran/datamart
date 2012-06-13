\name{resource}
\alias{resource}
\title{Generate Resource classes on the fly}
\usage{
  resource(id, verbose = getOption("verbose"))
}
\arguments{
  \item{id}{character, identifier of the resource}

  \item{verbose}{if TRUE, diagnostic messages. Defaults to
  getOption("verbose").}
}
\description{
  This function takes a character as input. If no class
  with this name exists, the function calls setClass and
  creates one, derived from Resource. The function returns
  its input, i.e. its use lies in its side effect.
}
\details{
  The function is useful for defining \code{query} methods
  for data classes derived from xdata. It enables
  dispatching on the signature, which is the main reason
  for the function.
}
\author{
  Karsten Weinert \email{k.weinert@gmx.net}
}

