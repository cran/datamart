\name{Resource-class}
\alias{resource}
\alias{Resource-class}
\title{S4 base class to represent resources}
\usage{
  resource(id, verbose = getOption("verbose"))
}
\arguments{
  \item{id}{character, identifier of the resource}

  \item{verbose}{if TRUE, diagnostic messages. Defaults to
  getOption("verbose").}
}
\description{
  This class is used as based class when defining new
  resources for the \code{query} method. The
  \code{resource} function takes a character and defines a
  S4 class derived from Resource. Thus dispatching on the
  signature is possible, which is the main reason for using
  this class.

  The \code{resource} function takes a character as input.
  If no class with this name exists, the function calls
  setClass and creates one, derived from Resource. The
  function returns its input, i.e. its use lies in its side
  effect.
}
\details{
  Usually, you do not use the class directly. The exception
  is to use the \code{resource} function in the signature
  argument of setMethod when defining your own queries.
}
\examples{
getSlots("Resource")
}

