\name{strsubst}
\alias{strsubst}
\title{Named substitution in strings}
\usage{
  strsubst(template, map, verbose = getOption("verbose"))
}
\arguments{
  \item{template}{character with $(VARS)}

  \item{map}{object with [ functionality e.g. a vector.
  Should return values that can be coerced to character}

  \item{verbose}{print debugging messages when TRUE,
  default is getOption("verbose")}
}
\value{
  character
}
\description{
  Simple template mechanism inspired by PEP-0292. Use lists
  or named character vectors (vectors not tested) as a
  mapping for substitution.
}
\details{
  Substitutions are marked by $(NAME).
}
\references{
  \url{http://www.python.org/dev/peps/pep-0292/}
  \url{http://stackoverflow.com/questions/8703398/conditional-gsub-replacement/8703832#8703832}
}

