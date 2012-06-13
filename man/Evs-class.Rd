\name{Evs-class}
\alias{Evs-class}
\title{Example for InternalData: German Expenditure Survey}
\description{
  Shows how to use xdata with local datasets. Only
  read-only access.
}
\examples{
xp <- expenditures()
queries(xp)
query(xp, "Categories")
query(xp, "Elasticity", categ="05")
}
\author{
  Karsten Weinert \email{k.weinert@gmx.net}
}

