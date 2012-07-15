\name{Evs-class}
\alias{Evs-class}
\alias{evs2008.lvl2}
\title{German Income and Expenditure Survey 2008 on private spendings, differentiated by household type and household income.}
\description{
  German Income and Expenditure Survey 2008 on private
  spendings, differentiated by household type and household
  income.
}
\examples{
xp <- expenditures()
queries(xp)
query(xp, "Categories")
query(xp, "Elasticity", categ="05")
}
\references{
  Statistisches Bundesamt: Wirtschaftsrechnungen.
  Einkommens- und Verbrauchsstichprobe. Einnahmen und
  Ausgaben privater Haushalte. Fachserie 15 Heft 4.
}

