\name{Evs-class}
\alias{Evs-class}
\alias{evs2008.lvl2}
\alias{expenditures}
\title{German Income and Expenditure Survey 2008 on private spendings, differentiated by household type and household income.}
\usage{
  expenditures()
}
\value{
  an object of class "Evs"
}
\description{
  Germany's Sample survey of income and expenditure
  (Einkommens- und Verbrauchsstichprobe, EVS) is conducted
  by the Federal Statistical Office. The data provided here
  is processed and is not identical with der Federal
  Office' data. Some information is lost by the processing.
  If you want more and/or more accurate data, contact the
  Federal Statistical Office.

  Constructor for Evs data source class
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

