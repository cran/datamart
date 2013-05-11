\name{MdReport-class}
\alias{MdReport-class}
\title{Buildable report}
\description{
  The main S4 class in this framework is \code{MdReport}.
  You can create a report with \code{mdreport}, which takes
  a template file name, and a list of variables as
  arguments. The new generic method \code{build}. can then
  be used to actually produce the report in various formats
  (markdown, xhtml).
}
\details{
  \code{strsubst} is a simple templating mechanism inspired
  from Python (PEP-0292). Variables in the template are
  marked by a preceeding dollar sign and get replaced with
  the value of the corresponding variables passed to
  \code{strsubst}.
}
\author{
  Karsten Weinert \email{k.weinert@gmx.net}
}

