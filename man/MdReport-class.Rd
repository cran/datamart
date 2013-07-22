\name{MdReport-class}
\alias{mdreport}
\alias{MdReport-class}
\title{Buildable report}
\usage{
  mdreport(tpl, name = "", xdata = NULL, clss = "MdReport",
    verbose = getOption("verbose"), ...)
}
\arguments{
  \item{tpl}{path to markdown template file}

  \item{name}{name of the Report, default ''}

  \item{xdata}{instance of Xdata class}

  \item{verbose}{diagnostic messages T/F}

  \item{clss}{class of the object, default 'MdReport'}

  \item{...}{number of targets}
}
\value{
  generic
}
\description{
  The main S4 class in this framework is \code{MdReport}.
  You can create a report with \code{mdreport}, which takes
  a template file name, and a list of variables as
  arguments. The new generic method \code{put}. can then be
  used to actually produce the report at various locations
  (directory, memory, blogging site).

  see class MdReport for details.
}
\details{
  \code{strsubst} is a simple templating mechanism inspired
  from Python (PEP-0292). Variables in the template are
  marked by a preceeding dollar sign and get replaced with
  the value of the corresponding variables passed to
  \code{strsubst}.
}
\examples{
getSlots("MdReport")
}

