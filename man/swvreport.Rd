\name{swvreport}
\alias{swvreport}
\title{Constructor for SweaveReport objects}
\usage{
  swvreport(tpl, name = NULL, clss = "SweaveReport",
    verbose = getOption("verbose"), ...)
}
\arguments{
  \item{tpl}{path to markdown template file}

  \item{name}{name of the Report, default ''}

  \item{verbose}{diagnostic messages T/F}

  \item{clss}{class of the constructed object, default
  'SweaveReport'}

  \item{...}{number of targets}
}
\value{
  generic
}
\description{
  see class SweaveReport for details.
}

