\name{Twttr-class}
\alias{twttr}
\alias{Twttr-class}
\title{A class for querying twitter}
\usage{
  twttr(user = "")
}
\arguments{
  \item{user}{character, twitter screenname}
}
\description{
  This class allows to query user timelines. Other
  resources have not been implemented yet. The class is
  subject to change in further versions.

  Use twttr() to initialize. If no user parameter are
  provided, then the global options pft.user is used.
}
\examples{
\dontrun{
  tw <- twttr(user="karstengweinert")
  query(tw, "User_timeline")
}
}

