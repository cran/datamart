\name{Twttr-class}
\alias{Twttr-class}
\title{A class for querying twitter}
\description{
  This class allows to query user timelines. Other
  resources have not been implemented yet. The class is
  subject to change in further versions.
}
\examples{
\dontrun{
  tw <- twttr(user="wagezudenken")
  query(tw, "User_timeline")
}
}

