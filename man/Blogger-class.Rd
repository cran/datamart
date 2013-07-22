\name{Blogger-class}
\alias{blogger}
\alias{Blogger-class}
\title{Location Class for Google's Blogger service}
\usage{
  blogger(blogtitle = getOption("blogger.blog"),
    email = getOption("blogger.username"),
    password = getOption("blogger.password"),
    clss = "Blogger")
}
\arguments{
  \item{blogtitle}{name of the (existing) blog. Defaults to
  getOption("blogger.blog").}

  \item{email}{email adress for authentication. Defaults to
  getOption("blogger.username").}

  \item{password}{password for authentication. Defaults to
  getOption("blogger.password").}

  \item{clss}{name of the class for convenient inheritance.
  Defaults to "Blogger".}
}
\value{
  Blogger
}
\description{
  This class implements a small subset of the blogger API.
  The authentication information (ClientLogin, no OAuth
  yet) and the title of the blog are passed to the
  constructor \code{blogger}. The \code{meta} method
  provides information on the submitted blogposts. The
  \code{put} method accepts a \code{BlogPostTarget} that
  can be transfered to Blogger.

  Instantiates an object and authenticates with google.
}
\examples{
getSlots("Blogger")
}

