\name{BlogPostTarget-class}
\alias{blogpost}
\alias{BlogPostTarget-class}
\title{A Target representing a blog post}
\usage{
  blogpost(name, content, label = "", draft = TRUE,
    overwrite = TRUE, clss = "BlogPostTarget")
}
\arguments{
  \item{name}{title of the blogpost}

  \item{content}{content of the post}

  \item{label}{character vector of keywords}

  \item{draft}{draft or not? default=TRUE}

  \item{overwrite}{overwrite or not? overwrite=TRUE}

  \item{clss}{class of the object, default
  'BlogPostTarget'}
}
\value{
  BlogPostTarget
}
\description{
  This is an internal class representing a blog post. Use
  MdReport instead.

  For internal use only
}
\examples{
getSlots("BlogPostTarget")
}

