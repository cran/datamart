\name{build_pkg}
\alias{build_pkg}
\title{Build source package}
\usage{
  build_pkg(pkg, no_vignettes = TRUE, src = FALSE)
}
\arguments{
  \item{pkg}{character, subfolder in devlib_path()}

  \item{no_vignettes}{logical, if TRUE no vignettes are
  build}

  \item{src}{logical, if FALSE (default), a binary
  distribution is created, a source distribution otherwise}
}
\value{
  used for its side effect
}
\description{
  Build source package
}

