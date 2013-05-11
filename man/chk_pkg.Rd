\name{chk_pkg}
\alias{chk_pkg}
\title{Checks a dev package}
\usage{
  chk_pkg(pkg, no_codoc = FALSE, no_examples = FALSE,
    no_install = FALSE, no_manual = FALSE,
    no_vignettes = FALSE, no_rebuild_vignettes = TRUE)
}
\arguments{
  \item{pkg}{character, name of the package}

  \item{no_codoc}{do not check for code/documentation
  mismatches}

  \item{no_examples}{do not run the examples in the Rd
  files}

  \item{no_install}{skip installation and associated tests}

  \item{no_manual}{do not produce the PDF manual}

  \item{no_vignettes}{do not check Sweave vignettes}

  \item{no_rebuild_vignettes}{do not re-build PDFs of
  vignettes}
}
\value{
  generic
}
\description{
  creates a Rcheck directory with various log files and
  prints diagnostic messages
}

