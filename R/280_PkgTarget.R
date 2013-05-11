#' Checks a dev package
#'
#' creates a Rcheck directory with various log files and prints
#' diagnostic messages
#'
#' @param pkg        character, name of the package
#' @param no_codoc        do not check for code/documentation mismatches
#' @param no_examples     do not run the examples in the Rd files
#' @param no_install      skip installation and associated tests
#' @param no_manual       do not produce the PDF manual
#' @param no_vignettes    do not check Sweave vignettes
#' @param no_rebuild_vignettes    do not re-build PDFs of vignettes
#'
#'
#' @return generic
#' @export
#'
### param no_tests        do not run code in 'tests' subdirectory
chk_pkg <- function(
    pkg, 
    no_codoc=FALSE, 
    no_examples=FALSE,
    no_install=FALSE,
    no_manual=FALSE,
    no_vignettes=FALSE,
    no_rebuild_vignettes=TRUE
  ) {
  old_dir <- getwd(); on.exit(setwd(old_dir))
  folder <- dirname(pkg)
  if(folder==".") folder <- getOption("devlib.path", ".")
  #folder <- file.path(folder, basename(pkg))
  setwd(folder)
  cmdline <- paste(file.path(R.home(), "bin", "R.exe"), "CMD check")
  if(no_codoc) cmdline <- paste(cmdline, "--no-codoc")
  if(no_examples) cmdline <- paste(cmdline, "--no-examples")
  if(no_install) cmdline <- paste(cmdline, "--no-install")
  if(no_manual) cmdline <- paste(cmdline, "--no-manual")
  if(no_vignettes) cmdline <- paste(cmdline, "--no-vignettes")
  if(no_rebuild_vignettes) cmdline <- paste(cmdline, "--no-rebuild-vignettes")
  cmdline <- paste(cmdline, basename(pkg))
  stopifnot(system(cmdline)==0)
}

#' Build source package
#'
#' @param pkg          character, subfolder in devlib_path()
#' @param no_vignettes logical, if TRUE no vignettes are build
#' @param src          logical, if FALSE (default), a binary distribution is created,
#'                     a source distribution otherwise
# --force               force removal of INDEX file
# --keep-empty-dirs     do not remove empty dirs
# --no-vignettes        do not rebuild package vignettes
# --no-manual           do not build the manual even if \Sexprs are present
# --resave-data=        re-save data files as compactly as possible:
# "no", "best", "gzip" (default)
# --resave-data         same as --resave-data=best
# --no-resave-data      same as --resave-data=no
# --compact-vignettes   try to compact PDF files under inst/doc (using qpdf)
# --binary              build pre-compiled binary packages, with option:
# --install-args=       command-line args to be passed to INSTALL, separated by spaces
#'
#' @return used for its side effect
#' @export
build_pkg <- function(pkg, no_vignettes=TRUE, src=FALSE) {
  old_dir <- getwd(); on.exit(setwd(old_dir))
  folder <- dirname(pkg)
  if(folder==".") folder <- getOption("devlib.path", ".")
  #folder <- file.path(folder, basename(pkg))
  setwd(folder)
  if(src)
      cmdline <- paste(file.path(R.home(), "bin", "R.exe"), "CMD build")
  else
      cmdline <- paste(shQuote(file.path(R.home(), "bin", "R.exe")), "CMD INSTALL --build")
  if(no_vignettes) cmdline <- paste(cmdline, "--no-vignettes")
  cmdline <- paste(cmdline, basename(pkg))
  stopifnot(system(cmdline)==0)
}

#' Build R package
#' 
#' @name PkgTarget-class
#' @rdname PkgTarget-class
#' @exportClass PkgTarget
setClass(
    Class="PkgTarget", 
    contains="Target"
)

#' Constructor for PkgTarget objects
#'
#' see class PkgTarget for details.
#'
#' @param name        name of the Report, default ''
#' @param verbose     diagnostic messages T/F
#' @param clss        class name, default 'PkgTarget'
#' @param ...         number of targets
#'
#' @return generic
#' @export
pkg <- function(name, clss="PkgTarget", verbose=getOption("verbose"), ...) {
  folder <- dirname(name)
  if(folder==".") folder <- getOption("devlib.path", ".")
  p <- file.path(folder, basename(name))
  if(!file.exists(p)) stop("could not resolve name '", name, "' to an existing folder.")
  # instantiate
  new(clss, name=p)
}

#' @rdname build-methods
#' @aliases build,PkgTarget,DirectoryLocation,missing-method
setMethod(
  f="build",
  signature=c(target="PkgTarget", where="DirectoryLocation", xdata="missing"),
  definition=function(target, where, xdata, verbose=TRUE, ...) {
    .NotYetImplemented()
    if(verbose) cat("sweaving in folder '", as.character(where),"'.\n")
    old_wd <- getwd(); on.exit(setwd(old_wd))
    setwd(as.character(where))
    Sweave(basename(target@tpl), ...)
    if(verbose) cat("working @ '", getwd(),"'.\n")
    
    setwd(as.character(where))
    tex <- paste(target@name, "tex", sep=".")
    if(verbose) cat("texifying '", tex,"'.\n")
    if(!file.exists(tex)) stop("Could not find Sweave output ", tex, ". Did Sweave fail?")
    cmd <- paste("texify -p -b -q", basename(tex))
    system(cmd)
    return(file.path(as.character(where), paste(target@name, "pdf", sep=".")))
  }
)

#' @rdname build-methods
#' @aliases build,PkgTarget,missing,missing-method
setMethod(
  f="build",
  signature=c(target="PkgTarget", where="missing", xdata="missing"),
  definition=function(target, where, xdata, verbose=TRUE, no_vignettes=TRUE, src=FALSE, ...) {
    build_pkg(target@name, no_vignettes=no_vignettes, src=src)
    if(verbose) cat("assigning folder '", dirname(target@name),"'.\n")
  }
)
