#' Wrapper for Sweave and pdf
#' 
#' The main S4 class in this framework is \code{SweaveReport}. You can
#' create a report with \code{swvreport}, which takes a sweave file name,
#' The generic method \code{build} 
#' can then be used to actually produce the report in pdf format.
#'
#' @name SweaveReport-class
#' @rdname SweaveReport-class
#' @exportClass SweaveReport
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
    Class="SweaveReport", 
    representation=representation(tpl="character"),
    contains="Target"
)

#' Constructor for SweaveReport objects
#'
#' see class SweaveReport for details.
#'
#' @param tpl         path to markdown template file
#' @param name        name of the Report, default ''
#' @param verbose     diagnostic messages T/F
#' @param clss        class of the constructed object, default 'SweaveReport'
#' @param ...         number of targets
#'
#' @return generic
#' @export
swvreport <- function(tpl, name=NULL, clss="SweaveReport", verbose=getOption("verbose"), ...) {
  if(!file.exists(tpl)) stop("invalid tpl argument, name of existing file expected.")
  if(is.null(name)) name <- paste(head(unlist(strsplit(basename(tpl), "\\.")),-1), sep="")
  # instantiate
  new(clss, name=name, tpl=tpl)
}

#' @rdname build-methods
#' @aliases build,SweaveReport,DirectoryLocation,ANY-method
setMethod(
  f="build",
  signature=c(target="SweaveReport", where="DirectoryLocation", xdata="ANY"),
  definition=function(target, where, xdata, verbose=TRUE, ...) {
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
#' @aliases build,SweaveReport,missing,missing-method
setMethod(
  f="build",
  signature=c(target="SweaveReport", where="missing", xdata="missing"),
  definition=function(target, where, xdata, verbose=TRUE, ...) {
    if(verbose) cat("assigning folder '", dirname(target@tpl),"'.\n")
    build(target=target, where=new("DirectoryLocation", path=dirname(target@tpl)), xdata=new("EmptySet"), ...)
  }
)
