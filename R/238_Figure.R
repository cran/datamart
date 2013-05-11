#' Figures created by query()
#' 
#'
#' @name FigureTarget-class
#' @rdname FigureTarget-class
#' @exportClass FigureTarget
setClass(
    Class="FigureTarget", 
    representation=representation(
      name="character",
      viz.call="call", 
      dev.fct="character",
      dev.args="list"
    ),
    contains="Target"
)


#' Helper function for buildung figures
#'
#' A wrapper to querying a resource
#' see class FigureTarget
#'
#' @param resource        character, name of the package
#' @param ...             do not check for code/documentation mismatches
#'
#' @return call
#' @export
viz <-  function(resource, ...) 
  as.call(list("query", resource=resource, ...))

#' Constructor for UnitSetManager objects
#'
#' Internal function to create an UnitSetManager object.
#'
#' @param name        name of the figure, default ''
#' @param viz.call    call for creating the figure
#' @param dev         character for the device, default 'png'
#' @param clss        class name, default 'FigureTarget'
#' @param ...         additional parameters passed to the device
#'
#' @return generic
#' @export
figtarget <- function(
  name="", 
  viz.call=viz(resource=name), 
  dev="png", 
  clss="FigureTarget", ...) {
  if(!is.character(dev)) stop("Please provide the device as character.")
  new(clss, name=name, viz.call=viz.call, dev.fct=dev, dev.args=list(...))
}

#' @rdname build-methods
#' @aliases build,FigureTarget,ANY,missing-method
setMethod(
  f="build",
  signature=c(target="FigureTarget", where="ANY", xdata="missing"),
  definition=function(target, where, xdata, ...) build(target, where, new("EmptySet"), ...)
)

#' @rdname build-methods
#' @aliases build,FigureTarget,DirectoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="FigureTarget", where="DirectoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata) {
    ext <- target@dev.fct
    ext <- switch(ext, 
      jpeg="jpg",
      postscript="ps", 
      bitmap="bmp", 
      cairo_pdf="pdf", 
      cairo_ps="ps", 
      tiff="tif", 
      win.metafile="wmf", 
      ext
    )
    if(nchar(ext)>3) stop("could not determine extensions for device: '", ext, "'")

    #' set up device
    fname <- target@name
    if(fname=="") fname <- basename(tempfile(pattern="Rplot", tmpdir="", fileext=""))
    fname <- paste(fname, ext, sep=".")
    fname_arg <- switch(ext,
      postscript="file",
      pdf="file",
      "filename"
    )
    dargs <- target@dev.args
    dargs[[fname_arg]] <- file.path(as.character(where), fname)
    do.call(target@dev.fct, dargs)
     
    #' draw
    if(!inherits(xdata, "EmptySet")) {
        call. <- as.list(target@viz.call)
        call.[["self"]] <- xdata
        as.call(call.)
    }
    eval(call.)

    dev.off()
    return(fname)
  }
)

#' @rdname build-methods
#' @aliases build,FigureTarget,MemoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="FigureTarget", where="MemoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata) {
    ifile <- build(target, new("DirectoryLocation", path=tempdir()), xdata)
    ext <- strtail(ifile,3)
    if(tolower(ext)=="jpg") ext <- "jpeg"
    tfile <- tempfile()
    on.exit(unlink(tfile))
    base64::encode(file.path(tempdir(), ifile), tfile)
    sprintf("data:image/%s;base64,\n%s", ext, paste(readLines(tfile), collapse = "\n"))
  }
)

