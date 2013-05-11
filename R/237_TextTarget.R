#' Buildable target that uses query()
#' 
#'
#' @name TextTarget-class
#' @rdname TextTarget-class
#' @exportClass TextTarget
setClass(
    Class="TextTarget", 
    representation=representation(text.fct="character", text.args="character"),
    contains="Target"
)

#' Constructor for TextReport objects
#'
#' see class TextReport for details.
#'
#' @param name        name of the Report, default ''
#' @param text.fct    function name
#' @param text.args   list of arguments past to text.fct
#' @param clss        class name, default 'TextTarget'
#'
#' @return generic
#' @export
texttarget <- function(name="", text.fct="query", text.args, clss="TextTarget") 
  new(clss, name=name, text.fct=text.fct, text.args=text.args)

#' @rdname build-methods
#' @aliases build,TextTarget,MemoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="TextTarget", where="MemoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata, ...) {
    qargs <- target@text.args
    if(target@text.fct=="query") qargs[["self"]] <- xdata
    do.call(target@text.fct, qargs)
  }
)

#' @rdname build-methods
#' @aliases build,TextTarget,DirectoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="TextTarget", where="DirectoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata) {
    fname <- target@name
    if(fname=="") fname <- basename(tempfile(pattern="Rout", tmpdir="", fileext=""))
    fname <- paste(fname, "txt", sep=".")
  }
)
