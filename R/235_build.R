#' build a target
#'
#' This generic builds a target. Currently, there
#' are two targets defined: markdown reports and
#' graphic files.
#'
#' @param target    an object of class Target or derived
#' @param where  an object of class Location or derived
#' @param xdata     an object of class Xdata or derived
#'
#' @export
#' @docType methods
#' @rdname build-methods
setGeneric(
  name="build",
  def=function(target, where, xdata, ...){standardGeneric("build")}
)

#' @rdname build-methods
#' @aliases build,Target,character,Xdata-method
setMethod(
  f="build",
  signature=c(target="Target", where="character", xdata="Xdata"),
  definition=function(target, where, xdata, ...) {
    if(where==":memory:") 
      newloc <- new("MemoryLocation")
    else if(isTRUE(file.info(where)$isdir)) 
      newloc <- new("DirectoryLocation", path=where)
    else
      stop("cannot interpret 'where' argument '", where, "', please pass ':memory:', a path to an existing directory or an Location object.")
    build(target, newloc, xdata, ...)
  }
)

#' @rdname build-methods
#' @aliases build,Target,character,missing-method
setMethod(
  f="build",
  signature=c(target="Target", where="character", xdata="missing"),
  definition=function(target, where, ...) {
    if(where==":memory:") 
      newloc <- new("MemoryLocation")
    else if(isTRUE(file.info(where)$isdir)) 
      newloc <- new("DirectoryLocation", path=where)
    else
      stop("cannot interpret 'where' argument '", where, "', please pass ':memory:', a path to an existing directory or an Location object.")
    build(target, newloc, new("EmptySet"), ...)
  }
)

#' @rdname build-methods
#' @aliases build,Target,missing,Xdata-method
setMethod(
  f="build",
  signature=c(target="Target", where="missing", xdata="Xdata"),
  definition=function(target, xdata, ...) build(target=target, where=new("DirectoryLocation", path=tempdir()), xdata=xdata, ...)
)

#' @rdname build-methods
#' @aliases build,Target,Location,missing-method
setMethod(
  f="build",
  signature=c(target="Target", where="Location", xdata="missing"),
  definition=function(target, where, ...) build(target, where, new("EmptySet"), ...)
)

#' @rdname build-methods
#' @aliases build,Target,missing,missing-method
setMethod(
  f="build",
  signature=c(target="Target", where="missing", xdata="missing"),
  definition=function(target, ...) build(target=target, where=new("DirectoryLocation", path=tempdir()), xdata=new("EmptySet"), ...)
)

#' @rdname build-methods
#' @aliases build,character,ANY,ANY-method
setMethod(
  f="build",
  signature=c(target="character", where="ANY", xdata="ANY"),
  definition=function(target, ...) target
)

