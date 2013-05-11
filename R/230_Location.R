#' S4 base class to represent output Locations
#'
#' @name Location-class
#' @rdname Location-class
#' @exportClass Location
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(Class="Location", representation=representation())

#' S4 base class to represent output in Memory
#'
#' @name MemoryLocation-class
#' @rdname MemoryLocation-class
#' @exportClass MemoryLocation
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(Class="MemoryLocation", representation=representation(), contains="Location")

#' Directory location
#'
#' @name DirectoryLocation-class
#' @rdname DirectoryLocation-class
#' @exportClass DirectoryLocation
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
  Class="DirectoryLocation", 
  representation=representation(path="character"), 
  contains="Location",
  validity=function(object) 
    if(!isTRUE(file.info(object@path)$isdir)) stop("invalid path argument, seems not to be a directory")
)

#' Show method for Directory
#'
#' @param object    an Target object
#'
#' @docType methods
#' @name show
#' @rdname show-methods
#' @aliases show,DirectoryLocation-method
#' @export
setMethod(
  f="show",
  signature="DirectoryLocation",
  definition=function(object) cat(sprintf("<Local Directory @ %s>\n", object@path))
)

#' as.character method for Directory
#'
#' This method returns the path to the directory
#' it represents.
#'
#' @param a Directory object
#'
#' @docType methods
#' @name as.character
#' @rdname as.character-methods
#' @aliases as.character,DirectoryLocation-method
#' @export
setMethod(
  f="as.character",
  signature="DirectoryLocation",
  definition=function(x) x@path
)
