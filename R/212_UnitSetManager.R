#' UnitSetManager -- A class for unit conversion data
#'
#' This is an internal class. It administers the unitsets
#' used by the \code{uconv} method. One instance, usually
#' the only one, is created at startup.
#'
#' @name UnitSetManager-class
#' @rdname UnitSetManager-class
setClass(
  Class="UnitSetManager", 
  representation=representation(usets="list"),
  contains="InternalData"
)

#' Constructor for UnitSetManager objects
#'
#' Internal function to create an UnitSetManager object.
#'
unitsetmanager <- function() internalData(name="usets", package="datamart", clss="UnitSetManager")

#' convert between numerical units
#'
#' This function converts between numerical units.
#' It works similar to the \code{iconv} function:
#' You provide vector \code{x} and a \code{from} and a \code{to} unit name
#' and the function converts.
#' If you omit the \code{from} parameter, it is tried to
#' lookup the current unit name by \code{Unit(x)}.
#'
#' Additionally, you may provide a unitset name. Here, the
#' analogy to \code{iconv} ceases. Think of unitset as a
#' dimension of units, or a context for units. Predefined
#' unitsets are "Length", "Mass", "Energy", which are dimensional
#' unitsets and "GaseousFuel", "CrudeOil", which provide the
#' right conversion factors for the given contexts. It is
#' recommened to provide the unitset name.
#'
#' A list of available unitsets and the units defined by them
#' can be obtained with \code{uconvlist()}. You may add your
#' own unitset(s) using \code{add_unitsets}.
#' 
#' @param x     numerical vector
#' @param from  character, unit to convert from. Default 'Unit(x)'
#' @param to    character, unit to convert to
#' @param uset  optional, character, unit set to use.
#'
#' @examples
#' uconv(1, "horse length", "m")
#' uconv(1:10, "t LNG", "GJ", "GaseousFuel")
#'
#' @export
#' @docType methods
#' @rdname uconv-methods
setGeneric(
  name="uconv",
  def=function(x, from, to, uset, ...){standardGeneric("uconv")}
)


#' @rdname uconv-methods
#' @aliases uconv,numeric,character,character,character-method
setMethod(
  f="uconv",
  signature=c(x="numeric", from="character", to="character", uset="character"), #workhorse
  definition=function(x, from, to, uset, ...) {
      if(length(.UnitSetManager)==0) assign("x", unitsetmanager(), envir=.UnitSetManager)
      obj <- try(query(.UnitSetManager$x, uset), silent=TRUE)
      if(is.null(obj)) stop("Unknown unitset: '", uset, "'")
      res <- NA
      
      if(is.numeric(obj)) {
        cf <- obj[from] / obj[to]
        names(cf) <- NULL
        res <- x*cf
      } else if(is.function(obj)) {
        res <- obj(x, from, to, ...)
      }
        
      if(is.na(res)) stop("Don't know how to convert from '", from, "' to '", to, "'.")
      
      return(res)
  }
)

#' @rdname uconv-methods
#' @aliases uconv,numeric,character,character,missing-method
setMethod(
  f="uconv",
  signature=c(x="numeric", from="character", to="character", uset="missing"), #iterate unitsets
  definition=function(x, from, to) {
      if(length(.UnitSetManager)==0) assign("x", unitsetmanager(), envir=.UnitSetManager)
      for (uset in queries(.UnitSetManager$x)) {
        res <- try(uconv(x=x, from=from, to=to, uset=uset), silent=TRUE)
        if(!inherits(res, "try-error")) {
          warning("No unitset specified, using '", uset, "'", call.=FALSE)
          return(res)
        }
      }
      stop("Don't know how to convert from '", from, "' to '", to, "'.")
  }
)

#' List unitsets and their units
#'
#' The function lists the currently available
#' unitsets and the units supported by them.
#'
#' @return named list, names=Unitsets, values=Units in these Unitsets
#' @export
uconvlist <- function() {
  if(length(.UnitSetManager)==0) assign("x", unitsetmanager(), envir=.UnitSetManager)
  usets <- queries(.UnitSetManager$x)
  res <- list()
  for (uset in usets) {
    obj <- query(.UnitSetManager$x, uset)
    if(is.numeric(obj))
        res[[uset]] <- names(obj)
    else if(is.function(obj))
        res[[uset]] <- formals(obj)$from
  }
  return(res)
}

# package-wide variable, lazy initialized, internal
.UnitSetManager <- new.env()

#' Measurement unit constants
#'
#' Unit set for converting Length, Energy, Mass, GaseousFuel, Temperature, CrudeOil
#'
#' Usually you use this dataset indirect with uconv.
#'
#' @references \url{http://en.wikipedia.org/wiki/Mass}
#' @references \url{http://en.wikipedia.org/wiki/Unit_of_length}
#' @references Erdmann/Zweifel (2005): Energieoekonomik, Tab. 2.2, based on IEA conversion factors
#' @references Erdmann/Zweifel (2005): Energieoekonomik, Tab. 9.1
#' @references http://de.wikipedia.org/wiki/Temperatur
#' @references Erdmann/Zweifel (2005): Energieoekonomik, Tab. 8.1
#'
#' @docType data
#' @name usets-data
#' @aliases usets Length Energy Mass GaseousFuel Temperature CrudeOil
NULL

