#' Convert between numerical units
#'
#' This function converts between numerical units.
#' It works similar to the \code{iconv} function:
#' You provide vector \code{x} and a \code{from} and a \code{to} unit name
#' and the function converts.
#'
#' Additionally, you may provide a unitset name. Here, the
#' analogy to \code{iconv} ceases. Think of unitset as a
#' dimension of units, or a context for units. Predefined
#' unitsets are "Length", "Mass", "Energy", and "Temperature". 
#' It is recommened to provide the unitset name.
#' A list of available unitsets and the units defined by them
#' can be obtained with \code{uconvlist()}. 
#'
#' @param x     numerical vector
#' @param from  character, unit to convert from. 
#' @param to    character, unit to convert to
#' @param uset  optional, character, unit set to use.
#'
#' @examples
#' uconv(1, "horse length", "m", "Length")
#' uconv(1:10, "TWh", "PJ", "Energy")
#'
#' @export
uconv <- function(x, from, to, uset=NULL) {
      # Singleton pattern: global variable .UnitSetManager
      if(length(.UnitSetManager)==0) assign("x", unitsetmanager(), envir=.UnitSetManager)
      
      # no uset provided? Iterate
      if(is.null(uset)) {
        for (uset in queries(.UnitSetManager$x)) {
          res <- try(uconv(x=x, from=from, to=to, uset=uset), silent=TRUE)
          if(!inherits(res, "try-error")) {
            warning("No unitset specified, using '", uset, "'", call.=FALSE)
            return(res)
          }
        }
        stop("uconv doesn't know how to convert from '", from, "' to '", to, "'.")
      }
      
      # get conversion resource
      obj <- try(query(.UnitSetManager$x, uset), silent=TRUE)
      if(is.null(obj)) stop("uconv doesn't know the unitset: '", uset, "'")
      
      # do conversion
      if(is.numeric(obj)) {
        cf <- obj[from] / obj[to]
        names(cf) <- NULL
        res <- x*cf
      } else if(is.function(obj)) {
        res <- obj(x, from, to)
      }
      
      # if unit name is wrong
      if(any(is.na(res))) stop("Don't know how to convert from '", from, "' to '", to, "'.")
      
      # finished
      return(res)
}

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
    else if(is.function(obj)) {
        # from call to vector
        tmp <- as.list(formals(obj)$from)
        res[[uset]] <- unlist(tmp[2:length(tmp)])
    }
  }
  return(res)
}


#' UnitSetManager -- A class for unit conversion data
#'
#' This is an internal class. It administers the unitsets
#' used by the \code{uconv} method. One instance, usually
#' the only one, is created at startup.
#'
#' @examples
#' getSlots("UnitSetManager")
#'
#' @name UnitSetManager-class
#' @rdname uconv
setClass(
  Class="UnitSetManager", 
  representation=representation(usets="list"),
  contains="Xdata"
)

#' Constructor for UnitSetManager objects
#'
#' Internal function to create an UnitSetManager object.
#'
#' @rdname uconv
unitsetmanager <- function() new("UnitSetManager")

#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,UnitSetManager,Length-method
setMethod(
  f="query",
  signature=c(self="UnitSetManager", resource=resource("Length")),
  definition=function(self, resource, ...) { 
    ret <- c(
    siunit("m", extended=TRUE), 
    inch=2.54/100,
    "in"=2.54/100,
    bigpts=0.0003527778, # big points = 1/72 in
    thou=2.54/100000,
    ft=0.3048,
    yard=0.9144,
    mile=1609.344,
    fathom= 1.8288,
    "nautical mile"=1852,
    furlong = 201,
    "horse length"=2.4,
    ly=9.46e+15,
    AU=1.5e+11,
    RE=6370000,
    pc=30.8e+15,
    li=500,
    "chi (PRC)"=1/3.0,
    "chi (Taiwan)"=10/33.,
    chek=0.371475,
    cun=33.3*10^(-3),
    tsun=37.148*10^(-3),
    sun=30.3*10^(-3)
    )
    comment(ret) <- "m"
    ret
  }
)

#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,UnitSetManager,Mass-method
setMethod(
  f="query",
  signature=c(self="UnitSetManager", resource=resource("Mass")),
  definition=function(self, resource, ...) { 
    ret <- c(
    siunit("g", 10^(-3)),
    siunit("t", 10^3),
    siunit("eV", 1.783*10^(-36)), # 1 GeV/c2 = 1.783*10^(-27) kg
    u=1.66*10^(-27),
    sl=14.593903,
    slug=14.593903,
    lb=0.45359237,
    mP=2.1765113*10^(-8)
  )
  comment(ret) <- "kg"
  ret
  }
)

#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,UnitSetManager,Energy-method
setMethod(
  f="query",
  signature=c(self="UnitSetManager", resource=resource("Energy")),
  definition=function(self, resource, ...) { 
    ret <- c(
    siunit("J", 10^(-6)),
    siunit("Wh", 3.6/10^(3)),
    siunit("toe", 41880),
    siunit("cal", 4.184*10^(-6)),
    siunit("tce", 29290),
    "BTU"=1055*10^(-6),
    Therm=105.5,
    
    # natural gas
    "cubic feet NG"=1.076,
    "m\uB3 NG"=38,
    
    # crude oil
    "bbl CL"=5713,
    "l CL"=35948498/10^6
    )
    comment(ret) <- "MJ"
    ret
  }
)


#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,UnitSetManager,Temperature-method
setMethod(
  f="query",
  signature=c(self="UnitSetManager", resource=resource("Temperature")),
  definition=function(self, resource, ...) { 
    retfun <- function(x, from=c("K", "\u00B0C", "\u00B0F", "\u00B0R"), to) {
    if(length(from)>0) stop("uconv needs exactly one character as 'from' parameter.")
    if(length(to)>0) stop("uconv needs exactly one character as 'to' parameter.")
    temp_units <- c("K", "\u00B0C", "\u00B0F", "\u00B0R")
    if(!from %in% temp_units) stop("unknown temperature unit from='", from, "'. Expected one of '", paste(temp_units, collapse="', '"), "'")
    if(!to %in% temp_units) stop("unknown temperature unit to='", to, "'. Expected one of '", paste(temp_units, collapse="', '"), "'")
    x <- switch(from,
      K=x,
      "\u00B0C"=x+273.15, # degr Celcius
      "\u00B0F"=5 * (x - 32)/9 + 273.15, # degr Fahrenheit
      "\u00B0R"=x * 5 / 9 # degr Rankine
    )
    switch(to,
      K=x,
      "\u00B0C"=x-273.15, # degr Celcius
      "\u00B0F"=9 * x /5 - 459.67, # degr Fahrenheit
      "\u00B0R"=x * 9 / 5 # degr Rankine
    )
    }
    comment(retfun) <- "K"
    retfun
  }
)


# package-wide variable, lazy initialized, internal
.UnitSetManager <- new.env()

#' Generate numeric vector with SI prefixes
#'
#' Given a basename for the unit, e.g. "m",
#' the function returns a named vector of
#' most or all (extended=TRUE) prefixes defined
#' by SI, i.e. "km", "Mm", "Gm", ... and "mm", "nm", ...
#'
#' By default, the prefixes "h", "d", "da" and "c" are not
#' generated. Use extended=TRUE to include these.
#'
#' Use \code{value} to define a conversion factor between
#' the unit without prefix and the base unit of the unitset
#' you are defining. \code{value} is how many units of
#' \code{uname} form one unit of the base unit. Default is 1.
#'
#' The function is internal.
#'
#' @param uname the name of the unit to define, e.g. "g"
#' @param value scaling factor, see details.
#' @param extended logical (default=FALSE), add not so common prefixes?
#'
#' @references
#' \url{http://en.wikipedia.org/wiki/International_System_of_Units}
#'
#' @return numeric
siunit <- function(uname, value=1.0, extended=FALSE) {
    res1 <- value * 10^(seq(from=0, to=24, by=3))
    names(res1) <- paste(c('', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y'), uname, sep="")
    
    res2 <- value *  10^(seq(from=-3, to=-24, by=-3))
    mu <- "\u03BC"
    Encoding(mu) <- "UTF-8"
    names(res2) <- paste(c('m', mu, 'n', 'p', 'f', 'a', 'z', 'y'), uname, sep="")
    
    res <- c(res1, res2)

    if(extended) {
      res3 <- value*c(100, 10, 0.1, 0.01)
      names(res3) <- paste(c("h", "da", "d", "c"), uname, sep="")
      res <- c(res, res3)
    }
    return(res)
}
  
