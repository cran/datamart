#' Buildable report
#' 
#' The main S4 class in this framework is \code{MdReport}. You can
#' create a report with \code{mdreport}, which takes a template file name,
#' and a list of variables as arguments. The new generic method \code{build}. 
#' can then be used to actually produce the report in various formats (markdown,
#' xhtml).
#'
#' \code{strsubst} is a simple
#' templating mechanism inspired from Python (PEP-0292).
#' Variables in the template are marked by a preceeding
#' dollar sign and get replaced with the value
#' of the corresponding variables passed to \code{strsubst}.
#'
#' @name MdReport-class
#' @rdname MdReport-class
#' @exportClass MdReport
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
    Class="MdReport", 
    representation=representation(tpl="character", vars="list"),
    contains="Target"
)

#' Constructor for MdReport objects
#'
#' see class MdReport for details.
#'
#' @param tpl         path to markdown template file
#' @param name        name of the Report, default ''
#' @param verbose     diagnostic messages T/F
#' @param clss        class of the object, default 'MdReport'
#' @param ...         number of targets
#'
#' @return generic
#' @export
mdreport <- function(tpl, name="", clss="MdReport", verbose=getOption("verbose"), ...) {
  # handle subtargets
  vars <- list(...)
  idx <- which(names(vars)=="")
  for (i in idx) {
    v <- vars[[i]]
    if(is.character(v@name) && v@name!="") names(vars)[[i]] <- v@name
  }
  if(verbose) {
    if(length(vars)==0) 
      cat("no subtargets.\n")
    else
      cat("subtargets: '", paste(names(vars), collapse="', '"), "'.\n")
  }
  
  # tpl filename?
  if(length(tpl)==1 && file.exists(tpl)) tpl <- readLines(tpl)
  if(!is.character(tpl)) stop("invalid tpl argument, name of existing file or character vector expected.")
  
  # interpret the first lines as meta data and convert them to subtargets  
  vname <- ""
  body_start <- 1
  for (l in tpl) {
      body_start <- body_start + 1
      if(grepl("^[\\s]*$", l, perl=TRUE)) break
      m <- strparse("^[\\s]+(?<vval>.*)", l) # continuation line?
      if(!is.null(m)) {
          if(vname=="") stop("invalid header.")
          vars[[vname]] <- paste(vars[[vname]], m[["vval"]])
          next
      }
      #browser()
      m <- strparse("^(?<vname>[^:\\s]+)[\\s]*:[\\s]*(?<vval>.*)$", l) # meta?
      if(!is.null(m)) {
          vname <- strcap(m[["vname"]])
          vars[[vname]] <- m[["vval"]]
      } else if(body_start==2) { # no meta
          if(verbose) cat("no meta data found.\n")
          body_start <- 1
          break
      }
  }
  if(verbose) {
    cat(sprintf("processed %d header lines.\n", body_start-1))
    if(length(vars)==0) 
      cat("still no subtargets.\n")
    else
      cat("subtargets after meta: '", paste(names(vars), collapse="', '"), "'.\n")
  }

  # body
  if(body_start>1) tpl <- tail(tpl, -body_start+1)
  tpl <- paste(tpl, collapse="\n")
  
  # instantiate
  new(clss, name=name, tpl=tpl, vars=vars)
}

#' @rdname build-methods
#' @aliases build,MdReport,DirectoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="MdReport", where="DirectoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata) {
    tgts <- lapply(target@vars, function(v) as.character(build(target=v, where=where, xdata=xdata)))
    subj <- tgts[["Subject"]]
    if(is.null(subj)) subj <- "Untitled"
    mdlines <- strsubst(target@tpl, tgts)
    html <- markdownToHTML(
      text=mdlines,
      options=c("safelink", "escape", "use_xhtml", "smartypants"),
      title=subj,
      extensions=NULL
    )
    htmlfile <- target@name
    if(htmlfile=="") htmlfile <- basename(tempfile(pattern="Rplot", tmpdir="", fileext=""))
    htmlfile <- paste(htmlfile, "html", sep=".")
    writeLines(html, file.path(as.character(where), htmlfile))
    return(htmlfile)
  }
)

#' @rdname build-methods
#' @aliases build,MdReport,MemoryLocation,Xdata-method
setMethod(
  f="build",
  signature=c(target="MdReport", where="MemoryLocation", xdata="Xdata"),
  definition=function(target, where, xdata, ...) {
    tgts <- lapply(target@vars, function(v) as.character(build(target=v, where=where, xdata=xdata)))
    mdlines <- strsubst(target@tpl, tgts)
    html <- markdownToHTML(
      text=mdlines,
      options=c("safelink", "escape", "use_xhtml", "fragment_only", "smartypants"),
      extensions=NULL
    )
    return(html)
  }
)

#' @rdname build-methods
#' @aliases build,MdReport,Blogger,Xdata-method
setMethod(
  f="build",
  signature=c(target="MdReport", where="Blogger", xdata="Xdata"),
  definition=function(target, where, xdata, draft=TRUE, overwrite=FALSE, ...) {
    mem <- new("MemoryLocation")
    html <- build(target, mem, xdata)
    
    subj <- target@vars[["Subject"]]
    if(inherits(subj, "Target")) subj <- build(subj, mem, xdata)
    if(!is.character(subj)) subj <- "Untitled"
    
    keyw <- target@vars[["Keywords"]]
    if(inherits(keyw, "Target")) keyw <- build(keyw, mem, xdata)
    if(!is.character(keyw)) keyw <- NULL
    
    return(post(where, posttitle=subj, content=html, label=keyw, draft=draft, overwrite=overwrite))
  }
) 


