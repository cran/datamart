#' A class for interactive queries
#' 
#' For internal use only.
#' 
#' @name Mediawiki-class
#' @rdname Mediawiki-class
setClass(
  Class="Mediawiki", 
  representation=representation(api="character"),
  contains="Xdata"
)

#' Constructor for mediawiki
#'
#' Set the api url as parameter.
#'
#' @param api string pointing to the API url. Usually ends with api.php
#' 
#' @return an object of class "mediawiki"
#' @references 
#' \url{http://www.mediawiki.org/wiki/API}
#' @export
mediawiki <- function(api="") {
  if (api=="") stop("No url to the api provided.")
  res <- new("Mediawiki", api=api)
  return(res)
}

#' Constructor for wikipedia
#'
#' This is a wrapper to the \code{mediawiki} function that
#' presets the URL for wikipedia. You may provide the lang
#' parameter (default "en") to specify a different wikipedia
#' than the english one.
#'
#' @param lang two letter code for the wikipedia language. Default "en".
#' 
#' @return Mediawiki
#' @references 
#' \url{http://www.mediawiki.org/wiki/API}
#' @export
wikipedia <- function(lang="en") mediawiki(api=paste("http://", lang, ".wikipedia.org/w/",sep=""))

#' For the Mediawiki class, the RaQueries a mediawiki database using its API.
#'
#' Use the mediawiki parameters titles, pageids, revids, prop, duplicatefiles, globalusage etc as parameters.
#' The output is considered as UTF-8.
#'
#' @param self reference of the Mediawiki object
#' @param resource a Raw object, or the string "Raw"
#' @param ... the parameters to the API 
#'             titles         - A list of titles to work on
#'             pageids        - A list of page IDs to work on
#'             revids         - A list of revision IDs to work on
#'             prop           - Which properties to get for the titles/revisions/pageids
#'                              Values (separate with '|'): info, revisions, links, langlinks, images, imageinfo, templates, categories, extlinks,
#'                              categoryinfo, duplicatefiles, globalusage
#'             list           - Which lists to get
#'                              categorymembers, deletedrevs, embeddedin, imageusage, logevents, recentchanges, search, usercontribs, watchlist, 
#'                              watchlistraw, exturlusage, users, random, protectedtitles, globalblocks, abuselog, abusefilters
#'                              Values (separate with '|'): allimages, allpages, alllinks, allcategories, allusers, backlinks, blocks, 
#'             meta           - Which meta data to get about the site
#'                              Values (separate with '|'): siteinfo, userinfo, allmessages, globaluserinfo
#'             generator      - Use the output of a list as the input for other prop/list/meta items
#'                              NOTE: generator parameter names must be prefixed with a 'g', see examples.
#'                              One value: links, images, templates, categories, duplicatefiles, allimages, allpages, alllinks, allcategories, 
#'                              backlinks, categorymembers, embeddedin, imageusage, search, watchlist, watchlistraw, exturlusage, random, 
#'                              protectedtitles
#'             redirects      - Automatically resolve redirects
#'             indexpageids   - Include an additional pageids section listing all returned page IDs.
#'             export         - Export the current revisions of all given or generated pages
#'             exportnowrap 
#' 
#' @return a list representation of the output
#' @references 
#' \url{http://en.wikipedia.org/w/api.php}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @examples
#' wiki <- mediawiki("http://de.wikipedia.org/w/")
#' query(wiki, prop="templates", titles="Soest")
#'
setMethod(
  f="query",
  signature=c(self="Mediawiki", resource=resource("Raw")),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    if (!require(RJSONIO)) stop("Could not load required library RJSONIO (install from Omegahat)")
    params <- list(...)
    url <- paste(self@api, "api.php?action=query&format=json", sep="")
    
    for (opt in names(params))
      url <- paste(url, URLencode(paste(opt, "=", params[[opt]], sep="")), sep="&")
    if(verbose) cat(url,"\n")
    
    #oldHTTPUserAgent <- getOption("HTTPUserAgent")
    #options(HTTPUserAgent=paste("R / datamart"))
    res <- fromJSON(url)$query
    #options(HTTPUserAgent=oldHTTPUserAgent)

    setEncoding <- function(x) {
      if (class(x)=="list") x <- lapply(x, setEncoding)
      if (class(x)=="character") Encoding(x) <- "UTF-8"
      x
    }  
    
    setEncoding(res)
})


#' Exists -- Check if Page with given Title exists.
#'
#' Use the mediawiki parameters titles, pageids, revids, prop, duplicatefiles, globalusage etc as parameters.
#' The output is considered as UTF-8.
#'
#' @param self reference of the Mediawiki object
#' @param resource a Exists object, or the string "Exists"
#' @param titles search term
#' @param verbose logical, print diagnostic messages?
#'
#' @return string with the url or ""
#' @references 
#' \url{http://en.wikipedia.org/w/api.php}
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @examples
#' wiki <- mediawiki("http://de.wikipedia.org/w/api.php")
#' query(wiki, prop="templates", titles="Soest")
setMethod(
  f="query",
  signature=c(self="Mediawiki", resource=resource("Exists")),
  definition=function(self, resource, titles, verbose=getOption("verbose"), ...) {
    s <- URLencode(titles)
    tmp <- query(self, "Raw", prop="info", titles=s)
    if(
      "-1" %in% names(tmp[["pages"]]) || 
      "missing" %in% names(tmp[["pages"]][[1]]) ||
      "invalid" %in% names(tmp[["pages"]][[1]])
    ) return("")

    return(paste(self@api, "index.php?title=",s, sep=""))
})

# query(wiki, list="categorymembers", cmlimit="500", cmtitle="Kategorie:Ort_in_Deutschland")
# query(wiki, list="allusers", aulimit="500")

# test <- function() {
  # m <- wikipedia()
  # query(m, "Raw", prop='revisions', rvprop='content', titles="Berlin")
# }







