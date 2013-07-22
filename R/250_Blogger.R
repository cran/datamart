#' Location Class for Google's Blogger service
#' 
#' This class implements a small subset of the blogger API. 
#' The authentication information (ClientLogin, no OAuth yet) and the title of the blog are passed to the 
#' constructor \code{blogger}.
#' The \code{meta} method provides information on the submitted blogposts. The \code{put} method accepts a
#' \code{BlogPostTarget} that can be transfered to Blogger.
#'
#' @examples
#' getSlots("Blogger")
#'
#' @name Blogger-class
#' @rdname Blogger-class
#' @exportClass Blogger
setClass(
    Class="Blogger", 
    representation=representation(
      google.auth="character", 
      curl.handle="CURLHandle", 
      nspaces="character", 
      blogid="character", 
      blogtitle="character"
    ),
    contains=c("Location", "Xdata")
)

#' Constructor for Blogger class
#'
#' Instantiates an object and authenticates with google.
#'
#' @param blogtitle          name of the (existing) blog. Defaults to getOption("blogger.blog").
#' @param email              email adress for authentication. Defaults to getOption("blogger.username").
#' @param password           password for authentication. Defaults to getOption("blogger.password").
#' @param clss               name of the class for convenient inheritance. Defaults to "Blogger".
#' 
#'
#' @return Blogger 
#' @rdname Blogger-class
#' @export
blogger <- function(
  blogtitle=getOption("blogger.blog"),
  email=getOption("blogger.username"), 
  password=getOption("blogger.password"),
  clss="Blogger"
) {
  #Authenticate with Google
  curlHandle = getCurlHandle(cookiefile="rcookies", ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
  x = postForm("https://www.google.com/accounts/ClientLogin",
         accountType="GOOGLE",
         service="blogger",
         Email=email,
         Passwd=password,
         source="private-datamart",
         curl = curlHandle)
  gtoken = unlist(strsplit(x, "\n"))
  parsed.gtoken <- unlist(strsplit(gtoken[3], "Auth="))
  if (length(parsed.gtoken) >= 2) {
    auth.token <- unlist(strsplit(gtoken[3], "Auth="))[[2]]
  } else {
    stop("Authentication failed.")
  }
  google.auth <- paste("GoogleLogin auth=", auth.token, sep='')
  
  # xml namespaces
  nspaces <- c(
    atom='http://www.w3.org/2005/Atom',
    openSearch='http://a9.com/-/spec/opensearchrss/1.0/',
    gd='http://schemas.google.com/g/2005'
  )
  
  # blogid
  uri <- 'http://www.blogger.com/feeds/default/blogs'
  feed <- getURL(
    uri, 
    .encoding = 'UTF-8', followlocation=TRUE, 
    httpheader=c(
      "Authorization"=google.auth),
      curl=curlHandle
  )
  feed <- xmlParse(feed)
  entries <- getNodeSet(feed, "//atom:entry", namespaces=nspaces)
  entries <- sapply(entries, function(n) c(tail(strsplit(xmlValue(n[["id"]]), "-")[[1]],1), xmlValue(n[["title"]])))
  tmp <- entries[2,]
  entries <- entries[1,]
  names(entries) <- tmp
  blogid <- entries[blogtitle]
  
  new(clss, google.auth=google.auth, curl.handle=curlHandle, nspaces=nspaces, blogid=blogid, blogtitle=blogtitle)
}


#' @rdname show-methods
#' @name show
#' @export
#' @docType methods
#' @aliases show show,Blogger-method
setMethod(
  f="show",
  signature="Blogger",
  definition=function(object) cat(sprintf("<blog '%s' @ Blogger.com>\n", object@blogtitle))
)

#' @docType methods
#' @rdname meta-methods
#' @name meta
#' @aliases meta meta,Blogger-method
#' @export
setMethod(
  f="meta",
  signature=c(self="Blogger"),
  definition=function(self) {
    uri <- sprintf('http://www.blogger.com/feeds/%s/posts/default', self@blogid)
    feed <- RCurl::getURL(
      uri, 
      .encoding = 'UTF-8', followlocation=TRUE, 
      httpheader=c(
        "Authorization"=self@google.auth,
        curl=self@curl.handle
      )
    )
    feed <- xmlParse(feed)
    entries <- getNodeSet(feed, "//atom:entry", namespaces=self@nspaces)
    entries <- sapply(entries, function(n) c(
      tail(strsplit(xmlValue(n[["id"]]), "-")[[1]],1), 
      xmlValue(n[["title"]]),
      xmlValue(n[["published"]]),
      xmlValue(n[["updated"]])
    ))
    entries <- as.data.frame(t(entries), stringsAsFactors=FALSE)
    colnames(entries) <- c("id", "title", "published", "updated")
    rownames(entries) <- entries[,"id"]
    entries$published <- strptime(strhead(entries$published, 19), "%Y-%m-%dT%H:%M:%s")
    entries$updated <- strptime(strhead(entries$updated, 19), "%Y-%m-%dT%H:%M:%s")

    entries <- entries[, -1] # remove id column
    return(entries)
  }
)

#' @rdname put-methods
#' @name put
#' @export
#' @docType methods
#' @aliases put put,BlogPostTarget,Blogger-method
setMethod(
  f="put",
  signature=c(target="BlogPostTarget", where="Blogger"),
  definition=function(target, where, ...) {
    entry <- newXMLNode("entry", namespace="atom", namespaceDefinitions=c(atom="http://www.w3.org/2005/Atom"))
      
    #title
    xtitle <- newXMLNode("title", namespace="atom", parent=entry)
    XML::xmlAttrs(xtitle)["type"] <- "xhtml"
    newXMLTextNode(target@name, parent = xtitle)
    
    #content
    xcontent <- newXMLNode("content", namespace="atom", parent=entry)
    XML::xmlAttrs(xcontent)["type"] <- "html"
    newXMLTextNode(target@content, parent = xcontent)
    
    #labels
    for (l in Filter(function(x) x!="",target@label)) {
        xl <- newXMLNode("category", namespace="atom", parent=entry)
        XML::xmlAttrs(xl)["scheme"] <- "http://www.blogger.com/atom/ns#"
        XML::xmlAttrs(xl)["term"] <- l
    }
    
    #draft?
    if(target@draft) {
      xcontrol <- newXMLNode("control", namespace="app", namespaceDefinitions=c(app="http://purl.org/atom/app#"), parent=entry)
      xdraft <- newXMLNode("draft", namespace="app", parent=xcontrol)
      newXMLTextNode("yes", parent = xdraft)
    }
    
    # overwrite?
    postid <- ""
    if(target@overwrite) {
      m <- meta(where) # this may take some time, it gets all blog entries.
      mvec <- rownames(m)
      names(mvec) <- m[,"title"]
      postid <- mvec[target@name] 
      if(is.na(postid)) postid <- "" 
    }
    if(postid=="")
      curlSetOpt(customrequest = "POST", curl = where@curl.handle)  
    else
      curlSetOpt(customrequest = "PUT", curl = where@curl.handle)  # POST does not work when overwriting.
    
    #send
    uri <- sprintf('http://www.blogger.com/feeds/%s/posts/default/%s', where@blogid, postid)
    feed <- postForm(
      uri,
      .opts = list(
        httpheader = c( 'Content-Type' = 'application/atom+xml', "Authorization"=where@google.auth),
        followlocation=TRUE, 
        postfields = saveXML(xmlDoc(entry), encoding="UTF-8")
      ),
      curl = where@curl.handle
    )
    
    #extract id
    feed <- xmlParse(feed)
    entry <- getNodeSet(feed, "//atom:entry", namespaces=where@nspaces)[[1]]
    entry <- tail(strsplit(xmlValue(entry[["id"]]), "-")[[1]],1)
    return(entry)

 }
)
