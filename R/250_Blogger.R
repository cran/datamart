#' Buildable target that uses query()
#' 
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

# constructor
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


#' Show method for Blogger
#'
#' @param object    an Target object
#'
#' @docType methods
#' @name show
#' @rdname show-methods
#' @aliases show,Blogger-method
#' @export
setMethod(
  f="show",
  signature="Blogger",
  definition=function(object) cat(sprintf("<blog '%s' @ Blogger.com>\n", object@blogtitle))
)

#' @rdname query-methods
#' @aliases query,Blogger,BlogPostIds,missing-method
setMethod(
  f="query",
  signature=c(self="Blogger", resource=resource("BlogPostIds"), dbconn="missing"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    uri <- sprintf('http://www.blogger.com/feeds/%s/posts/default', self@blogid)
    feed <- getURL(
      uri, 
      .encoding = 'UTF-8', followlocation=TRUE, 
      httpheader=c(
        "Authorization"=self@google.auth,
        curl=self@curl.handle
      )
    )
    feed <- xmlParse(feed)
    entries <- getNodeSet(feed, "//atom:entry", namespaces=self@nspaces)
    entries <- sapply(entries, function(n) c(tail(strsplit(xmlValue(n[["id"]]), "-")[[1]],1), xmlValue(n[["title"]])))
    tmp <- entries[2,]
    entries <- entries[1,]
    names(entries) <- tmp
    return(entries)
  }
)

post <- function(obj, posttitle, content, label=c(), draft=FALSE, overwrite=FALSE) {
  entry <- newXMLNode("entry", namespace="atom", namespaceDefinitions=c(atom="http://www.w3.org/2005/Atom"))
    
  #title
  xtitle <- newXMLNode("title", namespace="atom", parent=entry)
  xmlAttrs(xtitle)["type"] <- "xhtml"
  newXMLTextNode(posttitle, parent = xtitle)
  
  #content
  xcontent <- newXMLNode("content", namespace="atom", parent=entry)
  xmlAttrs(xcontent)["type"] <- "html"
  newXMLTextNode(content, parent = xcontent)
  
  #labels
  for (l in label) {
      xl <- newXMLNode("category", namespace="atom", parent=entry)
      xmlAttrs(xl)["scheme"] <- "http://www.blogger.com/atom/ns#"
      xmlAttrs(xl)["term"] <- l
  }
  
  #draft?
  if(draft) {
    xcontrol <- newXMLNode("control", namespace="app", namespaceDefinitions=c(app="http://purl.org/atom/app#"), parent=entry)
    xdraft <- newXMLNode("draft", namespace="app", parent=xcontrol)
    newXMLTextNode("yes", parent = xdraft)
  }
  
  # overwrite?
  postid <- ""
  if(is.character(overwrite) && nchar(overwrite)>0) 
    postid <- overwrite
  else if(is.logical(overwrite) && overwrite) {
    postid <- query(obj, "BlogPostIds")[posttitle] # this may take some time, it gets all blog entries.
    if(is.na(postid)) postid <- "" 
  }
  if(postid=="")
    curlSetOpt(customrequest = "POST", curl = obj@curl.handle)  
  else
    curlSetOpt(customrequest = "PUT", curl = obj@curl.handle)  # POST does not work when overwriting.
  
  #send
  uri <- sprintf('http://www.blogger.com/feeds/%s/posts/default/%s', obj@blogid, postid)
  feed <- postForm(
    uri,
    .opts = list(
      httpheader = c( 'Content-Type' = 'application/atom+xml', "Authorization"=obj@google.auth),
      followlocation=TRUE, 
      postfields = saveXML(xmlDoc(entry), encoding="UTF-8")
    ),
    curl = obj@curl.handle
  )
  
  #extract id
  feed <- xmlParse(feed)
  entry <- getNodeSet(feed, "//atom:entry", namespaces=obj@nspaces)[[1]]
  entry <- tail(strsplit(xmlValue(entry[["id"]]), "-")[[1]],1)
  return(entry)
}


