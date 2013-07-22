#' A Target representing a blog post
#' 
#' This is an internal class representing a blog post. Use
#' MdReport instead.
#' 
#' @examples
#' getSlots("BlogPostTarget")
#'
#' @name BlogPostTarget-class
#' @rdname BlogPostTarget-class
setClass(
    Class="BlogPostTarget", 
    representation=representation(
        content="character",
        label="character",
        draft="logical",
        overwrite="logical"
    ),
    contains="Target"
)

#' Constructor for BlogPostTarget objects
#'
#' For internal use only
#'
#' @param name        title of the blogpost
#' @param content     content of the post
#' @param label       character vector of keywords
#' @param draft       draft or not? default=TRUE
#' @param overwrite   overwrite or not? overwrite=TRUE
#' @param clss        class of the object, default 'BlogPostTarget'
#'
#' @return BlogPostTarget
#' @rdname BlogPostTarget-class
blogpost <- function(name, content, label="", draft=TRUE, overwrite=TRUE, clss="BlogPostTarget") {
  new(clss, name=name, content=content, label=label, draft=draft, overwrite=overwrite)
}
