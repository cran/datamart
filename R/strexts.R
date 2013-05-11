#' Capitalize a string
#'
#' The first character is uppercased, the other lowercased.
#'
#' @param s          character or character vector
#'
#' @return character 
#' @export
strcap <- function(s) paste(toupper(substr(s,1,1)), tolower(substring(s,2)), sep="")

#' Get the last n letters
#'
#' if n>0, return the last n letters of x
#' if n<0, return all but the first abs(n) letters of x
#'
#' @param s          character or character vector
#' @param n          numeric, default 1
#'
#' @return character 
#' @export
strtail <- function(s,n=1) {
  if(n<0) 
    substring(s,1-n) 
  else 
    substring(s,nchar(s)-n+1)
}

#' Get the first n letters
#'
#' if n>0, return the first n letters of x
#' if n<0, return all but the last abs(n) letters of x
#'
#' @param s          character or character vector
#' @param n          numeric, default 1
#'
#' @return character 
#' @export
strhead <- function(s,n=1) {
  if(n<0) 
    substr(s,1,nchar(s)+n) 
  else 
    substr(s,1,n)
}

#' Obfuscate string
#'
#' a fancy method to make the string unreadable
#' use strdecypt to revert (not safe across machines!)
#'
#' @param message    character or character vector
#'
#' @return character 
#' @export
strencrypt <- function(message) {
  oldletters <- c(LETTERS, letters, 0:9, " ")
  old <- paste(oldletters, collapse = "")
  set.seed(13)
  new <- paste(sample(oldletters), collapse="")
  chartr(old,new,message)
}

#' Obfuscate string
#'
#' reverts the action of strencrypt (not safe across machines!)
#'
#' @param message    character or character vector
#'
#' @return character 
#' @export
strdecrypt <- function(message) {
  oldletters <- c(LETTERS, letters, 0:9, " ")
  old <- paste(oldletters, collapse = "")
  set.seed(13)
  new <- paste(sample(oldletters), collapse="")
  chartr(new, old, message)
}

#' Named substitution in strings
#'
#' Simple template mechanism inspired by PEP-0292. Use lists
#' or named character vectors (vectors not tested) as a mapping for
#' substitution.
#'
#' Substitutions are marked by $(NAME).
#'
#' @param template   character with $(VARS)
#' @param map        object with [ functionality e.g. a list. Should return
#'                   values that can be coerced to character
#' @param verbose    print debugging messages when TRUE, default is getOption("verbose")
#'
#' @return character
#' @export
#' @references
#' \url{http://www.python.org/dev/peps/pep-0292/}
#' \url{http://stackoverflow.com/questions/8703398/conditional-gsub-replacement/8703832#8703832}
strsubst <- function(template, map, verbose=getOption("verbose")) {
  pat <- "\\$\\([^\\)]+\\)"
  res <- template
  m <- gregexpr(pat, template)
  idx <- which(sapply(m, function(x) x[[1]]!=-1)) # faster than 1:length(template)?
  for (i in idx) {
    line <- template[[i]]
    if(verbose) cat("input: |", template[[i]], "|\n")
    starts <- m[[i]]
    ml <- attr(m[[i]], "match.length")
    sym <- substring(line, starts+2, starts+ml-2)
    repl <- map[sym]
    idx1 <- is.null(repl)
    if(length(idx1)>0) {
      warning("Don't know how to replace '", sym, "'.")
      repl[idx1] <- sym[idx1]
    }
    norepl <- substring(line, c(1, starts+ml), c(starts-1, nchar(line)))
    res[[i]] <- paste(norepl, c(repl, ""), sep="", collapse="")
    if (verbose) cat("output: |", res[[i]], "|\n")
  }
  return(res)
}

#' Parse named patterns
#'
#' code based on examples for regexpr()
#'
#' @param x          character or character vector
#' @param pat        named pattern
#'
#' @return named character vector or matrix
#' @export
strparse <- function(pat, x) {
    parsed <- regexpr(pat, x, perl=TRUE)
    if (length(x)==1) {
        if(parsed[1]==-1) return(NULL)
        st <- attr(parsed, "capture.start")[1,]
        m <- substring(x, st, st + attr(parsed, "capture.length")[1,]-1)
        names(m) <- attr(parsed, "capture.names")
    } else {
        m <- do.call(rbind, lapply(seq_along(parsed), function(i) {
            if(parsed[i] == -1) return("")
            st <- attr(parsed, "capture.start")[i, ]
            substring(x[i], st, st + attr(parsed, "capture.length")[i, ] - 1)
        }))
        colnames(m) <- attr(parsed, "capture.names")
    }
    return(m)
}

#' Pattern-based recoding
#'
#' @param pats       vector of patterns
#' @param repls      vector of replacements
#' @param x          character or character vector
#' @param ...        additional parameter, passed to grepl
#'
#' @return replaced vector
#' @export
strrecode <- function(pats, repls, x, ...) {
    res <- rep(NA, length(x))
    hits <- rep(FALSE, length(x))
    for (i in seq_along(pats)) {
        new_hits <- grepl(pats[[i]],x[!hits],...)
        res[!hits][new_hits] <- repls[[i]]
        hits[!hits][new_hits] <- TRUE
        if(all(hits)) break
    }
    return(res)
}
