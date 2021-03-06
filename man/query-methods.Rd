% Generated by roxygen2 (4.0.2): do not edit by hand
\docType{methods}
\name{query}
\alias{query}
\alias{query,CsvData,character-method}
\alias{query,DirectoryLocation,character-method}
\alias{query,GoogleOAuth2,character-method}
\alias{query,InternalData,character-method}
\alias{query,Mashup,character-method}
\alias{query,MemoryLocation2,character-method}
\alias{query,Pastebin,character-method}
\alias{query,ResFunc,character-method}
\alias{query,SftpLocation,character-method}
\alias{query,UrlData,character-method}
\alias{query,WebLocation,character-method}
\alias{query,Xdata,character-method}
\alias{query,Xsparql,character-method}
\title{Request data from data source}
\usage{
query(self, resource, ...)

\S4method{query}{Xdata,character}(self, resource, ...)

\S4method{query}{Xsparql,character}(self, resource, maxrows = NULL,
  interactive = FALSE, typeconv = TRUE, verbose = getOption("verbose"),
  ...)

\S4method{query}{InternalData,character}(self, resource, ...)

\S4method{query}{UrlData,character}(self, resource,
  verbose = getOption("verbose"), ...)

\S4method{query}{Mashup,character}(self, resource, verbose = TRUE, ...)

\S4method{query}{DirectoryLocation,character}(self, resource,
  verbose = getOption("verbose"), extract.fct = readLines, ...)

\S4method{query}{MemoryLocation2,character}(self, resource, ...)

\S4method{query}{SftpLocation,character}(self, resource,
  verbose = getOption("verbose"), ...)

\S4method{query}{Pastebin,character}(self, resource,
  verbose = getOption("verbose"), ...)

\S4method{query}{ResFunc,character}(self, resource,
  verbose = getOption("verbose"), ...)

\S4method{query}{WebLocation,character}(self, resource,
  verbose = getOption("verbose"), extract.fct = readLines, ...)

\S4method{query}{CsvData,character}(self, resource,
  verbose = getOption("verbose"), for.update = FALSE, ...)

\S4method{query}{GoogleOAuth2,character}(self, resource,
  curl = RCurl::getCurlHandle(), ...)
}
\arguments{
\item{self}{an Xdata object}

\item{resource}{an identifier of the resource requested. End-user usually provide character,
developer use \code{resource} and dispatch on the type.}

\item{...}{additional parameter}

\item{maxrows}{(Xsparql) limit of lines to return (default NULL)}

\item{interactive}{(Xsparql) if TRUE, display result in chunks (default FALSE)}

\item{typeconv}{(Xsparql) if TRUE (default), convert numbers and dates}

\item{verbose}{print diagnostic messages, default=FALSE}

\item{extract.fct}{(DirectoryLocation) which function to use to read file (default \code{readLines})}

\item{for.update}{(CsvData) update before loading (default FALSE)}

\item{curl}{(GoogleOAuth2) curl handle}
}
\description{
This generic function is the main interface to
the data behind the Xdata layer. The first argument
is the data object, the second argument is an identifier
(type character), of the resource requested.
}
\details{
Depending on the data object, additional parameter can be
provided.
}

