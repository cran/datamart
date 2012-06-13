#' data object for eurostat SPARQL interface
#'
#' http://www4.wiwiss.fu-berlin.de/eurostat/
#'
#' @return a eurostat object, inherits from xsparql
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
NULL

setClass(Class="Eurostat", representation=representation(), contains="Xsparql")
eurostat <- function() {
    res <- new("Eurostat",
        url="http://www4.wiwiss.fu-berlin.de/eurostat/sparql",
        ns=c("d2r", "<http://sites.wiwiss.fu-berlin.de/suhl/bizer/d2r-server/config.rdf#>",
             "eurostat", "<http://www4.wiwiss.fu-berlin.de/eurostat/resource/eurostat/>",
             "db", "<http://www4.wiwiss.fu-berlin.de/eurostat/resource/>",
             "xsd", "<http://www.w3.org/2001/XMLSchema#>",
             "map", "<file:/C:/apps/eurostat/eurostat.n3#>",
             "rdfs", "<http://www.w3.org/2000/01/rdf-schema#>",
             "rdf", "<http://www.w3.org/1999/02/22-rdf-syntax-ns#>",
             "owl", "<http://www.w3.org/2002/07/owl#>"
            ) 
    )
    return(res)
}    

#' test query method for eurostat data object 
#'
#' Internal function, use query(eurostat(), resource, ...) instead.
#'
#' @param self       data object
#' @param resource   character describing the resource requested
#' @param opts       list of additional query parameters, currently unused
#'
#' @return a data.frame object
#' @method query eurostat
#' @name query.eurostat
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
NULL

setMethod(
  f="query",
  signature=c(self="Eurostat", resource=resource("Nuts0")),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    stmt <- paste("SELECT * WHERE {",
        "?s a eurostat:countries.",
        "?s rdfs:label ?lbl.",
        "?s eurostat:GDP ?gdp.",
        "?s eurostat:population_total ?pop.",
        "?s eurostat:electricity_consumption_GWh ?pow.}"
    )
    query(self, resource=stmt, verbose=verbose, ...)
  }
)
