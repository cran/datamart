#' A class for querying Dbpedia.org
#' 
#' This class defines some resources at dbpedia.
#' See \code{queries(dbpedia())} for a list of resources.
#'
#' @examples
#' \dontrun{
#'   dbp <- dbpedia()
#'   queries(dbpedia)
#'   query(dbp, "Nuts1")
#' }
#' 
#' @name Dbpedia-class
#' @rdname Dbpedia-class
#' @exportClass Dbpedia
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(Class="Dbpedia", representation=representation(), contains="Xsparql")


#' A data object for querying dbpedia
#'
#' @return a Dbpedia object, inherited from Xsparql
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
dbpedia <- function() {
    res <- new("Dbpedia",
        url="http://dbpedia.org/sparql", 
        ns=c("dbo", "<http://dbpedia.org/ontology/>",
             "rdf", "<http://xmlns.com/foaf/0.1>",
             "rdfs", "<http://www.w3.org/2000/01/rdf-schema#>",
             "owl", "<http://www.w3.org/2002/07/owl#>",
             "xsd", "<http://www.w3.org/2001/XMLSchema#>",
             "dc", "<http://purl.org/dc/elements/1.1/>",
             "foaf", "<http://xmlns.com/foaf/0.1/>",
             #"", "<http://dbpedia.org/resource/>", 
             "dbpedia2", "<http://dbpedia.org/property/>", 
             "dbpedia", "<http://dbpedia.org/>",
             "skos", "<http://www.w3.org/2004/02/skos/core#>"             
             )
    )
    return(res)
}

#' Query method for dbpedia data object 
#'
#' Internal function, use query(dbpedia(), "Nuts1", ...) instead.
#'
#' @param self       data object
#' @param resource   character describing the resource requested
#' @param verbose    if TRUE, diagnostic messages (default getOption("verbose"))
#'
#' @return a data.frame object
#' @docType methods
#' @rdname query-methods
#' @aliases query,Dbpedia,Nuts1-method
setMethod(
  f="query",
  signature=c(self="Dbpedia", resource=resource("Nuts1")),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    if(verbose) cat("query Dbpedia#Nuts1\n")
    stmt <- paste(
      "SELECT ?name, ?nuts, ?popDate, ?pop, ?area, ?gdp, ?popMetro WHERE {",
      "  ?s a <http://dbpedia.org/class/yago/StatesOfGermany>;",
      "     <http://dbpedia.org/property/nuts> ?nuts;",
      "     rdfs:label ?name . ",
      "  OPTIONAL { ?s <http://dbpedia.org/ontology/populationAsOf> ?popDate }",
      "  OPTIONAL { ?s <http://dbpedia.org/property/population> ?pop }",
      "  OPTIONAL { ?s <http://dbpedia.org/property/popMetro> ?popMetro }",
      "  OPTIONAL { ?s <http://dbpedia.org/property/gdp> ?gdp }",
      "  OPTIONAL { ?s <http://dbpedia.org/ontology/areaTotal> ?area } . ",
      "  FILTER (LANG(?name)='de') }",
      sep=""
    )
    query(self, resource=stmt, verbose=verbose, ...)
  }
)


