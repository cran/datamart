#' Example for InternalData: German Expenditure Survey
#' 
#' Shows how to use xdata with local datasets.
#' Only read-only access.
#'
#' @examples
#' xp <- expenditures()
#' queries(xp)
#' query(xp, "Categories")
#' query(xp, "Elasticity", categ="05")
#' 
#' 
#' @name Evs-class
#' @rdname Evs-class
#' @exportClass Evs
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
  Class="Evs", 
  contains="InternalData"
)

#' Constructor for Evs data source class
#'
#' Germany's Sample survey of income and expenditure (Einkommens- und Verbrauchsstichprobe, EVS) is conducted by the
#' Federal Statistical Office. The data provided here is processed and is not identical with der Federal Office' data.
#' Some information is lost by the processing. If you want more and/or more accurate data,
#' contact the Federal Statistical Office.
#'
#' @return an object of class "Evs"
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
#' @export
expenditures <- function() internalData(name="evs2008.lvl2", package="datamart", clss="Evs")


#' Example Query for German Expenditure dataset: Categories
#'
#' Returns typical expenses by Coicop2 categories. 
#' Specification of income group and household type is possible.
#' Return value either in percent (relative=TRUE, default) or in Euro.
#'
#' @param self       data object
#' @param resource   \code{Categories} object identifying the resource requested
#' @param income     income level, default "(all)"
#' @param hhtype     household type, default "(all)"
#' @param relative   if TRUE (default), return percentages, otherwise Euro
#' @param ...        other parameters, ignored
#'
#' @return vector
#' @docType methods
#' @rdname query-methods
#' @aliases query,Evs,Categories-method
setMethod(
  f="query",
  signature=c(self="Evs", resource=resource("Categories")),
  definition=function(self, resource, income="(all)", hhtype="(all)", relative=TRUE, ...) {
    
    dat <- subset(query(self, "Raw"), coicop2 != "15" & coicop2 != "00")
    income_lvls <- unique(dat$income)
    if(!income %in% income_lvls) stop("invalid 'income' argument, expected one of '", paste(income_lvls, collapse="', '"), "'.")
    hhtype_lvls <- unique(dat$hhtype)
    if(!hhtype %in% hhtype_lvls) stop("invalid 'hhtype' argument, expected one of '", paste(hhtype_lvls, collapse="', '"), "'.")
    dat <- dat[dat$income==income & dat$hhtype==hhtype,]
    if(relative) dat <- transform(dat, value=value/sum(value))
    res <- dat$value
    names(res) <- dat$coicop2de
    return(res)
  }
)


#' Example Query for German Expenditure dataset: Elasticity
#'
#' Plots expenditures by income group for a given category.
#'
#' @param self       data object
#' @param resource   \code{Elasticity} object identifying the resource requested
#' @param categ      category for which to plot income elasticity.
#' @param ...        other parameters, passed to beeswarm
#'
#' @return vector
#' @docType methods
#' @rdname query-methods
#' @aliases query,Evs,Elasticity-method
setMethod(
  f="query",
  signature=c(self="Evs", resource=resource("Elasticity")),
  definition=function(self, resource, categ="", xlab="", ylab="", main=NULL, ...) {
    #if(!require(beeswarm)) stop("could not load required package 'beeswarm'")
    dat <- subset(query(self, "Raw"), coicop2 != "15" & coicop2 != "00" & income != "(all)")
    cat_lvls <- unique(dat$coicop2)
    if (!categ %in% cat_lvls) stop("invalid 'categ' argument, expected one of '", paste(cat_lvls, collapse="', '"), "'.")
    
    income_lvls <- c("lt900", "900to1300", "1300to1500", "1500t2000", "2000t2600",
                    "2600t3600", "3600t5000", "5000t18000")
    dat$income <- factor(dat$income, levels=income_lvls)
    dat <- subset(dat, coicop2==categ)
    if(is.null(main)) main <- dat[1, "coicop2de"]
    
    par(mar=c(2,2,3,0)+0.1)
    boxplot(value ~ income, data=dat, ylab=ylab, main=main, ylim=c(0, 1000), ...)
    
    return(dat)
  }
)

#' Example Query for German Expenditure dataset: Trellis Plot of Elasticities
#'
#' Plots expenditures by income group for two categories.
#'
#' @param self       data object
#' @param resource   \code{Elasticities} object identifying the resource requested
#' @param ...        other parameters, passed to beeswarm
#'
#' @return vector
#' @docType methods
#' @rdname query-methods
#' @aliases query,Evs,Elasticities-method
setMethod(
  f="query",
  signature=c(self="Evs", resource=resource("Elasticities")),
  definition=function(self, resource, categ="", xlab="", ylab="", ...) {
    par(mfrow=c(2,1))
    # query(self, "Elasticity", categ="01/02", ...)
    # query(self, "Elasticity", categ="03", ...)
    # query(self, "Elasticity", categ="04", ...)
    # query(self, "Elasticity", categ="05", ...)
    query(self, "Elasticity", categ="06", ...)
    query(self, "Elasticity", categ="07", ...)
  }
)
# '01/02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'

