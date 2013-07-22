#' German Income and Expenditure Survey 2008 on private spendings, differentiated by household type and household income.
#'
#' Germany's Sample survey of income and expenditure (Einkommens- und Verbrauchsstichprobe, EVS) is conducted by the
#' Federal Statistical Office. The data provided here is processed and is not identical with der Federal Office' data.
#' Some information is lost by the processing. If you want more and/or more accurate data,
#' contact the Federal Statistical Office.
#'
#' @name Evs-class
#' @rdname Evs-class
#' @aliases evs2008.lvl2
#' @exportClass Evs
#' @references 
#' Statistisches Bundesamt: Wirtschaftsrechnungen. Einkommens- und Verbrauchsstichprobe. Einnahmen und Ausgaben privater Haushalte. Fachserie 15 Heft 4.
#' @examples
#' xp <- expenditures()
#' queries(xp)
#' query(xp, "Categories")
#' query(xp, "Elasticity", categ="05")
setClass(
  Class="Evs", 
  contains="InternalData"
)

#' Constructor for Evs data source class
#'
#' @return an object of class "Evs"
#' @export
#' @rdname Evs-class
expenditures <- function() internalData(name="evs2008.lvl2", package="datamart", clss="Evs")


#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,Evs,Categories-method
setMethod(
  f="query",
  signature=c(self="Evs", resource=resource("Categories")),
  definition=function(self, resource, income="(all)", hhtype="(all)", relative=TRUE, ...) {
    
    dat <- subset(query(self, "evs2008.lvl2"), coicop2 != "15" & coicop2 != "00")
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


#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,Evs,Elasticity-method
setMethod(
  f="query",
  signature=c(self="Evs", resource=resource("Elasticity")),
  definition=function(self, resource, categ="", xlab="", ylab="", main=NULL, ...) {
    #if(!require(beeswarm)) stop("could not load required package 'beeswarm'")
    dat <- subset(query(self, "evs2008.lvl2"), coicop2 != "15" & coicop2 != "00" & income != "(all)")
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

#' @rdname query-methods
#' @name query
#' @export
#' @docType methods
#' @aliases query query,Evs,Elasticities-method
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

