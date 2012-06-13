#' A wrapper class for Shapefiles
#' 
#' Polygons, and a convience query for plotting.
#' 
#' @name GeoData-class
#' @rdname GeoData-class
#' @exportClass GeoData
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
setClass(
  Class="GeoData", 
  contains="Xdata",
  representation=representation(shp="SpatialPolygonsDataFrame")
)

#' Constructor for GeoData, ESRI shapefiles 
#'
#' @param name  name of the dataset. Required.
#' @param package name of the package where the dataset is located. Default NULL.
#' @param clss  name of the class to create. Default InternalData, must be inherited from this class.
#'
#' @export
#' @author Karsten Weinert \email{k.weinert@@gmx.net}
shapefile <- function(directory, filename, clss="GeoData") {
    if(!require(rgdal)) stop("could not load required package 'rgdal'")
    if(!require("sp")) stop("could not load required package 'sp'")
    shp <- readOGR(directory, filename)
    new(clss, shp=shp)
}

#' Return the Column names of the data in the GeoData
#'
#' @param self     a GeoData object
#' @param resource an object of class DataColNames, usually created by query.xdata
#' @param ...      additional arguments, ignored.
#'
#' @docType methods
#' @rdname query-methods
#' @aliases query,GeoData,DataColNames-method
#' @export
setMethod(
  f="query",
  signature=c(self="GeoData", resource=resource("DataColNames")),
  definition=function(self, resource, ...) colnames(self@shp@data)
)

#' Plots a thematic aka choropleth map given metric data. Cuts the data in intervals before.
#'
#' @param self      a GeoData object
#' @param resource  an object of class PlotMetric, usually created by query.xdata.
#' @param x         Either a vector of numeric values to plot (names(x) is used to match) or a column name of the shape
#'                  file data to plot. Call query(x, "DataColNames") for a list of available columns.
#' @param by        the column name of the shape file data to use for merge. By default, the first column is used.
#'                  Call query(x, "DataColNames") for a list of available columns.
#' @param cuts      number of categories in the choropleth map.
#' @param pal       palette. A list of palette names is given by display.brewer.all(type="seq")
#' @param style     method for determining the categories. See classInt::classIntervals for details.
#'                  must be in c("equal", "pretty", "quantile", "kmeans", "hclust",
#'                  "bclust", "fisher", "jenks"), default="kmeans".
#' @param fixedBreaks only interpreted when style=='fixed', a list of three numeric vector of length cuts-1, to manually set the size categories.
#' @param inverse.color if TRUE, the colors are reversed (e.g. from dark to light instead from light to dark). Default is FALSE.
#' @param border    color of the shape border. Default is grey(0.9).
#' @param legend.x  x position of the legend. If NULL, no legend is displayed.
#' @param legend.y  y position of the legend. Can be omitted if legend.x is one of "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right" and 
#'                  "center". 
#' @param legend.unit      legend.unit to display in the legend. Default "".
#' @param cex       plot-wide cex parameter.
#' @param main      plot title, default none.
#' @param col.main  color of plot title, default "black"
#' @param ...       additional arguments, ignored.
#'
#' @docType methods
#' @rdname query-methods
#' @aliases query,GeoData,PlotMetric-method
#' @export
setMethod(
  f="query",
  signature=c(self="GeoData", resource=resource("PlotMetric")),
  definition=function(
    self, resource, x, by=1, cuts=4, pal="Greys", style="kmeans", 
    fixedBreaks=NULL, inverse.color=FALSE, border=grey(0.9), 
    legend.x=NULL, legend.y=NULL, legend.digits=0, legend.unit="", cex=0.6, main="", col.main="black", ...
    ) {
    
    if(!require("RColorBrewer")) stop("could not load required package 'RColorBrewer'")
    if(cuts<2 || cuts > 9) stop("invalid cuts argument; must be between 2 and 9")
    styles <- c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks")
    if(!(style %in% styles)) stop("invalid style argument: expected one of '", paste(styles, collapse="', '"), "'.")
    if(is.character(x)) {
     if(length(x)!=1 || !(x %in% colnames(self@shp@data)) || !is.numeric(self@shp@data[,x])) 
        stop("invalid x argument, either numeric vector or column name of the shp data expected.")
    } else if(!is.numeric(x)) {
        stop("invalid plot data; numeric expected.")
    } else if(length(x)!=length(self@shp)) {
        stop("invalid length of plot data; expected vector of length ", length(self@shp), ", got length ", length(x), " instead.")
    } else if(is.null(names(x))) {
        names(x) <- seq_along(x)
        warning("plot data without names, taking index numbers")
    }
    if(style=="fixed" && is.null(fixedBreaks)) stop("style 'fixed' requires additional fixedBreaks argument.")
    if(style=="fixed") cuts <- length(fixedBreaks)+1
  
    shp <- self@shp
    if(is.character(x)) 
        plot_var <- shp@data[,x]
    else
        plot_var <- x[as.character(shp@data[,by])]

    plot_pal <- brewer.pal(cuts, pal) #"OrRd"
    if(inverse.color) plot_pal <- plot_pal[cuts:1] 
    
    if(style!="fixed") 
        plot_intvl <- classIntervals(plot_var, cuts, style=style) 
    else 
        plot_intvl <- classIntervals(plot_var, n=cuts, style="fixed", fixedBreaks=c(-Inf, fixedBreaks, Inf))
    plot_colors <- findColours(plot_intvl, plot_pal)
    plot(shp, col=plot_colors, border=border)
    if(main!="") title(main, col.main=col.main)
    #text(coordinates(map_nw), labels=map_nw$kfz, cex=0.9, col="darkgrey")
    if(!is.null(legend.x) && legend.x != "") {
        plot_lbls <- format(round(plot_intvl$brks[seq(2, length(plot_intvl$brks)-1)], digits=legend.digits), big.mark=".", scientific=FALSE)
        plot_lbls <- c(
            paste("<", head(plot_lbls[1],1), legend.unit),
            paste(head(plot_lbls,-1), "-", tail(plot_lbls,-1), legend.unit),
            paste(">", tail(plot_lbls,1), legend.unit)
        )
        legend(
            x=legend.x,
            y=legend.y,
            legend=plot_lbls, 
            fill=attr(plot_colors, "palette"),
            bty="n",
            border=attr(plot_colors, "palette")
        )
    }
    return(NULL)
})





