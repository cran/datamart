#' Gapminder data source.
#'
#' Gapminder describes itself as a "fact tank" that promotes a fact based world view. 
#' On their website they provide a service that allows to create animated charts for various indicators, differentiated by country.
#' They also provide the underlying datasets for download. This S3 class serves as a wrapper for easy access to a subset of these data.
#'
#' Please note that neither Gapminder nor the package developer/maintainer are the data provider, except for a few cases.
#' Therefore you will have to go to the source to find out the terms of use for the specific indicator.
#'
#' This class defines some resources of the Gapminder Project.
#' See \code{queries(gapminder())} for a list of resources.
#'
#' @examples
#' \dontrun{
#'   gm <- gapminder()
#'   queries(gm)
#'   query(gm, "ReligionAndBabies")
#' }
#' 
#' @name Gapminder-class
#' @rdname Gapminder-class
#' @exportClass Gapminder
setClass(Class="Gapminder", representation=representation(), contains="UrlData")

#' Constructor for Gapminder class
#'
#' See Gapminder-class for details.
#'
#' @return Gapminder
#' @references 
#' \url{http://www.gapminder.org}
#' @export
gapminder <- function() urldata(
  template="https://docs.google.com/spreadsheet/pub?key=%s&output=csv",
  extract.fct=function(uri) getURL(uri, cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")),
  transform.fct=function(x) {
    res <- read.csv(textConnection(x), na.strings=c("..", "-"), stringsAsFactor=FALSE)
    
    tm <- try(as.Date(paste(substring(tail(colnames(res),-1),2),1,1,sep="-0")), silent=TRUE)
    if(!inherits(tm, "try-error")) { # as xts, if applicable
      nm <- res[,1]
      res <- res[,2:ncol(res)]
      
      res <- as.numeric(gsub("[',]", "", as.matrix(res)))
      res <- matrix(res, nrow=length(tm), ncol=length(nm), byrow=TRUE)
      res <- xts(res, tm)
      names(res) <- nm
    }
    return(res)
  },
  map.lst=list(
    "Population"="phAwcNAVuyj0XOoBL_n5tAQ&gid=0",
    "MainReligion"="0ArtujvvFrPjVdHUzTGVicFJZQ1NjaFhqelV5SDNxMVE&gid=1",
    "TotalFertilityRate"="phAwcNAVuyj0TAlJeCEzcGQ&gid=0",
    "PerCapitaCO2Emissions"="phAwcNAVuyj1gkNuUEXOGag&gid=0",
    "IncomePerCapita"="phAwcNAVuyj1jiMAkmq1iMg&gid=0",
    "InfantMortalityRate"="phAwcNAVuyj0NpF2PTov2Cw&gid=0",
    "LifeExpectancyAtBirth"="phAwcNAVuyj2tPLxKvvnNPA&gid=0",
    "AdolescentFertilityRate"="pyj6tScZqmEdIphYUHxcdLg&gid=0",
    "BirthsAttendedBySkilledHealthStaff"="pyj6tScZqmEfKY9bk02DBYA&gid=0",
    "ContraceptiveUse"="pyj6tScZqmEewsQOoKrtYJQ&gid=0",
    "CrudeBirthRate"="tUSeGJOQhafugwUvHvY-wLA&gid=0",
    "MaternalMortalityRate"="pyj6tScZqmEcVezxiMlWaRw&gid=0",
    "Under5MortalityRate"="phAwcNAVuyj05ZR69usyQIg&gid=0",
    "CrudeDeathRate"="tHyj-2jRvK3CCNJOc5Vm-HQ&gid=0",
    "PopulationGrowth"="pyj6tScZqmEcl2xDWbuJ8fg&gid=0",
    "SugarConsumption"="phAwcNAVuyj2sdmdhX9zuKg&gid=0",
    "GDP"="pyj6tScZqmEfI4sLVvEQtHw&gid=0",
    "ConsumerPricesIndex"="pyj6tScZqmEc3xNIyXiZ6EA&gid=0",
    "GDPImplicitDeflator"="pyj6tScZqmEcaHt8Y6cxXQg&gid=0",
    "CoalConsumption"="pyj6tScZqmEc1TmMiFdmOVg&gid=0",
    "HydroelectricityConsumption"="pyj6tScZqmEdNbX4qj9QLTA&gid=0",
    "NaturalGasConsumption"="pyj6tScZqmEcx9pD804Q0Aw&gid=0",
    "NuclearConsumption"="pyj6tScZqmEfiy57wnt-tEA&gid=0",
    "OilConsumption"="pyj6tScZqmEcm0fIa0IVtKw&gid=0",
    "CoalProduction"="pyj6tScZqmEdDid2ts7KvHg&gid=0",
    "ElectricityGeneration"="pyj6tScZqmEehRG-9mMHYdg&gid=0",
    "NaturalGasProduction"="pyj6tScZqmEfv2K6dZmskWg&gid=0",
    "OilProduction"="pyj6tScZqmEdNIa3ckVXaCQ&gid=0",
    "PrimaryEnergyConsumption"="pyj6tScZqmEeTCOezV8a3HA&gid=0",
    "CO2Emissions"="phAwcNAVuyj1NHPC9MyZ9SQ&gid=0",
    "SulfurEmissions"="t9SYWh7siLJDzyZYN1R4HfQ&gid=0",
    "TotalForestArea"="pp59adS3CHWeB1N1HlpFQVQ&gid=0",
    "PrimaryForestArea"="pp59adS3CHWeECA6Gf__BNQ&gid=0",
    "PlantedForestArea"="pp59adS3CHWc4aJd9fV8zZg&gid=0",
    "WoodRemoval"="pp59adS3CHWe8O-N9RgxzDw&gid=0",
    "BiomassStockInForest"="pp59adS3CHWcsSl830EklJA&gid=0",
    "TotalWaterWithdrawal"="rIG3ZWxv381t2bIL2BNaIVw&gid=0",
    "SurfaceArea"="pyj6tScZqmEeiMy8j86qDTg&gid=0",
    "BadTeethPerChild"="phAwcNAVuyj3Os9LVO_pRDA&gid=0",
    "PeopleLivingWithHIV"="pyj6tScZqmEe1GaiYJX2qGA&gid=0",
    "MalariaReportedCases"="pp59adS3CHWczfPHQMiqxCg&gid=0",
    "MalariaReportedDeaths"="pp59adS3CHWfZGL9qouvTbQ&gid=0",
    "WorkingHoursPerWeek"="rIMebcn9Eo2jSIm09HBLihg&gid=0",
    "UrbanPopulation"="pyj6tScZqmEfH89V6UQhpZA&gid=0",
    "WomensAgeAtFirstMarriage"="t4eF8H_jq_xyKCUHAX6VT1g&gid=0",
    "NumberOfBillionaires"="tNWhbu-1UIPPxtmRHtnINOQ&gid=0",
    "GiniIndex"="pyj6tScZqmEcjeKHnZq6RIg&gid=0",
    "BroadbandSubscribers"="pyj6tScZqmEcuy6dYkzGhfw&gid=0",
    "CellPhones"="pyj6tScZqmEcKuNdFCUo6TQ&gid=0",
    "PersonalComputers"="pyj6tScZqmEfUXdC83YSzfw&gid=0",
    "PatentApplications"="pyj6tScZqmEd5FA9xlfO9eA&gid=0",
    "PatentsGranted"="pyj6tScZqmEdMioz5VJKXHw&gid=0",
    "PatentsInForce"="pyj6tScZqmEe371ZVZl73eA&gid=0",
    "ArmsExports"="pyj6tScZqmEeTIhjRrVQtQA&gid=0",
    "ArmsImports"="pyj6tScZqmEfnPl7VRfT9WA&gid=0",
    "HumanDevelopmentIndex"="tyadrylIpQ1K_iHP407374Q&gid=0"
  ),
  clss="Gapminder"
)

#' @rdname query-methods
#' @aliases query,Gapminder,ReligionAndBabies,missing-method
setMethod(
  f="query",
  signature=c(self="Gapminder", resource=resource("ReligionAndBabies"), dbconn="missing"),
  definition=function(self, resource, verbose=getOption("verbose"), ...) {
    if(verbose) cat("query Gapminder#ReligionAndBabies\n")

    # babies per woman
    tmp <- query(self, "TotalFertilityRate")
    babies <- as.vector(tmp["2008"])
    names(babies) <- names(tmp)
    babies <- babies[!is.na(babies)]
    countries <- names(babies)
    
    # income per capita, PPP adjusted
    tmp <- query(self, "IncomePerCapita")
    income <- as.vector(tmp["2008"])
    names(income) <- names(tmp)
    income <- income[!is.na(income)]
    countries <- intersect(countries, names(income))
    
    # religion
    tmp <- query(self, "MainReligion")
    religion <- tmp[,"Group"]
    names(religion) <- tmp[,"Entity"]
    religion[religion==""] <- "unknown"
    colcodes <- c(
      Christian="blue", 
      "Eastern religions"="red", 
      Muslim="green", "unknown"="grey"
    )
    countries <- intersect(countries, names(religion))
    
    # plot
    par(mar=c(4,4,0,0)+0.1)
    plot(
      x=income[countries], 
      y=babies[countries], 
      col=colcodes[religion[countries]], 
      log="x",
      xlab="Income per Person, PPP-adjusted", 
      ylab="Babies per Woman"
    )
    legend(
      "topright", 
      legend=names(colcodes), 
      fill=colcodes, 
      border=colcodes
    )
    return(invisible(NULL))
  }
)
