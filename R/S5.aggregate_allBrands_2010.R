
#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
#' @export


S5.aggregate_allBrands_2010 <- function( Market = 'LOS ANGELES',
                                         plot = T,
                                         saveData = T){

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Markets.csv", sep=""))) {

  stop("file does not exist in project directory. Run Script 4
       (S4.explore_2010marketData.R) to generate the file called:
       D4.allBrands_2010analysis_Markets.csv")

} else{

  D4.allBrands_2010analysis_Markets <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Markets.csv", sep=""))

}

if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Brands.csv", sep=""))) {

  stop("file does not exist in project directory. Run Script 4
       (S4.explore_2010marketData.R) to generate the file called:
       D4.allBrands_2010analysis_Brands.csv")

} else{

  D4.allBrands_2010analysis_Brands <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Brands.csv", sep=""))

}


if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))) {

  stop("file does not exist in project directory. Run Script 2
       (S2.construct_2010marketData.R) to generate the file called:
       D2.marketData_2010.rds")

} else{

  D2.marketData_2010 <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))

}

dta <- D2.marketData_2010[[Market]]

# Add volume measures ---------------------------------------------------------
oz <- round(data.frame(oz=dta$VOL_EQ.x* 288))
total_oz <- (oz* dta$UNITS); colnames(total_oz) <- "total_oz"

total_gal <- (0.0078125* total_oz); colnames(total_gal) <- "total_gal"

dollarPerGal <- dta$DOLLARS/ total_gal;
colnames(dollarPerGal) <- "dollarPerGal"

dta_manip <- cbind(dta, oz, total_oz, total_gal,
                            dollarPerGal)

rm(oz, total_gal, total_oz, dollarPerGal, dta)

# Remove zero data ------------------------------------------------------------
dta_manip <- dplyr::filter(dta_manip, L5 !="ALL BRAND")
dta_manip <- dplyr::filter(dta_manip, dollarPerGal !="Inf")

# Explore Brands, Firms, and percenatges of Brands by Firms -------------------
uniqueBrands <- dta_manip
colnames(uniqueBrands)[20] <- "Brands"
uniqueBrands_U <- aggregate(UNITS ~ Brands, uniqueBrands, sum)
uniqueBrands_Dol <- aggregate(DOLLARS ~ Brands, uniqueBrands, sum)
uniqueBrands_TotGal <- aggregate(total_gal ~ Brands, uniqueBrands, sum)
uniqueBrands <- dplyr::full_join(uniqueBrands_U, uniqueBrands_Dol,
                                 by = "Brands")
uniqueBrands <- dplyr::full_join(uniqueBrands, uniqueBrands_TotGal,
                                 by = "Brands")
uniqueBrands <- dplyr::arrange(uniqueBrands, desc(UNITS))

rm(uniqueBrands_Dol, uniqueBrands_U, uniqueBrands_TotGal)

selectBrands <- subset(uniqueBrands, Brands %in%
                         D4.allBrands_2010analysis_Brands[,1])

prcntBrandRep <- (sum(selectBrands[, 2]) / sum(uniqueBrands$UNITS)) * 100


if(plot == T){

  dir.create(file.path(path.local,
                       "data_beerEthnicityConsumptionBrandChoice/plots"),
             showWarnings = FALSE)

  pdf(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/plots/plot.aggregateData_", gsub('[ ,]', '', Market), ".pdf", sep=""), width=11, height = 8.5)

  par(mfrow = c(1, 2))
  x <- selectBrands[order(selectBrands$UNITS) ,]
  dotchart(x$UNITS, labels = x$Brands,
         cex=.7,
         main = paste("Market: ", Market, " - Total Units by Brand", sep=""),
         xlab = "Units")
  x <- selectBrands[order(selectBrands$DOLLARS), ]
  dotchart(x$DOLLARS, labels = x$Brands,
         cex = 0.7,
         main =paste("Market: ", Market, " - Total Dollars by Brand", sep=""),
         xlab = "Dollars")

  dev.off()

  #par(mfrow = c(1, 2))
  x <- selectBrands[order(selectBrands$UNITS) ,]

  dotchart(x$UNITS, labels = x$Brands,
           main = paste("Market: ", Market, "\n Total Units by Brand",
                        sep=""), xlab = "Units")

  x <- selectBrands[order(selectBrands$DOLLARS), ]

  dotchart(x$DOLLARS, labels = x$Brands,
           main =paste("Market: ", Market, " \n Total Dollars by Brand",
                       sep=""), xlab = "Dollars")

} else{}

aggregateDataSummary <- data.frame(Units=selectBrands$UNITS,
                                   Dollars=selectBrands$DOLLARS,
                                   Total_Gallons=selectBrands$total_gal)

rownames(aggregateDataSummary) <- selectBrands$Brands

if(saveData == T){

  dta <- list()

  dta[[1]] <- prcntBrandRep
  dta[[2]] <- aggregateDataSummary

  names(dta) <- c("prcntBrandRep", "aggregateDataSummary_Brand")

  saveRDS(dta,
          paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D5.aggregate_allBrands_2010_", gsub('[ ,]', '', Market), ".rds", sep = ""))

} else{}

endTime <- Sys.time()

endTime - startTime

}
