

S4.allBrands_2010analysis <- function( brands= c("CHICAGO",
                                                 "DALLAS, TX",
                                                 "LOS ANGELES",
                                                 "SPOKANE",
                                                 "SYRACUSE")
                                      ){

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  path.local <- getwd()

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D3.explore_2010marketData.rds", sep=""))) {

    stop("file does not exist in project directory. Run Script 3
         (S3.explore_2010marketData.R) to generate the file called:
         D3.explore_2010marketData.rds")

  } else{

    dta <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D3.explore_2010marketData.rds", sep=""))

  }

  allBrands_2010analysis <- data.frame()

  tmp <- dta[brands]

  if(length(brands) == 1){

    allBrands_2010analysis <- data.frame(unlist(tmp[[1]][1], use.names = F))
    colnames(allBrands_2010analysis) <- "Brand_Name"

  } else{

    for(i in 1:length(brands)){

      tmp1 <- data.frame(unlist(tmp[[i]][1], use.names = F))
      allBrands_2010analysis <- rbind(allBrands_2010analysis, tmp1)
      i <- i+1
    }

    freqs <- data.frame(table(allBrands_2010analysis))

    allBrands_2010analysis <- dplyr::filter(freqs, Freq==length(brands))
    allBrands_2010analysis <- data.frame(allBrands_2010analysis[,1])
    colnames(allBrands_2010analysis) <- "Brand_Name"

    allBrands_2010analysis_main <- list()
    allBrands_2010analysis_main[[2]] <- allBrands_2010analysis
    allBrands_2010analysis_main[[1]] <- data.frame(brands)
    names(allBrands_2010analysis_main) <- c("Markets", "Brand_name")

    allBrands_2010analysis <- allBrands_2010analysis_main

    saveRDS(allBrands_2010analysis,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis.rds", sep = ""))

    write.csv(allBrands_2010analysis[[2]],
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Brands.csv", sep = ""), row.names = F)

    write.csv(allBrands_2010analysis[[1]],
              paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Markets.csv", sep = ""), row.names = F)

  }
}

