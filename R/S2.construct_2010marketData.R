

S2.construct_2010marketData <- function(fileType = 'feather',
                            marketNames = c("LOS ANGELES",
                                           "CHICAGO",
                                           "DALLAS, TX")) {

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  totalCount <- 5
  pb <- txtProgressBar(min = 0, max = totalCount, style = 3)
  count <- 1
  setTxtProgressBar(pb, count)

  marketNames <- data.frame(marketNames)

  path.local <- getwd()

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.", fileType, sep=""))) {

  stop("file does not exist in project directory. Run Script 1
             (S1.beer_2010DataCleaning) to generate the file called:
             D1.main_beer_drug_and_groc_4_2010.(feather, csv, rds)")

  } else{

    if(fileType == "csv"){

    main_beer_drug_and_groc_4_2010 <- suppressWarnings(read_csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.csv", sep=""), col_types = cols(X1 = col_skip())))

    } else if(fileType == "rds") {

      main_beer_drug_and_groc_4_2010 <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.rds", sep=""))

    } else if(fileType == "feather") {

      main_beer_drug_and_groc_4_2010 <- feather::read_feather(paste(path.local,"/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep=""))

    }

    count <- 2
    setTxtProgressBar(pb, count)

    if(marketNames[1,1] == 'all'){

      marketNames <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.mrkNames_2010.rds", sep=""))

    } else{

    }

    count <- 3
    setTxtProgressBar(pb, count)

    marketData_2010 <- list()

    for(i in 1:nrow(marketNames)) {

      market <- marketNames[i,1]

      tmp <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                           Market_Name == market)

      marketData_2010[[i]] <- tmp[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                      53:56, 71:76)]
      i = i + 1
    }

    count <- 4
    setTxtProgressBar(pb, count)

    names(marketData_2010) <- marketNames[,1]

    saveRDS(marketData_2010,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep = ""))

  }

  count <- 5
  setTxtProgressBar(pb, count)

  close(pb)

  endTime <- Sys.time()

  endTime - startTime
}
