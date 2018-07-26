

S2.construct_2010marketData <- function(fileType = 'feather',
                            marketNames = c("LOS ANGELES",
                                           "CHICAGO",
                                           "DALLAS, TX")) {

  # csv = "csv"
  # rds = "rds"
  # feather = "feather"

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

    marketData_2010 <- list()

    for(i in seq_along(marketNames)) {

    market <- marketNames[i]

    tmp <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                              Market_Name == market)

    marketData_2010[[i]] <- tmp[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                  53:56, 71:76)]
    i = i + 1
    }

    names(marketData_2010) <- marketNames

    saveRDS(marketData_2010,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep = ""))


  }

}
