

marketData <- function(){

  path.local <- getwd()
  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/main_beer_drug_and_groc_4_2010.feather", sep=""))){

    stop(print("file does not exist"))
  } else{

  main_beer_drug_and_groc_4_2010 <- feather::read_feather("data_beerEthnicityConsumptionBrandChoice/main_beer_drug_and_groc_4_2010.feather")

# Show all potiential market names --------------------------------------------

mrkNames <- data.frame(Market_Name =
                         unique(main_beer_drug_and_groc_4_2010$Market_Name))

mrkNames <- dplyr::arrange(mrkNames, Market_Name)


# create LA Market Data -------------------------------------------------------

market1 <- "LOS ANGELES"

LA_data_2010 <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                              Market_Name == market1)

LA_data_2010 <- LA_data_2010[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36, 53:56,
                                 71:76)]

# count <- 7
# setTxtProgressBar(pb, count)


# create Chicago Market Data --------------------------------------------------

market2 <- "CHICAGO"

CHICAGO_data_2010 <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                                   Market_Name == market2)

CHICAGO_data_2010 <- CHICAGO_data_2010[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                           53:56, 71:76)]

# count <- 8
# setTxtProgressBar(pb, count)


# create Dallas Market Data ---------------------------------------------------

market3 <- "DALLAS, TX"

DALLAS_data_2010 <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                                  Market_Name == market3)

DALLAS_data_2010 <<- DALLAS_data_2010[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                         53:56, 71:76)]




# if(save_2010_main_city_data == TRUE) {
#
#   # devtools::use_data(LA_data_2010, overwrite = T)
#   #
#   # devtools::use_data(CHICAGO_data_2010, overwrite = T)
#   #
#   # devtools::use_data(DALLAS_data_2010, overwrite = T)
#
# } else {
#
# }

  }

}





