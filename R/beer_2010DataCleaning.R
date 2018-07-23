



beer_2010DataCleaning <- function(save_2010_main_data = internal,
                                  save_2010_main_city_data = TRUE){
  startTime <- Sys.time()

  internal <- "internal"
  archival <- "archival"

  totalCount <- 10
  pb <- txtProgressBar(min = 0, max = totalCount, style = 3)
  count <- 1
  setTxtProgressBar(pb, count)

  path <- system.file(package = "beerEthnicityConsumptionBrandChoice")

# import raw IRI data ---------------------------------------------------------

  beer_drug_1583_1634 <- read.csv(paste(path, "/extdata/beer_drug_1583_1634",
                                      sep = ""), sep = "", quote = "")

  beer_groc_1583_1634 <- read.csv(paste(path, "/extdata/beer_groc_1583_1634",
                                      sep = ""), sep = "", quote = "")

  Delivery_Stores <- suppressMessages(readr::read_table(
    paste(path, "/extdata/Delivery_Stores", sep = "")))

  IRI_week_translation_2008_2017 <- readxl::read_excel(
  paste(path, "/extdata/IRI week translation_2008_2017.xls", sep = ""),
  col_types = c("numeric", "date", "date", "skip", "skip", "skip"))

  beer_prod_attr_2011_edit <- suppressMessages(
  readr::read_table(paste(path, "/extdata/beer_prod_attr_2011_edit",
                          sep = "")))

  prod11_beer <- readxl::read_excel(
    paste(path, "/extdata/prod11_beer.xlsx", sep = ""))

  count <- 2
  setTxtProgressBar(pb, count)

# join grocery store and drug store data --------------------------------------

  main_beer_drug_and_groc <- rbind(beer_drug_1583_1634, beer_groc_1583_1634)


# make UPC for main_beer_drug_and_groc ----------------------------------------

  upc_fun <- function(x){

    paste(

    stringr::str_pad(x[1], 2, "left", pad = "0"),

    stringr::str_pad(x[2], 2, "left", pad = "0"),

    stringr::str_pad(x[3], 5, "left", pad = "0"),

    stringr::str_pad(x[4], 5, "left", pad =  "0"), sep = "-")

    }

  temp <- main_beer_drug_and_groc[, 3:6]

  temp_main <- data.frame(upc = 1:nrow(temp))

  count <- 2.5
  setTxtProgressBar(pb, count)

  temp_main <- data.frame(upc = apply(temp, MARGIN = 1, upc_fun))

  count <- 2.75
  setTxtProgressBar(pb, count)

  main_beer_drug_and_groc <- cbind(main_beer_drug_and_groc, temp_main)

  count <- 3
  setTxtProgressBar(pb, count)


# join week codes, (IRI_week...) with main_beer_drug_and_groc file ------------

  main_beer_drug_and_groc_1 <- dplyr::left_join(x = main_beer_drug_and_groc,
                                       y = IRI_week_translation_2008_2017,
                                       by = c("WEEK" = "IRI Week"))


# make UPC for beer_prod_attr_2011_edit ---------------------------------------

  temp <- beer_prod_attr_2011_edit[, 1:4]

  temp_main <- data.frame(upc = 1:nrow(temp))

  temp_main <- data.frame(upc = apply(temp, MARGIN = 1, upc_fun))

  beer_prod_attr_2011_edit <- cbind(beer_prod_attr_2011_edit, temp_main)

  count <- 4
  setTxtProgressBar(pb, count)

# join main_beer_drug_and_groc_2 to prod11_beer and prod_beer_attr_2011_edit --

  main_beer_drug_and_groc_1[, 12] <- as.character(
    main_beer_drug_and_groc_1$upc)

  beer_prod_attr_2011_edit[, 37] <- as.character(beer_prod_attr_2011_edit$upc)

  main_beer_drug_and_groc_2 <- dplyr::left_join(x = main_beer_drug_and_groc_1,
                                                y = beer_prod_attr_2011_edit,
                                                by = c("upc" = "upc"))

  main_beer_drug_and_groc_3 <- dplyr::left_join(x = main_beer_drug_and_groc_2,
                                                y = prod11_beer,
                                                by = c("upc" = "UPC"))


# clean up large data.frames !!!!!!! caution, removes data from memory !!!!!!!!

  rm(upc_fun,
     prod11_beer,
     temp, temp_main,
     beer_drug_1583_1634,
     beer_groc_1583_1634,
     main_beer_drug_and_groc,
     beer_prod_attr_2011_edit,
     main_beer_drug_and_groc_1,
     main_beer_drug_and_groc_2,
     IRI_week_translation_2008_2017)

  count <- 5
  setTxtProgressBar(pb, count)


# join Delivery_Stores to main_beer_drug_and_groc -----------------------------

  # remove duplicate IRI_Key numbers
  unique_Delivery_stores <- dplyr::distinct(Delivery_Stores)

  main_beer_drug_and_groc_4_2010  <- dplyr::left_join(
    x = main_beer_drug_and_groc_3,
    y = unique_Delivery_stores,
    by = "IRI_KEY")


# clean up large data.frames !!!!!!! caution, removes data from memory !!!!!!!!

  rm(Delivery_Stores,
     unique_Delivery_stores,
     main_beer_drug_and_groc_3)

  count <- 6
  setTxtProgressBar(pb, count)


# -----------------------------------------------------------------------------
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# It is a good idea to save "main_beer_drug_and_groc_4_2010" to disk
# This is the starting point for the analysis...
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------------------------------------------------------------
# keep this data file in directory: data-raw

#devtools::use_data(main_beer_drug_and_groc_4_2010)

# if needed, import "main_beer_drug_and_groc_4_2010.rda" ----------------------

#load("data-raw/main_beer_drug_and_groc_4_2010.rda")

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

  count <- 7
  setTxtProgressBar(pb, count)


# create Chicago Market Data --------------------------------------------------

  market2 <- "CHICAGO"

  CHICAGO_data_2010 <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                                     Market_Name == market2)

  CHICAGO_data_2010 <- CHICAGO_data_2010[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                           53:56, 71:76)]

  count <- 8
  setTxtProgressBar(pb, count)


# create Dallas Market Data ---------------------------------------------------

  market3 <- "DALLAS, TX"

  DALLAS_data_2010 <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                                    Market_Name == market3)

  DALLAS_data_2010 <- DALLAS_data_2010[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                          53:56, 71:76)]


  if(save_2010_main_data == internal){

    feather::write_feather(
      main_beer_drug_and_groc_4_2010,
      paste(path, "/extdata/tmpData/main_beer_drug_and_groc_4_2010.feather",
            sep = ""))

    } else if(save_2010_main_data == archival){

      devtools::use_data(main_beer_drug_and_groc_4_2010, overwrite = T)

      } else if(save_2010_main_data == FALSE) {

      }

  count <- 9
  setTxtProgressBar(pb, count)

  if(save_2010_main_city_data == TRUE) {

    devtools::use_data(LA_data_2010, overwrite = T)

    devtools::use_data(CHICAGO_data_2010, overwrite = T)

    devtools::use_data(DALLAS_data_2010, overwrite = T)

  } else {

    }


  count <- 10
  setTxtProgressBar(pb, count)

  close(pb)

  endTime <- Sys.time()

  endTime - startTime

}

