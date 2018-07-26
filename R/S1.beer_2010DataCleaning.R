



S1.beer_2010DataCleaning <- function(save_2010_main_data = 'all_formats') {

  startTime <- Sys.time()

  # feather <- 'feather'
  # rds <- 'rds'
  # csv <- 'csv'
  # all_formats <- 'all_formats'

  totalCount <- 10
  pb <- txtProgressBar(min = 0, max = totalCount, style = 3)
  count <- 1
  setTxtProgressBar(pb, count)

  path_local <- getwd()

  dir.create(file.path(path_local,
                       "data_beerEthnicityConsumptionBrandChoice"),
             showWarnings = FALSE)

  path.pkg <- system.file(package = "beerEthnicityConsumptionBrandChoice")

# import raw IRI data ---------------------------------------------------------

  beer_drug_1583_1634 <- read.csv(
    paste(path.pkg, "/extdata/beer_drug_1583_1634",
          sep = ""), sep = "", quote = "")

  beer_groc_1583_1634 <- read.csv(
    paste(path.pkg, "/extdata/beer_groc_1583_1634",
          sep = ""), sep = "", quote = "")

  count <- 2
  setTxtProgressBar(pb, count)

  Delivery_Stores <- suppressMessages(readr::read_table(
    paste(path.pkg, "/extdata/Delivery_Stores", sep = "")))

  IRI_week_translation_2008_2017 <- readxl::read_excel(
  paste(path.pkg, "/extdata/IRI week translation_2008_2017.xls", sep = ""),
  col_types = c("numeric", "date", "date", "skip", "skip", "skip"))

  beer_prod_attr_2011_edit <- suppressMessages(
  readr::read_table(paste(path.pkg, "/extdata/beer_prod_attr_2011_edit",
                          sep = "")))

  prod11_beer <- readxl::read_excel(
    paste(path.pkg, "/extdata/prod11_beer.xlsx", sep = ""))

  count <- 3
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

  count <- 4.5
  setTxtProgressBar(pb, count)

  temp_main <- data.frame(upc = apply(temp, MARGIN = 1, upc_fun))

  count <- 5.75
  setTxtProgressBar(pb, count)

  main_beer_drug_and_groc <- cbind(main_beer_drug_and_groc, temp_main)

  count <- 6
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

  count <- 7
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

  count <- 8
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

  count <- 9
  setTxtProgressBar(pb, count)


# -----------------------------------------------------------------------------
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# It is a good idea to save "main_beer_drug_and_groc_4_2010" to disk
# This is the starting point for the analysis...
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# -----------------------------------------------------------------------------

  if(save_2010_main_data == 'feather'){

    feather::write_feather(
      main_beer_drug_and_groc_4_2010,
      paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep = ""))

    } else if(save_2010_main_data == 'rds') {

      saveRDS(main_beer_drug_and_groc_4_2010,
              paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.rds", sep = ""))

      } else if(save_2010_main_data == 'csv') {

        write.csv(main_beer_drug_and_groc_4_2010,
                paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.csv", sep = ""))

      } else if(save_2010_main_data == 'all_formats') {

        feather::write_feather(
          main_beer_drug_and_groc_4_2010,
          paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep = ""))

        saveRDS(main_beer_drug_and_groc_4_2010,
                paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.rds", sep = ""))

        write.csv(main_beer_drug_and_groc_4_2010,
                  paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.csv", sep = ""))

      }

  count <- 9.5
  setTxtProgressBar(pb, count)


# Show all potiential market names --------------------------------------------

  mrkNames_2010 <- data.frame(Market_Name =
                           unique(main_beer_drug_and_groc_4_2010$Market_Name))

  mrkNames_2010 <- dplyr::arrange(mrkNames_2010, Market_Name)

  saveRDS(mrkNames_2010,
            paste(path_local, "/data_beerEthnicityConsumptionBrandChoice/D1.mrkNames_2010.rds", sep = ""))


  count <- 10
  setTxtProgressBar(pb, count)

  close(pb)

  endTime <- Sys.time()

  endTime - startTime

}

