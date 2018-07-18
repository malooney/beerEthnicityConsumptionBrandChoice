

library(readr)
library(readxl)
library(dplyr)
library(stringr)

library(devtools)

# import raw IRI data ---------------------------------------------------------

beer_drug_1583_1634 <- read.csv("~/Documents/IRI_Original_Data/Year 10 and 11 DVD/Year10/beer/beer_drug_1583_1634", sep="")

beer_groc_1583_1634 <- read.csv("/Users/malooney/Documents/IRI_Original_Data/Year 10 and 11 DVD/Year10/beer/beer_groc_1583_1634", sep="")

Delivery_Stores <- read_table("/Users/malooney/Documents/IRI_Original_Data/Year 10 and 11 DVD/Year10/beer/Delivery_Stores")

IRI_week_translation_2008_2017 <- read_excel("/Users/malooney/Documents/IRI_Original_Data/Year 10 and 11 DVD/Year10/beer/IRI week translation_2008_2017.xls", col_types = c("numeric", "date", "date", "skip", "skip", "skip"))

beer_prod_attr_2011_edit <- read_table("/Users/malooney/Google Drive/digitalLibrary/*MS_Thesis/MS_Thesis/data/beer_prod_attr_2011_edit")

prod11_beer <- read_excel("~/Documents/IRI_Original_Data/Year 8 and 9 DVD/parsed stub files 2008-2011/prod11_beer.xlsx")


# join grocery store and drug store data --------------------------------------

main_beer_drug_and_groc <- rbind(beer_drug_1583_1634, beer_groc_1583_1634)


# make UPC for main_beer_drug_and_groc ----------------------------------------

upc_fun <- function(x){

  paste(
    str_pad(x[1], 2, "left", pad="0"),
    str_pad(x[2], 2, "left", pad="0"),
    str_pad(x[3], 5, "left", pad="0"),
    str_pad(x[4], 5, "left", pad="0"), sep="-")
}

temp <- main_beer_drug_and_groc[, 3:6]

temp_main <- data.frame(upc= apply(temp, MARGIN = 1, upc_fun))

main_beer_drug_and_groc <- cbind(main_beer_drug_and_groc, temp_main)


# join week codes, (IRI_week...) with main_beer_drug_and_groc file ------------

main_beer_drug_and_groc_1 <- left_join(x= main_beer_drug_and_groc,
                                       y= IRI_week_translation_2008_2017,
                                       by= c("WEEK" = "IRI Week"))


# make UPC for beer_prod_attr_2011_edit ---------------------------------------

pc_fun <- function(x){

  paste(
    str_pad(x[1], 2, "left", pad="0"),
    str_pad(x[2], 2, "left", pad="0"),
    str_pad(x[3], 5, "left", pad="0"),
    str_pad(x[4], 5, "left", pad="0"), sep="-")
}

temp <- beer_prod_attr_2011_edit[, 1:4]

temp_main <- data.frame(upc= apply(temp, MARGIN = 1, upc_fun))

beer_prod_attr_2011_edit <- cbind(beer_prod_attr_2011_edit, temp_main)


#join main_beer_drug_and_groc_2 to prod11_beer and prod_beer_attr_2011_edit ---

main_beer_drug_and_groc_2 <- left_join(x= main_beer_drug_and_groc_1,
                                       y= beer_prod_attr_2011_edit,
                                       by= c("upc" = "upc"))

main_beer_drug_and_groc_3 <- left_join(x= main_beer_drug_and_groc_2,
                                       y= prod11_beer, by= c("upc" = "UPC"))


# clean up large data.frames !!!!!!! caution, removes data from memory !!!!!!!!

rm(WEEK, pc_fun, upc_fun,
   prod11_beer,
   temp, temp_main,
   beer_drug_1583_1634,
   beer_groc_1583_1634,
   beer_prod_attr_2011_edit,
   main_beer_drug_and_groc,
   main_beer_drug_and_groc_1,
   main_beer_drug_and_groc_2,
   IRI_week_translation_2008_2017)


# join Delivery_Stores to main_beer_drug_and_groc -----------------------------

unique_Delivery_stores <- distinct(Delivery_Stores) # remove duplicate IRI_Key numbers

main_beer_drug_and_groc_4_2010  <- left_join(x= main_beer_drug_and_groc_3,
                                             y= unique_Delivery_stores,
                                             by= "IRI_KEY")

# clean up large data.frames !!!!!!! caution, removes data from memory !!!!!!!!

rm(Delivery_Stores,
   unique_Delivery_stores,
   main_beer_drug_and_groc_3)


# !!!!!!!!! It is a good idea to save "main_beer_drug_and_groc_4_2010" to disk.
# !!!!!!!!! This is the starting point for the analysis... --------------------

#devtools::use_data_raw(main_beer_drug_and_groc_4_2010)

# Show all potiential market names --------------------------------------------

mrkNames <- data.frame(Market_Name=
                         unique(main_beer_drug_and_groc_4_2010$Market_Name))

mrkNames <- arrange(mrkNames, Market_Name)


# create LA Market Data -------------------------------------------------------

market1 <- "LOS ANGELES"

LA_data_2010 <- filter(main_beer_drug_and_groc_4_2010, Market_Name == market1)

LA_data_2010 <- LA_data[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36, 53:56, 71:76)]

#devtools::use_data(LA_data_2010)


# create Chicago Market Data --------------------------------------------------

market2 <- "CHICAGO"

CHICAGO_data <- filter(main_beer_drug_and_groc_4_2010, Market_Name == market2)

CHICAGO_data_2010 <- CHICAGO_data[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36, 53:56,
                                     71:76)]

#devtools::use_data(CHICAGO_data_2010)


# create Dallas Market Data ---------------------------------------------------

market3 <- "DALLAS, TX"

DALLAS_data <- filter(main_beer_drug_and_groc_4_2010, Market_Name == market3)

DALLAS_data_2010 <- DALLAS_data[,c(1, 2, 7:14, 19, 21, 25:27, 30, 36, 53:56,
                                   71:76)]

#devtools::use_data(DALLAS_data_2010)



