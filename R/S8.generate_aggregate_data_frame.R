
#' @export

# Function - Generate Main Data Frame -----------------------------------------

S8.generate_aggregate_data_frame <- function(nWeeks = 52,
                                             nmkt = 52,
                                             city,
                                             saveAggregate_data_frame = T,
                                             out_algoRunTime = T)  {

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))) {

    stop("file does not exist in project directory. Run Script 2
         (S2.construct_2010marketData.R) to generate the file called:
         D2.marketData_2010.rds")

  } else{

    D2.marketData_2010 <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))

  }

  cityData <- D2.marketData_2010[[city]]

  if(!file.exists(paste(path.local, "/data/beer_MS_Data.csv", sep=""))) {

    stop("file does not exist in project directory. Move file called: beer_MS_Data.csv into project directory called: /data. This file should contain the product characteristics of interest.")

  } else{

    Beer_Characteristics_Master_List <- read.csv(paste(path.local, "/data/beer_MS_Data.csv", sep=""))

  }

  Beer_Characteristics_Master_List <- na.omit(Beer_Characteristics_Master_List)

# Add volume measures ---------------------------------------------------------
  oz <- round(data.frame(oz=cityData$VOL_EQ.x* 288))
  total_oz <- (oz* cityData$UNITS); colnames(total_oz) <- "total_oz"

  total_gal <- (0.0078125* total_oz); colnames(total_gal) <- "total_gal"

  dollarPerGal <- cityData$DOLLARS/ total_gal;
  colnames(dollarPerGal) <- "dollarPerGal"

  data_2010_manip <- cbind(cityData, oz, total_oz, total_gal,
                              dollarPerGal)

# Remove zero data ------------------------------------------------------------
  data_2010_manip <- dplyr::filter(data_2010_manip, L5 !="ALL BRAND")
  data_2010_manip <- dplyr::filter(data_2010_manip, dollarPerGal !="Inf")

  unqWeek <- unique(data_2010_manip$WEEK)
  unqBrands <- Beer_Characteristics_Master_List$Brand_Name

  if(city=="LOS ANGELES"){
    N <- (2798161)* (25.5/52)

  } else if(city=="CHICAGO") {
    N <- (1996235)* (29.1/52)

  } else if(city=="DALLAS, TX"){
    N <- (849924)* (34.4/52)
  } else{}

  tmp <- dplyr::filter(data_2010_manip, L5 %in% unqBrands)

  tmp_main <- data.frame()
  n <- 1
  nn <- n+1
  ii <- 1 #week
  i <- 1

    for(i in seq_along(unqWeek)){
      tmp1 <- dplyr::filter(tmp, WEEK==unqWeek[i])
        j <- 1 # brand

        for(j in seq_along(unqBrands)){
          tmp2_all <- dplyr::filter(tmp1, L5==unqBrands[j])
          tmp2 <- dplyr::filter(tmp1, L5==unqBrands[j] & PR==0)

          w_dollar <- sum(tmp2_all$dollarPerGal)
          W <- tmp2_all$dollarPerGal/w_dollar
          W_mean <- sum(W*tmp2_all$dollarPerGal)

          tmp2_pr <- dplyr::filter(tmp1, L5==unqBrands[j] & PR==1)
          w_dollar_pr <- sum(tmp2_pr$dollarPerGal)
          W_pr <- tmp2_pr$dollarPerGal / w_dollar_pr
          W_mean_pr <- sum(W_pr * tmp2_pr$dollarPerGal)

          tmp_main[n, 01] <- ii # cdid_week
          #tmp_main[nn, 01] <- ii # cdid_week
          tmp_main[n, 02] <- i
          #tmp_main[nn, 02] <- i
          tmp_main[n, 03] <- NA#unqChain[k] # chain
          #tmp_main[nn, 03] <- NA#unqChain[k] # chain
          tmp_main[n, 04] <- tmp2_all$WEEK[1]
          #tmp_main[nn, 04] <- tmp2$WEEK[1]
          tmp_main[n, 05] <- as.character(tmp2_all$`Calendar week starting on`[1])
          #tmp_main[nn, 05] <- as.character(tmp2$`Calendar week starting on`[1])
          tmp_main[n, 06] <- as.character(tmp2_all$`Calendar week ending on`[1])
          #tmp_main[nn, 06] <- as.character(tmp2$`Calendar week ending on`[1])
          tmp_main[n, 07] <- unqBrands[j] # brand
          #tmp_main[nn, 07] <- paste(unqBrands[j], "_pr", sep="") # brand
          tmp_main[n, 08] <- tmp2_all$L3[1] # Conglomerate
          #tmp_main[nn, 08] <- tmp2$L3[1] # Conglomerate
          tmp_main[n, 09] <- tmp2_all$L4[1] # Firm
          #tmp_main[nn, 09] <- tmp2$L4[1] # Firm
          tmp_main[n, 10] <- sum(tmp2_all$total_gal) # total gallons
          #tmp_main[nn, 10] <- sum(tmp2_pr$total_gal) # total gallons
          tmp_main[n, 11] <- mean(tmp2_all$dollarPerGal) # mean price1 ($/gal)
          #tmp_main[nn, 11] <- mean(tmp2_pr$dollarPerGal) # mean price1 ($/gal)
          tmp_main[n, 12] <- W_mean # weighted mean price2 ($/gal)
          #tmp_main[nn, 12] <- W_mean_pr # weighted mean price2 ($/gal)
          tmp_main[n, 13] <- sum(tmp2_pr$total_gal) / sum(tmp2_all$total_gal)
          tmp_main[n, 14] <- sum(tmp2_all$total_gal)/N # share
          #tmp_main[nn, 13] <- sum(tmp2_pr$total_gal)/N # share

          n <- n+1
          nn <- n+1
          j <- j+1
        }

      ii <- ii+1
  }

  colnames(tmp_main) <- c("cdid", "sub_cdid", "Chain", "week",
                          "week_start", "week_end", "Brand", "Conglomerate",
                          "Firm",  "total_gallons", "p1_mean", "p2_Wmean",
                          "prct_PR", "share")

  nbrn <- nrow(Beer_Characteristics_Master_List)

  constant <- data.frame("constant"= rep(1, times= nmkt* nbrn))

  outshr <- function(share, cdid, nmkt, nbrn){ # function to calculate outshr

    cdindex <- seq(nbrn, nbrn*nmkt, nbrn) # indexes the markets

    temp <- cumsum(share)
    sum1 <- temp[cdindex]
    sum1[2:length(sum1)] <- diff(sum1)
    outshr <- 1- sum1[cdid]
    return(outshr)
  }

  outshr <- data.frame(outshr= outshr(share=tmp_main$share,
                                      cdid=tmp_main$cdid,
                                      nmkt=nmkt,
                                      nbrn=nbrn))

  aggregate_data <- cbind(tmp_main, outshr)

  # tmp_char <- data.frame(matrix(ncol=ncol(Beer_Characteristics_Master_List)))
  # n <- 1
  # nn <- 2
  #
  # for(i in 1:nrow(Beer_Characteristics_Master_List)){
  #
  #   tmp_char[n, ] <- Beer_Characteristics_Master_List[i, ]
  #
  #   tmp_char[nn, 1] <- paste(Beer_Characteristics_Master_List[i, 1], "_pr",
  #                           sep="")
  #
  #   tmp_char[nn, -1] <- Beer_Characteristics_Master_List[i, -1]
  #
  #   n <- n+2
  #   nn <- n+1
  #
  # }
  #colnames(tmp_char) <- names(Beer_Characteristics_Master_List)

  brandDummy <- diag(nrow(Beer_Characteristics_Master_List))
  colnames(brandDummy) <- chartr(" ", "_",
                                 Beer_Characteristics_Master_List$Brand_Name)

  Beer_Characteristics_Master_List <- cbind(Beer_Characteristics_Master_List,
                                            brandDummy)

  aggregate_data <- dplyr::left_join(aggregate_data,
                              Beer_Characteristics_Master_List,
                              by= c("Brand" = "Brand_Name"))

  #aggregate_data$share[aggregate_data$share <=0] <- 0.000000000001

  if(saveAggregate_data_frame == T){

    saveRDS(aggregate_data,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D8.2010_aggregate_data.", gsub('[ ,]', '', city), ".rds", sep = ""))

  } else{}

  endTime <- Sys.time()

  if(out_algoRunTime == T) {

    hora <- list(starttime=startTime, endTime=endTime)

  } else {}


}
