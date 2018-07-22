


# Load Data -------------------------------------------------------------------

explore_Data <- function(data, ...) {

# Add volume measures ---------------------------------------------------------

  oz <- round(data.frame(oz = data$VOL_EQ.x * 288))

  total_oz <- (oz * data$UNITS)

  colnames(total_oz) <- "total_oz"

  total_gal <- (0.0078125 * total_oz)

  colnames(total_gal) <- "total_gal"

  dollarPerGal <- data$DOLLARS / total_gal

  colnames(dollarPerGal) <- "dollarPerGal"

  data_manip <- cbind(data, oz, total_oz, total_gal, dollarPerGal)

  rm(oz, total_gal, total_oz, dollarPerGal)

# Remove zero data ------------------------------------------------------------

  data_manip <- dplyr::filter(data_manip, L5 != "ALL BRAND")

  data_manip <- dplyr::filter(data_manip, dollarPerGal != "Inf")

  Brands <- unique(data_manip$L5)

  Weeks <- unique(data_manip$WEEK)

  Chains <- unique(data_manip$MskdName)

  num_Brands <- length(Brands)

  num_Weeks <- length(Weeks)

  num_Chains <- length(Chains)

  augWeeks <- num_Weeks + 1

  augChains <- num_Weeks + num_Chains

  explore_Data <- data.frame(matrix(NA, nrow = num_Weeks + num_Chains,
                                    ncol = num_Brands + 2))

  colnames(explore_Data) <- c("Week_Chain", "Week_Chain_Num", Brands)

  explore_Data[1:num_Weeks, 1] <- paste("Week", 1:num_Weeks, sep = "")

  explore_Data[augWeeks:augChains, 1] <- paste("Chain", 1:num_Chains, sep = "")

  explore_Data[1:num_Weeks, 2] <- unique(data_manip$WEEK)

  explore_Data[augWeeks:augChains, 2] <- unique(data_manip$MskdName)

  i <- 1

  j <- 3

  for (i in seq_along(Brands)) {

    tmp <- dplyr::filter(data_manip, L5 == Brands[i])

    tmp_week <- unique(tmp$WEEK)

    tmp_chain <- unique(tmp$MskdName)

    explore_Data[1:num_Weeks, j] <- ifelse(explore_Data[1:num_Weeks, 2]
                                           %in% tmp_week,
                                           explore_Data[1:52, 2], NA)

    explore_Data[augWeeks:augChains, j] <-
      ifelse(explore_Data[augWeeks:augChains, 2] %in%
               tmp_chain, explore_Data[augWeeks:augChains, 2], NA)
    j <- j + 1

    }

  rm(i, j, tmp, num_Brands, tmp_chain, tmp_week, augChains, augWeeks,
     num_Chains, num_Weeks)

  explore_Data_Complete <- explore_Data[, apply(explore_Data, 2,
                                                function(x) !any(is.na(x)))]

  Brands_CompleteData <- data.frame(
    Brand_Name = colnames(explore_Data_Complete[-c(1, 2)]))

  Brands_CompleteData <- dplyr::arrange(Brands_CompleteData, Brand_Name)

  list(Brands_CompleteData = Brands_CompleteData,
       explore_Data_Complete = explore_Data_Complete,
       explore_Data = explore_Data)
}
