

S0.masterScript <- function(S1 = TRUE,
                            S2 = TRUE,
                            S3 = FALSE) {

# Housekeeping ----------------------------------------------------------------

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  totalCount <- 4
  pb <- txtProgressBar(min = 0, max = totalCount, style = 3)
  count <- 1
  setTxtProgressBar(pb, count)

# -----------------------------------------------------------------------------

  if(S1 == TRUE){S1.beer_2010DataCleaning(save_2010_main_data = 'all_formats')
  } else{}

  count <- 2
  setTxtProgressBar(pb, count)

  if(S2 == TRUE){
    S2.construct_2010marketData(fileType = 'feather',
                       marketNames = 'all')
  } else{}

# -----------------------------------------------------------------------------

    count <- 3
  setTxtProgressBar(pb, count)

    if(S3 == TRUE){

    } else{}

# -----------------------------------------------------------------------------
  count <- 4
  setTxtProgressBar(pb, count)

  close(pb)

  endTime <- Sys.time()

  endTime - startTime

  }
