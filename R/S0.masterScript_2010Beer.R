

S0.masterScript <- function(S1 = TRUE,
                            S2 = TRUE,
                            S3 = TRUE){


  if(S1 == TRUE){S1.beer_2010DataCleaning(save_2010_main_data = 'all_formats')
  } else{}

  if(S2 == TRUE){
    S2.marketData_2010(fileType = 'feather',
                       marketNames = c("LOS ANGELES",
                                       "CHICAGO",
                                       "DALLAS, TX"))
  } else{}

  if(S3 == TRUE){

  } else{}

  }
