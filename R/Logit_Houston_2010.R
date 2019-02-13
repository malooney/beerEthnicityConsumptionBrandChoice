


rm(list=ls())
cat("\014")

library(AER)
library(hdm)
library(ucminf)
library(Rcpp)
library(mvQuad)
library(numDeriv)
library(randtoolbox)
library(rngWELL)
library(R.matlab)

library(beerEthnicityConsumptionBrandChoice)
library(pso)

library(pushoverr)

path.local <- rprojroot::find_rstudio_root_file()

#devtools::document()
#devtools::use_package("data.tree", "Suggests")

############## My settings for PushoverR ###########

userID <- "uqkdp5ieoh9cps6sqvrcvkk1i9ty17"
appToken <- "aje741dex7z3ro93zwu97cft5i36hw"

old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)



D8.2_2010_aggregate_data.CHICAGO <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.CHICAGO.rds")
D8.2_2010_aggregate_data.DALLASTX <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.DALLASTX.rds")
D8.2_2010_aggregate_data.HOUSTON <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.HOUSTON.rds")
D8.2_2010_aggregate_data.LOSANGELES <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.LOSANGELES.rds")
D8.2_2010_aggregate_data.SPOKANE <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.SPOKANE.rds")
D8.2_2010_aggregate_data.SYRACUSE <- readRDS("~/Google Drive/digitalLibrary/*beerEthnicityConsumptionBrandChoice/beerEthnicityConsumptionBrandChoice/data_beerEthnicityConsumptionBrandChoice/D8.2_2010_aggregate_data.SYRACUSE.rds")



ivs_Hausman_LA <- data.frame(
  D8.2_2010_aggregate_data.LOSANGELES[[1]]['Brand'],
  D8.2_2010_aggregate_data.LOSANGELES[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr'],

  scale(D8.2_2010_aggregate_data.LOSANGELES[[1]]["p2_Wmean_non_pr"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.LOSANGELES[[1]]["p2_Wmean_non_pr"], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.DALLASTX[[1]]["p2_Wmean_non_pr"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.DALLASTX[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.SPOKANE[[1]]["p2_Wmean_non_pr"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.SPOKANE[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.SYRACUSE[[1]]["p2_Wmean_non_pr"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.SYRACUSE[[1]]["p2_Wmean_non_pr"], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.CHICAGO[[1]]["p2_Wmean_non_pr"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.CHICAGO[[1]]["p2_Wmean_non_pr"], 2, sd, na.rm = TRUE)),
  # scale(D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.HOUSTON[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr'], center=F),
  log(D8.2_2010_aggregate_data.LOSANGELES[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr'])
)

colnames(ivs_Hausman_LA) <- c("Brand",
                              "z1_P_LOSANGELES", "z2_P_DallasTx",
                              "z3_P_SPOKANE", "z4_P_SYRACUSE", "z5_P_CHICAGO",
                              "z1_scale_P_LOSANGELES", "z2_scale_P_DallasTx",
                              "z3_scale_P_SPOKANE", "z4_scale_P_SYRACUSEe",
                              "z5_scale_P_CHICAGO", "z1_log_P_LOSANGELES",
                              "z2_log_P_DallasTx", "z3_log_P_SPOKANE",
                              "z4_log_P_SYRACUSE", "z5_log_P_CHICAGO"
)

constant <- data.frame(constant=
                         matrix(
                           rep(1,
                               nrow(D8.2_2010_aggregate_data.HOUSTON[[1]])),
                           ncol=1
                         )
)

scale_p2_Wmean <- scale(D8.2_2010_aggregate_data.HOUSTON[[1]]["p2_Wmean"], center = FALSE, scale = apply(D8.2_2010_aggregate_data.HOUSTON[[1]]["p2_Wmean"], 2, sd, na.rm = TRUE)); colnames(scale_p2_Wmean) <- "scale_p2_Wmean"

log_p2_Wmean <- data.frame(log_p2_Wmean=log(D8.2_2010_aggregate_data.HOUSTON[[1]][, "p2_Wmean"]))

beerHouston.data <- cbind(constant, D8.2_2010_aggregate_data.HOUSTON[[1]][c(1, 7,
                                                                       8)])
firm.id <- data.frame(firm.id=as.numeric(factor(beerHouston.data$Conglomerate)))

id <- 1000:(1000+nrow(beerHouston.data)-1)

beerHouston.data <- cbind(beerHouston.data, firm.id, id)

beerHouston.data <- cbind(beerHouston.data, scale_p2_Wmean, log_p2_Wmean)

beerHouston.data <- cbind(beerHouston.data,
                     D8.2_2010_aggregate_data.HOUSTON[[1]][c(12, 14, 16,
                                                             33:82)])

tmp_beerHouston.data <- D8.2_2010_aggregate_data.HOUSTON[[1]][, c(13, 17, 18, 19,
                                                             21:24)]

beerHouston.data <- cbind(beerHouston.data, tmp_beerHouston.data)

BLP_ivs <- log(hdm:::constructIV(beerHouston.data$firm.id, beerHouston.data$cdid,
                                 beerHouston.data$id, cbind(1, beerHouston.data[, c("ABV", "Calories_oz", "Carbs_oz")])))


beerHouston.data <- cbind(beerHouston.data, ivs_Hausman_LA[,2:16], BLP_ivs)

rm(constant, ivs_Hausman_LA, tmp_beerHouston.data, scale_p2_Wmean, log_p2_Wmean,
   BLP_ivs, firm.id, id)

saveRDS(beerHouston.data,
        paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/beerHouston.data.rds", sep = ""))

summary(lm.results1 <- lm(log(share) - log(outshr) ~ 1 +
                            p2_Wmean +
                            ABV +
                            prct_PR +
                            Calories_oz +
                            Carbs_oz +
                            USA +
                            Mexico,
                          data= beerHouston.data))



iv_lm.char <- paste(colnames(beerHouston.data[c(62:65, 68:69)]), collapse=" + ")

#iv_lm.ivs <- paste(colnames(beerHouston.data[c(72, 73, 75)]), collapse=" + ")
iv_lm.ivs <- paste(colnames(beerHouston.data[c(70:74, 85:92)]), collapse=" + ")

model.iv_lm.Brands <- formula(paste("log(share) - log(outshr)~  1 +
                                    p2_Wmean +", iv_lm.char, " | ",
                                    iv_lm.char, " + ", iv_lm.ivs,
                                    sep=""))

summary( iv.simple.logit1 <- ivreg( model.iv_lm.Brands,
                                    data= beerHouston.data))







