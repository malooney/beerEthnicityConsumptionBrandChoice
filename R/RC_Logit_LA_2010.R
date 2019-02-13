
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
library(readr)

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

ivs_Hausman <- data.frame(
  D8.2_2010_aggregate_data.CHICAGO[[1]]['Brand'],
  D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.HOUSTON[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr'],
  D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr'],

  scale(D8.2_2010_aggregate_data.CHICAGO[[1]][15], center = FALSE, scale = apply(D8.2_2010_aggregate_data.CHICAGO[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.DALLASTX[[1]][15], center = FALSE, scale = apply(D8.2_2010_aggregate_data.DALLASTX[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.HOUSTON[[1]][15], center = FALSE, scale = apply(D8.2_2010_aggregate_data.HOUSTON[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.SPOKANE[[1]][15], center = FALSE, scale = apply(D8.2_2010_aggregate_data.SPOKANE[[1]][15], 2, sd, na.rm = TRUE)),
  scale(D8.2_2010_aggregate_data.SYRACUSE[[1]][15], center = FALSE, scale = apply(D8.2_2010_aggregate_data.SYRACUSE[[1]][15], 2, sd, na.rm = TRUE)),
  # scale(D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.HOUSTON[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr'], center=F),
  # scale(D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr'], center=F),
  log(D8.2_2010_aggregate_data.CHICAGO[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.DALLASTX[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.HOUSTON[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.SPOKANE[[1]]['p2_Wmean_non_pr']),
  log(D8.2_2010_aggregate_data.SYRACUSE[[1]]['p2_Wmean_non_pr'])
)


colnames(ivs_Hausman) <- c("Brand",
                           "z1_P_Chicago", "z2_P_DallasTx",
                           "z3_P_Houston", "z4_P_Spokane", "z5_P_Syracuse",
                           "z1_scale_P_Chicago", "z2_scale_P_DallasTx",
                           "z3_scale_P_Houston", "z4_scale_P_Spokane",
                           "z5_scale_P_Syracuse", "z1_log_P_Chicago",
                           "z2_log_P_DallasTx", "z3_log_P_Houston",
                           "z4_log_P_Spokane", "z5_log_P_Syracuse"
                           )





constant <- data.frame(constant=
                         matrix(
                           rep(1,
                               nrow(D8.2_2010_aggregate_data.LOSANGELES[[1]])),
                           ncol=1
                         )
                       )

#scale_p2_Wmean <- data.frame(scale_p2_Wmean=scale(D8.2_2010_aggregate_data.LOSANGELES[[1]][, 12]))

scale_p2_Wmean <- scale(D8.2_2010_aggregate_data.LOSANGELES[[1]][12], center = FALSE, scale = apply(D8.2_2010_aggregate_data.LOSANGELES[[1]][12], 2, sd, na.rm = TRUE)); colnames(scale_p2_Wmean) <- "scale_p2_Wmean"

log_p2_Wmean <- data.frame(log_p2_Wmean=log(D8.2_2010_aggregate_data.LOSANGELES[[1]][, 12]))

beerLA.data <- cbind(D8.2_2010_aggregate_data.LOSANGELES[[1]][c(1)], constant, D8.2_2010_aggregate_data.LOSANGELES[[1]][c(7, 8)])

firm.id <- data.frame(firm.id=as.numeric(factor(beerLA.data$Conglomerate)))

id <- 1000:(1000 + nrow(beerLA.data) - 1)

beerLA.data <- cbind(beerLA.data, firm.id, id)

beerLA.data <- cbind(beerLA.data, D8.2_2010_aggregate_data.LOSANGELES[[1]][c(12)], scale_p2_Wmean, log_p2_Wmean)

# change if change of brands
beerLA.data <- cbind(beerLA.data,
                     D8.2_2010_aggregate_data.LOSANGELES[[1]][c(14, 16,
                                                                33:52)])

#beerLA.data <- cbind(constant, beerLA.data)

tmp_beerLA.data <- D8.2_2010_aggregate_data.LOSANGELES[[1]][, c(13, 17, 18, 19,
                                                              21:24)]

beerLA.data <- cbind(beerLA.data, tmp_beerLA.data)

BLP_ivs <- hdm:::constructIV(beerLA.data$firm.id, beerLA.data$cdid,
                                  beerLA.data$id, cbind(1, beerLA.data[, c("ABV", "Calories_oz", "Carbs_oz")]))


beerLA.data <- cbind(beerLA.data, ivs_Hausman[,2:16], BLP_ivs)

rm(constant, ivs_Hausman, tmp_beerLA.data, scale_p2_Wmean, log_p2_Wmean,
   BLP_ivs, firm.id, id)



if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D8.1.Beer_Characteristics_Study_List.rds", sep=""))) {

  stop("file does not exist in project directory. Run Script
       S8.1.studyBrandsSelect to generate the file called:
       D8.1.Beer_Characteristics_Study_List.rds")

} else{

  Beer_Characteristics_Master_List <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D8.1.Beer_Characteristics_Study_List.rds", sep=""))

}

Beer_Characteristics_Master_List <- na.omit(Beer_Characteristics_Master_List)


if(!file.exists(paste(path.local, "/data/beer_IV.csv", sep=""))) {

  stop("file does not exist in project directory.")

} else{

  beer_IV <- read_csv(paste(path.local, "/data/beer_IV.csv", sep=""))

}

a <- as.matrix(Beer_Characteristics_Master_List[,8:17])
b <- as.matrix(beer_IV[,2])

MW_IVs <- a %*% b

MW_IVs_singleCol<- rep(MW_IVs, 52)

MW_IVs <- diag(as.numeric(MW_IVs, ncol=50))

MW_IVs <- apply(replicate(52, MW_IVs), 1, rbind)


colnames(MW_IVs) <- paste("Z_MWage", chartr(" ", "_",
                                                Beer_Characteristics_Master_List$Brand_Name), sep="_")


beerLA.data <- cbind(beerLA.data, MW_IVs, MW_IVs_singleCol)



rm(a, b, beer_IV, Beer_Characteristics_Master_List, MW_IVs)



# simple logit regressions -----------------------------------------------------

lm.Brands <- paste(colnames(beerLA.data[12:61]), collapse = " + ")

model.lm.Brands <- formula(paste("log(share) - log(outshr)~  0 +
                                 p2_Wmean +", lm.Brands, sep=""))

summary(lm.results <- lm( model.lm.Brands,
                          data= beerLA.data))


summary(lm.results1 <- lm(log(share) - log(outshr) ~ 1 +
                            p2_Wmean +
                            ABV +
                            prct_PR +
                            Calories_oz +
                            Carbs_oz +
                            USA +
                            Mexico,
                          data= beerLA.data))



# simple iv logit regressions --------------------------------------------------

iv_lm.Brands <- paste(colnames(beerLA.data[12:31]), collapse=" + ")

#iv_lm.ivs <- paste(colnames(beerLA.data[c(41, 42, 44, 55:62)]), collapse=" + ")
iv_lm.ivs <- paste(colnames(beerLA.data[c(83)]),
                   collapse=" + ")

model.iv_lm.Brands <- formula(paste("log(share) - log(outshr)~  0 +
                                    p2_Wmean +", iv_lm.Brands, " | ",
                                    iv_lm.Brands, " + ", iv_lm.ivs,
                                    sep=""))

summary( iv.simple.logit <- ivreg( model.iv_lm.Brands,
                                   data= beerLA.data))

iv_lm.char <- paste(colnames(beerLA.data[c(32:34, 38:39)]), collapse=" + ")

iv_lm.char.exo <- paste(colnames(beerLA.data[c(32:34, 38:39)]), collapse=" + ")

#iv_lm.ivs <- paste(colnames(beerLA.data[c(72, 73, 75)]), collapse=" + ")
iv_lm.ivs <- paste(colnames(beerLA.data[c(83)]),
                   collapse=" + ")

model.iv_lm.char <- formula(paste("log(share) - log(outshr)~  1 +
                                    p2_Wmean +", iv_lm.char, " | ",
                                  iv_lm.char.exo, " + ", iv_lm.ivs,
                                    sep=""))

summary(iv.simple.logit.LA <- ivreg( model.iv_lm.char,
                             data= beerLA.data))












# import demographics data ----------------------------------------------------

path.pkg <- system.file(package = "beerEthnicityConsumptionBrandChoice")

LA_demog_2009_2010_2011 <- readRDS(
  paste(path.pkg, "/extdata/LA_demog_2009_2010_2011.rds",
        sep = ""))

cdid_demog <-  data.frame(cdid=1:52)

demogData <- subset(LA_demog_2009_2010_2011,!duplicated(LA_demog_2009_2010_2011$peridnum))

demog_age_sample <- data.frame(demogData$a_age)
rownames(demog_age_sample) <- c()

demog_age <- t(scale(demog_age_sample, center = FALSE, scale = apply(demog_age_sample, 2, sd, na.rm = TRUE)))
rownames(demog_age) <- c()
#demog_age <- t(scale(demog_age_sample, center = T))
#demog_age <- t(demog_age_sample)
demog_age_2 <- demog_age^2
demog_age <- cbind(cdid_demog, demog_age)
demog_age_2 <- cbind(cdid_demog, demog_age_2)

demog_income_sample <- data.frame(demogData$ftotval)

demog_income <- t(scale(demog_income_sample, center = FALSE, scale = apply(demog_income_sample, 2, sd, na.rm = TRUE)))
rownames(demog_income) <- c()
#demog_income_sample[demog_income_sample<=0] <- 0.0000000001
#demog_income <- t(scale(demog_income_sample, center = T))
#demog_income <- t(log(demog_income_sample))

demog_income_2 <- demog_income^2
demog_income <- cbind(cdid_demog, demog_income)
demog_income_2 <- cbind(cdid_demog, demog_income_2)

demog_ethnic_sample <- data.frame(demogData$pehspnon)
demog_ethnic_sample <- ifelse(demog_ethnic_sample==1, 1, 0)
demog_ethnic <- t(demog_ethnic_sample)
rownames(demog_ethnic) <- c()
demog_ethnic <- cbind(cdid_demog, demog_ethnic)


demographics <- c("income", "ethnic")
demographicData <- list("age"= demog_age,
                        "age_2"=demog_age_2,
                        "income"=demog_income,
                        "income_2"=demog_income_2,
                        "ethnic"=demog_ethnic)

rm(demog_age, demog_age_2, demog_age_sample, path.pkg, cdid_demog,
   demog_income, demog_income_2, LA_demog_2009_2010_2011, demog_income_sample,
   demog_ethnic_sample, demog_ethnic, demogData)


















Rcpp::sourceCpp('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/cppFunctions.cpp')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/helperFunctions.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/estimateBLP1.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/generics.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/price_elasticity_matrix.R')

source('/Users/malooney/Google Drive/digitalLibrary/*BLP_Algos/BLP_Algos/results_shape.R')


MDE= FALSE

if(MDE==TRUE){


  Xlin = paste(colnames(beerLA.data[c(7, 12:31)]), sep="")

} else {

  Xlin = c("constant",
           "p2_Wmean",
           "prct_PR",
           "ABV",
           "Calories_oz",
           "Carbs_oz",
           "USA",
           "Mexico")
}

Xrandom = c("constant",
            "p2_Wmean",
            "USA",
            "Mexico")


if(MDE==TRUE){

  Xexo = paste(colnames(beerLA.data[c(12:31)]), sep="")

} else {

  Xexo = c("ABV",
           "Calories_oz",
           "Carbs_oz",
           "USA",
           "Mexico" )
}

instruments = paste(colnames(beerLA.data[c(63:82)]),
                    sep="") #HausmanIV's
#instruments = paste(colnames(beerLA.data[c(70:74, 85:92)]), sep="")


K <- length(Xrandom) # number of random coefficients

beerLA.data$starting.delta <- iv.simple.logit.LA$fitted.values+ rnorm(length(beerLA.data$cdid), mean=0, sd= abs(iv.simple.logit.LA$residuals))

starting.guesses.delta <- beerLA.data$starting.delta

starting.guesses.theta2 <- matrix( rnorm(K*(length(demographics)+ 1), mean= 0, sd= 1), nrow= K, ncol= length(demographics)+ 1 )

# starting.guesses.theta2 <- matrix( rep(0.1, K), nrow= K,
#                                    ncol= length(demographics)+ 1 )

#starting.theta2[3,2] <- NA
#starting.theta2[6,1] <- NA
#starting.theta2[6,2] <- NA
#starting.theta2[7,2] <- NA
#starting.theta2[8,2] <- NA

rm(D8.2_2010_aggregate_data.CHICAGO, D8.2_2010_aggregate_data.DALLASTX,
   D8.2_2010_aggregate_data.HOUSTON, D8.2_2010_aggregate_data.LOSANGELES,
   D8.2_2010_aggregate_data.SPOKANE, D8.2_2010_aggregate_data.SYRACUSE,
   iv.simple.logit, iv.simple.logit.LA, lm.results, lm.results1,
   model.lm.Brands, model.iv_lm.Brands, lm.Brands,
   iv_lm.Brands, iv_lm.ivs, iv_lm.char)



#oneRun <- function(.){

estimation_params <- matrix(c(1e-9, 1e-16, 10000
                               # 1e-6, 1e-8, 100,
                               # 1e-9, 1e-12, 100,
                               # 1e-9, 1e-16, 100,
                               # 1e-9, 1e-16, 500
                               # 1e-9, 1e-16, 1000,
                               # 1e-9, 1e-16, 2500,
                               # 1e-9, 1e-16, 5000,
                               # 1e-9, 1e-16, 7500,
                               # 1e-9, 1e-16, 10000
                              ),
                             nrow=1, ncol=3, byrow = T)

# estimation_params <- matrix(c(1e-3, 1e-4, 10000,
#                               1e-6, 1e-8, 10000,
#                               1e-9, 1e-12, 10000,
#                               1e-9, 1e-16, 10000),
#                             nrow=4, ncol=3, byrow = T)

multi_Run_beer_looney <- list()

startTime= Sys.time()

for(j in 1:nrow(estimation_params)){

experiment <- list()

for(i in 1:1){

  experiment[[i]]  <- estimateBLP1(Xlin = Xlin,
               Xrandom = Xrandom,
               Xexo =  Xexo,
               instruments = instruments,
               shares = "share",
               cdid = "cdid",
               productData = beerLA.data,
               demographics = demographics,
               demographicData = demographicData,
               starting.guesses.theta2 = starting.guesses.theta2,
               solver.control = list(maxeval = 5000,
                                     solver.reltol = estimation_params[j,1]),
                                     #solver.reltol= 1e-9), #outer tol. 1e-9
               solver.method = "BFGS_matlab",
               starting.guesses.delta =  starting.guesses.delta,
               blp.control = list(inner.tol = estimation_params[j,2],
                 #inner.tol = 1e-16, #inner tol. 1e-16
                                  inner.maxit = 10000),
               integration.control= list(method= "Halton",
                                         # "MC", "MLHS", "Halton",
                                         # "randHalton",
                                         # "sgGH", "sgNH",
                                         # "sgTr1", "sgTr"
                                         #accuracyQuad=2,
                                         #amountNodes= 10000,
                                         amountNodes = estimation_params[j,3],
                                         #nodes= NULL,
                                         #weights=NULL,
                                         seed= 0429, #NULL
                                         output= TRUE),
               postEstimation.control= list(standardError = "robust",
                                            extremumCheck = F,
                                            elasticities = NULL),
               printLevel = 4)
  #}


#library(parallel)
#detectCores()
#cl <- makeCluster(1)

#start <- Sys.time()
#multi_Run_beer_looney[[2]] <- lapply(X= 1:1, FUN= oneRun)

# starting.guesses.theta2 <- matrix(experiment[[i]][["theta.rc"]],
#                                   nrow=3,
#                                   ncol=3)
#starting.guesses.delta <- experiment[[i]][["delta"]]

}
multi_Run_beer_looney[[j]] <- experiment
}

endTime= Sys.time()

difftime(endTime, startTime, units="min")



# msg <- "Analysis complete"
# # Send message
# pushover(message = msg,
#         user = userID,
#         app = appToken)

#end <- Sys.time()

#time <- end-start

#stopCluster(cl)
#rm(cl)

summary(multi_Run_beer_looney[[1]][[1]])








if(MDE==TRUE){

par(mfrow=c(2,1))
results_price <- results_shape(multi_Run_beer_looney[[1]])
plot(results_price[1,c(1:8)], type="l", ylim=c(-100,100))
for(i in 2:nrow(results_price)){
lines(results_price[i,c(1:8)], type="l")
}

plot(results_price[1,c(43:82)], type="l", ylim=c(-1,1))
for(i in 2:nrow(results_price)){
  lines(results_price[i,c(43:82)], type="l")
}

} else {

  par(mfrow=c(2,1))
  results_price <- results_shape(multi_Run_beer_looney[[1]])
  plot(results_price[1,c(1:8)], type="l", col="red", ylim=c(-150,150))
  abline(a=0, b=0, col="lightgrey")
  for(i in 2:nrow(results_price)){
    lines(results_price[i,c(1:8)], type="l", col="green")
  }

  plot(results_price[1,c(17:40)], type="l", ylim=c(-50,50), col="red")
  abline(a=0, b=0, col="lightgrey")
  for(i in 2:nrow(results_price)){
    lines(results_price[i,c(17:40)], type="l", col="green")
  }


}



#saveRDS(multi_Run_beer_looney, "multi_Run_beer_looney_006.rds")

#
# plot(density(results_price[,2]), xlim=c(-5,5))
# plot(density(results_price[,44]), xlim=c(-0.5,0.5))
# plot(density(results_price[,50]))
