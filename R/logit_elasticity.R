

logit_elasticity <- function(data,
                             model_param,
                             vcov_alpha_price){

  tmpData <- data.frame(matrix(nrow=50, ncol=50))
  rownames(tmpData) <- data[1:50,3]; colnames(tmpData) <- data[1:50,3]
  tmpData_se <- data.frame(matrix(nrow=50, ncol=50))
  rownames(tmpData_se) <- data[1:50,3]; colnames(tmpData_se) <- data[1:50,3]

  n <- 1
  nn <- 50
  elasData <- list()
  seData <- list()
  i=1;j=1

  for(ii in 1:52){

  datatemp <- data[n:nn,]


  own <-  model_param[2] * datatemp$p2_Wmean[1:50]  * (1 - datatemp$share[1:50])
  se_own <- sqrt(datatemp$p2_Wmean[1:50] * (1 - datatemp$share[1:50]) *
                   vcov_alpha_price * datatemp$p2_Wmean[1:50] * (1 - datatemp$share[1:50]))
  cross <- -1 * model_param[2] * datatemp$p2_Wmean[1:50] * datatemp$share[1:50]
  se_cross <- sqrt(datatemp$p2_Wmean[1:50] * datatemp$share[1:50] *
                     vcov_alpha_price * datatemp$p2_Wmean[1:50] * datatemp$share[1:50])
  for(j in 1:50){

    for(i in 1:50){

      if(i==j){

        tmpData[i,j] <- own[i]
        tmpData_se[i,j] <- se_own[i]


        } else{
          if(j==50){
          tmpData[i,j] <- cross[j-49]
          tmpData_se[i,j] <- se_cross[j-49]

          } else{

            tmpData[i,j] <- cross[j+1]
            tmpData_se[i,j] <- se_cross[j+1]
          }


        }
    }
  }
  n <- n+50
  nn <- nn+50
  elasData[[ii]] <- tmpData
  seData[[ii]] <- tmpData_se
  }
  elasData <- list(elasData=elasData, seData=seData)
}
