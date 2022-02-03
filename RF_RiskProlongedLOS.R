# Author: Prof. Igor Tona Peres, Dpt. of Industrial Engineering, PUC-Rio
# igor.peres@tecgraf.puc-rio.br
# Last updated: 02/01/2022

library(caret)
library(tidyverse)
library(base)

load('training.RData')

training = training%>%
  mutate(Desfecho_binario_LOS = if_else(hospital_length_stay_ajus_trunc<14,"Baixo","Alto"))

#predictors
predictors = read.csv(file="predictors_LOS.csv")
predictors = predictors$x
predictors = as.character(predictors)

training = training%>%
  select(predictors,Desfecho_binario_LOS)

brierScore = function(data,lev=NULL, model = NULL){
  y_pred = data[,lev[2]]
  y_true = ifelse(data$obs == lev[2],1,0)
  brier_val = mean((y_pred-y_true)^2)
  names(brier_val) = c(brierScore)
  brier_val
}

#summary function
fiveStats = function(...)c(multiClassSummary(...),brierScore(...)) 

#basic parameter tuning
fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE,
  classProbs = TRUE, summaryFunction = fiveStats)

#RF
library(ranger)
set.seed(476)
Grid = expand.grid(mtry = c(5:15),
                   min.node.size = c(3:10),
                   splitrule =  c("gini","extratrees","hellinger")
                      )

rf <- train(x=training[,-ncol(training)],
                  y= training$Desfecho_binario_LOS,
                  tuneGrid = Grid,
                  method="ranger",
            metric="brierScore",
            maximize = FALSE,
            trControl = fitControl)
save(rf,file="rf_RiskProlongedLOS.RData")

model = rf

#Prediction Analysis

load("rf_RiskProlongedLOS.RData")

library(MLmetrics)
library(ModelMetrics)

load('testing.RData')

testing = testing%>%
  mutate(Desfecho_binario_LOS = if_else(hospital_length_stay_ajus_trunc<14,"Baixo","Alto"))

testing = testing%>%
  select(predictors,Desfecho_binario_LOS)

Observed = data.frame(Observed=testing$Desfecho_binario_LOS)
Observed$Observed = if_else(Observed$Observed=="Alto",1,0)
Predicted = data.frame(Predicted = predict(model,newdata=testing,type="prob"))

brier(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)

auc(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)
ppv(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)
npv(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)
sensitivity(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)
specificity(predicted = Predicted$Predicted.Alto, actual = Observed$Observed)

#Calibration Belt
library(givitiR)
comparacao = cbind(Observed,Predicted)
cb <- givitiCalibrationBelt( comparacao$Observed, comparacao$Predicted.Alto,devel = "external")
plot(cb, main = "Calibration Belt for Random Forest",
     xlab = "Predicted risk of prolonged stay",
     ylab = "Observed prolonged stay")


