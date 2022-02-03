# Author: Prof. Igor Tona Peres, Dpt. of Industrial Engineering, PUC-Rio
# igor.peres@tecgraf.puc-rio.br
# Last updated: 02/01/2022

library(caret)
library(tidyverse)

load('training.RData')

#"D" represents "death" / "A" represents "no death" 
training$hospital_discharge_code_30day <- relevel(training$hospital_discharge_code_30day,
                                                  ref = "D")
#predictors
predictors = read.csv(file="predictors_Mortality.csv")
predictors = predictors$x
predictors = as.character(predictors)

training = training%>%
  select(predictors,hospital_discharge_code_30day)

library(base)
brierScore = function(data,lev=NULL, model = NULL){
  y_pred = data[,lev[2]]
  y_true = ifelse(data$obs == lev[2],1,0)
  brier_val = mean((y_pred-y_true)^2)
  names(brier_val) = c("brierScore")
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
                  y= training$hospital_discharge_code_30day,
                  tuneGrid = Grid,
                  method="ranger",
            metric="brierScore",
            maximize = FALSE,
            trControl = fitControl)
save(rf,file="rf_Mortality.RData")

model = rf

#Prediction Analysis

load("rf_Mortality.RData")

library(MLmetrics)
library(ModelMetrics)

load('testing.RData')

testing$hospital_discharge_code_30day <- relevel(testing$hospital_discharge_code_30day,
                                                 ref = "D")
testing = testing%>%
  select(predictors,hospital_discharge_code_30day)

Observed = data.frame(Observed=testing$hospital_discharge_code_30day)
Observed$Observed = if_else(Observed$Observed=="D",1,0)
Predicted = data.frame(Predicted = predict(model,newdata=testing,type="prob"))

brier(predicted = Predicted$Predicted.D, actual = Observed$Observed)
auc(predicted = Predicted$Predicted.D, actual = Observed$Observed)
ppv(predicted = Predicted$Predicted.D, actual = Observed$Observed)
npv(predicted = Predicted$Predicted.D, actual = Observed$Observed)
sensitivity(predicted = Predicted$Predicted.D, actual = Observed$Observed)
specificity(predicted = Predicted$Predicted.D, actual = Observed$Observed)

#Calibration Belt
library(givitiR)
comparacao = cbind(Observed,Predicted)
cb <- givitiCalibrationBelt( comparacao$Observed, comparacao$Predicted.D,devel = "external")
plot(cb, main = "Calibration Belt for Random Forest",
     xlab = "Predicted risk of mortality",
     ylab = "Observed mortality")
