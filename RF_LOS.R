# Author: Prof. Igor Tona Peres, Dpt. of Industrial Engineering, PUC-Rio
# igor.peres@tecgraf.puc-rio.br
# Last updated: 02/01/2022

library(caret)
library(tidyverse)

load('training.RData')

#predictors
predictors = read.csv(file="predictors_LOS.csv")
predictors = predictors$x
predictors = as.character(predictors)

training = training%>%
  select(predictors,hospital_length_stay_ajus_trunc)

#basic parameter tuning
fitControl <- trainControl(## 5-fold CV
  method = "cv", number = 5, verboseIter = TRUE,returnData = FALSE,trim = TRUE)

#RF
library(ranger)
set.seed(476)
Grid = expand.grid(mtry = c(5:15),
                      min.node.size = c(3:10),
                      splitrule =  c("variance","extratrees","maxstat","beta")
                      )

rf <- train(x=training[,-ncol(training)],
                  y= training$hospital_length_stay_ajus_trunc,
                  tuneGrid = Grid,
                  method="ranger",
                  metric="RMSE",
                  trControl = fitControl)

model = rf

#Prediction Analysis

load("rf_LOS.RData")

library(MLmetrics)

load('testing.RData')

testing = testing%>%
  select(predictors,hospital_length_stay_ajus_trunc)

Observed = data.frame(Observed=testing$hospital_length_stay_ajus_trunc)
Predicted = data.frame(Predicted = predict(rf,newdata=testing))
Predicted$Predicted[Predicted$Predicted<0] = 0
Predicted$Predicted[Predicted$Predicted>30] = 30

Erro = RMSE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
Erro
R2 = R2_Score(y_pred = Predicted$Predicted, y_true = Observed$Observed)
R2
MAE = MAE(y_pred = Predicted$Predicted, y_true = Observed$Observed)
MAE

comparacao_boosting = as.data.frame(cbind(Observed,Predicted))
correlacao = cor(comparacao_boosting$Observed,comparacao_boosting$Predicted)

ggplot(aes(x=Predicted,y=Observed),data=comparacao_boosting)+ggtitle("Calibration for Random Forest")+
  geom_smooth()+
  geom_segment(aes(x=0,y=0,xend=30,yend=30))