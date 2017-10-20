###########################################################################
###############       RANDOM FORESTS      #################################
###########################################################################


variables =  which(apply(train_data, 2, sum) > 5)
train_data1 = train_data[,variables]
test_data1 = test_data[,variables]

train_data1 <- as.data.frame(lapply(train_data1, factorize))
test_data1 <- as.data.frame(lapply(test_data1, factorize))
val.interest.high = NULL
val.interest.high[validate_interest == "High"] = 1
val.interest.high[is.na(val.interest.high)] =0
interest.low = NULL
interest.medium = NULL
interest.high = NULL
interest.low[interest == "Low"] = 1
interest.low[is.na(interest.low)] = 0
interest.medium[interest == "Medium"] = 1
interest.medium[is.na(interest.medium)] = 0
interest.high[interest == "High"] = 1
interest.high[is.na(interest.high)] = 0
interest.low <- as.factor(interest.low)
interest.medium <- as.factor(interest.medium)
interest.high <- as.factor(interest.high)


set.seed(11)
high.rfmodel <- randomForest(as.factor(interest)~. , seed = 54,data=train_data1,ntree=2000,importance=TRUE,sampsize = 50*c(1,1,1),strata=interest)
#rfmodel
#varImpPlot(rfmodel)
rf_train_prediction <- predict(high.rfmodel, train_data1, type = "prob")
tail(rf_valid_prediction)
#prediction <- cbind(prediction.low[,2], prediction.medium[,2], prediction.high[,2])
#predictionnorm <- prediction/rowSums(prediction) 
tail(validate_interest)
#interest.integer = interest
MultiLogLoss(rf_valid_prediction, as.integer(validate_interest)-1)


OOF_prediction <- data.frame(high.rf_prediction) %>%
  mutate(max_prob = max.col(., ties.method = "last") - 1 ,
         label = validate_interest )

head(OOF_prediction)


library(caret)
confusionMatrix((as.integer(val.interest.high) ) , 
                (factor(OOF_prediction$max_prob)) ,
                mode = "everything")




best <- function(X) which.max(X)
colnames(predictionnorm) <- c("Low", "Medium", "High")
OOF_prediction <- apply(prediction, 1, best) 
head(OOF_prediction)
library(caret)
confusionMatrix(factor(OOF_prediction), 
                (as.integer(as.factor(newlabel))) ,
                mode = "everything")

for ( i in 1:N)
  + { error[i] = log(prediction[i, as.integer(interest[i])])}
sum(error)/(-N)



val.med.idx = which(validate_interest == "Medium")
val.low.idx = which(validate_interest == "Low")
val.high.idx = which(validate_interest == "High")

