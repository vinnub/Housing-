med.idx = which(interest == "Medium")
low.idx = which(interest == "Low")
high.idx = which(interest == "High")
bst = list()

med.idx = which(interest == "Medium")
low.idx = which(interest == "Low")
high.idx = which(interest == "High")




xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "eta "= .2, "max_depth" =4,
                   "subsample" = 0.8, 
                   "max_delta_step" =0, 
                   "num_class" = numberOfClasses)

numberOfClasses <- length(unique(bagginterest))
nround    <- 2000 # number of XGBoost rounds
cv.nfold  <- 5
OOBlogloss = NULL
final_test_predict = array(dim = c(74659, 3,5))
validate_prediction <- array(dim = c(N - newN, 3,5))
for( i in 1:5) 
{

boot.population <- as.integer(low.idx)
boot.low.idx = sample(boot.population, size =30000, replace = FALSE)

boot.population <- as.integer(high.idx)
boot.high.idx = sample(boot.population, size = 3000, replace = FALSE)

boot.population <- as.integer(med.idx)
boot.med.idx = sample(boot.population, size = 8000, replace =FALSE)

bagg1 = rbind(train_data1[boot.low.idx,],train_data1[boot.med.idx,],train_data1[boot.high.idx,])
bagginterest = c(interest[boot.low.idx], interest[boot.med.idx], interest[boot.high.idx])
bagg1 <- as.data.frame(lapply(bagg1, factorize))



train_df <- data.table(bagg1, keep.rownames = F)
sparse_matrix <- sparse.model.matrix(bagginterest~.-1, data = train_df)
traindata = xgb.DMatrix(data = sparse_matrix, label =as.numeric(bagginterest)-1)

cv_model <- xgb.cv(params = xgb_params,
                   data = traindata,
                   nrounds = nround,
                   nfold = 5,
                   verbose = TRUE,
                   prediction = FALSE,
                   early_stopping_rounds = 5)



bst[[i]] <- xgboost(data = sparse_matrix, 
               label = as.numeric(bagginterest) - 1,
               params = xgb_params, 
               seed = 11,
               nrounds =100,nthread = 3,
               verbose = TRUE )

#testset = train_data1[-c(boot.high.idx, boot.low.idx, boot.med.idx),]

validate_df <- data.table::data.table(validate_data1, keep.rownames = F)
validate_sparse_matrix <- sparse.model.matrix(~.-1, data = validate_df)
#dim(validate_sparse_matrix)
test_predict1 <- predict(bst[[i]], validate_sparse_matrix)
test_predict <- matrix(test_predict1, ncol = 3, byrow = TRUE)
validate_prediction[,,i] <-test_predict 
print(i)
OOBlogloss[i] = print(MultiLogLoss(test_predict,as.integer(validate_interest)))


test_df <- data.table(test_data1, keep.rownames = F)
test_sparse_matrix <- sparse.model.matrix(~.-1, data = test_df)
final_test_predict1 <- predict(bst[[i]], test_sparse_matrix)
final_test_predict[,,i] <- matrix(final_test_predict1, ncol = 3, byrow = TRUE)






}





validpred = apply(validate_prediction,c(1,2),mean) 
finalpred = apply(final_test_predict,c(1,2),mean) 
head(baggpred)
head(interest)
MultiLogLoss(validpred, as.integer(validate_interest))
write.csv(cbind(testdata$listing_id, finalpred), file = "submission2_bagging.csv")









bagpred1 <- cv_model$pred

tail(bagpred)
tail(bagginterest)
head(pred)

MultiLogLoss(bagpred, as.integer(bagginterest))


head(prediction)
head(interest)
predictionnorm <- prediction/rowSums(prediction) 
head(predictionnorm)
MultiLogLoss(predictionnorm, as.integer(interest))

OOF_prediction <- data.frame(predictionnorm) %>%
  mutate(max_prob = max.col(., ties.method = "last") - 1 ,
         label = interest )

head(OOF_prediction)


library(caret)
confusionMatrix((as.integer(interest) - 1) , 
                (factor(OOF_prediction$max_prob)) ,
                mode = "everything")







