#rm(list=ls(all=TRUE))
library(pcalg)
library(nnet)
library(mlogit)
library(randomForest)
library(Matrix)
library(doParallel)
library(data.table)
library(readr)
require(knitr)
require(stringr)
library(MLmetrics)
library(tidyr)
library(xgboost)
library(stringi)
library(ggplot2)
library(tibble)
library(VennDiagram)
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

ALL_FEATURES <- read_csv("~/R/RentHop-Kaggle/new_train_data.csv")
long_lat = read_csv("~/R/RentHop-Kaggle/ALL_FEATURES.csv")
long_lat = long_lat %>% select( - X1)

train_data = as_data_frame(ALL_FEATURES)
train_data = train_data %>% select( - X1)
train_data = train_data %>% select( -latitude, -longitude)
colnames(train_data)

TEST_ALL_FEATURES <- read_csv("~/R/RentHop-Kaggle/new_test_data.csv")
test_data = as_data_frame(TEST_ALL_FEATURES)
test_data = test_data %>% select( - X1)

interest = as.factor(target)
levels(interest) <- c("Low", "Medium", "High")
levels(validate_interest) <- c("Low", "Medium", "High")

colnames(train_data)
colnames(train_data)[40:46] <- c("Photos", "Price", "Bedrooms", "Bathrooms", "Length", "Building group", "Manager Group")
colnames(test_data)[40:46] <- c("Photos", "Price", "Bedrooms", "Bathrooms", "Length", "Building group", "Manager Group")

train_data = train_data %>% select( -interest)


################################################################################
####################        PC ALGORITHM    ####################################
################################################################################

numlevel <- function(X) length(unique(X))

#pc_data[,40:46] = pc_data[,40:46]  - 1    # for CI tests, lowest number should be zero 
data1 = pc_data %>% select(q_photos, q_price, q_bed, q_bath, q_length, q_build, q_man, interest) 
data2 = pc_data %>% select(deal, quiet, shopping, maintained, `close to`, `no pets`, q_photos, q_price, q_bed, q_bath, q_length, q_build, q_man, interest) 

numlevels = apply(train_data, 2, numlevel)
suffStat <- list(dm = cbind(train_data -1, as.numeric(interest) - 1), nlev = c(numlevels,3), adaptDF = FALSE)
model <- pcalg::skeleton(suffStat,
                   indepTest = disCItest, ## (G^2 statistics independence test)
                   alpha = 0.1 *10^(-1), labels = c(colnames(train_data), "Interest"), verbose = FALSE, m.max = 1)
quartz()
plot(model)


pc_data$interest <- as.numeric(pc_data$interest)
# Fish <- mlogit.data(pc_data, shape = "wide", varying = NULL, choice = "interest")
# head(Fish)
# mlogit(interest ~., Fish)

################################################################################
####################      DECISION TREES      ###################################
################################################################################


#DECISION TREE
# install.packages('rattle')
# install.packages('rpart.plot')
# install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)


library(MLmetrics)
library(rpart)
N = 49352
set.seed(11)
dim(train_data1)
dim(validate_data1)
dtfit<- rpart(interest~. , data = train_data1,method="class",control=rpart.control(xval = 5, cp= 0, minsplit =70))
plot(dtfit)
dtfit
#fancyRpartPlot(dtfit)

# N = 49352

dt_train_pred <- predict(dtfit, train_data1, type = "class")
xtab = table(dt_train_pred, interest)
confusionMatrix(xtab)

dt_valid_pred <- predict(dtfit, train_data1, type = "prob")
xtab = table(dt_valid_pred, validate_interest)
xtab
confusionMatrix(xtab)

test_dt_pred <- predict(dtfit, test_data1, type = "class")

head(Prediction)
head(predicted)
#data1$interest[1:5]
#m = matrix(c(0.1,0.9,0.9,0.1), nrow = 2)
MultiLogLoss(dt_valid_pred, as.integer(interest))
MultiLogLoss(boost_prediction, as.integer(validate_interest))
MultiLogLoss((boost_prediction + dt_valid_pred )/rowSums(boost_prediction + dt_valid_pred), as.integer(validate_interest))

dtfit##
# Boosting with weights 
# How did I get 0.60 with decision trees 


################################################################################
####################      BOOSTING        ######################################
################################################################################
 
library(xgboost)
library(data.table)
library(Matrix)


train_data1 = train_data1
str(train_data1)



write.csv(train_data_nf3, file = "Train_data_date_knn.csv")
write.csv(test_data_nf3, file = "Test_data_date_knn.csv")

factorize <- function(X) as.factor(X)



dim(train_data1)


#TRYING PAIRWISE CLASSIFICATION
# newdata = train_data1 %>% filter(interest != "High")
# newlabel = as.data.frame(interest) %>% filter(interest != "High")
# newlabel = newlabel$interest
# 
# newdata1 = train_data1 %>% filter(interest != "Low")
# newlabel1 = as.data.frame(interest) %>% filter(interest != "Low")
# newlabel1 = newlabel1$interest



str(train_data1)
train_df <- data.table(train_data1, keep.rownames = F)
dim(train_df)
sparse_matrix <- sparse.model.matrix(~.-1, data = train_df)

sparse_matrix@Dimnames[[2]]

train_interest = as.factor((interest))
head(train_interest)

bst <- xgboost(data = sparse_matrix, seed = 54,
               label = as.numeric(interest) - 1,
               params = xgb_params, 
               nrounds = 175,nthread = 3,
               early_stopping_rounds = 50,
               verbose = TRUE )

validate_df <- data.table::data.table(validate_data1, keep.rownames = F)
validate_sparse_matrix <- sparse.model.matrix(~.-1, data = validate_df)
dim(validate_sparse_matrix)
validate_predict1 <- predict(bst, validate_sparse_matrix)
validate_predict <- matrix(validate_predict1, ncol = 3, byrow = TRUE)
boost_prediction <- validate_predict
MultiLogLoss(validate_predict, as.integer(validate_interest)-1)


test_df <- data.table(test_data1, keep.rownames = F)
test_sparse_matrix <- sparse.model.matrix(~.-1, data = test_df)
dim(test_sparse_matrix)
test_predict <- predict(bst, test_sparse_matrix)
final_test_predict <- matrix(test_predict, ncol = 3, byrow = TRUE)
write.csv(cbind(testdata$listing_id, final_test_predict), file = "submission8_classifier_features.csv")
head(final_test_predict)
weights = ifelse(interest=='Low', 1, ifelse(interest == "Medium", 1,1))


traindata = xgb.DMatrix(data = sparse_matrix, label =as.numeric(train_interest)-1)

numberOfClasses <- length(unique(interest))
nround    <- 2000 # number of XGBoost rounds
cv.nfold  <- 5


xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "eta "= .05, "max_depth" =4,
                   "subsample" = 0.8,
                   "max_delta_step" = 10, 
                   "num_class" = numberOfClasses, 
                   "min_child_weight" = 1
                   )

bin_xgb_params <- list("objective" = "binary:logistic",
                   "eval_metric" = "error",
                   "eta "= .05, "max_depth" =3,
                   "subsample" = 0.8, 
                   "max_delta_step" = 10)

set.seed(54)
cv_model <- xgb.cv(params = xgb_params,
                   data = traindata,
                   nrounds = nround,
                   nfold = 10,
                   verbose = TRUE,
                   prediction = TRUE, 
                   early_stopping_rounds = 20)
boost_train_pred <- cv_model$pred

head(cv_model$pred)
# head(interest)
# head(pred)

# MultiLogLoss(pred, as.integer(interest))
# 
# 
# head(prediction)
# head(interest)
low.pred = cbind( 1-pred, pred)
 head(low.pred)
# predictionnorm <- prediction/rowSums(prediction) 
# head(predictionnorm)
MultiLogLoss(boost_train_pred, as.integer(interest)-1)

OOF_prediction <- data.frame(high.pred) %>%
  mutate(max_prob = ifelse((high.pred[,2] > 0.5), 2, 1) ,
         label = interest )

head(OOF_prediction)


library(caret)
confusionMatrix((as.integer(interest.high) +1 ) , 
               (factor(OOF_prediction$max_prob) ) ,
                mode = "everything")

# compute feature importance matrix
names = sparse_matrix@Dimnames[[2]]
importance_matrix = xgb.importance(feature_names = names, model = bst)
head(importance_matrix)
gp = xgb.plot.importance(importance_matrix)
print(gp)


######################################################################
##############    GRID SEARCH FOR HYPER PARAMETERS      ##############
######################################################################

searchGridSubCol <- expand.grid(eta = c( 0.02,0.05, 0.1), 
                                max.depth = c(3,4,5), 
                                subsample = c(0.5,0.8,1))
ntrees <- 50

#Build a xgb.DMatrix object
DMMatrixTrain <- traindata

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
  
  #Extract Parameters to test
  currenteta <- parameterList[["eta"]]
  currentmax.depth <- parameterList[["max.depth"]]
  current.subsample <- parameterList[["subsample"]]
  
  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = 1500, nfold = 6, showsd = TRUE, 
                            verbose = TRUE, "objective" = "multi:softprob",
                           "eval_metric" = "mlogloss",
                           "num_class" = 3,  "max.depth" = currentmax.depth, "eta" =  currenteta,                               
                           "subsample" = current.subsample, "colsample_bytree" = 1, early_stopping_rounds = 20)
  
 
  #Save rmse of the last iteration
  rmse <- tail(xgboostModelCV$evaluation_log$test_mlogloss_mean, 1)
  rmsetrain <- tail(xgboostModelCV$evaluation_log$train_mlogloss_mean, 1)
  return(c(rmse,rmsetrain, currenteta, currentmax.depth, current.subsample))
  
})
jpeg("xgboost_tuning.jpg", width = 4, height = 3, units = 'in', res = 300)
 plot(rmseErrorsHyperparameters[1,], ylab = "CV test mlogloss", xlab = "Parameter Combination ID" , col = "blue")
dev.off()
 gridsearch3 <- rmseErrorsHyperparameters
grid <- cbind(gridsearch1, gridsearch2)
plot(grid[1,])

##################################################################################
########################    Conditional Inference Forests   #######################
####################################################################################
# library(party)
# set.seed(11)
# cfmodel <- cforest(interest~. ,  data=train_data1,weights = ifelse(interest=='Low', 1, ifelse(interest == "Medium", 2,3)),  controls=cforest_unbiased(ntree=5, mtry=5))
# cf_prediction <- predict(cfmodel, train_data1, OOB = TRUE, type = "prob")
# cf_p <- matrix(unlist(cf_prediction), ncol = 3, byrow = TRUE)
# MultiLogLoss(cf_p, interest)


##################################################################################
########################    Conditional Trees  #######################
####################################################################################
ctmodel <- cforest(interest~. ,  data=train_data1, controls=cforest_unbiased(mtry = 10, ntree = 100))
rct_prediction <- predict(ctmodel, train_data1_nf, OOB = TRUE,type = "prob")
head(ct_prediction)
head(interest)
ct_train_predict = matrix(0,N - newN, 3)
for( i in 1:(N - newN))
{  ct_valid_predict[i,1:3] = as.vector(ct_prediction[[i]])}
MultiLogLoss(ct_valid_predict, as.integer(validate_interest))

head(ct_valid_predict)
head(boost_prediction)
####################################################################
########################    CARET PACKAGE  #######################
#####################################################################

# # Create model with default paramters
# control <- trainControl(method="repeatedcv", number=2, repeats=2)
# seed <- 7
# metric <- "mlogloss"
# set.seed(seed)
# x = train_data1
# mtry <- ceiling(sqrt(ncol(x)))
# tunegrid <- expand.grid(.mtry=c(4:8))
# rf_default <- train(interest~., data=x, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
# print(rf_default)
# 
# 
# 
# # Algorithm Tune (tuneRF)
# set.seed(seed)
# bestmtry <- tuneRF(x, interest, stepFactor=1.5, improve=1e-5, ntree=500)
# print(bestmtry)
