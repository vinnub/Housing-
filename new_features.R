traintime = strptime(train$created, format = "%Y-%m-%d %H:%M:%S")
testtime = strptime(test$created, format = "%Y-%m-%d %H:%M:%S")

trainday = factor(format(traintime, "%a "))
trainmonth = factor(format(traintime, "%b"))
traindate = as.numeric(format(traintime, "%d"))
trainhour = as.numeric(format(traintime, "%H"))
trainminute = as.numeric(format(traintime, "%M"))

testday = factor(format(testtime, "%a "))
testmonth = factor(format(testtime, "%b"))
testdate = as.numeric(format(testtime, "%d"))
testhour = as.numeric(format(testtime, "%H"))
testminute = as.numeric(format(test_time, "%M"))

dim(train_data)
dim(test_data)
train_data_nf = cbind(train_data, long_lat$longitude, long_lat$latitude, trainday, trainmonth, traindate, trainhour, trainminute)
test_data_nf = cbind(test_data, test$longitude, test$latitude, testday, testmonth, testdate, testhour, testminute)
dim(train_data_nf)
dim(test_data_nf)
colnames(train_data_nf)
colnames(train_data_nf) <- str_replace_all(colnames(train_data_nf), c(  "'" = ""," " = "","-" = "", "/" = "", "_" = ""))
colnames(test_data_nf) <- str_replace_all(colnames(test_data_nf), c("'" = ""," " = "","-" = "", "/" = "", "_" = ""))
colnames(train_data_nf)[55] <- "walkincloset"
colnames(test_data_nf)[55] <- "walkincloset"
colnames(train_data_nf)[65] <- "actualphotos"
colnames(test_data_nf)[65] <- "actualphotos"
colnames(train_data_nf)[73] = colnames(test_data_nf)[73] = "Latitude"
colnames(train_data_nf)[72] = colnames(test_data_nf)[72] = "Longitude"
colnames(train_data_nf)[74:78] = colnames(test_data_nf)[74:78] = c("Day", "Month", "Date", "Hour", "Minute")
train_data_nf[,1:71] <- as.data.frame(lapply(train_data_nf[,1:71], factorize))
test_data_nf[,1:71] <- as.data.frame(lapply(test_data_nf[,1:71], factorize))
str(train_data_nf)


train_new_features = cbind(length_description, bathrooms, bedrooms, photos, price)
test_new_features = cbind(test_length_description, test_bathrooms,  test_bedrooms, test_photos, test_price)

train_data_nf2 = train_data_nf %>% select( -c(Length, Bathrooms, Bedrooms, Photos, Price))
test_data_nf2 = test_data_nf %>% select( -c(Length, Bathrooms, Bedrooms, Photos, Price))
train_data_nf2 = cbind(train_data_nf2, train_new_features)
test_data_nf2 = cbind(test_data_nf2, test_new_features)
colnames(train_data_nf2)[74:78] = colnames(test_data_nf2)[74:78] = c("Description", "bath", "bed", "photos", "price")
str(test_data_nf2)
dim(test_data_nf2)

train_rf_features = rbind(rf_train_prediction[,1:2], rf_valid_prediction[,1:2])
train_knn_features = unlist(list(knn_train_pred, knn_valid_pred))
train_dt_features = unlist(list(dt_train_pred, dt_valid_pred))

train_new_classifier_features = cbind.data.frame(train_nof,train_rf_features, factor(train_knn_features), train_dt_features)
test_new_classifier_features = cbind.data.frame(test_nof,rf_test_prediction[,1:2],  factor(knn_test_pred), test_dt_pred)
str(test_new_classifier_features)
head(train_new_classifier_features)
head(interest)

train_data_nf3 = cbind.data.frame(train_data_nf2, train_new_classifier_features)
test_data_nf3 = cbind.data.frame(test_data_nf2, test_new_classifier_features)
str(train_data_nf3)
str(test_data_nf3)
colnames(train_data_nf3)[c(79,82)] = colnames(test_data_nf3)[c(79,82)] <- c("NOF", "KNN")
colnames(train_data_nf3)[83] = colnames(test_data_nf3)[83] <- "Decisiontree"
which(colnames(train_data_nf3) != colnames(test_data_nf3))


train_build_man_new = cbind.data.frame(q_man, q_build)
test_build_man_new = cbind.data.frame(test_q_man, test_q_build)

train_data_nf4 = cbind.data.frame(train_data_nf2, train_build_man_new)
test_data_nf4 = cbind.data.frame(test_data_nf2, test_build_man_new)
str(train_data_nf4)
str(test_data_nf4)
colnames(test_data_nf4)
colnames(train_data_nf4)[c(79,80)] = colnames(test_data_nf4)[c(79,80)] <- c("managers", "buildings")

which(colnames(train_data_nf4) != colnames(test_data_nf4))



###     MANAGERS    ####
levels(manager)[freq_manager] -> a[1:21]
a[12]

q_man = NULL

 
  q_man [(manager %in% a)] = levels(manager) [manager[(manager %in% a)]]
q_man[is.na(q_man)] = "0"
tail(q_man)
q_man = as.factor(q_man)
test_q_man = NULL

test_q_man [(test_manager %in% a)] = levels(test_manager)[test_manager[(test_manager %in% a)] ]
test_q_man[is.na(test_q_man)] = "0"
tail(test_q_man)
test_q_man = as.factor(test_q_man)


#####     BUILDINGS     ###
b = NULL
levels(building)[freq_building] -> b[1:21]
q_build = NULL


q_build [(building %in% b)] = levels(building) [building[(building %in% b)]]
q_build[is.na(q_build)] = "0"
tail(q_build)
q_build = as.factor(q_build)
summary(q_build)
test_q_build = NULL

test_q_build [(test_building %in% b)] = levels(test_building)[test_building[(test_building %in% b)] ]
test_q_build[is.na(test_q_build)] = "0"
tail(test_q_build)
head(test_q_build)
test_q_build = as.factor(test_q_build)
summary(test_q_build)

str(test_data_nf4)

train_data_nf5 = train_data_nf4 %>% select(-c(Manager, Building))
test_data_nf5 = test_data_nf4 %>% select(-c(Manager, Building))
str(train_data_nf5)

newN = N - 3000
validate_data1 = train_data_nf5[(newN + 1): N, ]

train_data1 = train_data_nf5[1:newN,]
validate_interest = factor(target[(newN + 1): N ])
head(validate_interest)
interest = interest[1:newN ]
test_data1 =test_data_nf5
levels(interest) <- c("Low", "Medium", "High")
levels(validate_interest) <- c("Low", "Medium", "High")

