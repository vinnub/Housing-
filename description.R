description = tolower(train$description)
test_description = tolower(test$description)
####  LENGTH OF DESCRIPTION
length_description = as.numeric(lapply(description, nchar ))
test_length_description = as.numeric(lapply(test_description, nchar ))
hist(length_description, breaks = 100)


jpeg("length_dscrptn.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(length_description), aes(x = length_description)) + geom_histogram(binwidth = 50, aes(fill = as.factor(interest))) + xlab("Length of description") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))  +coord_cartesian(ylim = c(0,3750),xlim=c(0,1500))
dev.off()

#length_breaks = c(as.vector(quantile(length_description,c(0,0.05, .1, 0.2,0.5,.75, 0.9))), as.vector(quantile(length_description, 1) +1))
length_breaks = c(0,8, 122, 287, 564, 809, 1090, 19085)

q_length = cut(length_description, length_breaks, right = FALSE)
test_q_length =  cut(test_length_description, length_breaks, right = FALSE)
summary(q_length)
summary(test_q_length)
levels(q_length) <- c("[0,8)" ,"[8,122)" , "[122,287)" ,"[287,564)"  , "[564,809)" ,"[564,809)"   ,  "[287,564)" )
summary(q_length)
levels(test_q_length) <- c("[0,8)" ,"[8,122)" , "[122,287)" ,"[287,564)"  , "[564,809)" ,"[564,809)"   ,  "[287,564)" )
summary(test_q_length)
length_data = cbind(q_length, interest)
jpeg("final_length.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(length_data), aes(x=as.integer(q_length))) + xlab("Length of description") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =1, aes(fill = as.factor(interest)))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +scale_x_continuous(breaks = 1:5, labels=c("0-8","8-122","122-287","Rest","564-1090"))
dev.off()

length_data_details = as_data_frame(length_data) %>% group_by(q_length) %>% 
  summarise( count = n(),low = length(which(interest ==0)), medium= length(which(interest ==1)), high= length(which(interest ==2)), 
             high_p = high/(low+medium+high), medium_p = medium/(low+medium+high))


length_data_details


     # VARIABLE Q_LENGTH IS NOW A FACTOR 
########################  ##############################################

###########################################
############        KEYWORDS ##################



keywords = c("deal", "quiet", "shopping", "maintained",  "close to",  "transport", "no pets")
keywords_matrix = sapply(keywords, grepl, description, ignore.case = TRUE)
test_keywords_matrix = sapply(keywords, grepl, test_description, ignore.case = TRUE)

apply(test_keywords_matrix, 2, sum)
keywords_data = as_data_frame(cbind(keywords_matrix, interest))



keywords_data_details = keywords_data %>% group_by(`no pets`) %>% 
  summarise( count = n(),low = length(which(interest ==0)), medium= length(which(interest ==1)), high= length(which(interest ==2)), 
             high_p = high/(low+medium+high), medium_p = medium/(low+medium+high))
keywords_data_details

names(keywords_data)[5]<- "closeto"
colnames(keywords_data)[5]<- "closeto"
names(keywords_data)[7]<- "nopets"
colnames(keywords_data)[7]<- "nopets"

# keys <- NULL
# p <- ggplot(keywords_data) + ylab("COUNTS")
# for(i in 1:8) {
#   p <- p + aes_string(x = names(keywords_data)[i]) + xlab(colnames(keywords_data[i])) + 
#     geom_histogram(aes(fill = as.factor(interest))) +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
#   print(p)
#   
#  # temp_data = keywords_data %>% filter(keywords_data[,i] == 1)
# #   pie <- ggplot(temp_data, aes_string(x = colnames(temp_data)[i]), fill = factor(interest)) + geom_bar(width = 1) 
# # pie  
# # pie <- ggplot(temp_data, aes(x = `nopets`, fill = factor(interest))) + geom_bar(width = 1)
# # pie
# # pie + coord_polar(theta = "y")
#   keys[[i]] <- p
#   # ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)
# }
# 
# 
# 
# jpeg("Keywords.jpg", width = 10, height = 10, units = 'in', res = 300)
# multiplot(keys[[1]], keys[[2]], keys[[3]], keys[[4]], keys[[5]], keys[[6]], keys[[7]], keys[[8]] ,cols = 2)
# dev.off()
# 




###   PIE CHART WHOLE ### ### ### 
pie <- ggplot(as.data.frame(keywords_data$interest), aes(x = "", fill = factor(interest))) +
  geom_bar(width = 1)

pie

jpeg("PIE_CHART.jpg", width = 4, height = 3, units = 'in', res = 300)
pie + coord_polar(theta = "y")
dev.off()

## PIE CHART FOR KEYWORDS
keys = NULL
for(i in 1:7) {
  pie <- ggplot(keywords_data %>% filter(keywords_data[,i] == 1))  + aes_string(x = names(keywords_data)[i]) + xlab(colnames(keywords_data[i])) + 
    geom_bar(aes(fill = as.factor(interest))) +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
 pie = pie + coord_polar(theta = "y")
   print(pie)
  
 # temp_data = keywords_data %>% filter(keywords_data[,i] == 1)
  #   pie <- ggplot(temp_data, aes_string(x = colnames(temp_data)[i]), fill = factor(interest)) + geom_bar(width = 1) 
  # pie  
  # pie <- ggplot(temp_data, aes(x = `nopets`, fill = factor(interest))) + geom_bar(width = 1)
  # pie
  # pie + coord_polar(theta = "y")
  keys[[i]] <- pie
  # ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)
}


jpeg("Keywords_PIE_CHART.jpg", width = 10, height = 10, units = 'in', res = 300)
multiplot(keys[[1]], keys[[2]], keys[[3]], keys[[4]], keys[[5]], keys[[6]], keys[[7]] ,cols = 2)
dev.off()





###### FEATURES MATRIX IN DESCRIPTION 
final_feat_matrix = final_feat_matrix
final_test_feat_matrix = final_test_feat_matrix

features = colnames(final_feat_matrix)
features_matrix = sapply(features, grepl, description, ignore.case = TRUE)
test_features_matrix = sapply(features, grepl, test_description, ignore.case = TRUE)
features_matrix[,1] <- 0    #PETS INCLUDE NO PETS ALSO 
test_features_matrix[,1] <- 0    #PETS INCLUDE NO PETS ALSO 

apply(test_features_matrix, 2, sum)


dim(final_feat_matrix)
dim(features_matrix)
features_description = (final_feat_matrix | features_matrix)
test_features_description = (final_test_feat_matrix | test_features_matrix)

apply(test_features_description, 2, sum)

keywords_features = cbind(keywords_matrix, features_description)
keywords_features_2 = cbind(keywords_features, q_photos, q_price, q_bed, q_bath,  q_length)
test_keywords_features = cbind(test_keywords_matrix, test_features_description)
test_keywords_features_2 = cbind(test_keywords_features, test_q_photos, test_q_price, test_q_bed, test_q_bath,  test_q_length)

## ADD MANAGERS AND BUILDINGS
keywords_features_3 = cbind(keywords_features_2, q_build, q_man, interest)
test_keywords_features_3 = cbind(test_keywords_features_2, test_q_build, test_q_man)

head(keywords_features_3)
new_train_data = cbind( train_data$`Building group`, train_data$`Manager Group`, train_data$Length, train_data$Bathrooms, train_data$Bedrooms, train_data$Price, train_data$Photos, keywords_features)

colnames(new_train_data)[1:7] <- c("Building", "Manager", "Length", "Bathrooms", "Bedrooms", "Price", "Photos")
new_test_data = cbind( test_data$`Building group`, test_data$`Manager Group`, test_data$Length, test_data$Bathrooms, test_data$Bedrooms, test_data$Price, test_data$Photos, test_keywords_features)
colnames(new_test_data)[1:7] <- c("Building", "Manager", "Length", "Bathrooms", "Bedrooms", "Price", "Photos")


write.csv(new_train_data, file = "New_train_data.csv")
write.csv(new_test_data, file = "New_test_data.csv")
