require(knitr)
require(stringr)
library(tidyr)
library(stringi)
library(ggplot2)
library(tibble)
library(VennDiagram)
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

data <- fromJSON("train.json")
testdata <- fromJSON("test.json")
vars <- setdiff(names(testdata), c("photos", "features"))
train <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)
test <- map_at(testdata, vars, unlist) %>% tibble::as_tibble(.)
#UNIQUE FEATURES
length(unique(train$features))

feature = data.frame(feature = tolower(unlist(train$features))) %>%group_by(feature) %>%
 summarise(feature_count = n()) %>%
  arrange(desc(feature_count)) %>%
  filter(feature_count >= 20)

 kable(feature, caption = "Feature Count")
 write.csv(feature, file = "feature.csv")

#COMBINE FEATURES


# Hardwood
(feature %>%
  filter(str_detect(feature, 'pool'))) %>%
  kable(caption = "wood")

# Laundry in unit
feature %>%
  filter(str_detect(feature, paste(c('laundry', 'dryer', 'washer'), collapse="|"))) %>%
  filter(!str_detect(feature, "dishwasher")) %>%
  kable(caption = "Laundry in unit")

# Roof deck
feature %>%
  filter(str_detect(feature, 'pool')) %>%
  kable(caption = "Roof Deck")

# Outdoor space
feature %>%
  filter(str_detect(feature, 'outdoor')) %>%
  kable(caption = "Outdoor Space")

# Garden
feature %>%
  filter(str_detect(feature, 'garden')) %>%
  kable(caption = "Garden")

# Park
feature %>%
  filter(str_detect(feature, 'park')) %>%
  kable(caption = "Parking")





# # Pool
# new_feature_summary%>%
#   filter(str_detect(new_features, 'pool')) %>%
#   kable(caption = "Pool")


# GYM
# feature %>%
#   filter(str_detect(feature, 'fitness')) %>%
#   kable(caption = "Gym")

# 
# 
# # DOORMAN
# feature %>%
#   filter(str_detect(feature, 'doorman')) %>%
#   kable(caption = "Doorman")

# 
# 
# # INTERNET
# feature %>%
#   filter(str_detect(feature, 'Internet')) %>%
#   kable(caption = "Internet")

# 
# # Prewar
# new_feature_summary %>%
#   filter(str_detect(new_features, 'pool')) %>%
#   kable(caption = "Prewar")

tempfeatures = lapply(train$features, tolower)
testfeatures = lapply(test$features, tolower)

merge_wood <- function(X) str_replace_all(X, c("hardwood floors" =  "HARDWOOD", "hardwood" =  "HARDWOOD"))
merge_laundry_building <- function(X) str_replace_all(X,c("laundry in building" =  "LAUNDRY ROOM","laundry room" =  "LAUNDRY ROOM") )
merge_laundry_unit <- function(X) str_replace_all(X,c("washer & dryer" = "LAUNDRY IN UNIT", "in-unit laundry in unit" = "LAUNDRY IN UNIT","laundry in unit" = "LAUNDRY in UNIT",  "on-site laundry" = "LAUNDRY in UNIT","laundry" = "LAUNDRY in UNIT", "dryer in unit" = "LAUNDRY in UNIT","washer in unit" = "LAUNDRY in UNIT","washer/dryer" = "LAUNDRY in UNIT"))
merge_roof <- function(X) str_replace_all(X, c("private roof deck" = "ROOF DECK", "common roof deck" = "ROOF DECK","roof deck"= "ROOF DECK","roof-deck"= "ROOF DECK", "roofdeck"= "ROOF DECK", "deck" = "ROOF DECK"))
merge_outdoor_private <- function(X) str_replace_all(X, c("private outdoor space"=  "PRIVATE OUTDOOR", "private-outdoor-space" = "PRIVATE OUTDOOR"))
merge_outdoor_common <- function(X) str_replace_all(X, c("building-common-outdoor-space" = "COMMON OUTDOOR","common outdoor space" = "COMMON OUTDOOR","outdoor space" = "COMMON OUTDOOR","publicoutdoor" = "COMMON OUTDOOR", "outdoor areas" = "COMMON OUTDOOR"))
merge_garden <- function(X) str_replace_all(X,c("garden/patio"= "GARDEN","residents garden"= "GARDEN", "garden"= "GARDEN"))                                               
merge_parking <- function(X) str_replace_all(X, c("valet parking" = "PARKING", "full service parking"  = "PARKING", "on-site parking lot"  = "PARKING", "on-site parking"  = "PARKING", "common parking/garage" = "PARKING","parking space" = "PARKING","parking" = "PARKING") )                                               
merge_pool <- function(X) str_replace_all(X, c("swimming pool" = "POOL", "indoor pool" = "POOL","pool" = "POOL"))
merge_gym <- function(X) str_replace_all(X, c("health club" = "GYM", "fitness center" = "GYM", "gym/fitness" = "GYM", "gym" = "GYM"))
merge_doorman <- function(X) str_replace_all(X, c("ft doorman" = "DOORMAN","full-time doorman" = "DOORMAN", "doorman" = "DOORMAN"))
merge_internet <- function(X) str_replace_all(X, c("wifi access" = "INTERNET", "high speed internet" = "INTERNET"))
merge_prewar <- function(X) str_replace_all(X, c("pre-war" = "PRE-WAR", "prewar" = "PRE-WAR")) #10498
merge_terrace <- function(X) str_replace_all(X, c( "terrace" = "ROOF DECK")) # RD + 2313
merge_garage <- function(X) str_replace_all(X, c( "on-site garage" = "PARKING", "garage" = "PARKING"))  # Parking + 899
merge_renovate <- function(X) str_replace_all(X, c("newly renovated" = "RENOVATED", "renovated" = "RENOVATED"))  #497
merge_balcony <- function(X) str_replace_all(X, c("private balcony" = "BALCONY", "private-balcony" = "BALCONY", "balcony" = "BALCONY", "patio" = "BALCONY"))    #3194
merge_ceiling <- function(X) str_replace_all(X, c("high ceilings" = "HIGH CEILING", "high ceiling" = "HIGH CEILING"))   #813
merge_super <- function(X) str_replace_all(X, c("live-in superintendent" = "LIVE IN SI", "live-in super" = "LIVE IN SI", "live in super" = "LIVE IN SI", "concierge" = "LIVE IN SI", "concierge service" = "LIVE IN SI")) #573
merge_pets <- function(X) str_replace_all(X, c("cats allowed" = "PETS", "dogs allowed" = "PETS", "pets on approval" = "PETS", "pet friendly" = "PETS"))
merge_child <- function(X) str_replace_all(X, c("childrens playroom" = "CHILDREN'S PLAYROOM", "children's playroom" = "CHILDREN'S PLAYROOM"))


merge_more_features = c(merge_wood, merge_laundry_building, merge_laundry_unit, merge_roof, merge_outdoor_private, merge_outdoor_common, merge_garden, merge_parking,merge_pool, merge_gym, merge_doorman, merge_internet, merge_prewar, merge_terrace, merge_garage, merge_renovate, merge_balcony, merge_ceiling, merge_super, merge_pets, merge_child)                                              
for (  i in 1:length(merge_more_features))
{ tempfeatures <- lapply(tempfeatures,merge_more_features[[i]] )}

#FOR TEST
for (  i in 1:length(merge_more_features))
{ testfeatures <- lapply(testfeatures,merge_more_features[[i]] )}

final_feature_summary = data.frame(final_features = (unlist(tempfeatures))) %>%group_by(final_features) %>%
  summarise(final_feature_count = n()) %>%
  arrange(desc(final_feature_count)) %>%
  filter(final_feature_count >= 40)
kable(final_feature_summary, caption = "Feature Count")
write.csv(final_feature_summary, file = "final_feature.csv")


# final_feature_summary %>%
#    filter(str_detect(final_features,ignore.case("GYM"))) %>%
#    kable(caption = "Prewar")

final_features = as.character(final_feature_summary$final_features)

#Get the feature matrix in the following way
feat_matrix = sapply(final_features, grepl, tempfeatures, ignore.case = TRUE)
test_feat_matrix = sapply(final_features, grepl, testfeatures, ignore.case = TRUE)

final_feat_matrix = feat_matrix*1
final_test_feat_matrix = test_feat_matrix *1
target = train$interest_level
final_target = as.integer(as.character(factor(target, labels = c(3,1,2))) )
final_target = final_target - 1
interest = final_target
#Test if HIGH=3, MEDIUM = 2, LOW = 1
# index = c(1111,2232,11124,5535,02321,23579)
# final_target[index]
# target[index]

f_data = as.data.frame(cbind(final_feat_matrix, final_target))




############################
#################################
#       PART2 - Visualize data 
#################################
##############################



names(f_data) <- str_replace_all(names(f_data), c("'" = ""," " = "","-" = "", "/" = "", "_" = ""))
colnames(f_data) <- str_replace_all(colnames(f_data), c("'" = ""," " = "","-" = "", "/" = "", "_" = ""))
names(f_data)[42]<- "WALKinCLOSET"
colnames(f_data)[42]<- "WALKinCLOSET"

out <- NULL
p <- ggplot(f_data) + ylab("COUNTS")
for(i in 1:16) {
  p <- p + aes_string(x = names(f_data)[i]) + xlab(colnames(f_data[i])) + 
    geom_histogram(aes(fill = as.factor(final_target)))
  print(p)
  out[[i]] <- p
  # ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)
}



jpeg("TRY.jpg", width = 8, height = 8, units = 'in', res = 300)
multiplot(out[[1]], out[[2]], out[[3]], out[[4]], out[[5]], out[[6]], out[[7]], out[[8]] ,cols = 2)
dev.off()


suffStat <- list(dm = f_data, nlev = c(rep(2,58),3), adaptDF = FALSE)
suffStatbin <- list(dm = f_data[,1:58],  adaptDF = FALSE)


#############################################
##########################################
#####       EXTRA FEATURES 
############################################
##############################################


### NUMBER OF PHOTOS #####

new_f_data = cbind(f_data, photos)
photos = as.integer(lapply(train$photos, length))
summary(photos)
hist(photos, breaks = 100)
jpeg("photos.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(new_f_data, aes(x=photos)) + xlab("Number of photos") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =4, aes(fill = as.factor(new_f_data[,59])))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()
breaks = c(0,1,4,6,8,12,20,70)
q_photos = cut(photos, breaks = breaks, right = FALSE)
summary(q_photos)
new_f_data = cbind(f_data, photos)
new_f_data = cbind(new_f_data, as.integer(q_photos))
jpeg("photos_interval.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(new_f_data, aes(x=as.integer(q_photos))) + xlab("Photos interval") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =1, aes(fill = as.factor(new_f_data[,59])))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()

test_photos = as.integer(lapply(test$photos, length))
total_photos = c(photos, test_photos)
test_q_photos = cut(test_photos, breaks = breaks, right = FALSE)
summary(test_q_photos)




######### PRICE ######


price = train$price
test_price = test$price
jpeg("PRICE.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(price[price<10000]), aes(x = price[price<10000])) + geom_histogram(binwidth = 500, aes(fill = as.factor(new_f_data[price<10000,59]))) + xlab("PRICE") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()
breaks = c(0,1000,1500,2000,2500,3000,3500,4000,5500,10000,max(price) + 1)
q_price = cut(price, breaks = breaks, right = FALSE)
test_q_price = cut(test_price, breaks = breaks, right = FALSE)
summary(test_q_price)
new_f_data = cbind(f_data, as.integer(q_price))
jpeg("price_interval.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(new_f_data, aes(x=as.integer(q_price))) + xlab("Price interval") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =1, aes(fill = as.factor(new_f_data[,59])))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()

#######BEDROOMS #######

bedrooms = train$bedrooms
test_bedrooms = test$bedrooms
jpeg("BEDROOMS.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(bedrooms), aes(x = bedrooms)) + geom_histogram(binwidth = 1, aes(fill = as.factor(new_f_data[,59]))) + xlab("BEDROOMS") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()
breaks = c(0,1,2,3,4,5,9)
q_bed = cut(bedrooms, breaks = breaks, right = FALSE)
test_q_bed = cut(test_bedrooms, breaks = breaks, right = FALSE)
summary(q_bed)
summary(test_q_bed)
new_f_data = cbind(f_data, as.integer(q_bed))
jpeg("bedrooms_interval.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(new_f_data, aes(x=as.integer(q_bed))) + xlab("Bedrooms interval") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =1, aes(fill = as.factor(new_f_data[,59])))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()



######    BATHROOMS ########


bathrooms = train$bathrooms
test_bathrooms = test$bathrooms 
jpeg("Bathrooms.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(bathrooms), aes(x = bathrooms)) + geom_histogram(binwidth = 0.5, aes(fill = as.factor(new_f_data[,59]))) + xlab("BEDROOMS") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()
breaks = c(0,1,1.5,2,2.5,max(test_bathrooms) + 1)
q_bath = cut(bathrooms, breaks = breaks, right = FALSE)
test_q_bath = cut(test_bathrooms, breaks = breaks, right = FALSE)
summary(test_q_bath)
new_f_data = cbind(f_data, as.integer(q_bath))
jpeg("bath_interval.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(new_f_data, aes(x=as.integer(q_bath))) + xlab("Bedrooms interval") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))+ geom_histogram( binwidth =1, aes(fill = as.factor(new_f_data[,59])))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
dev.off()



####################################################
#######   LOCATIONS ##############################
###################################################


latitude = train$latitude
longitude = train$longitude
new_f_data = cbind(f_data, latitude, longitude)

ggplot(new_f_data, aes(x=latitude, y = longitude)) + xlab("Latitude") + ylab ("Longitude") + scale_fill_discrete(labels = c("Low", "Medium", "High")) +guides(fill=guide_legend(title="Popularity"))+geom_point( aes(colour = as.factor(new_f_data[,59])))  




# make a scatter plot using long and lat and then plot the popularity
plot(latitude[latitude >40.5 & latitude< 41.5], longitude[latitude >40.5 & latitude< 41.5])

ll_index = which(latitude >40.55 & latitude< 40.95 & longitude > -74.1 & longitude < -73.7)
new_long = longitude[ll_index]
new_lat = latitude[ll_index]
new_pop =(as.factor(new_f_data[ll_index,59]))
jpeg("locations.jpg", width = 12, height = 10, units = 'in', res = 400)
ggplot(as.data.frame(cbind(new_long, new_lat)), aes(x=new_lat, y = new_long)) + xlab("Latitude") + ylab ("Longitude") +geom_point( aes(colour = new_pop), size = 0.5)  + scale_colour_discrete(labels = c("Low", "Medium", "High")) +guides(colour=guide_legend(title="Popularity"))
dev.off()

abs_high_index = which(new_f_data[,59] == 2)  #THESE ARE THE ABSOLUTE INDICES, 
abs_medium_index = which(new_f_data[,59]== 1)  
abs_low_index = which(new_f_data[,59] == 0)

high_index = which(new_pop == 2)  #THESE ARE NOT THE ABSOLUTE INDICES, 
medium_index = which(new_pop == 1)  #THESE ARE THE INDICES WRT NEW POP which is a shorter popularity vector
low_index = which(new_pop == 0)

#HIGH POPULARITY LOCATIONS
jpeg("locations_high.jpg", width = 12, height = 10, units = 'in', res = 400)
ggplot(as.data.frame(cbind(new_long[high_index], new_lat[high_index])), aes(x=new_lat[high_index], y = new_long[high_index])) + xlab("Latitude") + ylab ("Longitude") +geom_point(  colour = "BLUE", size = 0.5)  + scale_colour_discrete(labels = c("High")) +guides(colour=guide_legend(title="Popularity"))
dev.off()

#LOW POP LOCATIONS
jpeg("locations_low.jpg", width = 12, height = 10, units = 'in', res = 400)
ggplot(as.data.frame(cbind(new_long[low_index], new_lat[low_index])), aes(x=new_lat[low_index], y = new_long[low_index])) + xlab("Latitude") + ylab ("Longitude") +geom_point(  colour = "RED", size = 0.5)  + scale_colour_discrete(labels = c("LOW")) +guides(colour=guide_legend(title="Popularity"))
dev.off()

#MEDIUM POP LOCATIONS
jpeg("locations_medium.jpg", width = 12, height = 10, units = 'in', res = 400)
ggplot(as.data.frame(cbind(new_long[medium_index], new_lat[medium_index])), aes(x=new_lat[medium_index], y = new_long[medium_index])) + xlab("Latitude") + ylab ("Longitude") +geom_point(  colour = "darkgreen", size = 0.5)  + scale_colour_discrete(labels = c("LOW")) +guides(colour=guide_legend(title="Popularity"))
dev.off()


###############################################################
######    FEATURES AS OF NOW
features1 = cbind(final_feat_matrix, q_photos, q_price, q_bed, q_bath, latitude, longitude )


###############################################################v
#NUMBER OF FEATURES 
train_nof = as.integer(sapply(tempfeatures, length))
test_nof = as.integer(sapply(testfeatures, length))
jpeg("numberoffeatures.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(numberoffeatures), aes(x = numberoffeatures)) + geom_histogram(binwidth = 1, aes(fill = as.factor(new_f_data[,59]))) + xlab("Number of features") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(xlim=c(0,20))
dev.off()

####################################################################
############      Buildings       ###############################
############        and         ####################################
############       MANAGERS      # ###################################
########################################################################

#Building
building = (as.factor(train$building_id))
building_data = cbind(building, manager, target)
ggplot(as.data.frame(building_data), aes(x=manager, y = building)) + xlab("Building ID") + ylab ("Manager ID") + geom_point( aes(colour = as.factor(new_f_data[,59]))) + scale_colour_discrete(labels = c("Low", "Medium", "High")) +guides(colour=guide_legend(title="Popularity"))
building_data = as_data_frame(building_data)
test_building = (as.factor(test$building_id))


#BUILDING
freq_building = (building_data %>% count(building) %>% arrange(desc(n)) %>% filter(n> 123))$building
freq_build_data = building_data  %>% filter( building %in% freq_building)
jpeg("buildings.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(freq_build_data), aes(x = building)) + geom_histogram(binwidth = 100, aes(fill = as.factor(interest))) + xlab("Building ID") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(ylim=c(0,400))#+
dev.off()

#NEW DATASET SUMMARY
freq_build_data_details = freq_build_data %>% group_by(building) %>% 
  summarise( count = n(),low = length(which(interest ==0)), medium= length(which(interest ==1)), high= length(which(interest ==2)), 
             high_p = high/(low+medium+high), medium_p = medium/(low+medium+high))

#IMAGE
jpeg("freq_build_high_P.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(freq_build_data_details, aes(x = building)) + geom_bar( aes(y = high_p),colour = "blue", width =20, stat="identity")+ xlab("Building ID") + ylab ("HIGH %") + geom_line(aes(y = 0.07778813),   color = "red")
dev.off()
######

#BASED ON CONFIDENCE INTERVALS CALCULATED USING BOOTSTRAP
unusual_high_100_right = (freq_build_data_details[which(freq_build_data_details$high_p > 0.13223),])$building
unusual_high_100_left = (freq_build_data_details[which(freq_build_data_details$high_p < 0.02374),])$building
unusual_medium_100_right = (freq_build_data_details[which(freq_build_data_details$medium_p > 0.30903),])$building
unusual_medium_100_left =   (freq_build_data_details[which(freq_build_data_details$medium_p < 0.1452),])$building
  
unusual_build_right = union(unusual_high_100_right, unusual_medium_100_right)
unusual_build_left = union(unusual_high_100_left, unusual_medium_100_left)
unusual_build = union(unusual_build_left, unusual_build_right)

unusual_build_data = freq_build_data %>% filter( building %in% unusual_build)
jpeg("unusual_buildings.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(unusual_build_data), aes(x = building)) + geom_histogram(binwidth = 20, aes(fill = as.factor(interest))) + xlab("Manager ID") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(ylim=c(0,320))
dev.off()

###########################
#########   MANAGERS    #######
#########################

ggplot(as.data.frame(manager), aes(x = manager)) + geom_histogram(binwidth = 1, aes(fill = as.factor(new_f_data[,59]))) + xlab("Managers") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(xlim=c(0,20))
manager =(as.factor(train$manager_id))
manager_data = cbind(manager, target)
manager_data = as_data_frame(manager_data)
test_manager = (as.factor(test$manager_id))

freq_manager = (manager_data %>% count(manager) %>% arrange(desc(n)) %>% filter(n> 200))$manager
freq_man_data = manager_data  %>% filter( manager %in% freq_manager)
jpeg("managers.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(freq_man_data), aes(x = manager)) + geom_histogram(binwidth = 20, aes(fill = as.factor(interest))) + xlab("Manager ID") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(ylim=c(0,720))
dev.off()


#NEW DATASET SUMMARY for managers
freq_man_data_details = freq_man_data %>% group_by(manager) %>% 
  summarise( count = n(),low = length(which(interest ==0)), medium= length(which(interest ==1)), high= length(which(interest ==2)), 
             high_p = high/(low+medium+high), medium_p = medium/(low+medium+high))

freq_man_data_details %>% arrange(desc(count))



#BASED ON CONFIDENCE INTERVALS CALCULATED USING BOOTSTRAP
man_unusual_high_100_right = (freq_man_data_details[which(freq_man_data_details$high_p > 0.11500911),])$manager
man_unusual_high_100_left = (freq_man_data_details[which(freq_man_data_details$high_p < 0.04112089),])$manager
man_unusual_medium_100_right = (freq_man_data_details[which(freq_man_data_details$medium_p > 0.2846151),])$manager
man_unusual_medium_100_left =   (freq_man_data_details[which(freq_man_data_details$medium_p < 0.1690149),])$manager

unusual_man_right = union(man_unusual_high_100_right, man_unusual_medium_100_right)
unusual_man_left = union(man_unusual_high_100_left, man_unusual_medium_100_left)
unusual_man = union(unusual_man_left, unusual_man_right)

unusual_man_data = freq_man_data %>% filter( manager %in% unusual_man)
jpeg("unusual_managers.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(unusual_man_data), aes(x = manager)) + geom_histogram(binwidth = 20, aes(fill = as.factor(interest))) + xlab("Manager ID") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(ylim=c(0,720))
dev.off()

###     BUILDINGS vs MANAGERS
build_man_data = building_data %>% filter( manager %in% unusual_man | building %in% unusual_build)
jpeg("unusual_building_managers.jpg", width = 4, height = 3, units = 'in', res = 300)
ggplot(as.data.frame(build_man_data), aes(x=manager, y = building)) + xlab("Building ID") + ylab ("Manager ID") + geom_point( aes(colour = as.factor(interest))) + scale_colour_discrete(labels = c("Low", "Medium", "High")) +guides(colour=guide_legend(title="Popularity"))
dev.off()

########   QUANTIZING BUILDINGS  #######
build_group1 = intersect(unusual_high_100_left, unusual_medium_100_left) 
build_group2 = intersect(unusual_high_100_left, unusual_medium_100_right)
build_group3 = setdiff(unusual_medium_100_left, build_group1)
build_group4 = intersect(unusual_high_100_right, unusual_medium_100_right)
build_group5 = setdiff(unusual_medium_100_right, build_group4)
build_group5 = setdiff(build_group5, build_group2)



test_build_group1 = levels(building)[build_group1]
test_build_group2 = levels(building)[build_group2]
test_build_group3 = levels(building)[build_group3]
test_build_group4 = levels(building)[build_group4]
test_build_group5 = levels(building)[build_group5]

q_build = NULL
q_build [as.integer(building) %in% build_group1] = 1
q_build [as.integer(building) %in% build_group2] = 2
q_build [as.integer(building) %in% build_group3] = 3
q_build [as.integer(building) %in% build_group4] = 4
q_build [as.integer(building) %in% build_group5] = 5
q_build[is.na(q_build)] = 6

test_q_build = NULL
test_q_build [test_building %in% test_build_group1] = 1
test_q_build [test_building %in% test_build_group2] = 2
test_q_build [test_building %in% test_build_group3] = 3
test_q_build [test_building %in% test_build_group4] = 4
test_q_build [test_building %in% test_build_group5] = 5
test_q_build[is.na(test_q_build)] = 6
summary(as.factor(test_q_build))

#ggplot(as.data.frame(q_build), aes(x =q_build)) + geom_histogram(binwidth = 1, aes(fill = as.factor(interest))) + xlab("Building group") + ylab ("COUNT") +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High")) +coord_cartesian(ylim=c(0,9000))



build_keys = NULL
for(i in 1:6) {
  pie <- ggplot(as.data.frame(cbind(q_build, interest)) %>% filter(q_build == i))  + aes(x = q_build) + xlab(paste("Group", i)) + 
    geom_bar(aes(fill = as.factor(interest))) +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
  pie = pie + coord_polar(theta = "y")
  print(pie)
  
  # temp_data = keywords_data %>% filter(keywords_data[,i] == 1)
  #   pie <- ggplot(temp_data, aes_string(x = colnames(temp_data)[i]), fill = factor(interest)) + geom_bar(width = 1) 
  # pie  
  # pie <- ggplot(temp_data, aes(x = `nopets`, fill = factor(interest))) + geom_bar(width = 1)
  # pie
  # pie + coord_polar(theta = "y")
  build_keys[[i]] <- pie
  # ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)
}


jpeg("BUILDINGS_PIE_CHART.jpg", width = 10, height = 10, units = 'in', res = 300)
multiplot(build_keys[[1]], build_keys[[2]], build_keys[[3]], build_keys[[4]], build_keys[[5]], build_keys[[6]],cols = 2)
dev.off()


########   QUANTIZING MANAGERS  #######
man_group1 = union(intersect(man_unusual_high_100_left, man_unusual_medium_100_left),2146) 
man_group2 = intersect(man_unusual_high_100_left, man_unusual_medium_100_right)
man_group3 = setdiff(man_unusual_high_100_left, man_group1)
man_group3 = setdiff(man_group3, man_group2)
man_group4 = intersect(man_unusual_high_100_right, man_unusual_medium_100_right)
man_group5 = setdiff(man_unusual_medium_100_right, man_group4)
man_group5 = setdiff(man_group5, man_group2)

test_man_group1 = levels(manager)[man_group1]
test_man_group2 = levels(manager)[man_group2]
test_man_group3 = levels(manager)[man_group3]
test_man_group4 = levels(manager)[man_group4]
test_man_group5 = levels(manager)[man_group5]

q_man = NULL
q_man [as.integer(manager) %in% man_group1] = 1
q_man [as.integer(manager) %in% man_group2] = 2
q_man [as.integer(manager) %in% man_group3] = 3
q_man [as.integer(manager) %in% man_group4] = 4
q_man [as.integer(manager) %in% man_group5] = 5
q_man[is.na(q_man)] = 6

test_q_man = NULL
test_q_man [test_manager %in% test_man_group1] = 1
test_q_man [test_manager %in% test_man_group2] = 2
test_q_man [test_manager %in% test_man_group3] = 3
test_q_man [test_manager %in% test_man_group4] = 4
test_q_man [test_manager %in% test_man_group5] = 5
test_q_man[is.na(test_q_man)] = 6
summary(as.factor(q_man))

man_keys = NULL
for(i in 1:6) {
  pie <- ggplot(as.data.frame(cbind(q_man, interest)) %>% filter(q_man == i))  + aes(x = q_man) + xlab(paste("Group", i)) + 
    geom_bar(aes(fill = as.factor(interest))) +guides(fill=guide_legend(title="Popularity"))  + scale_fill_discrete(labels = c("Low", "Medium", "High"))
  pie = pie + coord_polar(theta = "y")
  print(pie)
  
  
  man_keys[[i]] <- pie
  # ggsave(filename=paste("Plot of Profit versus",colnames(movieSummary[i]),".pdf",sep=" "), plot=p)
}


jpeg("MANAGERS_PIE_CHART.jpg", width = 10, height = 10, units = 'in', res = 300)
multiplot(man_keys[[1]], man_keys[[2]], man_keys[[3]], man_keys[[4]], man_keys[[5]], man_keys[[6]],cols = 2)
dev.off()
