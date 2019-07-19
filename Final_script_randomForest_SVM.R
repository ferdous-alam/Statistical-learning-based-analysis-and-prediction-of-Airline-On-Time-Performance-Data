# rm(list=ls()) # clear environment
setwd("U:/Spring_2019/STAT 6500/Project");
library(caret);
library(lattice);
library(ggplot2);
library(readr);
library(plyr); 
library(dplyr); 
library(dummies);
library(e1071); 
library(rpart);
library(randomForest);

## import data ###

flight <- read_csv("Flights_AllClusters.csv",col_types = cols(X1 = col_skip()))
names(flight); # name of the variables
#str(flight);
#head(flight);


######################################
## Data Preprocessing
######################################

# group depTime into small levels
flight$DepTimeBlk[flight$DepTimeBlk %in% c("0600-0659", "0700-0759", "0800-0859", "0900-0959", 
                                           "1000-1059", "1100-1159", "1200-1259")] <- 1

flight$DepTimeBlk[flight$DepTimeBlk %in% c("1300-1359", "1400-1459", "1500-1559", "1600-1659", 
                                           "1700-1759", "1800-1859", "1900-1959")] <- 2
flight$DepTimeBlk[flight$DepTimeBlk %in% c("2000-2059", "2100-2159", "2200-2259", "2300-2359", 
                                           "0001-0559")] <-3
count(flight, 'DepTimeBlk')

## group arrTime into small levels
flight$ArrTimeBlk[flight$ArrTimeBlk %in% c("0600-0659", "0700-0759", "0800-0859", "0900-0959", 
                                           "1000-1059", "1100-1159", "1200-1259")] <- 1
flight$ArrTimeBlk[flight$ArrTimeBlk %in% c("1300-1359", "1400-1459", "1500-1559", "1600-1659", 
                                           "1700-1759", "1800-1859", "1900-1959")] <- 2
flight$ArrTimeBlk[flight$ArrTimeBlk %in% c("2000-2059", "2100-2159", "2200-2259", "2300-2359", 
                                           "0001-0559")] <-3
count(flight, 'ArrTimeBlk')


## group Month into small levels
flight$Month[flight$Month %in% c(3,4,5)] <- "Spring"
flight$Month[flight$Month %in% c(6,7,8)] <- "Summer"
flight$Month[flight$Month %in% c(9,10,11)] <-"Fall"
flight$Month[flight$Month %in% c(12,1,2)] <-"Winter"
count(flight, 'Month')

##############################
## Sample Data 
#############################
flight <- flight[sample(1:nrow(flight), 1000, replace=FALSE),]
#dim(flight);
flight_data <-select(flight,DepDel15, Month, ArrTimeBlk, DepTimeBlk, AirTime, Year, CarrierCluster,FlightNumCluster,OriginCluster, DestCluster)
names(flight_data)
flight_data[["DepDel15"]] = factor(flight_data[["DepDel15"]])
flight_data[["Year"]] = factor(flight_data[["Year"]])
flight_data[["CarrierCluster"]] = factor(flight_data[["CarrierCluster"]])
flight_data[["FlightNumCluster"]] = factor(flight_data[["FlightNumCluster"]])
flight_data[["OriginCluster"]] = factor(flight_data[["OriginCluster"]])
flight_data[["DestCluster"]] = factor(flight_data[["DestCluster"]])

####################################
## Training and test split 
####################################
set.seed(1)
intrain <- createDataPartition(y = flight_data$DepDel15, p= 0.7, list = FALSE)
training <- flight_data[intrain,]
testing <- flight_data[-intrain,]

################################
## encode to dummy variables 
################################
dummies <- dummyVars(~ ., data=training[,-c(1,5)])
c2 <- predict(dummies, training[,-c(1,5)])
d_training <- as.data.frame(cbind(training$DepDel15,training$AirTime, c2))

dummies <- dummyVars(~ ., data=testing[,-c(1,5)])
c2 <- predict(dummies, testing[,-c(1,5)])
d_test <- as.data.frame(cbind(testing$DepDel15, training$AirTime, c2))


##########################################
### Support Vector Machine Classification  
##########################################
gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
tune.out <- tune.svm(as.factor(V1) ~., data=d_training, 
                     kernel='radial', cost=2^(-1:5), gamma = gammalist)
summary(tune.out)
summary(tune.out$best.model)
svm1 <- predict(tune.out$best.model, d_test[,-1])
confusionMatrix(svm1, as.factor(d_test$V1))

tune.out2 <- tune.svm(as.factor(V1) ~., data=d_training, 
                      kernel='linear', cost=2^(-1:5), gamma = gammalist)
summary(tune.out2)
summary(tune.out2$best.model)
svm2 <- predict(tune.out2$best.model, d_test[,-1])
confusionMatrix(svm2, as.factor(d_test$V1))


##########################################
### Random Forest 
##########################################

# training set
model2_train <- randomForest(as.factor(V1) ~., data = d_training, ntree = 500, mtry = 6, importance = TRUE);
model2_train

# plot variable importance 
importance(model2_train)        
varImpPlot(model2_train)

# test set
predValid <- predict(model2_train, d_test, type = "class")
mean(predValid == d_test$V1)                    
table(predValid,d_test$V1)

# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(as.factor(V1) ~., data = d_training, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, d_test, type = "class")
  a[i-2] = mean(predValid == d_test$V1)
}
 
a
 
plot(3:8,a)













