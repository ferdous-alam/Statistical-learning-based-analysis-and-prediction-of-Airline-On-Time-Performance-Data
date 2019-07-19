#install.packages('caret')
#install.packages("dummies")
library(caret)
library(lattice)
library(ggplot2)
library(dplyr)
library(dummies)	
library(readr)
library(plyr)
library(e1071)
library(pROC)
library(ROCR)


flight <- read_csv("Flights_AllClusters_Unscaled.csv", 
                   col_types = cols(X1 = col_skip()))

#flight$ArrDel15 <-0
#flight$ArrDel15[flight$DepDelayMinutes > 14] <-1

# preprocessing 

## group depTime into small levels
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

## sample and subset data
set.seed(1)
flight <- flight[sample(1:nrow(flight), 10000, replace=FALSE),]

flight_data <-flight[,c("ArrDel15", "DepDel15", "AirTime","Month", "ArrTimeBlk", "DepTimeBlk", "Year", "CarrierCluster","FlightNumCluster","OriginCluster", "DestCluster")]
names(flight_data)
#flight_data[["ArrDel15"]] = factor(flight_data[["ArrDel15"]])
flight_data[["Year"]] = factor(flight_data[["Year"]])
flight_data[["CarrierCluster"]] = factor(flight_data[["CarrierCluster"]])
flight_data[["FlightNumCluster"]] = factor(flight_data[["FlightNumCluster"]])
flight_data[["OriginCluster"]] = factor(flight_data[["OriginCluster"]])
flight_data[["DestCluster"]] = factor(flight_data[["DestCluster"]])


## Training and test split ########
dummies <- dummyVars(~ ., data=flight_data[,-c(1:3)])
c2 <- predict(dummies, flight_data[,-c(1:3)])
data <- as.data.frame(cbind(flight_data$ArrDel15,flight_data$DepDel15,flight_data$AirTime, c2))
names(data)[1]<-'ArrDel15'
names(data)[2]<-'DepDel15'
names(data)[3]<-'Airtime'

set.seed(1)
intrain <- createDataPartition(y = data$ArrDel15, p= 0.7, list = FALSE)
training <- data[intrain,]
testing <- data[-intrain,]

#### logistic regression
fit<- glm(ArrDel15 ~. , data = training, family = binomial)
summary(fit)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)

tr.prob <- predict(fit, type = "response")
pred.tr <- ifelse(tr.prob > 0.19, "1", "0")
table(pred.tr,training$ArrDel15)

test.prob <- predict(fit, newdata = testing,type = "response")
pred.test <- ifelse(test.prob > 0.19, "1", "0")
table(pred.test,testing$ArrDel15)

library(pROC)

png("Roc_arr.png", width = 6, height = 4, units = 'in', res = 1200)

roc_curve<-roc(training$ArrDel15, as.vector(fitted.values(fit)), percent=F,   boot.n=1000, ci.alpha=0.9, stratified=FALSE, plot=TRUE, grid=TRUE, show.thres=TRUE, legacy.axes = TRUE, reuse.auc = TRUE,
    # print.thres = c(0.30,0.35, 0.40, 0.45,0.48, 0.50,0.55, 0.60),#
    print.auc = TRUE, print.thres.col = "blue", ci=TRUE, ci.type="bars", print.thres.cex = 0.7, main = paste("ROC curve using","(N = ",nrow(training),")") )
dev.off()










