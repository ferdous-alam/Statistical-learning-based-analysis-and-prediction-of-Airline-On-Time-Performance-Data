
library(e1071)
library(dplyr)

flights_reduced$Flight<-1
flights_reduced$Route<-paste(flights_reduced$Origin,flights_reduced$Dest)

#set.seed(1)
#a<-sample(nrow(df),300000)
#training<-rbind(df[a,])
#testing<-df[-a,]

#kmeans clustering of locations based on frequency of flights, and number of destinations
#first, build info on the locations
locations<-{}
locations<-flights_reduced %>% 
  group_by(Origin) %>% 
  summarize(Flights=sum(Flight),
            Routes=length(unique(Route)),
            Carriers=length(unique(Carrier)),
            DepDels=sum(DepDel15),
            ArrDels=sum(ArrDel15),
            DepTimeBlks=length(unique(DepTimeBlk)),
            FlightNum=length(unique(FlightNum)))

a<-as.data.frame(scale(locations[,2:8]))

#next, clustering
clusters<-kmeans(a[,1:7],4)
locations$clusters<-as.factor(clusters$cluster)
plot(locations$Flights,locations$Routes, col=locations$clusters, xlab = "Frequency of Flights", ylab = "Routes Connected", main = "Airport Clustering")

library(cluster) 
clusplot(locations, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

#how many clusters is best? (results below are from running the code above with different #s of clusters)
#2 components explain 93.52% of point variability
#3 components explain 96.11% of point variability
#4 components explain 96.9% of point variability
#5 components explain 94.65% of point variability

# Determine number of clusters
wss <- (nrow(locations[,2:8])-1)*sum(apply(locations[,2:8],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(locations[,2:8], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "Number of Location Clusters")



#next, cluster the airlines
#first, build info on the carriers
carriers<-{}
carriers<-flights_reduced %>% 
  group_by(Carrier) %>% 
  summarize(Flights=sum(Flight),
            Routes=length(unique(Route)),
            DepDels=sum(DepDel15),
            ArrDels=sum(ArrDel15),
            DepTimeBlks=length(unique(DepTimeBlk)),
            FlightNum=length(unique(FlightNum)),
            Origins=length(unique(Origin)),
            TailNum=length(unique(TailNum)))

a<-as.data.frame(scale(carriers[,2:9]))

#next, clustering
clusters<-kmeans(a[,1:8],4)
carriers$clusters<-as.factor(clusters$cluster)
plot(carriers$Flights,carriers$Routes, col=carriers$clusters, xlab = "Frequency of Flights", ylab = "Routes Connected", main = "Airline Clustering")

clusplot(carriers, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Determine number of clusters
wss <- (nrow(carriers[,2:9])-1)*sum(apply(carriers[,2:9],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(carriers[,2:8], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "Number of Airline Clusters")


#finally, cluster flight #s
#first, build info on the carriers
flightnums<-{}
flightnums<-flights_reduced %>% 
  group_by(FlightNum) %>% 
  summarize(Flights=sum(Flight),
            Routes=length(unique(Route)),
            DepDels=sum(DepDel15),
            ArrDels=sum(ArrDel15),
            DepTimeBlks=length(unique(DepTimeBlk)),
            Carriers=length(unique(Carrier)),
            Origins=length(unique(Origin)),
            TailNum=length(unique(TailNum)))

a<-as.data.frame(scale(flightnums[,2:9]))

#next, clustering
clusters<-kmeans(a[,1:8],6)
flightnums$clusters<-as.factor(clusters$cluster)
plot(flightnums$Flights,flightnums$Routes, col=flightnums$clusters, xlab = "Frequency of Flights", ylab = "Routes Connected", main = "FlightNums Clustering")

clusplot(flightnums, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Determine number of clusters
wss <- (nrow(flightnums[,2:9])-1)*sum(apply(flightnums[,2:9],2,var))
for (i in 2:15) wss[i] <- sum(kmeans(flightnums[,2:8], 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main = "Number of FlightNums Clusters")

#now we have 4 clusters for aiports, 4 for airlines, and 6 for flight numbers

#now, having concluded that these clusters are good enough, we apply these labels to the training and testing data and upload that for future use.
a<-merge(flights_reduced,locations,by.x="Origin", by.y = "Origin")
a$OriginCluster<-a$clusters #do some tidying up after that, too
a$clusters<-NULL
a$X<-NULL
a$OriginFlights<-a$Flights.y
a$Flights.y<-NULL
a$Flights.x<-NULL
a$OriginRoutes<-a$Routes
a$Routes<-NULL

#then for the destination, using the same clusters
b<-merge(a,locations,by.x = "Dest", by.y = "Origin")
b$DestFlights<-b$Flights
b$DestRoutes<-b$Routes
b$DestCluster<-b$clusters
b$Flights<-NULL
b$Routes<-NULL
b$clusters<-NULL

#and for airlines using those clusters
c<-merge(b,carriers,by.x = "Carrier", by.y = "Carrier")
c$TailNum.y<-NULL
c$Origins<-NULL
c$FlightNum.y<-NULL
c$DepTimeBlks.y<-NULL
c$DepTimeBlks.x<-NULL
c$Flights.y<-NULL
c$Flights.x<-NULL
c$TailNum<-c$TailNum.x
c$TailNum.x<-NULL
c$FlightNum.y<-NULL
c$FlightNum<-c$FlightNum.x
c$FlightNum.x<-NULL
c$Carriers.x<-NULL
c$Carriers.y<-NULL
c$DepDels.x<-NULL
c$DepDels.y<-NULL
c$ArrDels.x<-NULL
c$ArrDels.y<-NULL
c$CarrierCluster<-c$clusters
c$clusters<-NULL

#and for flightnumbers using those clusters
d<-merge(c,flightnums,by.x = "FlightNum",by.y = "FlightNum")
d$TailNum.y<-NULL
d$TailNum<-d$TailNum.x
d$TailNum.x<-NULL
d$ArrDels.y<-NULL
d$ArrDels<-d$ArrDels.x
d$ArrDels.x<-NULL
d$DepDels.y<-NULL
d$DepDels<-d$DepDels.x
d$DepDels.x<-NULL
d$Routes.x<-NULL
d$Routes.y<-NULL
d$Flights.y<-NULL
d$Flights.x<-NULL
d$FlightNumCluster<-d$clusters
d$clusters<-NULL
d$DepTimeBlks.x<-NULL
d$DepTimeBlks.y<-NULL
d$Carriers<-NULL
d$Origins<-NULL
d$ArrDels<-NULL
d$DepDels<-NULL

#now, this dataset is reasonably tidy and contains cluster info on origin, dest, carrier, and flight#

write.csv(d,"Flights_AllClusters.csv")
#note - the number of observations has fallen by 1; seems that one of the destination airports was only a destination once and never an origin
#this makes sense, given that our data is a random sample

