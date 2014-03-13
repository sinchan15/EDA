library("doBy")
library(ggplot2)
#install.packages("ggmap")
#install.packages("rworldmap")
library(ggmap)
library(rworldmap)
library("plyr")

flights<-read.csv("flights.csv")

#To Convert the Origin City to a unique list of city and corresponding state
list <- unique(strsplit(as.character(flights$Origin.City), ","))
origin<-ldply(list)
colnames(origin)<-c("City", "State")


#To Convert the Destination City to a unique list of city and corresponding state
list <- unique(strsplit(as.character(flights$Destination.City), ","))
destination<-ldply(list)
colnames(destination)<-c("City", "State")

#get latitude and longitude
#Calling the API like a webservice and getting the value since it
#is not available readily in the dataset
#This is a one time job. We can just fetch the latitude and longitude
#details and persist it and need to be updated when a new air port 
#is added or introduced

library(stringr)
ads1 <- unique(origin$City)
ads2 <- unique(origin$State)
ads <- paste(ads1, ads2,"USA",sep = ",")
ads <- str_trim(ads)
lonlat<-geocode(ads)
origin<-cbind(origin, lonlat)


#plot the map
map(database= "world", xlim = c(-139.3, -58.8), ylim = c(13.5, 55.7), col="grey80", fill=TRUE, projection="gilbert", orientation= c(90,0,225))
title("Spread of airports in USA")
lon<-lonlat$lon
lat<-lonlat$lat
coord <- mapproject(lon, lat, proj="gilbert", orientation=c(90, 0, 225))
points(coord, pch=20, cex=1.2, col="red")  #plot converted points

#newmap <- getMap(resolution = "low")
#Limit it to Eurpoe (the limits are latitude/longitude coordinates)
#plot(newmap, xlim = c(-139.3, -58.8), ylim = c(13.5, 55.7), asp = 1, main = "US from worldmap")


pas_seat_ratio<-summaryBy(Passengers/Seats+Flights~flights$Origin.City, data=(subset(flights, Seats>0)),FUN=c(sum,mean))

colnames(pas_seat_ratio) <- c("origin","pasToSeatRatioSum","FlightSum","pasToSeatRatio","FlightRatio")

#create categories
pas_seat_ratio$demand[pas_seat_ratio$pasToSeatRatio>=0.6]<-"More Demand"
pas_seat_ratio$demand[pas_seat_ratio$pasToSeatRatio>0.3 & pas_seat_ratio$pasToSeatRatio<0.6]<-"Average"
pas_seat_ratio$demand[pas_seat_ratio$pasToSeatRatio>=0 & pas_seat_ratio$pasToSeatRatio<=0.3]<-"Poor Demand"

#Convert the column to a factor
pas_seat_ratio$demand<-factor(pas_seat_ratio$demand)

#Plot Demand distribution among Airports
plot(pas_seat_ratio$demand, type='o', col="blue", ann=FALSE)
title(main="Demand distribution among Airports", col.main="red", font.main=4)
title(ylab="# of Airports --->", col.lab=rgb(0,0.5,0))
title(xlab="Demand", col.lab=rgb(0,0.5,0))


#Distribution of flights in the areas of high demand, low and average demand areas
mean_Flights<-summaryBy(FlightRatio~pas_seat_ratio$demand, data=pas_seat_ratio,FUN=mean)
flights$ <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
x <- data.frame(flights$Seats, flights$Passengers)

mat = as.matrix(x)

kclus <- kmeans(mat,5)

#plotcluster(mat,kclus$cluster)

plot(mat,col = kclus$cluster)
