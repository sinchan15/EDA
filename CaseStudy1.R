data1 <- read.csv ("C:/Users/Sinchan/Desktop/Data Intensive Computing/doing_data_science-master/dds_datasets/nyt1.csv", header = T,sep = ',')
data1
head(data1)
data1 <-
  transform(
    data1,
    age_group =
      ifelse (
        Age %in% seq (0, 17),
        "<18",
        ifelse (
          Age %in% seq (18, 24),
          "18 - 24",
          ifelse (
            Age %in% seq (25, 34),
            "25-34",
            ifelse (
              Age %in% seq (35, 44),
              "35 - 44",
              ifelse (
                Age %in% seq (45, 54),
                "45 - 54",
                "> 65"
              )
            )
          )
        )
      )
  )

head(data1)
data1$agecat <-cut(data1$Age,c(-Inf,0,18,24,34,44,54,64,Inf))
data1

install.packages("doBy")
library("doBy")
siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
summaryBy(Age~agecat, data =data1, FUN=siterange)
summary(data1$S)

install.packages("ggplot2")
library(ggplot2)
q1 <- ggplot(data1, aes(x=Impressions, fill=age_group))
q1 +geom_histogram(binwidth=1)
ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))
+geom_boxplot()

q2 <- ggplot(subset(data1, Impressions>0), aes(x=Clicks,colour=agecat))
q2 + geom_density()
attach(data1)
hist(Gender, breaks = 5, col = "green")

ggplot(subset(data1, Clicks>0), aes(x=agecat, y=Clicks, fill=agecat)) + geom_boxplot()
siterange<-function(x){c(length(x),min(x),mean(x),max(x))}
summaryBy(Age~age_group, data=data1, FUN=siterange, fun.names=c("length","Min","Mean","Max"))

summaryBy(Gender+Signed_In+Impressions+Clicks~age_group, data=data1)

data1<-cbind(CTR=as.numeric(data1$Clicks/data1$Impressions), data1)
summaryBy(Impressions+Clicks+ as.numeric(CTR)~age_group, data=(subset(data1, Signed_In>0)),FUN=siterange)
summaryBy(Gender+Signed_In+Impressions+Clicks+CTR_group~age_group, data=(subset(data1, Signed_In>0)),FUN=siterange)

#create categories
data1$HeOrShe1[data1$Gender==0]<-"Female"
data1$HeOrShe1[data1$Gender>0]<-"Male"
data1$HeOrShe1[data1$Signed_In==0]<-"UnKnown"
data1$HeOrShe1<-factor(data1$HeOrShe1)
head(data1)

ggplot(subset(data1, Impressions > 0), aes(x=age_group, fill=HeOrShe1))+geom_histogram(binwidth=1)
ggplot(data1, aes(x=age_group, fill=HeOrShe1))+geom_histogram(binwidth=1)
ggplot(subset(data1, Clicks > 0), aes(x=age_group, fill=HeOrShe1))+geom_histogram(binwidth=1)
ggplot(subset(data1, Clicks>0), aes(x=HeOrShe1, fill=age_group))+geom_histogram(binwidth=1)


ggplot(data1, aes(x=Clicks/Impressions, fill=age_group))+geom_histogram(binwidth=1)

qplot(age_group,HeOrShe1)

ggplot(data1, aes(x=age_group, y=Clicks, fill=age_group))+geom_boxplot()
ggplot(data1, aes(x=Signed_In, fill=CTR_group), label = "Signed_In", "Not Signed_In")+geom_histogram(binwidth = 1)


#Define a new variable to segment or categorize users based on
#their click behavior
data1$CTR_group<-cut(data1$Clicks/data1$Impressions, c(-Inf,0,0.25,0.5,0.75,1,Inf))
ggplot(subset(data1, Clicks>0), aes(fill=HeOrShe1, x=CTR_group))+geom_histogram(binwidth=1)

#create categories based on impression
data1$scode[data1$Impressions==0]<-"NoImps"
data1$scode[data1$Impressions>0]<-"Imps"
data1$scode[data1$Clicks>0]<-"Clicks"

#Convert the column to a factor
data1$scode<-factor(data1$scode)
head(data1)

#look at levels
clen<-function(x){length(x)}
etable<-summaryBy(Impressions~scode+Gender+age_group, data=data1, FUN=clen)
etable

#Plot Click Through Rate Across Dates
mean_CTR<-aggregate(CTR~Date, data=data_final, FUN=mean)
plot(mean_CTR$CTR, type='o', col="red", ann=FALSE)
title(main="Click Through Rate Across Dates", col.main="red", font.main=4)
title(xlab="Dates in Month May", col.lab=rgb(0,0.5,0))
title(ylab="Average Click Through Rates", col.lab=rgb(0,0.5,0))

#Plot Total Impressions Across Dates
sum_Imps<-aggregate(Impressions~Date, data=data_final, FUN =sum)
plot(sum_Imps$Impressions, type='o', col="blue", ann=FALSE)
title(main="Total # of Impressions Across Dates", col.main="red", font.main=4)
title(xlab="Dates in Month May", col.lab=rgb(0,0.5,0))
title(ylab="Impressions", col.lab=rgb(0,0.5,0))

#Plot Total Clicks Across Dates
sum_Clicks<-aggregate(Clicks~Date, data=data_final, FUN=sum)
plot(sum_Clicks$Clicks, type='o', col="blue", ann=FALSE)
title(main="Total # of Clicks Across Dates", col.main="red", font.main=4)
title(xlab="Dates in Month May", col.lab=rgb(0,0.5,0))
title(ylab="Clicks", col.lab=rgb(0,0.5,0))

#The above plots shows that the Clicks and Impressions almost follow the same 
# pattern but there is an improvement in the CTR due to the improvement in the
# number of clicks and constant nature of impressions

ggplot(data_final, aes(x=Date, fill=age_group))+geom_histogram(binwidth=1)
ggplot(data_final, aes(x=Date, fill=he_or_she))+geom_histogram(binwidth=1)
