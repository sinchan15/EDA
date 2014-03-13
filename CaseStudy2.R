require(gdata)
installed.packages("gdata")
library("gdata")
bk<-read.xls("rollingsales_manhattan.xls",pattern="BOROUGH")
getwd()
head(bk)
summary(bk)
??gsub
x <- c("23455","23456" , "abc")  
bk$SALE.PRICE.N<- as.numeric(gsub("[^[:digit:]]","",bk$SALE.PRICE))
y<-gsub("[^[:digit:]]", "", x)
y

x <- c("23455","23456" , "abc")

y<-as.numeric(gsub("[^[:digit:]]", "", x))
y
count(is.na(bk$SALE.PRICE.N))
names(bk)<-tolower(name(bk))
install.packages("thePackage")
library("thePackage")
??count
?length

count(is.na(bk$SALE.PRICE.N))
names(bk)<-tolower(names(bk))

bk$gross.sqft<-as.numeric(gsub("[^[:digit:]]","",bk$gross.square.feet))
bk$land.sqft<-as.numeric(gsub("[^[:digit:]]","",bk$land.square.feet))
bk$sale.date<-as.Date(bk$sale.date)
bk$year.built<-as.numeric(as.character(bk$year.built))


attach(bk)

hist(sale.price.n)
hist(sale.price.n[sale.price.n>0])
hist(gross.sqft[sale.price.n==0], col="red")
detach(bk)
bk.sale<-bk[bk$sale.price.n!=0,]

plot (bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log(bk.sale$gross.sqft),log(bk.sale$sale.price.n), col = 'red')
bkhomes<-bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
plot(log(bk.$gross.sqft),log(bk.homes$sale.price.n))

bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
plot (log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))

bk.homes[which(bk.homes$sale.price.n<100000),] [order
                                                (bk.homes[which(bk.homes$sale.price.n<100000),]
                                                 $sale.price.n),]


bk.homes$outliners<-(log(bk.homes$sale.price.n)<=5 ) + 0
bk.homes<-bk.homes[which(bk.homes$outliners==0),]
plot(log(bk.homes$gross.sqft),log(bk.homes$sale.price.n))
