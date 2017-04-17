#This is master project
#first read the data

Bwssb=read.csv("bwssbdata.csv",header = TRUE)
head(Bwssb)
summary(Bwssb)

#now we have to found the outlier in the consumtion column for each customer
head(Bwssb)
class(Bwssb$region)
#subset the data region wise,first find the number of region
unique(Bwssb$regioncode)
class(Bwssb$region)
unique(Bwssb$region)
Bwssb$region=as.factor(Bwssb$region)
#now we have to subset the data by region 
Bwssb_11=subset(Bwssb,region=="R1")
head(Bwssb_11)
Bwssb_1=subset(Bwssb,regioncode=="1")
Bwssb_2=subset(Bwssb,regioncode=="2") 
Bwssb_3=subset(Bwssb,regioncode=="3") 
Bwssb_4=subset(Bwssb,regioncode=="4") 
Bwssb_5=subset(Bwssb,regioncode=="5") 
Bwssb_6=subset(Bwssb,regioncode=="6")
Bwssb_7=subset(Bwssb,regioncode=="7") 
Bwssb_8=subset(Bwssb,regioncode=="8") 
Bwssb_9=subset(Bwssb,regioncode=="9") 
Bwssb_10=subset(Bwssb,regioncode=="10") 
Bwssb_11=subset(Bwssb,regioncode=="11")
Bwssb_12=subset(Bwssb,regioncode=="12") 

head(Bwssb_2)
head(Bwssb_1)
summary(Bwssb_1)
tsoutliers <- function(x,plot=TRUE)
{
  x <- as.ts(x)
  if(frequency(x)>1)
    resid <- stl(x,s.window="periodic",robust=TRUE)$time.series[,3]
  else
  {
    tt <- 1:length(x)
    resid <- residuals(loess(x ~ tt))
  }
  resid.q <- quantile(resid,prob=c(0.25,0.75))
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5*iqr*c(-1,1)
  score <- abs(pmin((resid-limits[1])/iqr,0) + pmax((resid - limits[2])/iqr,0))
  if(plot)
  {
    plot(x)
    x2 <- ts(rep(NA,length(x)))
    x2[score>0] <- x[score>0]
    tsp(x2) <- tsp(x)
    points(x2,pch=19,col="red")
    return(invisible(score))
  }
  else
    return(score)
}
tsoutliers(Bwssb_1$consumption,plot=TRUE)
library(tm)
Bwssb_1$consumption=
Bwssb_1$consumption=ts(Bwssb_1$consumption,start=c(2006,1),end=c(2009,12),frequency=12)
class(Bwssb_1$consumption)
Bwssb_1$consumption=as.ts(Bwssb_1$consumption)
class(Bwssb_1$consumption)
consumption_1=Bwssb_1$consumption
plot(consumption_1[1:48])
fix(Bwssb_1)
#Now i have to find the cluster among the customer
plot(Bwssb_1$consumption)



library(tsoutliers)
library(forecast)
water_1=auto.arima(Bwssb_1$consumption[1:48])
water=(Bwssb_1$consumption[1:48])
class(water)
water=as.ts(water)
water_model=tso(water,types = c("AO","LS","IO"))
plot(water_model)





head(Bwssb_1)
#now we have segregated region wise 
#we have to do the segreagation streetwise
unique(Bwssb_1$tax)
#subset on the tax side as it is proxy of street
Bwssb_1_street_1=subset(Bwssb_1,Bwssb_1$tax==1500)
Bwssb_1_street_2=subset(Bwssb_1,Bwssb_1$tax==2750)

unique(Bwssb_2$tax)
unique(Bwssb_3$tax)
unique(Bwssb_4$tax)
unique(Bwssb_5$tax)
unique(Bwssb_6$tax)
unique(Bwssb_7$tax)
unique(Bwssb_8$tax)
unique(Bwssb_9$tax)
unique(Bwssb_10$tax)
unique(Bwssb_11$tax)
unique(Bwssb_12$tax)
unique(Bwssb$tax)
head(Bwssb)


head(Bwssb_7)
Bwssb_7_street_1=subset(Bwssb_7,Bwssb_7$tax==5500)
#now i have to make this data as single row in term consumption
consumption=
