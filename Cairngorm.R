################################################################################
################################################################################
###############################Data validation: Cairngorm#######################
################################################################################
################################################################################
install.packages("rgdal")

library(rgdal)
library(ggmap)
library(rworldmap)

setwd("H:\\National Scale Data\\CairngormsValidation")


cg_bound<-readOGR(dsn= "LTSER", layer="New_Cairngorm_NP_Boundary") 
#load shapefile


summary(cg_bound)  #look at file

str(cg_bound)


plot(cg_bound)    #plot polygon

boundary<-spTransform(cg_bound,CRS("+init=epsg:4326"))
#transform coordinates system

plot(boundary)

#plot the polygon on a static map using ggmap

bb<-bbox(boundary)                    #get the bbox as spatial ref for map
b<-(bb-rowMeans(bb))*3+rowMeans(bb)   #multiply to get a larger scale map

cg.b1 <- ggmap(get_map(location = b)) #get the map from GoogleMaps

cg.b1 + geom_polygon(data = boundary, aes(x = long, y = lat, group = group), alpha = 0.3,fill="darkolivegreen2",colour="darkgreen")
#plot the map




################################################################################
################################################################################

library(RCurl)
library(XML)
library(httr)


api_key<-"8b3a8551185c0113ed72e60c530d0b51"

secret<- "767533312d7907d6"

myapp<-oauth_app("flickr",key= api_key,secret= secret)
           #creates the app passing the key and secret



ep<-oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token"   #get authentication credentials from the API
                    ,authorize="https://www.flickr.com/services/oauth/authorize",
                    access="https://www.flickr.com/services/oauth/access_token")


sig<-oauth1.0_token(ep,myapp,cache=FALSE)                                                      #creates variable with authentication credentials

fl_sig <- sign_oauth1.0(myapp,sig)



baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",
                  api_key,sep="")


pics<-NULL
year<-seq(2009,2014,1)
bbox<-paste(bb[1,1],bb[2,1],bb[1,2],bb[2,2],sep=",")
hasgeo<-"1"
extras<-"date_taken,geo,tags"
perpage<-"250"
format<-"rest"


 for (y in 1:length(year)){
      for (m in 1:12){ daymin<-"01"
            
           ifelse ((m==4|m==6|m==9|m==11), daymax<-"30",daymax<-"31")
           if (m==2){
           ifelse (year[y]==2012, daymax<-29,daymax<-28)
                    }

           ifelse (m==10|m==11|m==12,
                    mindate<-as.character(paste(year[y],m,daymin,sep="-")),
                      mindate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymin,sep="-")))
                      
           ifelse (m==10|m==11|m==12,
                    maxdate<-as.character(paste(year[y],m,daymax,sep="-")),
                      maxdate<-as.character(paste(year[y],paste("0",m,sep="")
                      ,daymax,sep="-")))
                                

           
           getPhotos <- paste(baseURL
                         ,"&min_taken_date=",mindate,
                         "&max_taken_date=",maxdate,"&bbox=",bbox,
                         "&has_geo=",hasgeo,"&extras=",extras,
                         "&per_page=",perpage,"&format=",format,sep="")



           getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                  (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr") ))


           #parse the total number of pages
           pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
           pages_data[] <- lapply(pages_data, as.character)
           pages_data[] <- lapply(pages_data, as.integer)
           colnames(pages_data)<- "value"
           total_pages <- pages_data["pages","value"]
           
           pics_tmp<-NULL


           # loop thru pages of photos and save the list in a DF
           for(i in 1:total_pages){
               getPhotos <- paste(baseURL
                           ,"&min_taken_date=",mindate,
                         "&max_taken_date=",maxdate,"&bbox=",bbox,
                         "&has_geo=",hasgeo,"&extras=",extras,
                         "&per_page=",perpage,"&format=",format,"&page="
                           ,i,sep="")

                getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                  (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                  ,useInternalNodes = TRUE ))

                  id<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"id")
                  owner<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"owner")
                  datetaken<-xpathSApply(getPhotos_data,"//photo",xmlGetAttr,
                                         "datetaken")
                  tags<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,"tags")
                  latitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,
                                         "latitude")
                  longitude<- xpathSApply(getPhotos_data,"//photo",xmlGetAttr,
                                         "longitude")
                  if(length(id)==0) {break}
                  else{tmp_df<-data.frame(cbind(id,owner,datetaken,tags,latitude
                                           ,longitude),
                                           stringsAsFactors=FALSE)

                  tmp_df$page <- i
                  pics_tmp<-rbind(pics_tmp,tmp_df)
                  }}           
                  
                  pics<-rbind(pics,pics_tmp)
                   
}}




Sys.time()
#[1] "2016-03-04 11:00:39 GMT"



################################
####Data formatting
################################

#####Date#####

library(lubridate)

#pics$date<-ymd_hms(pics$datetaken)     #transform character strings into
                                               #time format

pics$month<-month(ymd_hms(pics$datetaken),label=T)   
                                                          #create variable month

pics$year<-year(ymd_hms(pics$datetaken))             
                                                          #create variable year




####Coordinates####

options(digits=9)

pics$latitude<-as.numeric(pics$latitude)

pics$longitude<-as.numeric(pics$longitude)

####################################
##########Save dataset
####################################

library(stringr)


pics$datetaken<- str_replace_all(pics$datetaken, " ","_")
pics$tags<- str_replace_all(pics$tags, "[[:punct:]]","_")
pics$tags<- str_replace_all(pics$tags, " ","_")




setwd("H:\\National Scale Data\\CairngormsValidation")

write.table(pics,"CairngormsPics.txt", row.names=F,sep="\t", quote=F)




setwd("H:\\National Scale Data\\CairngormsValidation")

Cairngorms<-read.table("CairngormsPics.txt",header=T)


Cairngorms$id<-as.character(Cairngorms$id)
Cairngorms$owner<-as.character(Cairngorms$owner)
Cairngorms$datetaken<-as.character(Cairngorms$datetaken)
Cairngorms$tags<-as.character(Cairngorms$tags)
Cairngorms$year<-as.factor(Cairngorms$year)

print(levels(Cairngorms$month))
Cairngorms$month<- factor(Cairngorms$month,levels(Cairngorms$month)
                  [c(5,4,8,1,9,7,6,2,12,11,10,3)])

##########################################
#############Mapping
##########################################


ggplot() +  geom_polygon(data=boundary, aes(x=long, y=lat, group=group))+  
  geom_point(data=Cairngorms, aes(x=longitude, y=latitude), color="red")


bb<-bbox(boundary)                    #get the bbox as spatial ref for map
b<-(bb-rowMeans(bb))*4+rowMeans(bb)   #multiply to get a larger scale map

cg.b1 <- ggmap(get_map(location = b,maptype="toner")) #get the map from GoogleMaps

cg.b1 + geom_polygon(data = boundary, aes(x = long, y = lat, group = group), alpha = 0.3,fill="darkolivegreen",colour="darkgreen")+geom_point(data=Cairngorms, aes(x=longitude, y=latitude), color="red",size=1.5)
#plot the map



#########################################
################Clipping
#########################################
library(maptools)

geopics<-Cairngorms

coordinates(geopics)<-c("longitude","latitude")


crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  # geographical, datum WGS84
proj4string(geopics) <- crs.geo


#Create a shapefile
writePointsShape(geopics, "Cairngorms")

#read the shapefile
geopics <- readShapePoints("Cairngorms.shp")
proj4string(geopics) <- crs.geo

#transform polygon
boundary<-spTransform(cg_bound,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
#and give same projection as points
proj4string(boundary) <- crs.geo

#subset dataframe
Cairngorms_sub<-geopics[boundary,]

Cairngorms_sub<-Cairngorms_sub@data

bb<-bbox(boundary)                    #get the bbox as spatial ref for map
b<-(bb-rowMeans(bb))*4+rowMeans(bb)   #multiply to get a larger scale map

cg.b1 <- ggmap(get_map(location = b,maptype="toner")) #get the map from GoogleMaps

cg.b1 + geom_polygon(data = boundary, aes(x = long, y = lat, group = group), alpha = 0.3,fill="darkolivegreen",colour="darkgreen")+geom_point(data=Cairngorms_sub, aes(x=longitude, y=latitude), color="red",size=1.5)
#plot the map


####eliminating duplicates 

library(lubridate)

Cairngorms_sub$month<-factor(Cairngorms_sub$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#Cairngorms_sub$date<-ymd_hms(Cairngorms_sub$datetaken)  #transform character strings into
                                               #time format
Cairngorms_sub$id<-as.character(Cairngorms_sub$id)
Cairngorms_sub$owner<-as.character(Cairngorms_sub$owner)
Cairngorms_sub$datetaken<-as.character(Cairngorms_sub$datetaken)
Cairngorms_sub$tags<-as.character(Cairngorms_sub$tags)
Cairngorms_sub$year<-as.factor(Cairngorms_sub$year)

                                 
Cairngorms_sub$dateonly <- sapply(strsplit(Cairngorms_sub$datetaken, "_"), "[[", 1)
Cairngorms_sub$dateonly<-as.character(Cairngorms_sub$dateonly)

d<-duplicated(Cairngorms_sub[,c(2,10)])

Cairngorms_UD<-Cairngorms_sub[-which(d=="TRUE"),]

write.table(Cairngorms_UD,"Cairngorms_UD.txt", row.names=F,sep="\t", quote=F)

###################################
######Temporal trends
###################################

C<-read.table("Cairngorms_UD.txt",header=T)

library(lubridate)

C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#C$date<-ymd_hms(C$datetaken)  #transform character strings into
                                               #time format
C$id<-as.character(C$id)
C$owner<-as.character(C$owner)
C$datetaken<-as.character(C$datetaken)
C$tags<-as.character(C$tags)
C$year<-as.factor(C$year)
C$dateonly<-as.character(C$dateonly)
                                                 

plot(C$month)

plot(C$year)




library(lattice)

histogram(~month|year,data=C)


################################################
############Timeseries
################################################
setwd("H:\\National Scale Data\\CairngormsValidation")
library(lubridate)
library(zoo)

C<-read.table("Cairngorms_UD.txt",header=T)

C$id<-as.character(C$id)
C$owner<-as.character(C$owner)
C$datetaken<-as.character(C$datetaken)
C$tags<-as.character(C$tags)
C$year<-as.factor(C$year)
C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


C$date<-ymd(C$dateonly)  #transform character strings into
                                               #time format


flickr_TS<-tapply(C$owner,C$date,length)       #counts observations per date

flickr_TSdf<-as.data.frame.table(flickr_TS)           #converts to dataframe

colnames(flickr_TSdf)<-c("Date","Visits")

flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)               #converts to date format

flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year


par(mfrow=c(4,1))    
plot(flickr_TS_daily)
plot(flickr_TS_monthly)
plot(flickr_TS_qtr)
plot(flickr_TS_year)


realC<-read.table("CairngormsVD_TimeSeries.txt",header=T)

realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y")

#realC<-realC[,-2]

realC_TSdf<-as.data.frame(realC)           #converts to dataframe

colnames(realC)<-c("Date","Visits")

realC$Date<-as.Date(as.yearmon(realC$Date))               #converts to date format

realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))   #make daily time series

realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    #aggregate by year

dev.new()
par(mfrow=c(4,1))
plot(flickr_TS_monthly)    
plot(realC_TS_monthly)
plot(flickr_TS_year)
plot(realC_TS_year)

tiff(filename="H://National Scale Data//CairngormsValidation//MonthlyTS.tiff",width=5000,height=3000,res=300)
par(mfrow=c(2,1),oma=c(2,2,2,2))

par(mar=c(0,4,3,0))
plot(flickr_TS_monthly,col="blue", xaxt="n",                                                                                               
     main="Monthly visits to the Cairngorms National Park",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
par(mar=c(3,4,0,0))
plot(realC_TS_monthly,col="red", yaxt="n",                                                                                               
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
axis(side=2, at=c(100000,400000),labels=c("100000","400000"),cex.axis=2.5)

mtext("Empirical                    Flickr",side=2,cex=2.5,outer=T)
mtext("Time",side=1,cex=2.5,outer=T )
dev.off()


tiff(filename="H://National Scale Data//CairngormsValidation//AnnualTS.tiff",width=5000,height=3000,res=300)
par(mfrow=c(2,1),oma=c(2,2,2,2))

par(mar=c(0,4,3,0))
plot(flickr_TS_year,col="blue", xaxt="n",                                                                                               
     main="Annual visits to the Cairngorms National Park",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
par(mar=c(3,4,0,0))
plot(realC_TS_year,col="red", yaxt="n",                                                                                               
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
axis(side=2, at=c(3200000,3500000),labels=c("3.2","3.5"),cex.axis=2.5)

mtext("Empirical(M)                  Flickr    ",side=2,cex=2.5,outer=T)
mtext("Time",side=1,cex=2.5,outer=T )
dev.off()



flickr_TS_monthly<-as.data.frame(flickr_TS_monthly)
realC_TS_monthly<-as.data.frame(realC_TS_monthly)

stflickr<-scale(flickr_TS_monthly,scale=T)      #standardise TS
strealC<-scale(realC_TS_monthly,scale=T)

plot(strealC~stflickr)                          #plot

Date<-seq.Date(from=as.Date("2009-01-01"),to=as.Date("2014-12-31"),by="month")

TS<-as.data.frame(cbind(Date,stflickr,strealC))
colnames(TS)<-c("Date","stflickr","strealC")
TS$Date<-as.Date(TS$Date)
TS$Year<-year(ymd(TS$Date)) 
TS$Month<-month(ymd(TS$Date))

for (i in 1:length(TS$Month)){
if (TS$Month[i]==12|TS$Month[i]==1|TS$Month[i]==2) {TS$Season[i]<-"Winter"}
else { if (TS$Month[i]==3|TS$Month[i]==4|TS$Month[i]==5) {TS$Season[i]<-"Spring"}
     else{ if (TS$Month[i]==6|TS$Month[i]==7|TS$Month[i]==8) {TS$Season[i]<-"Summer"} 
           else {TS$Season[i]<-"Autumn"}
           }}}
           
           
for (i in 1:length(TS$Month)){
if (TS$Month[i]==12|TS$Month[i]==1|TS$Month[i]==2) {TS$Season1[i]<-1}
else { if (TS$Month[i]==3|TS$Month[i]==4|TS$Month[i]==5) {TS$Season1[i]<-2}
     else{ if (TS$Month[i]==6|TS$Month[i]==7|TS$Month[i]==8) {TS$Season1[i]<-3} 
           else {TS$Season1[i]<-4}
           }}}
           
TS$Season<-as.factor(TS$Season)

TS$Month2<-seq(1:length(TS$Month))

library(nlme)

model.1<-gls(strealC~stflickr+Year+Season,data=TS)

summary(model.1)

acf(resid(model.1,type="normalized"))

model.2<-gls(strealC~stflickr+Year+Season, data=TS, correlation= corAR1(form=~1|Season))

summary(model.2)


acf(resid(model.2,type="normalized"))

model.3<-gls(strealC~stflickr+Year+Season, data=TS, correlation= corAR1(form=~1|Year))

summary(model.3)


acf(resid(model.3,type="normalized"))

model.4<-gls( strealC~stflickr+Year+Season, data=TS, correlation= corARMA(c(0.2,0.2,0.2,0.3,0.3,0.3),p=3,q=3))

acf(resid(model.4,type="normalized"))               #still autocorrelation ant lag 12

pacf(resid(model.4,type="normalized"))

AIC(model.1,model.2,model.3,model.4)
#        df      AIC
#model.1  7 87.66895
#model.2  8 88.80540
#model.3  8 86.25574
#model.4 13 79.94394

ar.yw(resid(model.4))



par(mfrow=c(1,3))
boxplot(resid(model.4,type="normalized")~TS$Year)            #but spread of residuals is much better
abline(h=0,col="red")


boxplot(resid(model.4,type="normalized")~TS$Season)
abline(h=0,col="red")

plot(resid(model.4,type="normalized"))
abline(h=0,col="red")

plot(model.4)

new.data.2009<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2009]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2009]),length=100),Year=2009,Season="Spring")                        #
new.data.2010<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2010]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2010]),length=100),Year=2010,Season="Spring")                   
new.data.2011<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2011]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2011]),length=100),Year=2011,Season="Spring")
new.data.2012<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2012]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2012]),length=100),Year=2012,Season="Spring")   
new.data.2013<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2013]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2013]),length=100),Year=2013,Season="Spring")   
new.data.2014<-data.frame(stflickr=seq(min(TS$stflickr[TS$Season=="Spring"&TS$Year==2014]),max(TS$stflickr[TS$Season=="Spring"&TS$Year==2014]),length=100),Year=2014,Season="Spring")   

library(AICcmodavg)

P1<-predictSE.gls(model.4,new.data.2009,se.fit=TRUE)
P2<-predictSE.gls(model.4,new.data.2010,se.fit=T)
P3<-predictSE.gls(model.4,new.data.2011,se.fit=T)
P4<-predictSE.gls(model.4,new.data.2012,se.fit=T)
P5<-predictSE.gls(model.4,new.data.2013,se.fit=T)
P6<-predictSE.gls(model.4,new.data.2014,se.fit=T)


#plot(TS$strealC~TS$stflickr)

par(mfrow=c(2,3))

plot(TS[which(TS$Year==2009 &TS$Season=="Spring" ),"strealC"]~TS[which(TS$Year==2009&TS$Season=="Spring" ),"stflickr"],main="Spring 2009")                               #       
lines(new.data.2009$stflickr,P1$fit,col="black",lwd=2)
lines(new.data.2009$stflickr,P1$fit-1.96*P1$se.fit,col="black",lty=2,lwd=2)
lines(new.data.2009$stflickr,P1$fit+1.96*P1$se.fit,col="black",lty=2,lwd=2)

plot(TS[which(TS$Year==2010&TS$Season=="Spring" ),"stflickr"]~TS[which(TS$Year==2010&TS$Season=="Spring" ),"strealC"],main="Spring 2010")
lines(new.data.2010$stflickr,P2$fit,col="red",lwd=2)
lines(new.data.2010$stflickr,P2$fit-1.96*P2$se.fit,col="red",lty=2,lwd=2)
lines(new.data.2010$stflickr,P2$fit+1.96*P2$se.fit,col="red",lty=2,lwd=2)
 
plot(TS[which(TS$Year==2011&TS$Season=="Spring" ),"stflickr"]~TS[which(TS$Year==2011&TS$Season=="Spring" ),"strealC"],main="Spring 2011")
lines(new.data.2011$stflickr,P3$fit,col="blue",lwd=2)
lines(new.data.2011$stflickr,P3$fit-1.96*P3$se.fit,col="blue",lty=2,lwd=2)
lines(new.data.2011$stflickr,P3$fit+1.96*P3$se.fit,col="blue",lty=2,lwd=2)

plot(TS[which(TS$Year==2012&TS$Season=="Spring" ),"stflickr"]~TS[which(TS$Year==2012&TS$Season=="Spring" ),"strealC"],main="Spring 2012")
lines(new.data.2012$stflickr,P4$fit,col="green",lwd=2)
lines(new.data.2012$stflickr,P4$fit-1.96*P4$se.fit,col="green",lty=2,lwd=2)
lines(new.data.2012$stflickr,P4$fit+1.96*P4$se.fit,col="green",lty=2,lwd=2)

plot(TS[which(TS$Year==2013&TS$Season=="Spring" ),"stflickr"]~TS[which(TS$Year==2013&TS$Season=="Spring" ),"strealC"],main="Spring 2013")
lines(new.data.2013$stflickr,P5$fit,col="yellow",lwd=2)
lines(new.data.2013$stflickr,P5$fit-1.96*P5$se.fit,col="yellow",lty=2,lwd=2)
lines(new.data.2013$stflickr,P5$fit+1.96*P5$se.fit,col="yellow",lty=2,lwd=2)

plot(TS[which(TS$Year==2014&TS$Season=="Spring" ),"stflickr"]~TS[which(TS$Year==2014&TS$Season=="Spring" ),"strealC"],main="Spring 2014")
lines(new.data.2014$stflickr,P6$fit,col="grey",lwd=2)
lines(new.data.2014$stflickr,P6$fit-1.96*P6$se.fit,col="grey",lty=2,lwd=2)
lines(new.data.2014$stflickr,P6$fit+1.96*P6$se.fit,col="grey",lty=2,lwd=2)


#library(visreg)
#
#par(mfrow=c(1,3))
#visreg(model.4)
#
#
#library(effects)
#
#plot(allEffects(model.4))
# 
# TS$Season2<-c(1,1,rep(2:24,each=3),25)
#TS$Date2<-seq(1:length(TS$Date))
#
#model.5<-gls(strealC~stflickr+stflickr^2+cos(2*pi*Date2/12)+sin(2*pi*Date2/12), data=TS,correlation= corARMA(p=3,q=3))
#
#acf(resid(model.5,type="normalized"))               #still autocorrelation ant lag 12
#
#pacf(resid(model.5,type="normalized"))
#
#AIC(model.1,model.2,model.3,model.4,model.5)
##        df      AIC
##model.1  7 87.66895
##model.2  8 88.80540
##model.3  8 86.25574
##model.4 13 79.94394
#
#
#par(mfrow=c(1,3))
#boxplot(resid(model.5,type="normalized")~TS$Year)            #but spread of residuals is much better
#abline(h=0,col="red")
#
#
#boxplot(resid(model.5,type="normalized")~TS$Season)
#abline(h=0,col="red")
#
#plot(resid(model.5,type="normalized"))
#abline(h=0,col="red")
#
#plot(model.5)

#
#
##dynamic regression
#
#setwd("H:\\National Scale Data\\CairngormsValidation")
#library(lubridate)
#library(zoo)
#
#C<-read.table("Cairngorms_UD.txt",header=T)
#
#C$id<-as.character(C$id)
#C$owner<-as.character(C$owner)
#C$datetaken<-as.character(C$datetaken)
#C$tags<-as.character(C$tags)
#C$year<-as.factor(C$year)
#C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
#                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
#
#
#C$date<-ymd(C$dateonly)  transform character strings into
#                                               time format
#
#
#flickr_TS<-tapply(C$owner,C$date,length)       counts observations per date
#
#flickr_TSdf<-as.data.frame.table(flickr_TS)           converts to dataframe
#
#colnames(flickr_TSdf)<-c("Date","Visits")
#
#flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)               converts to date format
#
#flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))   make daily time series
#
#
#flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) aggregate daily time series by month
#
#flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     aggregate by quarter
#
#flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    aggregate by year
#
#
#realC<-read.table("CairngormsTimeSeries.txt",header=T)
#
#realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y")
#
#realC<-realC[,-2]
#
#realC_TSdf<-as.data.frame(realC)           converts to dataframe
#
#colnames(realC)<-c("Date","Visits")
#
#realC$Date<-as.Date(as.yearmon(realC$Date))               converts to date format
#
#realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))   make daily time series
#
#realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    aggregate by year
#
#
#
#empirical<-ts(realC_TS_monthly)
#flickr<-ts(flickr_TS_monthly)
#Date<-seq.Date(from=as.Date("2009-01-01"),to=as.Date("2014-12-31"),by="month")
#lag.realC = lag(empirical, -1)                                #creates lagged Ys
#datets<-ts(Date)
#
#timser<-as.data.frame(cbind(datets,empirical,flickr, lag.realC) )
#
#timser$datets<-as.Date(timser$datets)
#timser$Year<-year(ymd(timser$datets)) 
#timser$Month<-month(ymd(timser$datets))
#
#
#for (i in 1:length(timser$Month)){ if (timser$Month[i]!=NA){
#if (timser$Month[i]==12|timser$Month[i]==1|timser$Month[i]==2) {timser$Season[i]<-"Winter"}
#else { if (timser$Month[i]==3|timser$Month[i]==4|timser$Month[i]==5) {timser$Season[i]<-"Spring"}
#     else{ if (timser$Month[i]==6|timser$Month[i]==7|timser$Month[i]==8) {timser$Season[i]<-"Summer"} 
#           else {timser$Season[i]<-"Autumn"}
#           }}} else {timser$Season[i]<-"NA"}
#           }
#           
#timser$Season[73]<-NA
#           
#timser$Season<-as.factor(timser$Season)
#
#
#
#
#dreg<-lm(empirical~ flickr+ lag.realC+Season,data=timser)
#


#Decomposition and cross-correlation

setwd("H:\\National Scale Data\\CairngormsValidation")
library(lubridate)
library(zoo)
library(TSA)
library(forecast)

C<-read.table("Cairngorms_UD.txt",header=T)

C$id<-as.character(C$id)
C$owner<-as.character(C$owner)
C$datetaken<-as.character(C$datetaken)
C$tags<-as.character(C$tags)
C$year<-as.factor(C$year)
C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


C$date<-ymd(C$dateonly)  #transform character strings into
                                               #time format


flickr_TS<-tapply(C$owner,C$date,length)       #counts observations per date

flickr_TSdf<-as.data.frame.table(flickr_TS)           #converts to dataframe

colnames(flickr_TSdf)<-c("Date","Visits")

flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)               #converts to date format

flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year


realC<-read.table("CairngormsVD_TimeSeries.txt",header=T)

realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y")

#realC<-realC[,-2]

realC_TSdf<-as.data.frame(realC)           #converts to dataframe

colnames(realC)<-c("Date","Visits")

realC$Date<-as.Date(as.yearmon(realC$Date))               #converts to date format

realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))   #make daily time series

realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    #aggregate by year

flickr_TS_monthly<-as.data.frame(flickr_TS_monthly)
realC_TS_monthly<-as.data.frame(realC_TS_monthly)

stflickr<-as.data.frame(scale(flickr_TS_monthly,scale=T))      #standardise TS
strealC<-as.data.frame(scale(realC_TS_monthly,scale=T))


empirical<-ts(strealC$realC_TS_monthly,start=c(2009,1),frequency=12)
flickr<-ts(stflickr$flickr_TS_monthly,start=c(2009,1),frequency=12)

#decomposition

dec_empirical<-stl(empirical,"periodic")
dec_flickr<-stl(flickr,"periodic")


plot(dec_empirical,main="Empirical")
dev.new()
plot(dec_flickr,main="Flickr")

#cross-correlation
par(mfrow=c(2,2))
acf(cbind(flickr,empirical),type="partial")


dev.new()
ccf(empirical,flickr, type = "correlation",plot = TRUE)


crosscor<-ccf(empirical,flickr, type = "correlation",plot = FALSE)
max_crosscor<-max(crosscor$acf)
max_crosscor
crosscor$lag[which(crosscor$acf > max_crosscor-0.01 & crosscor$acf < max_crosscor+0.01)]


#flickr_TS_monthly<-as.data.frame(flickr_TS_monthly)
#realC_TS_monthly<-as.data.frame(realC_TS_monthly)
#
#
#empirical<-ts(realC_TS_monthly$realC_TS_monthly,start=c(2009,1),frequency=12)
#flickr<-ts(flickr_TS_monthly$flickr_TS_monthly,start=c(2009,1),frequency=12)


me.dif<-ts.intersect(diff(diff(empirical),12),diff(diff(flickr),12))
prewhiten(as.vector(me.dif[,1]),as.vector(me.dif[,2]),
ylab='CCF',main="Emprirical & Flickr")

emp_model<-arima(empirical,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12),xreg=flickr)                                                           
emp_model
#Call:
#arima(x = empirical, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), 
#    period = 12), xreg = flickr)
#
#Coefficients:
#          ma1     sma1     xreg
#      -1.0000  -0.5585  -0.0180
#s.e.   0.0881   0.2054   0.0235
#
#sigma^2 estimated as 0.01207:  log likelihood = 41.8,  aic = -77.6



par(mfrow=c(3,1))

plot(window(rstandard(emp_model),start=c(2009,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)


acf(as.vector(window(rstandard(emp_model),start=c(2009,1))),lag.max=36)

pacf(as.vector(window(rstandard(emp_model),start=c(2009,1))),lag.max=36)

win.graph(width=3, height=3,pointsize=8)
hist(window(rstandard(emp_model),start=c(2009,1)),
xlab='Standardized Residuals')



win.graph(width=2.5,height=2.5,pointsize=8)
qqnorm(window(rstandard(emp_model),start=c(2009,1)))
qqline(window(rstandard(emp_model),start=c(2009,1)))



Year<-rep(2009:2014,each=12)

emp_model_auto<-auto.arima(empirical,xreg=Year)
summary(emp_model_auto)
#Series: empirical 
#ARIMA(0,0,3) with zero mean     
#
#Coefficients:
#         ma1     ma2     ma3   Year
#      1.2206  1.2076  0.7515  0e+00
#s.e.  0.0798  0.1101  0.0826  2e-04
#
#sigma^2 estimated as 0.2239:  log likelihood=-48.17
#AIC=106.34   AICc=107.24   BIC=117.72
#
#Training set error measures:
#                     ME      RMSE       MAE       MPE    MAPE     MASE
#Training set 0.00547553 0.4598386 0.3657397 -147.7705 203.962 3.826904
#                     ACF1
#Training set -0.002157291




flickr_model_auto<-auto.arima(flickr,xreg=Year)
summary(flickr_model_auto)
#Series: flickr 
#ARIMA(2,0,0)(1,0,0)[12] with non-zero mean 
#
#Coefficients:
#         ar1     ar2    sar1  intercept    Year
#      0.3934  0.2735  0.5983  -573.2535  0.2849
#s.e.  0.1226  0.1222  0.1009   259.6419  0.1291
#
#sigma^2 estimated as 0.2563:  log likelihood=-53.49
#AIC=118.97   AICc=120.27   BIC=132.63
#
#Training set error measures:
#                      ME      RMSE       MAE      MPE     MAPE      MASE
#Training set 0.009126655 0.4883263 0.4013729 39.87444 106.4376 0.6547921
#                     ACF1
#Training set -0.002947834


emp_model_auto2<-auto.arima(empirical)
summary(emp_model_auto2)
#Series: empirical 
#ARIMA(0,0,0)(0,1,1)[12] with drift         
#
#Coefficients:
#         sma1  drift
#      -0.5930  2e-03
#s.e.   0.2104  7e-04
#
#sigma^2 estimated as 0.01225:  log likelihood=45.36
#AIC=-84.72   AICc=-84.29   BIC=-78.44
#
#Training set error measures:
#                       ME       RMSE        MAE       MPE     MAPE     MASE
#Training set -0.001541859 0.09933648 0.06964738 -27.22242 88.19248 0.728753
#                   ACF1
#Training set -0.1801164



flickr_model_auto2<-auto.arima(flickr)
summary(flickr_model_auto2)
#Series: flickr 
#ARIMA(0,1,1)(0,1,1)[12]                    
#
#Coefficients:
#          ma1     sma1
#      -0.6125  -0.5810
#s.e.   0.1107   0.1774
#
#sigma^2 estimated as 0.2417:  log likelihood=-43.5
#AIC=93.01   AICc=93.44   BIC=99.24
#
#Training set error measures:
#                      ME      RMSE      MAE     MPE     MAPE      MASE        ACF1
#Training set -0.01735914 0.4374168 0.320343 42.0673 101.2606 0.5226015 -0.07061296


#diagnostic empirical model 1
par(mfrow=c(3,1))

plot(window(rstandard(emp_model_auto),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)


acf(as.vector(window(rstandard(emp_model_auto),start=c(2010,1))),lag.max=36)

pacf(as.vector(window(rstandard(emp_model_auto),start=c(2010,1))),lag.max=36)

dev.new()
par(mfrow=c(1,2))
hist(window(rstandard(emp_model_auto),start=c(2010,1)),
xlab='Standardized Residuals')

qqnorm(window(rstandard(emp_model_auto),start=c(2010,1)))
qqline(window(rstandard(emp_model_auto),start=c(2010,1)))


##diagnostic flickr model 1
par(mfrow=c(3,1))

plot(window(rstandard(flickr_model_auto),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)

acf(as.vector(window(rstandard(flickr_model_auto),start=c(2010,1))),lag.max=36)
pacf(as.vector(window(rstandard(flickr_model_auto),start=c(2010,1))),lag.max=36)


dev.new()
par(mfrow=c(1,2))

hist(window(rstandard(flickr_model_auto),start=c(2010,1)),
xlab='Standardized Residuals')

qqnorm(window(rstandard(flickr_model_auto),start=c(2010,1)))
qqline(window(rstandard(flickr_model_auto),start=c(2010,1)))

#diagnostic empirical model 2

par(mfrow=c(3,1))

plot(window(rstandard(emp_model_auto2),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)

acf(as.vector(window(rstandard(emp_model_auto2),start=c(2010,1))),lag.max=36)

pacf(as.vector(window(rstandard(emp_model_auto2),start=c(2010,1))),lag.max=36)

dev.new()
par(mfrow=c(1,2))
hist(window(rstandard(emp_model_auto2),start=c(2010,1)),
xlab='Standardized Residuals')

qqnorm(window(rstandard(emp_model_auto2),start=c(2010,1)))
qqline(window(rstandard(emp_model_auto2),start=c(2010,1)))


##diagnostic flickr model 2
par(mfrow=c(3,1))

plot(window(rstandard(flickr_model_auto2),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)


acf(as.vector(window(rstandard(flickr_model_auto2),start=c(2010,1))),lag.max=36)

pacf(as.vector(window(rstandard(flickr_model_auto2),start=c(2010,1))),lag.max=36)

dev.new()
par(mfrow=c(1,2))

hist(window(rstandard(flickr_model_auto2),start=c(2010,1)),
xlab='Standardized Residuals')

qqnorm(window(rstandard(flickr_model_auto2),start=c(2010,1)))
qqline(window(rstandard(flickr_model_auto2),start=c(2010,1)))



#############################
###Spectral analysis
#############################
setwd("H:\\National Scale Data\\CairngormsValidation")
library(lubridate)
library(zoo)
#library(dplR)
library(WaveletComp)

C<-read.table("Cairngorms_UD.txt",header=T)

C$id<-as.character(C$id)
C$owner<-as.character(C$owner)
C$datetaken<-as.character(C$datetaken)
C$tags<-as.character(C$tags)
C$year<-as.factor(C$year)
C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


C$date<-ymd(C$dateonly)  #transform character strings into
                                               #time format


flickr_TS<-tapply(C$owner,C$date,length)       #counts observations per date

flickr_TSdf<-as.data.frame.table(flickr_TS)           #converts to dataframe

colnames(flickr_TSdf)<-c("Date","Visits")

flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)               #converts to date format

flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year


realC<-read.table("CairngormsVD_TimeSeries.txt",header=T)

realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y")

#realC<-realC[,-2]

realC_TSdf<-as.data.frame(realC)           #converts to dataframe

colnames(realC)<-c("Date","Visits")

realC$Date<-as.Date(as.yearmon(realC$Date))               #converts to date format

realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))   #make daily time series

realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    #aggregate by year

stflickr<-as.data.frame(flickr_TS_monthly)
strealC<-as.data.frame(realC_TS_monthly)

#stflickr<-as.data.frame(scale(flickr_TS_monthly,scale=T))      #standardise TS
#strealC<-as.data.frame(scale(realC_TS_monthly,scale=T))


empirical<-strealC$realC_TS_monthly
flickr<-stflickr$flickr_TS_monthly

#create dataframe with two time series
my.data <-data.frame(x = empirical , y = flickr, date= realC$Date)

#produce wavelet power spectrum of empirical time series
E.my.w = analyze.wavelet(my.data, "x",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot

tiff(filename="H://National Scale Data//CairngormsValidation//EmpWVSpectrum.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wt.image.modified(E.my.w, main="Empirical time series",show.date=T,timelab="",legend.params = list(lab = "wavelet power levels",width=1, mar = 10))
dev.off()



#produce wavelet power spectrum of flickr time series
F.my.w = analyze.wavelet(my.data, "y",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot
tiff(filename="H://National Scale Data//CairngormsValidation//FlickrWVSpectrum.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wt.image.modified(F.my.w, main="Flickr time series",show.date=T,timelab="",legend.params = list(lab = "wavelet power levels",width=1, mar = 10))
dev.off()



#average power of empirical and flickr time series
tiff(filename="H://National Scale Data//CairngormsValidation//AVGPower.tiff",width=3000,height=3000,res=300)
par(mfrow=c(1,2),mar=c(6,4,3,1))
wt.avg.modified(E.my.w, main="Empirical",lwd=2,legend.coords = "bottomright",averagelab="")
wt.avg.modified(F.my.w,main="Flickr",lwd=2,legend.coords = "bottomright",averagelab="")
mtext("average wavelet power",side=1,cex=2,outer=T,line=-1.5)
dev.off()

#calculate coherency
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
loess.span = 0,
dt = 1, make.pval = T, n.sim = 100)
#and plot
tiff(filename="H://National Scale Data//CairngormsValidation//CrossCorr.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wc.image.modified(my.wc, which.image = "wc",legend.params = list(lab = "wavelet coherence",width=1, mar = 10),
timelab = "", periodlab = "period (months)",show.date= T)
dev.off()

names(my.wc$series)[2:3]<-c("Empirical","Flickr")      #change names of time series in wc object

#plot phase difference for the significant period in coherency
tiff(filename="H://National Scale Data//CairngormsValidation//PhaseDiff.tiff",width=3000,height=3000,res=300)
par(mar=c(6,6,3,1))
wc.sel.phases.modified(my.wc, sel.lower = 8, sel.upper = 16,only.sig = T, siglvl = 0.05, show.date = T,
which.sig = "wc",legend.coords = "topright", legend.horiz = F,
phaselim = c(-pi,+2*pi), main = "", sub = "",timelab="")
dev.off()

#dev.new()
#wc.sel.phases(my.wc, sel.lower = 4, sel.upper = 8,only.sig = T, siglvl = 0.05,
#which.sig = "wc",
#legend.coords = "topright", legend.horiz = F,
#phaselim = c(-pi,+pi), main = "", sub = "")
#
#dev.new()
#wc.phasediff.image(my.wc, which.contour = "wc", siglvl = 0.1,
#legend.params = list(lab = "phase difference levels"))
#
#dev.new()
#wc.image(my.wc, which.image = "wp",
#legend.params = list(lab = "cross-wavelet power"),
#timelab = "time (months)", periodlab = "period (months)",show.date= T)
#     
#
#wc.avg(my.wc, sigpch = 20)

#plot histogram of phase differences
tiff(filename="H://National Scale Data//CairngormsValidation//PhaseDiffHist.tiff",width=3000,height=3000,res=300)
par(mar=c(6,6,3,1))
hist(my.wc$Angle,xaxt="n",yaxt="n",xlim=c(-2*pi,2*pi),col="black",ylab="",main="",xlab="")
axis(side=1,at=c(-2*pi,-pi,0,pi,2*pi), labels=c("-2pi","-pi","0","pi","2pi"),cex.axis=2)
mtext("Phase differences", side = 1,cex=2,line=4)
axis(side=2,at=seq(from=0,to=1400,200),labels=c("0","200","400","600","800","1000","1200","1400"),cex.axis=2)
mtext("Number of values", side = 2,cex=2,line=4)
dev.off()

################################
####Wheather
################################
setwd("H:\\National Scale Data\\CairngormsValidation")
library(lubridate)
library(zoo)
#library(WaveletComp)
library(TSA)
library(forecast)
library(nlme)

WC<-read.table("CairngormsWeather.txt",header=T)
WC$Date<-as.Date(paste("01", WC$Month,WC$Year, sep = "-"), format = "%d-%m-%Y")

Tmax.TS.month <- with(WC,zoo(Tmax, as.Date(Date)))   #make daily time series
Tmax.TS.year<- aggregate(Tmax.TS.month,  as.Date(cut(time(Tmax.TS.month), "year")), sum, na.rm = TRUE)    #aggregate by year

Tmin.TS.month <- with(WC,zoo(Tmin, as.Date(Date)))   #make daily time series
Tmin.TS.year<- aggregate(Tmin.TS.month,  as.Date(cut(time(Tmin.TS.month), "year")), sum, na.rm = TRUE)    #aggregate by year

Frost.TS.month <- with(WC,zoo(Frost, as.Date(Date)))   #make daily time series
Frost.TS.year<- aggregate(Frost.TS.month,  as.Date(cut(time(Frost.TS.month), "year")), sum, na.rm = TRUE)    #aggregate by year

Rain.TS.month <- with(WC,zoo(Rainfall, as.Date(Date)))   #make daily time series
Rain.TS.year<- aggregate(Rain.TS.month,  as.Date(cut(time(Rain.TS.month), "year")), sum, na.rm = TRUE)    #aggregate by year

dev.new()
par(mfrow=c(4,1))


plot(Tmax.TS.month,col="blue",
     main="Maximum Temperature",
     xlab="",ylab="degC",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(Tmin.TS.month,col="red",
     main="Minimum Temperature",
     xlab="",ylab="degC",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)

plot(Frost.TS.month,col="blue",
     main="Frost",
     xlab="",ylab="days",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(Rain.TS.month,col="red",
     main="Rainfall",
     xlab="",ylab="mm",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
#axis(side=2, at=c(50000,100000,200000),labels=c("50000","100000","200000"),cex.axis=2.5)


dev.new()
par(mfrow=c(4,1))


plot(Tmax.TS.year,col="blue",
     main="Maximum Temperature",
     xlab="",ylab="degC",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(Tmin.TS.year,col="red",
     main="Minimum Temperature",
     xlab="",ylab="degC",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)

plot(Frost.TS.year,col="blue",
     main="Frost",
     xlab="",ylab="days",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(Rain.TS.year,col="red",
     main="Rainfall",
     xlab="",ylab="mm",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
#axis(side=2, at=c(50000,100000,200000),labels=c("50000","100000","200000"),cex.axis=2.5)

C<-read.table("Cairngorms_UD.txt",header=T)

C$id<-as.character(C$id)
C$owner<-as.character(C$owner)
C$datetaken<-as.character(C$datetaken)
C$tags<-as.character(C$tags)
C$year<-as.factor(C$year)
C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


C$date<-ymd(C$dateonly)  #transform character strings into
                                               #time format


flickr_TS<-tapply(C$owner,C$date,length)       #counts observations per date

flickr_TSdf<-as.data.frame.table(flickr_TS)           #converts to dataframe

colnames(flickr_TSdf)<-c("Date","Visits")

flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)               #converts to date format

flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year



realC<-read.table("CairngormsVD_TimeSeries.txt",header=T)

realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y")

#realC<-realC[,-2]

realC_TSdf<-as.data.frame(realC)           #converts to dataframe

colnames(realC)<-c("Date","Visits")

realC$Date<-as.Date(as.yearmon(realC$Date))               #converts to date format

realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))   #make daily time series

realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    #aggregate by year


flickr_TS_monthly<-as.data.frame(flickr_TS_monthly)
realC_TS_monthly<-as.data.frame(realC_TS_monthly)

stflickr<-scale(flickr_TS_monthly,scale=T)      #standardise TS
strealC<-scale(realC_TS_monthly,scale=T)


Date<-seq.Date(from=as.Date("2009-01-01"),to=as.Date("2014-12-31"),by="month")

TS<-as.data.frame(cbind(Date,stflickr,strealC,WC$Tmax,WC$Tmin,WC$Frost,WC$Rainfall))
colnames(TS)<-c("Date","stflickr","strealC","Tmax","Tmin","Frost","Rain")
TS$Date<-as.Date(TS$Date)
TS$Year<-year(ymd(TS$Date)) 
TS$Month<-month(ymd(TS$Date))

for (i in 1:length(TS$Month)){
if (TS$Month[i]==12|TS$Month[i]==1|TS$Month[i]==2) {TS$Season[i]<-"Winter"}
else { if (TS$Month[i]==3|TS$Month[i]==4|TS$Month[i]==5) {TS$Season[i]<-"Spring"}
     else{ if (TS$Month[i]==6|TS$Month[i]==7|TS$Month[i]==8) {TS$Season[i]<-"Summer"} 
           else {TS$Season[i]<-"Autumn"}
           }}}
           
           
for (i in 1:length(TS$Month)){
if (TS$Month[i]==12|TS$Month[i]==1|TS$Month[i]==2) {TS$Season1[i]<-1}
else { if (TS$Month[i]==3|TS$Month[i]==4|TS$Month[i]==5) {TS$Season1[i]<-2}
     else{ if (TS$Month[i]==6|TS$Month[i]==7|TS$Month[i]==8) {TS$Season1[i]<-3} 
           else {TS$Season1[i]<-4}
           }}}
           
TS$Season<-as.factor(TS$Season)

TS$Month2<-seq(1:length(TS$Month))

plot(TS$stflickr~TS$Tmax)

#######################################
#######Linear modeling
#######################################

model.1.Tmax<-gls(strealC~stflickr+Season+Tmax,data=TS)

summary(model.1.Tmax)

acf(resid(model.1.Tmax,type="normalized"))


model.1.Tmin<-gls(strealC~stflickr+Season+Tmin,data=TS)

summary(model.1.Tmin)

acf(resid(model.1.Tmin,type="normalized"))


model.1.Rain<-gls(strealC~stflickr+Season+Rain,data=TS)

summary(model.1.Rain)

acf(resid(model.1.Rain,type="normalized"))




AIC(model.1.Tmin,model.1.Tmax,model.1.Rain)
#             df       AIC
#model.1.Tmin  7  79.42260
#model.1.Tmax  7  74.50888
#model.1.Rain  7 119.01739

#model with Tmax is the best model

dev.new()
acf(resid(model.1.Tmax,type="normalized"))               

dev.new()
pacf(resid(model.1.Tmax,type="normalized"))

par(mfrow=c(1,3))
boxplot(resid(model.1.Tmax,type="normalized")~TS$Season)            
abline(h=0,col="red")


plot(resid(model.1.Tmax,type="normalized")~TS$Tmax)
abline(h=0,col="red")

plot(resid(model.1.Tmax,type="normalized"))
abline(h=0,col="red")

plot(model.1.Tmax)

summary(model.1.Tmax)

####################################
####arima models
####################################

flickr_TS_monthly<-as.data.frame(flickr_TS_monthly)
realC_TS_monthly<-as.data.frame(realC_TS_monthly)

stflickr<-as.data.frame(scale(flickr_TS_monthly,scale=T))      #standardise TS
strealC<-as.data.frame(scale(realC_TS_monthly,scale=T))


empirical<-ts(strealC$realC_TS_monthly,start=c(2009,1),frequency=12)
flickr<-ts(stflickr$flickr_TS_monthly,start=c(2009,1),frequency=12)


emp.model<-auto.arima(empirical)
summary(emp.model)
#Series: empirical 
#ARIMA(0,0,0)(2,1,0)[12] with drift         
#
#Coefficients:
#         sar1     sar2   drift
#      -0.5912  -0.5240  0.0042
#s.e.   0.1446   0.1294  0.0010
#
#sigma^2 estimated as 0.02712:  log likelihood=19.8
#AIC=-31.6   AICc=-30.87   BIC=-23.22
#
#Training set error measures:
#                      ME      RMSE       MAE      MPE     MAPE      MASE
#Training set 0.006814837 0.1465191 0.1066235 2.339632 28.09322 0.6264845
#                   ACF1
#Training set -0.1013968





flickr.model.Tmax<-auto.arima(flickr,xreg=TS$Tmax)
summary(flickr.model.Tmax)
#Series: flickr 
#ARIMA(0,1,1)(0,0,1)[12]                    
#
#Coefficients:
#          ma1    sma1  TS$Tmax
#      -0.5081  0.4197   0.0974
#s.e.   0.1192  0.1222   0.0155
#
#sigma^2 estimated as 0.2322:  log likelihood=-48.69
#AIC=105.38   AICc=105.98   BIC=114.43
#
#Training set error measures:
#                     ME      RMSE       MAE      MPE     MAPE      MASE
#Training set 0.01178245 0.4682993 0.3707359 44.55862 95.84309 0.6048116
#                   ACF1
#Training set 0.02752707



flickr.model.Tmin<-auto.arima(flickr,xreg=TS$Tmin)
summary(flickr.model.Tmin)
#Series: flickr 
#ARIMA(1,1,0)(1,0,0)[12]                    
#
#Coefficients:
#          ar1    sar1  TS$Tmin
#      -0.4544  0.4761   0.0778
#s.e.   0.1126  0.1223   0.0252
#
#sigma^2 estimated as 0.2628:  log likelihood=-53.43
#AIC=114.85   AICc=115.46   BIC=123.9
#
#Training set error measures:
#                      ME      RMSE       MAE      MPE     MAPE      MASE
#Training set 0.005847319 0.4981686 0.3948505 36.16943 109.4443 0.6441516
#                    ACF1
#Training set -0.07763281



flickr.model.Rain<-auto.arima(flickr,Rain.TS.month)                              #this is best model
summary(flickr.model.Rain)                                                      #arima structure differs from empirical model
#Series: flickr                                                                 #but coefficients are very similar
#ARIMA(0,1,1)(0,1,1)[12]                    
#
#Coefficients:
#          ma1     sma1  TS$Rain
#      -0.6406  -0.5574  -0.0017
#s.e.   0.1025   0.1730   0.0012
#
#sigma^2 estimated as 0.2404:  log likelihood=-42.62
#AIC=93.24   AICc=93.98   BIC=101.55
#
#Training set error measures:
#                      ME      RMSE       MAE      MPE     MAPE      MASE
#Training set -0.01566629 0.4323945 0.3190613 48.12624 101.2292 0.5205105
#                    ACF1
#Training set -0.09099892





#diagnostic empirical model 1
par(mfrow=c(3,1))

plot(window(rstandard(emp.model),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)


acf(as.vector(window(rstandard(emp.model),start=c(2010,1))),lag.max=36,main="")

pacf(as.vector(window(rstandard(emp.model),start=c(2010,1))),lag.max=36,main="")

dev.new()
par(mfrow=c(1,2))
hist(window(rstandard(emp.model),start=c(2010,1)),
xlab='Standardized Residuals',main="")

qqnorm(window(rstandard(emp.model),start=c(2010,1)))
qqline(window(rstandard(emp.model),start=c(2010,1)))


##diagnostic flickr model 1
par(mfrow=c(3,1))

plot(window(rstandard(flickr.model.Rain),start=c(2010,1)),
ylab='Standardized Residuals',type='o')
abline(h=0)

acf(as.vector(window(rstandard(flickr.model.Rain),start=c(2010,1))),lag.max=36,main="")
pacf(as.vector(window(rstandard(flickr.model.Rain),start=c(2010,1))),lag.max=36,main="")


dev.new()
par(mfrow=c(1,2))

hist(window(rstandard(flickr.model.Rain),start=c(2010,1)),
xlab='Standardized Residuals',main="")

qqnorm(window(rstandard(flickr.model.Rain),start=c(2010,1)))
qqline(window(rstandard(flickr.model.Rain),start=c(2010,1)))

WC15<-read.table("CairngormsWeather2015-16.txt",header=T)
WC15$Date<-as.Date(paste("01", WC15$Month,WC15$Year, sep = "-"), format = "%d-%m-%Y")


Rain.TS.month15 <- with(WC15,zoo(Rain, as.Date(Date)))   #make daily time series


emp.fcast <- forecast(emp.model,h=16)
flickr.fcast <- forecast(flickr.model.Rain,xreg=Rain.TS.month15 )

plot(emp.fcast,col="firebrick1",fcol="firebrick4",shadecols=c("lightcoral","lightpink"),lwd=3,flwd=3, main="",ylab="Cairngorms empirical User Days",cex=2.5)
dev.new()
plot(flickr.fcast,col="dodgerblue",fcol="dodgerblue4",lwd=3,flwd=3,main="",ylab="Cairngorms Flickr User Days",cex=2.5)



par(oma=c(2,2,2,2))
par(mar=c(7,4,0,4))

plot(c(min(TS$Date),max(TS$Date)),c(0,200),type="n", yaxt="n",xaxt="n",
     main="",xlab="",ylab="",lwd=3,cex.lab=2.5, 
     cex.axis=2.5, cex.main=2.5, cex=2.5)

lines(Rain.TS.month,col="darkorange",lwd=3)                          
lines(flickr_TS_monthly,col="dodgerblue",lwd=3)                         
axis(side=2, at=c(50,100,150),labels=c("50","100","150"),cex.axis=2.5)
axis(side=4, at=c(50,100,200),labels=c("50","100","200"),cex.axis=2.5)
axis(side=1,at=TS$Date[seq(from=1,to=72,6)],labels=F)
text(x=TS$Date[seq(from=1,to=72,6)], y=par()$usr[3]-0.1*(par()$usr[4]-par()$usr[3]),
labels= format(TS$Date[seq(from=1,to=72,6)],"%Y-%b"), srt=45, adj=1, xpd=TRUE,cex=1.5)

mtext("Flickr User Days",side=2,cex=2.5,outer=T)
mtext("Rainfall (mm)",side=4,cex=2.5,outer=T)

legend(min(TS$Date),205, # places a legend at the appropriate place 
c("Flickr UD","Rain"), # puts text in the legend 

lty=c(1,1), # gives the legend appropriate symbols (lines)

lwd=c(3,3),col=c("dodgerblue","darkorange"),cex=1.5,bty="n") # gives the legend lines the correct color and width

rect(c(TS$Date[40],TS$Date[52]),c(0,0),c(TS$Date[46],TS$Date[58]), c(200,200), border = "gray26",lty=6,lwd=2.5) # coloured
                                                                                                      









