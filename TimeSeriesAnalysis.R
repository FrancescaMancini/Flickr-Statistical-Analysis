################################################################################
################################################################################
###############################Time series######################################
################################################################################
################################################################################
library(lubridate)
library(zoo)
library(WaveletComp)


setwd("H:\\National Scale Data\\Flickr\\Final Datasets")

birdwatch<-read.table("birdwatchingmined.txt",header=T)


sealwatch<-read.table("sealDef.txt",header=T)


WDwatching<-read.table("Whale&Dolphinwatching.txt",header=T)

birdwatch$id<-as.character(birdwatch$id)
birdwatch$owner<-as.character(birdwatch$owner)
birdwatch$datetaken<-as.character(birdwatch$datetaken)
birdwatch$tags<-as.character(birdwatch$tags)
birdwatch$year<-as.factor(birdwatch$year)
birdwatch$month<-factor(birdwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

birdwatch$date<-parse_date_time(birdwatch$dateonly,"dmy")  #transform character strings into
                                               #time format

sealwatch$id<-as.character(sealwatch$id)
sealwatch$owner<-as.character(sealwatch$owner)
sealwatch$datetaken<-as.character(sealwatch$datetaken)
sealwatch$tags<-as.character(sealwatch$tags)
sealwatch$year<-as.factor(sealwatch$year)
sealwatch$month<-factor(sealwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

sealwatch$date<-parse_date_time(sealwatch$dateonly,"dmy") #transform character strings into
                                               #time format


WDwatching$id<-as.character(WDwatching$id)
WDwatching$owner<-as.character(WDwatching$owner)
WDwatching$datetaken<-as.character(WDwatching$datetaken)
WDwatching$tags<-as.character(WDwatching$tags)
WDwatching$year<-as.factor(WDwatching$year)
WDwatching$month<-factor(WDwatching$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

WDwatching$date<-parse_date_time(WDwatching$dateonly,"dmy")  #transform character strings into
                                               #time format



birds_TS<-tapply(birdwatch$owner,birdwatch$date,length)       #counts observations per date

birds_TSdf<-as.data.frame.table(birds_TS)           #converts to dataframe

colnames(birds_TSdf)<-c("Date","Visits")

birds_TSdf$Date<-as.Date(birds_TSdf$Date)               #converts to date format

birds_TS_daily <- with(birds_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


birds_TS_monthly <- aggregate(birds_TS_daily,  as.Date(cut(time(birds_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

birds_TS_qtr<- aggregate(birds_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

birds_TS_year<- aggregate(birds_TS_daily,  as.Date(cut(time(birds_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year

par(mfrow=c(4,1))
plot(birds_TS_daily)
plot(birds_TS_monthly)
plot(birds_TS_qtr)
plot(birds_TS_year)


seals_TS<-tapply(sealwatch$owner,sealwatch$date,length)       #counts observations per date

seals_TSdf<-as.data.frame.table(seals_TS)           #converts to dataframe

colnames(seals_TSdf)<-c("Date","Visits")

seals_TSdf$Date<-as.Date(seals_TSdf$Date)               #converts to date format

seals_TS_daily <- with(seals_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


seals_TS_monthly <- aggregate(seals_TS_daily,  as.Date(cut(time(seals_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

seals_TS_qtr<- aggregate(seals_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

seals_TS_year<- aggregate(seals_TS_daily,  as.Date(cut(time(seals_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year

dev.new()
par(mfrow=c(4,1))
plot(seals_TS_daily)
plot(seals_TS_monthly)
plot(seals_TS_qtr)
plot(seals_TS_year)



WDwatching_TS<-tapply(WDwatching$owner,WDwatching$date,length)       #counts observations per date

WDwatching_TSdf<-as.data.frame.table(WDwatching_TS)           #converts to dataframe

colnames(WDwatching_TSdf)<-c("Date","Visits")

WDwatching_TSdf$Date<-as.Date(WDwatching_TSdf$Date)               #converts to date format

WDwatching_TS_daily <- with(WDwatching_TSdf,zoo(Visits, as.Date(Date)))   #make daily time series


WDwatching_TS_monthly <- aggregate(WDwatching_TS_daily,  as.Date(cut(time(WDwatching_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

WDwatching_TS_qtr<- aggregate(WDwatching_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

WDwatching_TS_year<- aggregate(WDwatching_TS_daily,  as.Date(cut(time(WDwatching_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year

dev.new()
par(mfrow=c(4,1))
plot(WDwatching_TS_daily)
plot(WDwatching_TS_monthly)
plot(WDwatching_TS_qtr)
plot(WDwatching_TS_year)


tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//WidlifewatchingTS.tiff",
     width=3000,height=3000,res=300)

par(mfrow=c(3,1))


plot(birds_TS_monthly,col="royalblue1",
     main="Birds",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(seals_TS_monthly,col="gold",
     main="Seals",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)


plot(WDwatching_TS_monthly,col="deeppink",
     main="Whales&Dolphins",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries
#axis(side=2, at=c(50000,100000,200000),labels=c("50000","100000","200000"),cex.axis=2.5)

mtext("Flickr users",side=2,cex=2.5,outer=T)

dev.off()

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//WildlifewatchingYearTS.tiff",
     width=3000,height=3000,res=300)

par(mfrow=c(3,1))

plot(birds_TS_year,col="royalblue1",
     main="Birds",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)                           #plot multivariate timesries

plot(seals_TS_year,col="gold",
     main="Seals",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5)

plot(WDwatching_TS_year,col="deeppink",
     main="Whales&Dolphins",
     xlab="",ylab="",
     lwd=3,cex.lab=2.5, cex.axis=2.5, cex.main=2.5, cex=2.5) 
mtext("Flickr users",side=2,cex=2.5,outer=T)

dev.off()

tt <- seq(min(birds_TSdf$Date), max(birds_TSdf$Date), "month")
birds_TS_monthly<-merge(birds_TS_monthly, zoo(, tt), fill = 0)
birds<-ts(birds_TS_monthly,start=c(2005,1),frequency=12)


seals_TS_monthly<-merge(seals_TS_monthly, zoo(, tt), fill = 0)
seals<-ts(seals_TS_monthly,start=c(2005,1),frequency=12)


WDwatching_TS_monthly<-merge(WDwatching_TS_monthly, zoo(, tt), fill = 0)
WDwatching<-ts(WDwatching_TS_monthly,start=c(2005,1),frequency=12)


################################
########decomposition
################################

dec_birds<-stl(birds,"periodic")
dev.new()
plot(dec_birds,main="Bird watching")


dec_seals<-stl(seals,"periodic")
dev.new()
plot(dec_seals,main="Seal watching")


dec_WDwatching<-stl(WDwatching,"periodic")
dev.new()
plot(dec_WDwatching,main="Whales&Dolphins")


#############################
###Spectral analysis
#############################

library(WaveletComp)

#create dataframe with time series
my.data.birds <-data.frame(x = as.data.frame(birds), date= tt)

my.data.seals <-data.frame(x = as.data.frame(seals), date= tt)

my.data.WD <-data.frame(x = as.data.frame(WDwatching), date= tt)

#produce wavelet power spectrum of empirical time series
my.w.birds = analyze.wavelet(my.data.birds, "x",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//BirdwatchingWVSpectrum.tiff",width=2000,height=2000,res=300)

wt.image(my.w.birds, main="Bird watching",legend.params = list(lab = "wavelet power levels", mar = 4.7), show.date=T)

dev.off()

#produce wavelet power spectrum of flickr time series
my.w.seals = analyze.wavelet(my.data.seals, "x",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//SealwatchingWVSpectrum.tiff",width=2000,height=2000,res=300)

wt.image(my.w.seals, main="Seal watching",legend.params = list(lab = "wavelet power levels", mar = 4.7), show.date=T)

dev.off()


my.w.WD = analyze.wavelet(my.data.WD, "x",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//W&DwatchingWVSpectrum.tiff",width=2000,height=2000,res=300)

wt.image(my.w.WD, main="Whale&Dolphin watching",legend.params = list(lab = "wavelet power levels", mar = 4.7), show.date=T)

dev.off()

#average power of empirical and flickr time series
tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//AvgPowSpectra.tiff",width=3000,height=2000,res=300)

par(mfrow=c(1,3))
wt.avg(my.w.birds, main="Bird watching")
wt.avg(my.w.seals,main="Seal watching")
wt.avg(my.w.WD,main="Whale&Dolphin watching")

dev.off()

#######################################################
###################Spatio-temporal analysis
#######################################################
library(lubridate)
#library(stpp)
library(rgdal)
#library(spacetime)



setwd("H:\\National Scale Data\\Flickr\\Final Datasets")



sealwatch<-read.table("sealDef.txt",header=T)

WDwatching<-read.table("Whale&Dolphinwatching.txt",header=T)

birdwatch<-read.table("birdwatchingmined.txt",header=T)

birdwatch$id<-as.character(birdwatch$id)
birdwatch$owner<-as.character(birdwatch$owner)
birdwatch$datetaken<-as.character(birdwatch$datetaken)
birdwatch$tags<-as.character(birdwatch$tags)
birdwatch$year<-as.factor(birdwatch$year)
birdwatch$month<-factor(birdwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

birdwatch$date<-parse_date_time(birdwatch$dateonly,"dmy")  #transform character strings into
                                               #time format


sealwatch$id<-as.character(sealwatch$id)
sealwatch$owner<-as.character(sealwatch$owner)
sealwatch$datetaken<-as.character(sealwatch$datetaken)
sealwatch$tags<-as.character(sealwatch$tags)
sealwatch$year<-as.factor(sealwatch$year)
sealwatch$month<-factor(sealwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

sealwatch$date<-parse_date_time(sealwatch$dateonly,"dmy") #transform character strings into
                                               #time format



WDwatching$id<-as.character(WDwatching$id)
WDwatching$owner<-as.character(WDwatching$owner)
WDwatching$datetaken<-as.character(WDwatching$datetaken)
WDwatching$tags<-as.character(WDwatching$tags)
WDwatching$year<-as.factor(WDwatching$year)
WDwatching$month<-factor(WDwatching$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

WDwatching$date<-parse_date_time(WDwatching$dateonly,"dmy")  #transform character strings into
                                               #time format


##birdwatch<-birdwatch[order(as.Date(birdwatch$date, format="%d/%m/%Y")),]
##bird.ll <- SpatialPoints(birdwatch[, c(5, 6)])
##proj4string(bird.ll) <- CRS("+proj=longlat +datum=WGS84")
##bird.date<-as.Date(birdwatch[, 12])
##bird.st <- STIDF(bird.ll, bird.date, data.frame(counts = as.vector
##(as.matrix(birdwatch))))
##
##stplot(bird.st)
##
#
#
#
#birdwatch.sp <- as.3dpoints(x=birdwatch[,6],y=birdwatch[,5],t=birdwatch[,12])
##makes a dataset containing 3 variables: the coordinates and the dates of the flickr photos
#
#Scot<-readOGR(dsn= "H:\\National Scale Data\\ScotOutline\\12nm_Limit_WGS84.shp", layer="12nm_Limit_WGS84") 
#Scot<-spTransform(Scot,CRS("+proj=longlat +datum=WGS84"))
#proj4string(Scot) <- CRS("+proj=longlat +datum=WGS84")
#polys <- attr(Scot,'polygons')
#npolys <- length(polys)
#for (i in 1:npolys){
#  poly <- polys[[i]]
#  polys2 <- attr(poly,'Polygons')
#  npolys2 <- length(polys2)
#  for (j in 1:npolys2){
#     #do stuff with these values
#     coords <- coordinates(polys2[[j]])
#  }
#}
#
#colnames(coords)<-c("x","y")
#
#
#plot(birdwatch.sp,s.region=coords,pch = 19,mark=T,mark.col=3)  #produces a plot of the spatial distribution of the points and the cumulative distribution of the times
#
##stan(birdwatch.sp, tlim=c(2005-01-01,2005-12-31),twid=diff(tlim)/12,bgpoly = coords, bgframe = FALSE)
##
##animation(birdwatch.sp, runtime = 10, cex = 0.5, s.region = coords)
##
#birdwatch.sp2<-as.3dpoints(birdwatch.sp[, 1] / 1000, birdwatch.sp[, 2] / 1000, birdwatch.sp[,3])
#
#S<-coords/1000
#
#Mt <- density(birdwatch.sp[, 3])
#mut <- Mt$y[findInterval(birdwatch.sp[, 3], Mt$x)] * dim(birdwatch.sp)[1]
#h <- mse2d(as.points(birdwatch.sp[, 1:2]),coords, nsmse = 50, range = 4)
#h <- h$h[which.min(h$mse)]
#Ms <- kernel2d(as.points(birdwatch.sp[, 1:2]),coords, h0 = h, nx = 5000, ny = 5000)
#atx <- findInterval(x = birdwatch.sp[, 1], vec = Ms$x)
#aty <- findInterval(x = birdwatch.sp[, 2], vec = Ms$y)
#mhat <- NULL
#for(i in 1:length(atx)) mhat <- c(mhat, Ms$z[atx[i], aty[i]])
#u <- seq(0, 10, by = 1)
#v <- seq(0, 15, by = 1)
#stik <- STIKhat(xyt = birdwatch.sp,  lambda = mhat * mut/dim(birdwatch.sp)[1], dist = u, times = v)
#g <- PCFhat(xyt = birdwatch.sp, lambda = mhat * mut/dim(birdwatch.sp)[1], dist = 1:20, times = 1:20)
#




library(ggplot2)
library(maptools)
library(plyr)
library(rgdal)


UK<-readOGR(dsn= "H:\\National Scale Data\\UK Outline\\UK_Outline_WGS84.shp", layer="UK_Outline_WGS84")


UK<-spTransform(UK,CRS("+proj=longlat +datum=WGS84"))
proj4string(UK) <- CRS("+proj=longlat +datum=WGS84")


#coordinates(birdwatch)<-c("longitude","latitude")


#proj4string(birdwatch) <- CRS("+proj=longlat +datum=WGS84")


#Create a shapefile


UK@data$id = rownames(UK@data)
UK.Df<-fortify(UK, region="id")

bird.points<-fortify(birdwatch)

plot.years <- ggplot(data=bird.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95",size=0.3) +
             #geom_line(size=1)+ 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        

plot.years

#tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//BirdHotspotMap_2005-2015.tiff",width=3000,height=3000,res=300)
#plot.years
#dev.off()

pdf("H://National Scale Data//Flickr//TimeSeriesAnalysis//birdHotspotMap2.pdf",width=7.4,height=8)
plot.years
dev.off()



ggsave("BirdHotspotMap_2005-2015",plot=plot.years,device="pdf",path="H:\\National Scale Data//Flickr//TimeSeriesAnalysis",dpi=600)


bird.points$Season <- factor(quarters(bird.points$date), 
                      levels = c("Q1", "Q2", "Q3", "Q4"), 
                      labels = c("winter", "spring", "summer", "fall"))


plot_2005<- ggplot(data=bird.points[bird.points$year=="2005",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2005 by season") +
             facet_wrap(~ Season)+
             theme(text=element_text(size=20))
        
                                                     
plot_2005  

dev.new()
plot_2006<- ggplot(data=bird.points[bird.points$year=="2006",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2006 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2006  
  
dev.new()
plot_2007<- ggplot(data=bird.points[bird.points$year=="2007",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2007 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2007  

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//BirdHotspotMap_2007.tiff",width=3000,height=3000,res=300)
plot_2007
dev.off()


ggsave("BirdHotspotMap_2007",plot=plot_2007,device="pdf",path="H:\\National Scale Data//Flickr//TimeSeriesAnalysis",dpi=600)
  

dev.new()
plot_2008<- ggplot(data=bird.points[bird.points$year=="2008",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2008 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2008  

dev.new()
plot_2009<- ggplot(data=bird.points[bird.points$year=="2009",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2009 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2009  


dev.new()
plot_2010<- ggplot(data=bird.points[bird.points$year=="2010",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2010 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2010  

dev.new()
plot_2011<- ggplot(data=bird.points[bird.points$year=="2011",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2011 by season") +          
             facet_wrap(~ Season)
        
                                                     
plot_2011  

dev.new()
plot_2012<- ggplot(data=bird.points[bird.points$year=="2012",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2012 by season") +          
             facet_wrap(~ Season)
        
                                                     
plot_2012  


dev.new()
plot_2013<- ggplot(data=bird.points[bird.points$year=="2013",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +          
             facet_wrap(~ Season)+
             theme(text=element_text(size=18))
        
                                                     
plot_2013  

tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//BirdHotspotMap_2013.tiff",width=3000,height=3000,res=300)
plot_2013
dev.off()


ggsave("BirdHotspotMap_2013",plot=plot_2013,device="pdf",path="H:\\National Scale Data//Flickr//TimeSeriesAnalysis",dpi=600)


dev.new()
plot_2014<- ggplot(data=bird.points[bird.points$year=="2014",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2014 by season") +          
             facet_wrap(~ Season)
        
                                                     
plot_2014  


dev.new()
plot_2015<- ggplot(data=bird.points[bird.points$year=="2015",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) + 
             ggtitle("Birdwatching in Scotland in 2015 by season") +          
             facet_wrap(~ Season)
        
                                                     
plot_2015 

bird.plot.seasons<- ggplot(data=bird.points,
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) + 
             ggtitle("Bird watching in Scotland from 2005 to 2015 by season") +
             facet_wrap(~ Season)

bird.plot.seasons
 
 
 
#############outside cities
birdwatch<-read.table("birdwatchingmined.txt",header=T)
cities<-readOGR(dsn= "H:\\National Scale Data\\Flickr\\TimeSeriesAnalysis\\EdinGlas.shp", layer="EdinGlas")

cities<-spTransform(cities,CRS("+proj=longlat +datum=WGS84"))
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")

writePointsShape(birdwatch, "BirdwatchRural")

#read the shapefile
birdwatch <- readShapePoints("BirdwatchRural.shp")
proj4string(birdwatch) <- CRS("+proj=longlat +datum=WGS84")

#transform polygon
cities<-spTransform(cities,CRS("+proj=longlat +datum=WGS84"))
#and give same projection as points
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")


  
incities2<-over(SpatialPoints(birdwatch),SpatialPolygons(cities@polygons),
          returnlist=TRUE)

birdwatch_sub<-birdwatch[-which(!is.na(incities2)),]

write.table(birdwatch_sub,"BirdwatchRural.txt", row.names=F,sep="\t", quote=F)


birdwatch<-read.table("BirdwatchRural.txt",header=T)

birdwatch$id<-as.character(birdwatch$id)
birdwatch$owner<-as.character(birdwatch$owner)
birdwatch$datetaken<-as.character(birdwatch$datetaken)
birdwatch$tags<-as.character(birdwatch$tags)
birdwatch$year<-as.factor(birdwatch$year)
birdwatch$month<-factor(birdwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

birdwatch$date<-parse_date_time(birdwatch$dateonly,"dmy")  #transform character strings into
                                               #time format
                                               
UK@data$id = rownames(UK@data)
UK.Df<-fortify(UK, region="id")

bird.points<-fortify(birdwatch)

plot.years <- ggplot(data=bird.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        

plot.years

pdf("H://National Scale Data//Flickr//TimeSeriesAnalysis//birdRuralHotspotMap.pdf",width=7.4,height=8)
plot.years
dev.off()




bird.points$Season <- factor(quarters(bird.points$date), 
                      levels = c("Q1", "Q2", "Q3", "Q4"), 
                      labels = c("winter", "spring", "summer", "fall"))


  
dev.new()
plot_2009<- ggplot(data=bird.points[bird.points$year=="2009",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1,shape=".")+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             ggtitle("Birdwatching in Scotland in 2009 by season") +
             facet_wrap(~ Season)
        
                                                     
plot_2009  

pdf("H://National Scale Data//Flickr//TimeSeriesAnalysis//birdRuralHotspotMap2009.pdf",width=5.5,height=6.5)
plot_2009
dev.off()




#######################################
#######Seals
#######################################

seal.points<-fortify(sealwatch)

seal.plot.years <- ggplot(data=seal.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             #ggtitle("Seal watching in Scotland from 2005 to 2015")+
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        

seal.plot.years

pdf("H://National Scale Data//Flickr//TimeSeriesAnalysis//sealhotspotmap.pdf",width=7.4,height=8)
seal.plot.years
dev.off()



seal.points$Season <- factor(quarters(seal.points$date), 
                      levels = c("Q1", "Q2", "Q3", "Q4"), 
                      labels = c("winter", "spring", "summer", "fall"))


seal.plot_2005<- ggplot(data=seal.points[seal.points$year=="2005",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2005 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2005  

dev.new()
seal.plot_2006<- ggplot(data=seal.points[seal.points$year=="2006",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2006 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2006  
  
dev.new()
seal.plot_2007<- ggplot(data=seal.points[seal.points$year=="2007",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2007 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2007  
  

dev.new()
seal.plot_2008<- ggplot(data=seal.points[seal.points$year=="2008",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2008 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2008  

dev.new()
seal.plot_2009<- ggplot(data=seal.points[seal.points$year=="2009",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2009 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2009  


dev.new()
seal.plot_2010<- ggplot(data=seal.points[seal.points$year=="2010",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2010 by season") +
             facet_wrap(~ Season)
        
                                                     
seal.plot_2010  

dev.new()
seal.plot_2011<- ggplot(data=seal.points[seal.points$year=="2011",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2011 by season") +          
             facet_wrap(~ Season)
        
                                                     
seal.plot_2011  

dev.new()
seal.plot_2012<- ggplot(data=seal.points[seal.points$year=="2012",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2012 by season") +          
             facet_wrap(~ Season)
        
                                                     
seal.plot_2012  


dev.new()
seal.plot_2013<- ggplot(data=seal.points[seal.points$year=="2013",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2013 by season") +          
             facet_wrap(~ Season)
        
                                                     
seal.plot_2013  


dev.new()
seal.plot_2014<- ggplot(data=seal.points[seal.points$year=="2014",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Seal watching in Scotland in 2014 by season") +          
             facet_wrap(~ Season)
        
                                                     
seal.plot_2014  


dev.new()
seal.plot_2015<- ggplot(data=seal.points[seal.points$year=="2015",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) + 
             ggtitle("Seal watching in Scotland in 2015 by season") +          
             facet_wrap(~ Season)
        
                                                     
seal.plot_2015  


seal.plot.seasons<- ggplot(data=seal.points,
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) + 
             ggtitle("Seal watching in Scotland from 2005 to 2015 by season") +
             facet_wrap(~ Season)


tiff(filename="H://National Scale Data//Flickr//TimeSeriesAnalysis//SealHotspotMap_Seasons_2005-2015.tiff",
     width=3000,height=3000,res=300)
seal.plot.seasons
dev.off()

ggsave("SealHotspotMap_Seasons_2005-2015",plot=seal.plot.seasons,device="pdf",path="H:\\National Scale Data//Flickr//TimeSeriesAnalysis",dpi=300)



########################################
#####Dolphins & Whales
########################################


WD.points<-fortify(WDwatching)

WD.plot.years <- ggplot(data=WD.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha= ..level..),#h=c(0.6,0.3), 
             geom = "polygon")+#colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             #ggtitle("Whale&Dolphin watching in Scotland from 2005 to 2015")+
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        
WD.plot.years

pdf("H://National Scale Data//Flickr//TimeSeriesAnalysis//W&Dhotspotmap.pdf",width=7.4,height=8)
WD.plot.years
dev.off()



WD.points$Season <- factor(quarters(WD.points$date), 
                      levels = c("Q1", "Q2", "Q3", "Q4"), 
                      labels = c("winter", "spring", "summer", "fall"))


WD.plot_2005<- ggplot(data=WD.points[WD.points$year=="2005",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2005 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2005  

dev.new()
WD.plot_2006<- ggplot(data=WD.points[WD.points$year=="2006",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2006 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2006  
  
dev.new()
WD.plot_2007<- ggplot(data=WD.points[WD.points$year=="2007",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2007 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2007  
  

dev.new()
WD.plot_2008<- ggplot(data=WD.points[WD.points$year=="2008",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2008 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2008  

dev.new()
WD.plot_2009<- ggplot(data=WD.points[WD.points$year=="2009",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2009 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2009  


dev.new()
WD.plot_2010<- ggplot(data=WD.points[WD.points$year=="2010",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2010 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2010  

dev.new()
WD.plot_2011<- ggplot(data=WD.points[WD.points$year=="2011",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2011 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2011  

dev.new()
WD.plot_2012<- ggplot(data=WD.points[WD.points$year=="2012",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2012 by season") + 
             facet_wrap(~ Season)
        
                                                     
WD.plot_2012  


dev.new()
WD.plot_2013<- ggplot(data=WD.points[WD.points$year=="2013",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2013 by season") + 
             facet_wrap(~ Season)
        
                                                     
WD.plot_2013  


dev.new()
WD.plot_2014<- ggplot(data=WD.points[WD.points$year=="2014",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             ggtitle("Whale&Dolphin watching in Scotland in 2014 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2014  


dev.new()
WD.plot_2015<- ggplot(data=WD.points[WD.points$year=="2015",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) + 
             ggtitle("Whale&Dolphin watching in Scotland in 2015 by season") +
             facet_wrap(~ Season)
        
                                                     
WD.plot_2015  


WD.plot.seasons<- ggplot(data=WD.points,
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             #geom_path(Scot.fort,aes(x=long,y=lat,group=id))+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), 
             geom = "polygon", colour = "grey95") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) + 
             ggtitle("Whale&Dolphin watching in Scotland from 2005 to 2015 by season") +
             facet_wrap(~ Season)

WD.plot.seasons














