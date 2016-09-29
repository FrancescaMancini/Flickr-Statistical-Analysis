#######################################################
###################Spatio-temporal analysis
#######################################################
library(lubridate)
library(rgdal)
library(ggplot2)
library(maptools)
library(plyr)

#load datasets
sealwatch<-read.table("sealwatching.txt",header=T)

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


#read shapefile of UK coastline
UK<-readOGR(dsn= "UK_Outline_WGS84.shp", layer="UK_Outline_WGS84")


UK<-spTransform(UK,CRS("+proj=longlat +datum=WGS84"))             #transform geographic coordinates
proj4string(UK) <- CRS("+proj=longlat +datum=WGS84")              #project



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
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        

plot.years

#plot density per season
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


#####################################################
##plot density excluding Edinburgh and Glasgow
#####################################################
cities<-readOGR(dsn= "EdinGlas.shp", layer="EdinGlas")                #load shapefile with Edinburgh and Glasgow boundaries

writePointsShape(birdwatch, "BirdwatchRural")                         #make points shapefile


birdwatch <- readShapePoints("BirdwatchRural.shp")                    #read the shapefile
proj4string(birdwatch) <- CRS("+proj=longlat +datum=WGS84")           #and project

#transform polygon
cities<-spTransform(cities,CRS("+proj=longlat +datum=WGS84"))
#and give same projection as points
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")


incities2<-over(SpatialPoints(birdwatch),SpatialPolygons(cities@polygons),
          returnlist=TRUE)                                                    #identify points in cities boundaries

birdwatch_sub<-birdwatch[-which(!is.na(incities2)),]                          #exclude from dataset

birdwatch_sub$id<-as.character(birdwatch$id)
birdwatch_sub$owner<-as.character(birdwatch$owner)
birdwatch_sub$datetaken<-as.character(birdwatch$datetaken)
birdwatch_sub$tags<-as.character(birdwatch$tags)
birdwatch_sub$year<-as.factor(birdwatch$year)
birdwatch_sub$month<-factor(birdwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

birdwatch_sub$date<-parse_date_time(birdwatch$dateonly,"dmy")  #transform character strings into
                                               #time format
                                               
UK@data$id = rownames(UK@data)
UK.Df<-fortify(UK, region="id")

bird.points<-fortify(birdwatch_sub)

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
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha= ..level..), 
             geom = "polygon")+
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(text=element_text(size=18),legend.position = c(.9, .15))
        
WD.plot.years

