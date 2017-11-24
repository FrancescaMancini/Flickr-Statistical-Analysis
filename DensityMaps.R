###############################################
# R code to make density maps of Flickr data
# created by Francesca Mancini
# depends on sealwatching.txt, Whale&Dolphinwatching.txt, birdwatchingmined.txt, UK_Outline_WGS84.shp and EdinGlas.shp
# last modified 04/11/2016
###############################################
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
UK<-readOGR(dsn= "Scotland_polygon//Scotland.shp", layer="Scotland")


UK<-spTransform(UK,CRS("+proj=longlat +datum=WGS84"))             #transform geographic coordinates
proj4string(UK) <- CRS("+proj=longlat +datum=WGS84")              #project



UK@data$id = rownames(UK@data)
UK.Df<-fortify(UK, region="id")

bird.points<-fortify(birdwatch)

plot.years <- ggplot(data=bird.points,aes(x=longitude, y=latitude)) +
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(data=bird.points,aes(x=longitude, y=latitude),
              color="dodgerblue4",size=0.5)+
             stat_density2d(aes(x = longitude, y = latitude,
              fill = ..level.., alpha = ..level..), geom = "polygon") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
                   axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
                   axis.text.y=element_blank(), axis.ticks.y=element_blank(),
                   text=element_text(size=18),legend.position = c(.9, .15),
                   panel.grid.major = element_blank(),                            # eliminates grid lines from background
                   panel.background = element_blank()) 
        
tiff(filename="BirdDens.tiff",width=3000,height=3000,res=300)
plot.years
dev.off()

#plot density per season
bird.points$Season <- factor(quarters(bird.points$date), 
                      levels = c("Q1", "Q2", "Q3", "Q4"), 
                      labels = c("winter", "spring", "summer", "fall"))


plot_2009<- ggplot(data=bird.points[bird.points$year=="2009",],
            aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="gray82", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4", size=0.5)+ 
             stat_density2d(aes(x = longitude, y = latitude,  
             fill = ..level.., alpha = ..level..), geom = "polygon") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .75), guide = FALSE) +
             facet_wrap(~ Season)+
            theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
            axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
            axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            text=element_text(size=18),
            panel.grid.major = element_blank(),                            # eliminates grid lines from background
            panel.background = element_blank()) +
            labs(fill = "Density")

        
tiff(filename="Fig.4.tiff",width=3000,height=3000,res=300)                                                     
plot_2009 + 
annotate("point", x = -3.188267, y = 55.953252, shape = 15, size = 2, fill = "blue") +
annotate("point", x = -4.2576300, y = 55.8651500, shape = 17, size = 2, fill = "blue") +
annotate ("point", x = -3.272498, y = 57.943938, shape = 8) 
dev.off()

#####################################################
##plot density excluding Edinburgh and Glasgow
#####################################################
cities<-readOGR(dsn= "EdinGlas.shp", layer="EdinGlas")                #load shapefile with Edinburgh and Glasgow boundaries
bird.points <- birdwatch

coordinates(birdwatch) <- c("longitude","latitude")                   #make object spatial
proj4string(birdwatch) <- CRS("+proj=longlat +datum=WGS84")           #and project

#transform polygon
cities<-spTransform(cities,CRS("+proj=longlat +datum=WGS84"))
#and give same projection as points
proj4string(cities) <- CRS("+proj=longlat +datum=WGS84")


incities2<-over(SpatialPoints(birdwatch),SpatialPolygons(cities@polygons),
          returnlist=TRUE)                                                    #identify points in cities boundaries

bird.points<-bird.points[-which(!is.na(incities2)),]                          #exclude from dataset

#birdwatch@data$id<-as.character(birdwatch@data$id)
#birdwatch_sub$owner<-as.character(birdwatch$owner)
#birdwatch_sub$datetaken<-as.character(birdwatch$datetaken)
#birdwatch_sub$tags<-as.character(birdwatch$tags)
#birdwatch_sub$year<-as.factor(birdwatch$year)
#birdwatch_sub$month<-factor(birdwatch$month,levels=c("Jan","Feb","Mar","Apr","May",
#                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

#birdwatch_sub$date<-parse_date_time(birdwatch$dateonly,"dmy")  #transform character strings into
                                               #time format
                                               
UK@data$id = rownames(UK@data)
UK.Df<-fortify(UK, region="id")

bird.points<-fortify(bird.points)

plot.years <- ggplot(data=bird.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="black", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(bird.points$longitude)-0.05),
              (max(bird.points$longitude)+0.05)), 
              ylim = c((min(bird.points$latitude)-0.05),
              (max(bird.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4", size=0.5)+
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), geom = "polygon") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
             axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
             axis.text.y=element_blank(), axis.ticks.y=element_blank(),
             text=element_text(size=18),legend.position = c(.9, .15),
             panel.grid.major = element_blank(),                            # eliminates grid lines from background
             panel.background = element_blank())
        
tiff(filename="BirdDensRural.tiff",width=3000,height=3000,res=300)   
plot.years
dev.off()

#######################################
#######Seals
#######################################

seal.points<-fortify(sealwatch)

seal.plot.years <- ggplot(data=seal.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="gray82", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(seal.points$longitude)-0.05),
              (max(seal.points$longitude)+0.05)), 
              ylim = c((min(seal.points$latitude)-0.05),
              (max(seal.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+ 
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha = ..level..), geom = "polygon") + 
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
             axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
             axis.text.y=element_blank(), axis.ticks.y=element_blank(),
             text=element_text(size=18),legend.position = c(.9, .15),
             panel.grid.major = element_blank(),                            # eliminates grid lines from background
             panel.background = element_blank()) +
             labs(fill = "Density\n")
        
tiff(filename="Fig.5.tiff",width=3000,height=3000,res=300)   
seal.plot.years + 
#Shetland
annotate("point", x = -1.892431, y = 60.588436, shape = 3) +
#Firth of Forth
annotate("point", x = -3.005066, y = 56.083106, shape = 4) +
#Tay
annotate("point", x = -3.053825, y = 56.434017, shape = 25, fill = "black") +
#Newburgh
annotate("point", x = -2.001910, y = 57.319547, shape = 19)
dev.off()

########################################
#####Dolphins & Whales
########################################
WD.points<-fortify(WDwatching)

WD.plot.years <- ggplot(data=WD.points,aes(x=longitude, y=latitude))+
             geom_polygon(data=UK.Df,aes(x=long, y=lat, group=group), 
             color="gray82", fill="gray82") + coord_fixed() +
              coord_map(orientation = NULL, 
              xlim =c((min(WD.points$longitude)-0.05),
              (max(WD.points$longitude)+0.05)), 
              ylim = c((min(WD.points$latitude)-0.05),
              (max(WD.points$latitude)+0.05))) +
              geom_point(color="dodgerblue4",size=1)+  
             stat_density2d(aes(x = longitude, 
             y = latitude,  fill = ..level.., alpha= ..level..), geom = "polygon")+
             scale_fill_gradient(low = "yellow", high = "red") +
             scale_alpha(range = c(.25, .5), guide = FALSE) +
             facet_wrap(~ year)+
             theme(axis.title.x=element_blank(), axis.text.x=element_blank(),  # don't display x and y axes labels, titles and tickmarks 
             axis.ticks.x=element_blank(),axis.title.y=element_blank(),   
             axis.text.y=element_blank(), axis.ticks.y=element_blank(),
             text=element_text(size=18),legend.position = c(.9, .15),
             panel.grid.major = element_blank(),                            # eliminates grid lines from background
             panel.background = element_blank()) +
             labs(fill = "Density\n")

tiff(filename="Fig.6.tiff",width=3000,height=3000,res=300)           
WD.plot.years +
#Aberdeen
annotate("point", x = -2.094278, y = 57.149717, shape = 8, size = 2) +
#Chanonry Point
annotate("point", x = -4.093254, y = 57.574128, shape = 1, size = 2) 
dev.off()

