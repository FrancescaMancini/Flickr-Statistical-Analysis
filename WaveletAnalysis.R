###############################################
# R code to conduct wavelet analysis
# created by Francesca Mancini
# depends on CairngormsFVD.txt and CairngormsSVD.txt
# last modified 04/11/2016
###############################################

library(lubridate)
library(zoo)
library(WaveletComp)

C<-read.table("CairngormsFVD.txt",header=T)                                #load Flickr visitor days dataset

C$id<-as.character(C$id)                                                   #set photo id as character string
C$owner<-as.character(C$owner)                                             #set photographer id as character string
C$datetaken<-as.character(C$datetaken)                                     #set date as character string
C$tags<-as.character(C$tags)                                               #set tags as character string
C$year<-as.factor(C$year)                                                  #set year as a factor   
C$month<-factor(C$month,levels=c("Jan","Feb","Mar","Apr","May",                         
                         "Jun","Jul","Aug","Sep","Oct","Nov","Dec"))       #set order of month factor levels


C$date<-ymd(C$dateonly)                                                    #transform character strings into time format
                                   
flickr_TS<-tapply(C$owner,C$date,length)                                   #counts observations per date

flickr_TSdf<-as.data.frame.table(flickr_TS)                                #converts to dataframe

colnames(flickr_TSdf)<-c("Date","Visits")                                  #set column names of dataframe

flickr_TSdf$Date<-as.Date(flickr_TSdf$Date)                                #converts to date format

flickr_TS_daily <- with(flickr_TSdf,zoo(Visits, as.Date(Date)))            #make daily time series


flickr_TS_monthly <- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "month")), sum, na.rm = TRUE) #aggregate daily time series by month

flickr_TS_qtr<- aggregate(flickr_TS_daily, as.yearqtr, sum, na.rm = TRUE)     #aggregate by quarter

flickr_TS_year<- aggregate(flickr_TS_daily,  as.Date(cut(time(flickr_TS_daily), "year")), sum, na.rm = TRUE)    #aggregate by year


realC<-read.table("CairngormsSVD.txt",header=T)                            #load STEAM visitor days dataset

realC$Date<-as.Date(paste("01-", realC$Date, sep = ""), format = "%d-%b-%y") #converts to correct date format

realC_TSdf<-as.data.frame(realC)                                             #converts to dataframe

colnames(realC)<-c("Date","Visits")                                          #set column names of dataframe

realC$Date<-as.Date(as.yearmon(realC$Date))                                  #converts to date format

realC_TS_monthly <- with(realC,zoo(Visits, as.Date(Date)))                   #make daily time series

realC_TS_year<- aggregate(realC_TS_monthly,  as.Date(cut(time(realC_TS_monthly), "year")), sum, na.rm = TRUE)    #aggregate by year

stflickr<-as.data.frame(flickr_TS_monthly)                                   #convert to dataframe
strealC<-as.data.frame(realC_TS_monthly)

empirical<-strealC$realC_TS_monthly
flickr<-stflickr$flickr_TS_monthly

#create dataframe with two time series
my.data <-data.frame(x = empirical , y = flickr, date= realC$Date)

#produce wavelet power spectrum of empirical time series
E.my.w = analyze.wavelet(my.data, "x",loess.span = 0, dt = 1, make.pval = T, n.sim = 100)

#and plot
tiff(filename="EmpWVSpectrum.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wt.image.modified(E.my.w, main="Empirical time series",show.date=T,timelab="",legend.params = list(lab = "wavelet power levels",width=1, mar = 10))
dev.off()



#produce wavelet power spectrum of flickr time series
F.my.w = analyze.wavelet(my.data, "y",loess.span = 0, dt = 1, 
make.pval = T, n.sim = 100)
#and plot
tiff(filename="FlickrWVSpectrum.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wt.image.modified(F.my.w, main="Flickr time series",show.date=T,timelab="",legend.params = list(lab = "wavelet power levels",width=1, mar = 10))
dev.off()


#calculate coherence
my.wc <- analyze.coherency(my.data, my.pair = c("x","y"),
loess.span = 0,
dt = 1, make.pval = T, n.sim = 100)

#and plot
tiff(filename="CrossCorr.tiff",width=3000,height=3000,res=300)
par(mar=c(6,4,3,1))
wc.image.modified(my.wc, which.image = "wc",legend.params = list(lab = "wavelet coherence",width=1, mar = 10),
timelab = "", periodlab = "period (months)",show.date= T)
dev.off()

names(my.wc$series)[2:3]<-c("Empirical","Flickr")      #change names of time series in wc object

#plot phase difference for the significant period in coherency
tiff(filename="PhaseDiff.tiff",width=3000,height=3000,res=300)
par(mar=c(6,6,3,1))
wc.sel.phases.modified(my.wc, sel.lower = 8, sel.upper = 16,only.sig = T, siglvl = 0.05, show.date = T,
which.sig = "wc",legend.coords = "topright", legend.horiz = F,
phaselim = c(-pi,+2*pi), main = "", sub = "",timelab="")
dev.off()



