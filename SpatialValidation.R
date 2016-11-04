###############################################
# R code to conduct spatial validation of Flickr data
# created by Francesca Mancini
# depends on flickr_survey_pop20KM.txt, flickr_survey_pop10KM.txt and flickr_survey_pop5KM.txt
# last modified 04/11/2016
###############################################

library(statmod)
library(gstat)
library(sp)
require(spdep)
require(maptools)
library(pscl)
library(visreg)


###############################################
#########validation 20 Km
###############################################
fsp20km<-read.table("flickr_survey_pop20KM.txt",header=T)      #load 20km resoltion dataset

fsp20km$flickrbin<-NULL                                        #create binomial variable (presence/absence of Flickr photos)
for(i in 1:length(fsp20km$flickr)){
   ifelse(fsp20km$flickr[i]>=1,fsp20km$flickrbin[i]<-1,fsp20km$flickrbin[i]<-0)}

subs<-fsp20km[fsp20km$flickr!=0,]                              #create dataset wiht number of Flickr photos without 0s

f1.1<-formula(flickrbin~survey,weights=popcount)               
f1<-formula(flickr~survey,weights=popcount)

bin1<-glm(f1.1,data=fsp20km,family=binomial)                   #fit binomial model
pois1<-glm(f1,data=subs,family="poisson")                      #fit poisson model
pois2<-glm(f1,data=subs,family="quasipoisson")                 #fit quasipoisson model
nb1<-glm.nb(f1,data=subs)                                      #fit negative binomial model

AIC(pois1, pois2, nb1)                                         #model selection

par(mfrow=c(2,2))                                              #model validation
plot(pois2)
par(mfrow=c(2,2))
plot(nb1)

coordinates(fsp20km)<-c("X", "Y")                              #set coordinates of data
coordinates(subs)<-c("X", "Y")

Vario.bin1<-variogram(resid(bin1)~1,data=fsp20km)              #calculate variograms
Vario.bin2<-variogram(resid(bin1)~1,data=fsp20km,alpha=c(0,45,90,135,180,225,275,320))


Vario.nb1<-variogram(resid(nb1)~1,data=subs) 
Vario.nb2<-variogram(resid(nb1)~1,data=subs,alpha=c(0,45,90,135,180,225,275,320))

plot(Vario.bin1)
dev.new()
plot(Vario.bin2)
dev.new()
plot(Vario.nb1)
dev.new()
plot(Vario.nb2)

################################
##Spatial eigenvector mapping
################################

coords.bin<-fsp20km[,1:2]
coords.nb<-subs[,1:2]
neighbour.bin<-tri2nb(coords.bin)                                       #define neighbours
neighbour.nb<-tri2nb(coords.nb)

nb.bin.list<-nb2listw(neighbour.bin)                                    #assign weights
nb.nb.list<-nb2listw(neighbour.nb)




flic.meglm.nb1<-ME(f1,data=subs,listw=nb.nb.list,nsim=100,alpha=0.05,   
                   ,family=negative.binomial(theta= 0.1302))            #calculate eigenvectors



flic.meglm.bin1<-ME(f1.1,data=fsp20km,listw=nb.bin.list,family=binomial)


eigen1<-fitted(flic.meglm.nb1)[,1]                                      #plot eigenvectors
eigen2<-fitted(flic.meglm.nb1)[,2]
coords.nb$e1.nb<-eigen1
coords.nb$e2.nb<-eigen2
coords.nb$e3.nb<-fitted(flic.meglm.nb1)[,3]
coords.nb$e4.nb<-fitted(flic.meglm.nb1)[,4]
coords.nb$e5.nb<-fitted(flic.meglm.nb1)[,5]


coordinates(coords)<-c("X", "Y")

bubble(coords.nb,"e1.nb")
bubble(coords.nb,"e2.nb")
bubble(coords.nb,"e3.nb")
bubble(coords.nb,"e4.nb")
bubble(coords.nb,"e5.nb")


eigen1<-fitted(flic.meglm.bin1)[,1]
eigen2<-fitted(flic.meglm.bin1)[,2]
coords.bin$e1.bin<-eigen1
coords.bin$e2.bin<-eigen2
coords.bin$e3.bin<-fitted(flic.meglm.bin1)[,3]
coords.bin$e4.bin<-fitted(flic.meglm.bin1)[,4]
coords.bin$e5.bin<-fitted(flic.meglm.bin1)[,5]
coords.bin$e6.bin<-fitted(flic.meglm.bin1)[,6]


coordinates(coords)<-c("X", "Y")

bubble(coords.bin,"e1.bin")
bubble(coords.bin,"e2.bin")
bubble(coords.bin,"e3.bin")
bubble(coords.bin,"e4.bin")
bubble(coords.bin,"e5.bin")
bubble(coords.bin,"e6.bin")



##############################
###Fit ME-GLMs
##############################
fittedME.bin.20k<- fitted(flic.meglm.bin1)
fittedME.nb.20k<- fitted(flic.meglm.nb1)

f.bin2<-formula(flickrbin~survey+fittedME.bin.20k,weights=popcount)
f.nb2<-formula(flickr~survey+fittedME.nb.20k,weights=popcount)

f.bin3<-formula(flickrbin~survey+fittedME.bin.20k[,1],weights=popcount)
f.nb3<-formula(flickr~survey+fittedME.nb.20k[,1],weights=popcount)

f.bin4<-formula(flickrbin~survey+fittedME.bin.20k[,1:2],weights=popcount)
f.nb4<-formula(flickr~survey+fittedME.nb.20k[,1:2],weights=popcount)

f.bin5<-formula(flickrbin~survey+fittedME.bin.20k[,1:3],weights=popcount)
f.nb5<-formula(flickr~survey+fittedME.nb.20k[,1:3],weights=popcount)

f.bin6<-formula(flickrbin~survey+fittedME.bin.20k[,1:6],weights=popcount)
f.nb6<-formula(flickr~survey+fittedME.nb.20k[,1:4],weights=popcount)



bin2<-glm(f.bin2,data=fsp20km,family="binomial")
nb2<-glm.nb(f.nb2,data=subs)
bin3<-glm(f.bin3,data=fsp20km,family="binomial")
nb3<-glm.nb(f.nb3,data=subs)
bin4<-glm(f.bin4,data=fsp20km,family="binomial")
nb4<-glm.nb(f.nb4,data=subs)
bin5<-glm(f.bin5,data=fsp20km,family="binomial")
nb5<-glm.nb(f.nb5,data=subs)
bin6<-glm(f.bin6,data=fsp20km,family="binomial")
nb6<-glm.nb(f.nb6,data=subs)



AIC(bin1,bin2,bin3,bin4,bin5,bin6,nb1,nb2,nb3,nb4,nb5,nb6)


#model validation
par(mfrow=c(2,2))
plot(bin2)

par(mfrow=c(2,2))
plot(nb6)


########predictions

####predictions negative binomial
unlist.fittedME.nb.20k<-unlist(fittedME.nb.20k[])

unlist.fittedME.nb.20k<-as.data.frame(fittedME.nb.20k[])

subs.20.2<-cbind(subs,unlist.fittedME.nb.20k)

f.nb2.2<-formula(flickr~survey+vec2+vec5+vec12+vec17+vec26, weights=popcount)

nb2.2<-glm.nb(f.nb2.2,data=subs.20.2)

pred.20k.nb<-visreg(nb2.2,"survey",scale="response",plot=F)


####predictions binomial 
unlist.fittedME.bin.20k<-unlist(fittedME.bin.20k[])

unlist.fittedME.bin.20k<-as.data.frame(fittedME.bin.20k[])

fsp20km.2<-cbind(fsp20km,unlist.fittedME.bin.20k)

f.bin2.2<-formula(flickrbin~survey+vec2+vec3+vec4+vec29+vec61+vec55+vec27, weights=popcount)

bin2.2<-glm(f.bin2.2,data=fsp20km.2,family=binomial)


pred.20k.bin<-visreg(bin2.2,"survey",scale="response",plot=F)


#####################################################
############validation 10km
#####################################################
fs10km<-read.table("flickr_survey_pop10KM.txt",header=T)
fs10km$flickrbin<-NULL
for(i in 1:length(fs10km$flickr)){
   ifelse(fs10km$flickr[i]>=1,fs10km$flickrbin[i]<-1,fs10km$flickrbin[i]<-0)}

fs10km$flickrbin<-as.integer(fs10km$flickrbin)

subs.10<-fs10km[fs10km$flickr!=0,]

coordinates(fs10km)<-c("X", "Y")
coordinates(subs.10)<-c("X", "Y")

################################
##Spatial eigenvector mapping
################################

coords.bin<-fs10km[,1:2]
coords.nb<-subs.10[,1:2]
neighbour.bin<-tri2nb(coords.bin)
neighbour.nb<-tri2nb(coords.nb)

nb.bin.list<-nb2listw(neighbour.bin)
nb.nb.list<-nb2listw(neighbour.nb)

f1.bin<-formula(flickrbin~survey,weights=popcount)
f1.nb<-formula(flickr~survey,weights=popcount)


flic.meglm.nb.10k<-ME(f1.nb,data=subs.10,listw=nb.nb.list
                   ,family=negative.binomial(theta= 9))


flic.meglm.bin.10k<-ME(f1.bin,data=fs10km,listw=nb.bin.list,
               family=binomial)



coords.bin$e1<-fitted(flic.meglm.bin)[,1]
coords.bin$e2<-fitted(flic.meglm.bin)[,2]
coords.bin$e3<-fitted(flic.meglm.bin)[,3]


coordinates(coords.bin)<-c("X", "Y")

bubble(coords.bin,"e1")
bubble(coords.bin,"e2")
bubble(coords.bin,"e3")


coords.nb$e1<-fitted(flic.meglm.nb)[,1]

coordinates(coords.nb)<-c("X", "Y")

bubble(coords.nb,"e1")



##############################
###Fit ME-GLMs
##############################

fittedME.bin.10k<- fitted(flic.meglm.bin.10k)
fittedME.nb.10k<- fitted(flic.meglm.nb.10k)


f.bin1<-formula(flickrbin~survey, weights=popcount)
f.nb1<-formula(flickr~survey,weights=popcount)


f.bin.10k.2<-formula(flickrbin~survey+fittedME.bin.10k, weights=popcount)
f.nb.10k.2<-formula(flickr~survey+fittedME.nb.10k, weights=popcount)

f.bin3<-formula(flickrbin~survey+fittedME.bin.10k[,1], weights=popcount)
f.nb3<-formula(flickr~survey+fittedME.nb.10k[,1], weights=popcount)

f.bin4<-formula(flickrbin~survey+fittedME.bin.10k[,1:2], weights=popcount)
f.nb4<- formula(flickr~survey+fittedME.nb.10k[,1:2], weights=popcount)

f.bin5<-formula(flickrbin~survey+fittedME.bin.10k[,1:3], weights=popcount)
f.nb5<-formula(flickr~survey+fittedME.nb.10k[,1:3], weights=popcount)

f.bin6<-formula(flickrbin~survey+fittedME.bin.10k[,1:4], weights=popcount)
f.nb6<-formula(flickr~survey+fittedME.nb.10k[,1:4], weights=popcount)

f.bin7<-formula(flickrbin~survey+fittedME.bin.10k[,1:5], weights=popcount)

f.bin8<-formula(flickrbin~survey+fittedME.bin.10k[,1:10], weights=popcount)

f.bin9<-formula(flickrbin~survey+fittedME.bin.10k[,1:20], weights=popcount)




bin1<-glm(f.bin1,data=fs10km,family=binomial)
nb1<-glm.nb(f.nb1,data=subs.10)

bin.10k.2<-glm(f.bin.10k.2,data=fs10km,family=binomial)
nb.10k.2<-glm.nb(f.nb.10k.2,data=subs.10)

bin3<-glm(f.bin3,data=fs10km,family=binomial)
nb3<-glm.nb(f.nb3,data=subs.10)

bin4<-glm(f.bin4,data=fs10km,family=binomial)
nb4<-glm.nb(f.nb4,data=subs.10)

bin5<-glm(f.bin5,data=fs10km,family=binomial)
nb5<-glm.nb(f.nb5,data=subs.10)

bin6<-glm(f.bin6,data=fs10km,family=binomial)
nb6<-glm.nb(f.nb6,data=subs.10)

bin7<-glm(f.bin7,data=fs10km,family=binomial)

bin8<-glm(f.bin8,data=fs10km,family=binomial)

bin9<-glm(f.bin9,data=fs10km,family=binomial)

AIC(bin1,bin.10k.2,bin3,bin4,bin5,bin6,bin7,bin8,nb1,nb.10k.2,nb3,nb4,nb5,nb6)

#model validation

par(mfrow=c(2,2))
plot(nb.10k.2)

par(mfrow=c(2,2))
plot(bin.10k.2)

########predictions 

####predictions negative binomial
unlist.fittedME.nb.10k<-unlist(fittedME.nb.10k[])

unlist.fittedME.nb.10k<-as.data.frame(fittedME.nb.10k[])

subs.10.2<-cbind(subs.10,unlist.fittedME.nb.10k)

f.nb.10k.2.2<-formula(flickr~survey+vec2+vec34+vec14+vec83+vec39, weights=popcount)

nb.10k.2.2<-glm.nb(f.nb.10k.2.2,data=subs.10.2)

pred.10k.nb<-visreg(nb.10k.2.2,"survey",scale="response",plot=F)
 

####predictions binomial
unlist.fittedME.bin.10k<-unlist(fittedME.bin.10k[])

unlist.fittedME.bin.10k<-as.data.frame(fittedME.bin.10k[])

fs10km.2<-cbind(fs10km,unlist.fittedME.bin.10k)

f.bin.10k.2.2<-formula(flickrbin~survey+vec2+vec3+vec4+vec46+vec1+vec20+vec41+vec28+vec103+vec13+vec18+vec85+vec47+vec23+vec43+vec138+vec176+vec273+vec37+vec11+vec17+vec68, weights=popcount)

bin.10k.2.2<-glm(f.bin.10k.2.2,data=fs10km.2,family=binomial)


pred.10k.bin<-visreg(bin.10k.2.2,"survey",scale="response",plot=F)

#######################################
##########validation 5km
#######################################

fs5km<-read.table("flickr_survey_pop5KM.txt",header=T)
fs5km$flickrbin<-NULL
for(i in 1:length(fs5km$flickr)){
   ifelse(fs5km$flickr[i]>=1,fs5km$flickrbin[i]<-1,fs5km$flickrbin[i]<-0)}

fs5km$flickrbin<-as.integer(fs5km$flickrbin)

subs.5<-fs5km[fs5km$flickr!=0,]



coordinates(fs5km)<-c("X", "Y")
coordinates(subs.5)<-c("X", "Y")

################################
##Spatial eigenvector mapping
################################
coords.bin<-fs5km[,1:2]
coords.nb<-subs.5[,1:2]
neighbour.bin<-tri2nb(coords.bin)
neighbour.nb<-tri2nb(coords.nb)

nb.bin.list<-nb2listw(neighbour.bin)
nb.nb.list<-nb2listw(neighbour.nb)

f1.bin<-formula(flickrbin~survey,weights=popcount)
f1.nb<-formula(flickr~survey,,weights=popcount)


flic.meglm.nb.5k<-ME(f1.nb,data=subs.5,listw=nb.nb.list
                   ,family=negative.binomial(theta= 9))


flic.meglm.bin.5k<-ME(f1.bin,data=fs5km,listw=nb.bin.list,
               family=binomial)


coords.bin$e1<-fitted(flic.meglm.bin.5k)[,1]
coords.bin$e2<-fitted(flic.meglm.bin.5k)[,2]
coords.bin$e3<-fitted(flic.meglm.bin.5k)[,3]
coords.bin$e10<-fitted(flic.meglm.bin.5k)[,10]


coordinates(coords.bin)<-c("X", "Y")

e1<-bubble(coords.bin,"e1")
e2<-bubble(coords.bin,"e2")
e3<-bubble(coords.bin,"e3")
e10<-bubble(coords.bin,"e10")


##############################
###Fit ME-GLMs
##############################
fittedME.bin.5k<- fitted(flic.meglm.bin.5k)
fittedME.nb.5k<- fitted(flic.meglm.nb.5k)

f.bin1<-formula(flickrbin~survey, weights=popcount)
f.nb1<-formula(flickr~survey,weights=popcount)

f.bin.5k.2<-formula(flickrbin~survey+fittedME.bin.5k[,1:60], weights=popcount)
f.nb.5k.2<-formula(flickr~survey+fittedME.nb.5k, weights=popcount)

f.bin3<-formula(flickrbin~survey+fittedME.bin.5k[,1], weights=popcount)
f.nb3<-formula(flickr~survey+fittedME.nb.5k[,1], weights=popcount)

f.bin4<-formula(flickrbin~survey+fittedME.bin.5k[,1:2], weights=popcount)
f.nb4<-formula(flickr~survey+fittedME.nb.5k[,1:2], weights=popcount)

f.bin5<-formula(flickrbin~survey+fittedME.bin.5k[,1:3], weights=popcount)
f.nb5<-formula(flickr~survey+fittedME.nb.5k[,1:3], weights=popcount)

f.bin6<-formula(flickrbin~survey+fittedME.bin.5k[,1:10], weights=popcount)
f.nb6<-formula(flickr~survey+fittedME.nb.5k[,1:4], weights=popcount)

f.bin7<-formula(flickrbin~survey+fittedME.bin.5k[,1:20], weights=popcount)
f.nb7<-formula(flickr~survey+fittedME.nb.5k[,1:5], weights=popcount)

f.bin8<-formula(flickrbin~survey+fittedME.bin.5k[,1:30], weights=popcount)
f.nb8<-formula(flickr~survey+fittedME.nb.5k[,1:10], weights=popcount)

f.bin9<-formula(flickrbin~survey+fittedME.bin.5k[,1:40], weights=popcount)
f.nb9<-formula(flickr~survey+fittedME.nb.5k[,1:13], weights=popcount)

f.bin10<-formula(flickrbin~survey+fittedME.bin.5k[,1:50], weights=popcount)



bin1<-glm(f.bin1,data=fs5km,family=binomial)
nb1<-glm.nb(f.nb1,data=subs.5)

bin.5k.2<-glm(f.bin.5k.2,data=fs5km,family=binomial)
nb.5k.2<-glm.nb(f.nb.5k.2,data=subs.5)


bin3<-glm(f.bin3,data=fs5km,family=binomial)
nb3<-glm.nb(f.nb3,data=subs.5)

bin4<-glm(f.bin4,data=fs5km,family=binomial)
nb4<-glm.nb(f.nb4,data=subs.5)

bin5<-glm(f.bin5,data=fs5km,family=binomial)
nb5<-glm.nb(f.nb5,data=subs.5)

bin6<-glm(f.bin6,data=fs5km,family=binomial)
nb6<-glm.nb(f.nb6,data=subs.5)

bin7<-glm(f.bin7,data=fs5km,family=binomial)
nb7<-glm.nb(f.nb7,data=subs.5)

bin8<-glm(f.bin8,data=fs5km,family=binomial)
nb8<-glm.nb(f.nb8,data=subs.5)

bin9<-glm(f.bin9,data=fs5km,family=binomial)
nb9<-glm.nb(f.nb9,data=subs.5)

bin10<-glm(f.bin10,data=fs5km,family=binomial)


AIC(bin1,bin.5k.2,bin3,bin4,bin5,bin6,bin7,bin8,bin9,bin10,nb1,nb.5k.2,nb3,nb4,nb5,nb6,nb7,nb8,nb9)

#model validation

par(mfrow=c(2,2))
plot(bin.5k.2)

par(mfrow=c(2,2))
plot(nb2)



########predictions

####predictions negative binomial
unlist.fittedME.nb.5k<-unlist(fittedME.nb.5k[])

unlist.fittedME.nb.5k<-as.data.frame(fittedME.nb.5k[])

subs.5.2<-cbind(subs.5,unlist.fittedME.nb.5k)

f.nb.5k.2.2<-formula(flickr~survey+vec2+vec25+vec37+vec3+vec10+vec77+vec62+vec12+vec43+vec70+vec115+vec491+vec131+vec99, weights=popcount)

nb.5k.2.2<-glm.nb(f.nb.5k.2.2,data=subs.5.2)

pred.5k.nb<-visreg(nb.5k.2.2,"survey",scale="response",plot=F)

####plotting binomial
unlist.fittedME.bin.5k<-unlist(fittedME.bin.5k[,1:60])

unlist.fittedME.bin.5k<-as.data.frame(fittedME.bin.5k[,1:60])

fs5km.2<-cbind(fs5km,unlist.fittedME.bin.5k)

f.bin.5k.2.2<-formula(flickrbin~survey+vec2+vec6+vec7+vec14+vec1+vec77+vec62+vec24+vec17+vec199+vec81+vec101+vec11+vec614+vec214+vec20+vec63+vec324+vec148+vec286+vec78+vec28+vec44+vec410+vec493+vec651+vec262+vec354+vec281+vec456+vec259+vec10+vec37+vec12+vec26+vec76+vec351+vec592+vec8+vec104+vec47+vec409+vec15+vec569+vec693+vec16+vec22+vec1061+vec222+vec582+vec973+vec888+vec415+vec524+vec50+vec629+vec345+vec119+vec231+vec87, weights=popcount)

bin.5k.2.2<-glm(f.bin.5k.2.2,data=fs5km.2,family=binomial)


pred.5k.bin<-visreg(bin.5k.2.2,"survey",scale="response",plot=F)





