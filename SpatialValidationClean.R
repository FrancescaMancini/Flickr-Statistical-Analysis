library(statmod)
library(gstat)
library(sp)
require(spdep)
require(maptools)
library(pscl)
library(visreg)


setwd("H:\\National Scale Data\\Flickr\\Spatial validation")

###############################################
#########validation 20 Km
###############################################
fsp20km<-read.table("datasets\\flickr_survey_pop20KM.txt",header=T)

fsp20km$flickrbin<-NULL
for(i in 1:length(fsp20km$flickr)){
   ifelse(fsp20km$flickr[i]>=1,fsp20km$flickrbin[i]<-1,fsp20km$flickrbin[i]<-0)}

f1.1<-formula(flickrbin~survey,weights=popcount)
f1<-formula(flickr~survey,weights=popcount)


bin1<-glm(f1.1,data=fsp20km,family=binomial)
plot(residuals(bin1)~fitted(bin1))

subs<-fsp20km[fsp20km$flickr!=0,]
pois1<-glm(f1,data=subs,family="poisson")
pois2<-glm(f1,data=subs,family="quasipoisson")
nb1<-glm.nb(f1,data=subs)

AIC(pois1, pois2, nb1)
#      df      AIC
#pois1  2 6059.996
#pois2  2       NA
#nb1    3 1425.829



par(mfrow=c(2,2))
plot(pois2)
par(mfrow=c(2,2))
plot(nb1)

coordinates(fsp20km)<-c("X", "Y")
coordinates(subs)<-c("X", "Y")

Vario.bin1<-variogram(resid(bin1)~1,data=fsp20km)
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

coords.bin<-fsp20km[,1:2]
coords.nb<-subs[,1:2]
neighbour.bin<-tri2nb(coords.bin)
neighbour.nb<-tri2nb(coords.nb)

nb.bin.list<-nb2listw(neighbour.bin)
nb.nb.list<-nb2listw(neighbour.nb)




flic.meglm.nb1<-ME(f1,data=subs,listw=nb.nb.list,nsim=100,alpha=0.05,
                   ,family=negative.binomial(theta= 0.1302))
flic.meglm.nb1
#  Eigenvector ZI     pr(ZI)
#0          NA NA 0.00990099
#1           2 NA 0.00990099
#2           5 NA 0.01980198
#3          12 NA 0.00990099
#4          17 NA 0.01980198
#5          26 NA 0.06930693








flic.meglm.bin1<-ME(f1.1,data=fsp20km,listw=nb.bin.list,family=binomial)
flic.meglm.bin1
#  Eigenvector ZI pr(ZI)
#0          NA NA   0.01
#1           2 NA   0.01
#2           3 NA   0.01
#3           4 NA   0.01
#4          29 NA   0.01
#5          61 NA   0.02
#6          55 NA   0.04
#7          27 NA   0.09










eigen1<-fitted(flic.meglm.nb1)[,1]
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




###models
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
#     df       AIC
#bin1  2  406.3614
#bin2  9  307.4662
#bin3  3  369.8716
#bin4  4  345.8928
#bin5  5  327.1081
#bin6  8  310.5762
#nb1   3 1425.8286
#nb2   8 1335.4566
#nb3   4 1359.5855
#nb4   5 1355.1223
#nb5   6 1342.1004
#nb6   7 1337.6112






summary(bin2)

#Call:
#glm(formula = f.bin2, family = "binomial", data = fsp20km)
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.1709  -0.5207   0.0333   0.6325   3.2060  
#
#Coefficients:
#                             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                  -3.89927    0.44002  -8.861  < 2e-16 ***
#survey                        0.37983    0.03991   9.518  < 2e-16 ***
#fitted(flic.meglm.bin1)vec2  26.30831    4.07776   6.452 1.11e-10 ***
#fitted(flic.meglm.bin1)vec3  16.36755    3.33662   4.905 9.32e-07 ***
#fitted(flic.meglm.bin1)vec4  15.26319    3.33006   4.583 4.57e-06 ***
#fitted(flic.meglm.bin1)vec29 -6.36467    2.88312  -2.208  0.02727 *  
#fitted(flic.meglm.bin1)vec61 -9.58926    3.15315  -3.041  0.00236 ** 
#fitted(flic.meglm.bin1)vec55  8.44357    3.15011   2.680  0.00735 ** 
#fitted(flic.meglm.bin1)vec27  7.09296    3.18223   2.229  0.02582 *  
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 525.38  on 378  degrees of freedom
#Residual deviance: 289.47  on 370  degrees of freedom
#AIC: 307.47
#
#Number of Fisher Scoring iterations: 6






summary(nb2)
#Call:
#glm.nb(formula = f.nb6, data = subs, init.theta = 1.088892902,
#    link = log)
#
#Deviance Residuals:
#    Min       1Q   Median       3Q      Max
#-2.2810  -1.0095  -0.4188   0.1964   3.8595
#
#Coefficients:
#                                    Estimate Std. Error z value Pr(>|z|)
#(Intercept)                          0.98891    0.19272   5.131 2.88e-07 ***
#survey                               0.09910    0.01227   8.078 6.58e-16 ***
#fitted(flic.meglm.nb1)[, 1:5]vec2  -10.65733    1.27149  -8.382  < 2e-16 ***
#fitted(flic.meglm.nb1)[, 1:5]vec5   -2.81720    1.05464  -2.671 0.007557 **
#fitted(flic.meglm.nb1)[, 1:5]vec12   3.80456    1.03171   3.688 0.000226 ***
#fitted(flic.meglm.nb1)[, 1:5]vec17   2.51987    1.01301   2.488 0.012864 *
#fitted(flic.meglm.nb1)[, 1:5]vec26  -2.20966    1.03550  -2.134 0.032851 *
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Negative Binomial(1.0889) family taken to be 1)
#
#    Null deviance: 338.39  on 190  degrees of freedom
#Residual deviance: 203.32  on 184  degrees of freedom
#AIC: 1335.5
#
#Number of Fisher Scoring iterations: 1
#
#
#              Theta:  1.089
#          Std. Err.:  0.114
#
# 2 x log-likelihood:  -1319.457



#diagnostic binomial
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


####plotting binomial
unlist.fittedME.bin.20k<-unlist(fittedME.bin.20k[])

unlist.fittedME.bin.20k<-as.data.frame(fittedME.bin.20k[])

fsp20km.2<-cbind(fsp20km,unlist.fittedME.bin.20k)

f.bin2.2<-formula(flickrbin~survey+vec2+vec3+vec4+vec29+vec61+vec55+vec27, weights=popcount)

bin2.2<-glm(f.bin2.2,data=fsp20km.2,family=binomial)


pred.20k.bin<-visreg(bin2.2,"survey",scale="response",plot=F)


#####################################################
############validation 10km
#####################################################

setwd("H:\\National Scale Data\\Flickr\\Spatial validation")

fs10km<-read.table("datasets\\flickr_survey_pop10KM.txt",header=T)
fs10km$flickrbin<-NULL
for(i in 1:length(fs10km$flickr)){
   ifelse(fs10km$flickr[i]>=1,fs10km$flickrbin[i]<-1,fs10km$flickrbin[i]<-0)}

fs10km$flickrbin<-as.integer(fs10km$flickrbin)

subs.10<-fs10km[fs10km$flickr!=0,]




coordinates(fs10km)<-c("X", "Y")
coordinates(subs.10)<-c("X", "Y")


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
flic.meglm.nb.10k
#  Eigenvector ZI pr(ZI)
#0          NA NA   0.01
#1           2 NA   0.01
#2          34 NA   0.01
#3          14 NA   0.01
#4          83 NA   0.02
#5          39 NA   0.16







flic.meglm.bin.10k<-ME(f1.bin,data=fs10km,listw=nb.bin.list,
               family=binomial)

flic.meglm.bin.10k
#   Eigenvector ZI pr(ZI)
#0           NA NA   0.01
#1            2 NA   0.01
#2            3 NA   0.01
#3            4 NA   0.01
#4           46 NA   0.01
#5            1 NA   0.01
#6           20 NA   0.01
#7           41 NA   0.01
#8           28 NA   0.01
#9          103 NA   0.01
#10          13 NA   0.01
#11          18 NA   0.01
#12          85 NA   0.01
#13          47 NA   0.01
#14          23 NA   0.01
#15          43 NA   0.01
#16         138 NA   0.01
#17         176 NA   0.01
#18         273 NA   0.01
#19          37 NA   0.01
#20          11 NA   0.04
#21          17 NA   0.04
#22          68 NA   0.09



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



###models

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
#     df      AIC
#bin1  2 1302.148
#bin2 24 1029.976
#bin3  3 1230.942
#bin4  4 1189.224
#bin5  5 1165.782
#bin6  6 1147.383
#bin7  7 1140.730
#bin8 12 1097.244
#nb1   3 2373.359
#nb2   8 2211.134
#nb3   4 2256.651
#nb4   5 2251.195
#nb5   6 2238.964
#nb6   7 2222.940







summary(bin.10k.2)

#
#Call:
#glm(formula = f.bin2, family = binomial, data = fs10km)
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-3.0516  -0.6140  -0.2472   0.4729   3.2655  
#
#Coefficients:
#                        Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             -4.95090    0.30248 -16.367  < 2e-16 ***
#survey                   0.36655    0.02485  14.752  < 2e-16 ***
#fittedME.bin.10kvec2   -42.64232    4.38801  -9.718  < 2e-16 ***
#fittedME.bin.10kvec3    32.16171    3.95950   8.123 4.56e-16 ***
#fittedME.bin.10kvec4   -14.35413    3.28270  -4.373 1.23e-05 ***
#fittedME.bin.10kvec46   14.74353    3.26216   4.520 6.20e-06 ***
#fittedME.bin.10kvec1    13.84926    2.97489   4.655 3.23e-06 ***
#fittedME.bin.10kvec20  -11.63398    2.95864  -3.932 8.42e-05 ***
#fittedME.bin.10kvec41   12.22459    3.17026   3.856 0.000115 ***
#fittedME.bin.10kvec28  -11.82224    2.79547  -4.229 2.35e-05 ***
#fittedME.bin.10kvec103  -9.20025    3.01684  -3.050 0.002291 ** 
#fittedME.bin.10kvec13  -17.22035    3.54200  -4.862 1.16e-06 ***
#fittedME.bin.10kvec18  -11.53627    3.15694  -3.654 0.000258 ***
#fittedME.bin.10kvec85   11.89622    3.10401   3.833 0.000127 ***
#fittedME.bin.10kvec47    7.52749    3.05446   2.464 0.013723 *  
#fittedME.bin.10kvec23   -9.75823    2.95867  -3.298 0.000973 ***
#fittedME.bin.10kvec43    7.36463    3.13325   2.350 0.018749 *  
#fittedME.bin.10kvec138  -8.14564    3.02359  -2.694 0.007059 ** 
#fittedME.bin.10kvec176   8.88344    2.85353   3.113 0.001851 ** 
#fittedME.bin.10kvec273  -9.66358    2.92647  -3.302 0.000960 ***
#fittedME.bin.10kvec37    7.08694    2.73677   2.590 0.009611 ** 
#fittedME.bin.10kvec11  -10.07786    3.14631  -3.203 0.001360 ** 
#fittedME.bin.10kvec17    6.00836    2.86316   2.099 0.035861 *  
#fittedME.bin.10kvec68   -7.87265    3.01662  -2.610 0.009061 ** 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 1566.37  on 1285  degrees of freedom
#Residual deviance:  981.98  on 1262  degrees of freedom
#AIC: 1030
#
#Number of Fisher Scoring iterations: 6

#






par(mfrow=c(2,2))
plot(bin.10k.2)



summary(nb.10k.2)

#Call:
#glm.nb(formula = f.nb2, data = subs.10, init.theta = 1.136204387,
#    link = log)
#
#Deviance Residuals:
#    Min       1Q   Median       3Q      Max
#-1.8589  -0.9684  -0.4926   0.1604   3.7663
#
#Coefficients:
#                            Estimate Std. Error z value Pr(>|z|)
#(Intercept)                 1.389409   0.124404  11.168  < 2e-16 ***
#survey                      0.026685   0.007742   3.447 0.000567 ***
#fitted(flic.meglm.nb)vec2  10.820955   1.124192   9.626  < 2e-16 ***
#fitted(flic.meglm.nb)vec34 -1.379581   1.017163  -1.356 0.175003
#fitted(flic.meglm.nb)vec14  4.304955   1.016345   4.236 2.28e-05 ***
#fitted(flic.meglm.nb)vec83  4.307995   1.031975   4.175 2.99e-05 ***
#fitted(flic.meglm.nb)vec39 -3.873861   1.032568  -3.752 0.000176 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Negative Binomial(1.1362) family taken to be 1)
#
#    Null deviance: 599.55  on 382  degrees of freedom
#Residual deviance: 387.43  on 376  degrees of freedom
#AIC: 2211.1
#
#Number of Fisher Scoring iterations: 1
#
#
#              Theta:  1.1362
#          Std. Err.:  0.0903
#
# 2 x log-likelihood:  -2195.1340

par(mfrow=c(2,2))
plot(nb.10k.2)


########predictions effects

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

setwd("H:\\National Scale Data\\Flickr\\Spatial validation")

fs5km<-read.table("datasets\\flickr_survey_pop5KM.txt",header=T)
fs5km$flickrbin<-NULL
for(i in 1:length(fs5km$flickr)){
   ifelse(fs5km$flickr[i]>=1,fs5km$flickrbin[i]<-1,fs5km$flickrbin[i]<-0)}

fs5km$flickrbin<-as.integer(fs5km$flickrbin)

subs.5<-fs5km[fs5km$flickr!=0,]



coordinates(fs5km)<-c("X", "Y")
coordinates(subs.5)<-c("X", "Y")


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

flic.meglm.nb.5k
#   Eigenvector ZI pr(ZI)
#0           NA NA   0.01
#1            2 NA   0.01
#2           25 NA   0.01
#3           37 NA   0.01
#4            3 NA   0.01
#5           10 NA   0.01
#6           77 NA   0.01
#7           62 NA   0.01
#8           12 NA   0.01
#9           43 NA   0.01
#10          70 NA   0.01
#11         115 NA   0.02
#12         491 NA   0.02
#13         131 NA   0.03
#14          99 NA   0.10




flic.meglm.bin.5k<-ME(f1.bin,data=fs5km,listw=nb.bin.list,
               family=binomial)


flic.meglm.bin.5k
#   Eigenvector ZI pr(ZI)
#0           NA NA   0.01
#1            2 NA   0.01
#2            6 NA   0.01
#3            7 NA   0.01
#4           14 NA   0.01
#5            1 NA   0.01
#6           77 NA   0.01
#7           62 NA   0.01
#8           24 NA   0.01
#9           17 NA   0.01
#10         199 NA   0.01
#11          81 NA   0.01
#12         101 NA   0.01
#13          11 NA   0.01
#14         614 NA   0.01
#15         214 NA   0.01
#16          20 NA   0.01
#17          63 NA   0.01
#18         324 NA   0.01
#19         148 NA   0.01
#20         286 NA   0.01
#21          78 NA   0.01
#22          28 NA   0.01
#23          44 NA   0.01
#24         410 NA   0.01
#25         493 NA   0.01
#26         651 NA   0.01
#27         262 NA   0.01
#28         354 NA   0.01
#29         281 NA   0.01
#30         456 NA   0.01
#31         259 NA   0.01
#32          10 NA   0.01
#33          37 NA   0.01
#34          12 NA   0.01
#35          26 NA   0.01
#36          76 NA   0.01
#37         351 NA   0.01
#38         592 NA   0.01
#39           8 NA   0.01
#40         104 NA   0.01
#41          47 NA   0.01
#42         409 NA   0.01
#43          15 NA   0.01
#44         569 NA   0.01
#45         693 NA   0.01
#46          16 NA   0.01
#47          22 NA   0.01
#48        1061 NA   0.01
#49         222 NA   0.01
#50         582 NA   0.01
#51         973 NA   0.01
#52         888 NA   0.01
#53         415 NA   0.01
#54         524 NA   0.01
#55          50 NA   0.01
#56         629 NA   0.02
#57         345 NA   0.01
#58         119 NA   0.02
#59         231 NA   0.05
#60          87 NA   0.08







coords.bin$e1<-fitted(flic.meglm.bin.5k)[,1]
coords.bin$e2<-fitted(flic.meglm.bin.5k)[,2]
coords.bin$e3<-fitted(flic.meglm.bin.5k)[,3]
coords.bin$e10<-fitted(flic.meglm.bin.5k)[,10]


coordinates(coords.bin)<-c("X", "Y")

e1<-bubble(coords.bin,"e1")

tiff(filename="H://National Scale Data//Flickr//Spatial validation//e15kbin.tiff",width=2000,height=2000,res=300)
e1
dev.off()

e2<-bubble(coords.bin,"e2")

tiff(filename="H://National Scale Data//Flickr//Spatial validation//e25kbin.tiff",width=2000,height=2000,res=300)
e2
dev.off()

e3<-bubble(coords.bin,"e3")

tiff(filename="H://National Scale Data//Flickr//Spatial validation//e35kbin.tiff",width=2000,height=2000,res=300)
e3
dev.off()

e10<-bubble(coords.bin,"e10")

tiff(filename="H://National Scale Data//Flickr//Spatial validation//e105kbin.tiff",width=2000,height=2000,res=300)
e10
dev.off()


coords.nb$e1<-fitted(flic.meglm.nb.5k)[,1]

coordinates(coords.nb)<-c("X", "Y")

bubble(coords.nb,"e1")



###models

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
#      df      AIC
#bin1   2 3287.433
#bin2  62 2545.689
#bin3   3 3186.767
#bin4   4 3123.494
#bin5   5 3065.143
#bin6  12 2917.566
#bin7  22 2807.090
#bin8  32 2716.917
#bin9  42 2626.804
#bin10 52 2576.815
#nb1    3 3286.583
#nb2   17 3023.205
#nb3    4 3135.286
#nb4    5 3112.887
#nb5    6 3092.472
#nb6    7 3061.047
#nb7    8 3058.609
#nb8   13 3032.696
#nb9   16 3026.726






summary(bin.5k.2)


#Call:
#glm(formula = f.bin2, family = binomial, data = fs5km)
#
#Deviance Residuals:
#    Min       1Q   Median       3Q      Max
#-2.1757  -0.4394  -0.2317  -0.0810   3.5790
#
#Coefficients:
#                               Estimate Std. Error z value Pr(>|z|)
#(Intercept)                    -5.45420    0.19311 -28.243  < 2e-16 ***
#survey                          0.27739    0.01316  21.071  < 2e-16 ***
#fitted(flic.meglm.bin)vec2     62.48448    5.44081  11.484  < 2e-16 ***
#fitted(flic.meglm.bin)vec6     24.94800    4.32016   5.775 7.71e-09 ***
#fitted(flic.meglm.bin)vec7    -25.30433    3.91858  -6.458 1.06e-10 ***
#fitted(flic.meglm.bin)vec14   -12.07561    4.21158  -2.867 0.004141 **
#fitted(flic.meglm.bin)vec1     44.38238    4.52444   9.809  < 2e-16 ***
#fitted(flic.meglm.bin)vec77    18.03989    4.04376   4.461 8.15e-06 ***
#fitted(flic.meglm.bin)vec62   -12.67431    3.89501  -3.254 0.001138 **
#fitted(flic.meglm.bin)vec24    -0.59049    3.86558  -0.153 0.878591
#fitted(flic.meglm.bin)vec17    32.65214    4.52626   7.214 5.44e-13 ***
#fitted(flic.meglm.bin)vec199  -16.62448    3.90307  -4.259 2.05e-05 ***
#fitted(flic.meglm.bin)vec81    -8.74064    3.57733  -2.443 0.014552 *
#fitted(flic.meglm.bin)vec101  -11.32749    3.94629  -2.870 0.004099 **
#fitted(flic.meglm.bin)vec11     8.70684    4.36862   1.993 0.046257 *
#fitted(flic.meglm.bin)vec614  -19.47222    3.95582  -4.922 8.55e-07 ***
#fitted(flic.meglm.bin)vec214  -12.50942    3.63154  -3.445 0.000572 ***
#fitted(flic.meglm.bin)vec20    29.37880    4.50710   6.518 7.11e-11 ***
#fitted(flic.meglm.bin)vec63    14.40393    3.70302   3.890 0.000100 ***
#fitted(flic.meglm.bin)vec324   -7.37863    3.82160  -1.931 0.053511 .
#fitted(flic.meglm.bin)vec148  -12.25976    3.47532  -3.528 0.000419 ***
#fitted(flic.meglm.bin)vec286  -10.62694    3.86740  -2.748 0.005999 **
#fitted(flic.meglm.bin)vec78   -12.67770    3.98257  -3.183 0.001456 **
#fitted(flic.meglm.bin)vec28    27.42039    4.12942   6.640 3.13e-11 ***
#fitted(flic.meglm.bin)vec44   -19.94593    4.18845  -4.762 1.92e-06 ***
#fitted(flic.meglm.bin)vec410   10.36857    3.47242   2.986 0.002827 **
#fitted(flic.meglm.bin)vec493   12.04069    3.92368   3.069 0.002150 **
#fitted(flic.meglm.bin)vec651   12.36619    3.56805   3.466 0.000529 ***
#fitted(flic.meglm.bin)vec262  -11.41451    3.56760  -3.199 0.001377 **
#fitted(flic.meglm.bin)vec354   11.65717    3.54525   3.288 0.001009 **
#fitted(flic.meglm.bin)vec281    3.82499    3.60420   1.061 0.288572
#fitted(flic.meglm.bin)vec456  -11.30688    3.48630  -3.243 0.001182 **
#fitted(flic.meglm.bin)vec259    9.60841    3.59665   2.671 0.007552 **
#fitted(flic.meglm.bin)vec10   -22.45973    4.27777  -5.250 1.52e-07 ***
#fitted(flic.meglm.bin)vec37    19.38075    3.95939   4.895 9.84e-07 ***
#fitted(flic.meglm.bin)vec12   -25.64546    4.79745  -5.346 9.01e-08 ***
#fitted(flic.meglm.bin)vec26   -11.47182    4.57029  -2.510 0.012070 *
#fitted(flic.meglm.bin)vec76    -9.86400    3.51339  -2.808 0.004992 **
#fitted(flic.meglm.bin)vec351   12.48240    3.65228   3.418 0.000632 ***
#fitted(flic.meglm.bin)vec592   11.70436    3.59252   3.258 0.001122 **
#fitted(flic.meglm.bin)vec8      8.94162    4.58854   1.949 0.051333 .
#fitted(flic.meglm.bin)vec104    6.28249    3.62949   1.731 0.083460 .
#fitted(flic.meglm.bin)vec47   -12.99607    4.37303  -2.972 0.002960 **
#fitted(flic.meglm.bin)vec409    9.68815    3.61790   2.678 0.007410 **
#fitted(flic.meglm.bin)vec15    15.00581    4.40234   3.409 0.000653 ***
#fitted(flic.meglm.bin)vec569   -8.52942    3.46783  -2.460 0.013910 *
#fitted(flic.meglm.bin)vec693  -12.21980    3.65713  -3.341 0.000834 ***
#fitted(flic.meglm.bin)vec16    11.17509    4.10137   2.725 0.006436 **
#fitted(flic.meglm.bin)vec22    11.76720    4.16602   2.825 0.004734 **
#fitted(flic.meglm.bin)vec1061   9.92403    3.69548   2.685 0.007243 **
#fitted(flic.meglm.bin)vec222    7.51319    3.71873   2.020 0.043346 *
#fitted(flic.meglm.bin)vec582   -9.17446    3.63785  -2.522 0.011671 *
#fitted(flic.meglm.bin)vec973  -10.15494    3.58176  -2.835 0.004580 **
#fitted(flic.meglm.bin)vec888   -9.68083    3.65155  -2.651 0.008022 **
#fitted(flic.meglm.bin)vec415   -7.54768    3.45351  -2.186 0.028851 *
#fitted(flic.meglm.bin)vec524    8.54087    3.47351   2.459 0.013938 *
#fitted(flic.meglm.bin)vec50    -5.69285    4.07821  -1.396 0.162739
#fitted(flic.meglm.bin)vec629   -8.21480    3.51952  -2.334 0.019592 *
#fitted(flic.meglm.bin)vec345    7.98178    3.70733   2.153 0.031321 *
#fitted(flic.meglm.bin)vec119    6.85745    3.43475   1.996 0.045881 *
#fitted(flic.meglm.bin)vec231   -7.54397    3.50398  -2.153 0.031321 *
#fitted(flic.meglm.bin)vec87     8.42595    3.56852   2.361 0.018217 *
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#    Null deviance: 3615.4  on 4600  degrees of freedom
#Residual deviance: 2421.7  on 4539  degrees of freedom
#AIC: 2545.7
#
#Number of Fisher Scoring iterations: 7








par(mfrow=c(2,2))
plot(bin.5k.2)



summary(nb.5k.2)


#Call:
#glm.nb(formula = f.nb2, data = subs.5, init.theta = 1.456325882,
#    link = log)
#
#Deviance Residuals:
#    Min       1Q   Median       3Q      Max
#-2.0512  -0.8624  -0.4868   0.2098   4.2230
#
#Coefficients:
#                              Estimate Std. Error z value Pr(>|z|)
#(Intercept)                   1.158087   0.096354  12.019  < 2e-16 ***
#survey                        0.011021   0.006379   1.728  0.08405 .
#fitted(flic.meglm.nb)vec2   -11.639657   1.090169 -10.677  < 2e-16 ***
#fitted(flic.meglm.nb)vec25   -3.170729   0.963560  -3.291  0.00100 ***
#fitted(flic.meglm.nb)vec37   -4.388136   1.030603  -4.258 2.06e-05 ***
#fitted(flic.meglm.nb)vec3     6.037391   0.996167   6.061 1.36e-09 ***
#fitted(flic.meglm.nb)vec10   -1.642097   0.980277  -1.675  0.09391 .
#fitted(flic.meglm.nb)vec77   -0.494197   0.961550  -0.514  0.60728
#fitted(flic.meglm.nb)vec62   -2.121480   1.016209  -2.088  0.03683 *
#fitted(flic.meglm.nb)vec12    4.300794   1.012096   4.249 2.14e-05 ***
#fitted(flic.meglm.nb)vec43    1.085675   0.919991   1.180  0.23796
#fitted(flic.meglm.nb)vec70    3.319654   1.050121   3.161  0.00157 **
#fitted(flic.meglm.nb)vec115  -1.866049   0.983207  -1.898  0.05771 .
#fitted(flic.meglm.nb)vec491   1.420726   0.952211   1.492  0.13569
#fitted(flic.meglm.nb)vec131  -2.519200   0.973567  -2.588  0.00966 **
#fitted(flic.meglm.nb)vec99   -2.358192   0.987677  -2.388  0.01696 *
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Negative Binomial(1.4563) family taken to be 1)
#
#    Null deviance: 937.73  on 613  degrees of freedom
#Residual deviance: 570.10  on 598  degrees of freedom
#AIC: 3023.2
#
#Number of Fisher Scoring iterations: 1
#
#
#              Theta:  1.456
#          Std. Err.:  0.103
#
# 2 x log-likelihood:  -2989.205
#


par(mfrow=c(2,2))
plot(nb2)



########predictions effects

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

############################################
########multipanel plots of effects
############################################

pdf("H://National Scale Data//Flickr//Spatial validation//efplotbin.pdf",width=7.4,height=3.5)
par(mfrow=c(1,3),mar=c(4,5,2,2)+0.1)
plot(pred.20k.bin,rug=2,xlab="",ylab="Probability of Flickr picture",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.10k.bin,rug=2,xlab="Survey",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.5k.bin,rug=2,xlab="",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
dev.off()

postscript("H://National Scale Data//Flickr//Spatial validation//efplot_5k_bin.eps", width = 7.4, height = 3.5,onefile = FALSE, paper = "special")
par(mfrow=c(1,3),mar=c(4,5,2,2)+0.1)
plot(pred.20k.bin,rug=2,xlab="",ylab="Probability of Flickr picture",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.10k.bin,rug=2,xlab="Survey",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.5k.bin,rug=2,xlab="",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
dev.off()


pdf("H://National Scale Data//Flickr//Spatial validation//efplotnb.pdf",width=7.4,height=3.5)
par(mfrow=c(1,3),mar=c(4,5,2,2)+0.1)
plot(pred.20k.nb,rug=T,xlab="",ylab="Number of Flickr picture",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.10k.nb,rug=T,xlab="Survey",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.5k.nb,rug=T,xlab="",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
dev.off()

postscript("H://National Scale Data//Flickr//Spatial validation//efplotnb.eps", width = 7.4, height = 3.5,onefile = FALSE, paper = "special")
par(mfrow=c(1,3),mar=c(4,5,2,2)+0.1)
plot(pred.20k.nb,rug=T,xlab="",ylab="Number of Flickr picture",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.10k.nb,rug=T,xlab="Survey",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
plot(pred.5k.nb,rug=T,xlab="",ylab="",cex.lab=1.5, cex.axis=1.5,cex=1.5)
dev.off()
























