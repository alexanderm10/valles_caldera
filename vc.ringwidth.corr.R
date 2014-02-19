#load in time series for VLF and VUF sites
vufp1c <- read.csv("vufp1chronos.csv", header=T) # importing data
vufp2c <- read.csv("vufp2chronos.csv", header=T)
vuf.all <- read.csv("vufall.csv", header=T)

vlfp1c <- read.csv("vlfp1chronos.csv", header=T)
vlfp2c <- read.csv("vlfp2chronos.csv", header=T)
vlf.all <- read.csv("vlfall-30yrspline.csv", header=T)

####################################################
#basic time series stuff
#acf
#pacf
#ARIMA modeling
#looking at whole plots for this
#for plot by plot look at valles_timeseries_analysis.r
####################################################
par(new=F)
for(j in 2:ncol(cru.mean.temp)){
  ccf(cru.mean.temp[65:109,j], cru.mean.temp[65:109,j], type="correlation", lag.max=10)
  par(new=F)  
}

par(new=F)
for(j in 2:ncol(cru.precip)){
  ccf(cru.precip[65:109,j], cru.precip[65:109,j], type="correlation", lag.max=10)
  par(new=F)  
}

par(new=F)
for(j in 2:ncol(vc.pdsi)){
  ccf(vc.pdsi[113:158,j], vc.pdsi[113:158,j], type="correlation", lag.max=10)
  par(new=F)  
}

ccf(snotel$sd.swe, snotel$sd.swe, type="correlation", lag.max=10)


#VLF site
ccf(vlf.all$std[37:83], vlf.all$std[37:83], type = "correlation")
acf(vlf.all$std[37:83], type="correlation")
pacf(vlf.all$std[37:83])



#VUF site
ccf(vuf.all$std[43:89], vuf.all$std[43:89], type = "correlation")
acf(vuf.all$std[43:89], type="correlation")
pacf(vuf.all$std[43:89])

####################################################
#climate correlations with Entire Ring Width
####################################################
#upload climate data
valles.vpd <- read.csv("valles_vpd2.csv", header=T)
cru.mean.temp <- read.csv("VC_CRU_meanT.csv", header=T)
cru.max.temp <- read.csv("VC_CRU_Tmax.csv", header=T)
cru.min.temp <- read.csv("VC_CRU_Tmin.csv", header=T)
cru.precip <- read.csv("VC_CRU_precip.csv", header=T)
vc.pdsi <- read.csv("VC_PDSI.csv", header=T)
#snowtel data
snotel<- read.csv("all_swe.csv", header=T)

#VLF site

vlf.all.mean.t<- cor(vlf.all[c(37:81),2], cru.mean.temp[c(65:109),], method = "pearson")
vlf.all.precip<- cor(vlf.all[c(37:81),2], cru.precip[c(65:109),], method = "pearson")
vlf.all.pdsi<- cor(vlf.all[c(37:82),2], vc.pdsi[c(113:158),], method = "pearson")
vlf.all.vpd<- cor(vlf.all[c(37:81),2], valles.vpd[c(64:108),], method = "pearson")
vlf.all.snotel<- cor(vlf.all[c(52:83),2], snotel[c(1:32),2], method = "pearson")



#VUF site

vuf.all.mean.t<- cor(vuf.all[c(43:87),2], cru.mean.temp[c(65:109),], method = "pearson")
vuf.all.precip<- cor(vuf.all[c(43:87),2], cru.precip[c(65:109),], method = "pearson")
vuf.all.pdsi<- cor(vuf.all[c(43:88),2], vc.pdsi[c(113:158),], method = "pearson")
vuf.all.vpd<- cor(vuf.all[c(43:87),2], valles.vpd[c(64:108),], method = "pearson")
vuf.all.snotel<- cor(vuf.all[c(58:89),2], snotel[c(1:32),2], method = "pearson")


##################
#Bargraphs of Correlations
###################

#VLF p1
par(mfrow=c(2,3), las=3,tck= 0.025)
barplot(vlf.all.mean.t[,2:18] ,main = "VLF.tmean", ylim=c(-0.7,0.7), col=ifelse(vlf.all.mean.t[,2:18] > 0.297 | vlf.all.mean.t[,2:18] < -0.297, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vlf.all.precip[,2:18] ,main = "precip", ylim=c(-0.7,0.7), col=ifelse(vlf.all.precip[,2:18] > 0.2907 |  vlf.all.precip[,2:18] < -0.2907, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vlf.all.pdsi[,2:18] ,main = "PDSI", ylim=c(-0.7,0.7), col=ifelse(vlf.all.pdsi[,2:18] > 0.2876 |  vlf.all.pdsi[,2:18] < -0.2876, "blue", "white"))
abline(h = 0.2876, lty="dashed", col="red", lwd=2)
abline(h = -0.2876, lty="dashed", col="red", lwd=2)
barplot(vlf.all.vpd[,2:15] ,main = "VPD", ylim=c(-0.7,0.7), col=ifelse(vlf.all.vpd[,2:15] > 0.2907 |  vlf.all.vpd[,2:15] < -0.2907, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vlf.all.snotel ,main = "April Snowpack", ylim=c(-0.7,0.7), col=ifelse(vlf.all.snotel > 0.3440 |  vlf.all.snotel < -0.3440, "blue", "white"))
abline(h = 0.3440, lty="dashed", col="red", lwd=2)
abline(h = -0.3440, lty="dashed", col="red", lwd=2)


#VUF Site
par(mfrow=c(2,3), las=3,tck= 0.025, cex=2)
barplot(vuf.all.mean.t[,2:18] ,main = "vuf.tmean", ylim=c(-0.7,0.7), col=ifelse(vuf.all.mean.t[,2:18] > 0.297 | vuf.all.mean.t[,2:18] < -0.297, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vuf.all.precip[,2:18] ,main = "precip", ylim=c(-0.7,0.7), col=ifelse(vuf.all.precip[,2:18] > 0.2907 |  vuf.all.precip[,2:18] < -0.2907, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vuf.all.pdsi[2:18] ,main = "PDSI", ylim=c(-0.7,0.7), col=ifelse(vuf.all.pdsi[,2:18] > 0.2876 |  vuf.all.pdsi[,2:18] < -0.2876, "blue", "white"))
abline(h = 0.2876, lty="dashed", col="red", lwd=2)
abline(h = -0.876, lty="dashed", col="red", lwd=2)
barplot(vuf.all.vpd[2:15] ,main = "VPD", ylim=c(-0.7,0.7), col=ifelse(vuf.all.vpd[,2:15] > 0.2907 |  vuf.all.vpd[,2:15] < -0.2907, "blue", "white"))
abline(h = 0.291, lty="dashed", col="red", lwd=2)
abline(h = -0.291, lty="dashed", col="red", lwd=2)
barplot(vuf.all.snotel ,main = "April Snowpack", ylim=c(-0.7,0.7), col=ifelse(vuf.all.snotel > 0.3440 |  vuf.all.snotel < -0.3440, "blue", "white"))
abline(h = 0.3440, lty="dashed", col="red", lwd=2)
abline(h = -0.3440, lty="dashed", col="red", lwd=2)



#####################################################
#look at isotopes vs. ring widths
#####################################################
#oxygen isotopes vs ring width
lm2<-lm(vuf.all[c(59:89),2]~o.spruce$oxygen)
summary(lm2)

sqrt(0.001151)

par(new=F)
par(mfrow= c(1,1))
par(mar=c(5, 4, 4, 4) + 0.1)
plot(std ~ year, data=vuf.all, type ="l", ylab="Index values", xlab="Year", xlim=c(1980,2011))
par(new=T)
plot(o.spruce$oxygen ~ o.spruce$year, type ="l", axes=F, lwd = 2, ylab="", xlab="",xlim = c(1980,2011), col="blue")
axis(4)
mtext("Spruce oxygen iso",side=4,col="blue",line=2.5)
text(1995, 30.5, "r = 0.001")
text(1995, 30.25, "p = 0.8562")

#carbon isotioes vs. ring width
lm3<-lm(vuf.all[c(58:89),2]~c.spruce$carbon)
summary(lm3)

sqrt(0.03664)
#r = -0.19

par(new=F)
par(mfrow= c(1,1))
par(mar=c(5, 4, 4, 4) + 0.1)
plot(std ~ year, data=vuf.all, type ="l", ylab="Index values", xlab="Year", xlim=c(1980,2011))
par(new=T)
plot(o.spruce$oxygen ~ o.spruce$year, type ="l", axes=F, lwd = 2, ylab="", xlab="",xlim = c(1980,2011), col="blue")
axis(4)
mtext("Spruce oxygen iso",side=4,col="blue",line=2.5)
text(1995, 30.5, "r = 0.001")
text(1995, 30.25, "p = 0.8562")