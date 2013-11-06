all.carbon <- read.csv("valles_carbon_adj.csv", header=T)

o.fir <- read.csv("18O-fir.csv", header =T)
names(o.fir)<- c("year", "o.fir")

o.spruce <- read.csv("18O-spruce.csv", header =T)
names(o.spruce)<- c("year", "o.spruce")

o.pipo <- read.csv("pipo_o_iso.csv", header=T)
names(o.pipo)<- c("year", "o.pipo")

step.one <- merge(o.fir, o.spruce, all.x=T, all.y=T)
all.oxygen <- merge(step.one, o.pipo, all.x=T, all.y=T)
row.names(all.oxygen)<- all.oxygen$year
write.csv(all.oxygen, "all.oxygen.csv")
all.oxygen2<-read.csv("all.oxygen2.csv", header=T)

################################################
#Making a file of first differences of the carbon and oxygen isotopes
all.carbon2<- all.carbon[,c(1, 7,9,10)]
summary(all.carbon2)
write.csv(all.carbon2,"all.carbon2.csv")

all.carbon3 <- read.csv("all.carbon3.csv", header=T)
summary(all.carbon3)





###############################################
#looking at first differnces of the carbon isotopes
################################################
#checking that the isotope series actually correlate with one another
#carbon
row.names(all.carbon3)<- all.carbon3$year

corr.rwl.seg(all.carbon3[, 7:9], seg.length = 30, bin.floor=0)

#oxygen
corr.rwl.seg(all.oxygen2[,2:4], seg.length=30, bin.floor=0)



#Basic time seres stuff

ccf(all.carbon3$dif.c.fir, all.carbon3$dif.c.fir, type="correlation",na.action=na.omit)
acf(all.carbon3$dif.c.fir, type="correlation",na.action=na.omit)
pacf(all.carbon3$dif.c.fir,na.action=na.omit)

ccf(all.carbon3$dif.c.spruce, all.carbon3$dif.c.spruce, type="correlation",na.action=na.omit)
acf(all.carbon3$dif.c.spruce, type="correlation",na.action=na.omit)
pacf(all.carbon3$dif.c.spruce,na.action=na.omit)

ccf(all.carbon3$dif.c.pipo, all.carbon3$dif.c.pipo, type="correlation",na.action=na.omit)
acf(all.carbon3$dif.c.pipo, type="correlation",na.action=na.omit)
pacf(all.carbon3$dif.c.pipo,na.action=na.omit)

#################
#oxygen series
ccf(all.oxygen2$dif.o.fir, all.oxygen2$dif.o.fir, type="correlation",na.action=na.omit)
acf(all.oxygen2$dif.o.fir, type="correlation",na.action=na.omit)
pacf(all.oxygen2$dif.o.fir,na.action=na.omit)

ccf(all.oxygen2$dif.o.spruce, all.oxygen2$dif.o.spruce, type="correlation",na.action=na.omit)
acf(all.oxygen2$dif.o.spruce, type="correlation",na.action=na.omit)
pacf(all.oxygen2$dif.o.spruce,na.action=na.omit)

ccf(all.oxygen2$dif.o.pipo, all.oxygen2$dif.o.pipo, type="correlation",na.action=na.omit)
acf(all.oxygen2$dif.o.pipo, type="correlation",na.action=na.omit)
pacf(all.oxygen2$dif.o.pipo,na.action=na.omit)



lm.fir<- lm(all.carbon3$dif.c.fir[45:74]~all.oxygen2$dif.o.fir[45:74])
summary(lm.fir)

lm.spruce <- lm(all.carbon3$dif.c.spruce[45:74]~all.oxygen2$dif.o.spruce[45:74])
summary(lm.spruce)

lm.pipo <- lm(all.carbon3$dif.c.pipo[44:74]~all.oxygen2$dif.o.pipo[44:74])
summary(lm.pipo)

plot(all.carbon3$dif.c.fir[44:74]~all.oxygen2$dif.o.fir[44:74], pch=16, col="blue",xlim=range(all.oxygen2$dif.o.fir, na.rm=T), ylim=range(all.carbon3$dif.c.fir, na.rm=T), xlab= "∂18O", ylab = "∂13C", main="Dual isotopes 1981-2012")
abline(lm.fir, col="blue", lwd=4)
par(new=T)
plot(all.carbon3$dif.c.spruce[44:74]~all.oxygen2$dif.o.spruce[44:74], pch=16, col="red",xlim=range(all.oxygen2$dif.o.fir, na.rm=T), ylim=range(all.carbon3$dif.c.fir, na.rm=T), xlab="", ylab="")
abline(lm.spruce, col="red", lwd=4)
par(new=T)
plot(all.carbon3$dif.c.pipo[44:74]~all.oxygen2$dif.o.pipo[44:74], pch=16, col="dark green",xlim=range(all.oxygen2$dif.o.fir, na.rm=T), ylim=range(all.carbon3$dif.c.fir, na.rm=T), xlab="", ylab="")
abline(lm.pipo, col="dark green", lwd=4)
legend("topleft", legend=c("Fir", "Spruce", "pipo"), pch=16, col=c("blue", "red", "dark green"))
legend("topright", legend=c("R = 0.19", "R = 0.51*","R = 0.58*"),lwd=4, col=c("blue", "red", "dark green"))




#Time Series
#Carbon
plot(dif.c.fir ~ year, data=all.carbon3, type="l", lwd=3, col="blue", ylim=range(all.carbon3$dif.c.fir, na.rm=T) ,ylab="∂13C", xlab="Year")
par(new=T)
plot(dif.c.spruce ~ year, data=all.carbon3, type="l", lwd=3, col="red", xlim=range(all.carbon3$year),ylim=range(all.carbon3$dif.c.fir, na.rm=T), ylab="", xlab="")
par(new=T)
plot(dif.c.pipo ~ year, data=all.carbon3, type="l", lwd=3, col="dark green", xlim=range(all.carbon3$year),ylim=range(all.carbon3$dif.c.fir, na.rm=T), ylab="", xlab="")
#par(new=T)
#plot(carbon ~ year, data=c.asp, type="l", lwd=4, col="orange", xlim=range(c.fir$year),ylim=c(-25,-15))
#hold off on aspen for now
legend("topleft", legend=c("Fir", "Spruce", "pipo"), lwd=3, col=c("blue","red", "dark green"))
abline(v=2000, lty="dashed", lwd=2)
abline(v=2002, lty="dashed", lwd=2)
abline(v=2006, lty="dashed", lwd=2)
abline(v=1996, lty="dashed", lwd=2)
abline(v=2010, lty="dashed", lwd=2)
abline(v=1971, lty="dashed", lwd=2)
abline(v=1963, lty="dashed", lwd=2)
abline(v=1973, lty="dashed", lwd=2)



#oxygen
plot(dif.o.fir ~ year, data=all.oxygen2, type="l", lwd=4, col="blue", ylim=range(all.oxygen2$dif.o.fir,na.rm=T), ylab= "∂18O", xlab="Year")
par(new=T)
plot(dif.o.spruce ~ year, data=all.oxygen2, type="l", lwd=4, col="red", xlim=range(all.oxygen2$year), ylim=range(all.oxygen2$dif.o.fir,na.rm=T), ylab="", xlab="")
par(new=T)
plot(dif.o.pipo~ year, data=all.oxygen2, type="l", lwd=4, col="dark green", xlim=range(all.oxygen2$year), ylim=range(all.oxygen2$dif.o.fir,na.rm=T), ylab="", xlab="")
legend("topleft", legend=c("Fir", "Spruce", "pipo"), lwd=4, col=c("blue","red", "dark green"))
abline(v=2000, lty="dashed", lwd=2)
abline(v=2002, lty="dashed", lwd=2)
abline(v=2006, lty="dashed", lwd=2)
abline(v=1996, lty="dashed", lwd=2)
abline(v=2010, lty="dashed", lwd=2)
abline(v=1971, lty="dashed", lwd=2)

#Vapor Pressure Defecit
valles.vpd <- read.csv("valles_vpd2.csv", header=T)
#merge carbon iso time series with valles.vpd
#carbon.vpd <- merge(valles.vpd, all.carbon3, all.x=T, all.y=T)
#temperature
cru.mean.temp <- read.csv("VC_CRU_meanT.csv", header=T)
cru.max.temp <- read.csv("VC_CRU_Tmax.csv", header=T)
cru.min.temp <- read.csv("VC_CRU_Tmin.csv", header=T)

#carbon.tmean <- merge(cru.mean.temp, all.carbon3, all.x=T, all.y=T)
#carbon.tmax <- merge(cru.max.temp, all.carbon3, all.x=T, all.y=T)
#carbon.tmin <- merge(cru.min.temp, all.carbon3, all.x=T, all.y=T)

#oxygen.tmean <- merge(cru.mean.temp, all.oxygen2, all.x=T, all.y=T)
#oxygen.tmax <- merge(cru.max.temp, all.oxygen2, all.x=T, all.y=T)
#oxygen.tmin <- merge(cru.min.temp, all.oxygen2, all.x=T, all.y=T)

cru.precip <- read.csv("VC_CRU_precip.csv", header=T)

#carbon.precip <- merge(cru.precip, all.carbon3, all.x=T, all.y=T)
#oxygen.precip <- merge(cru.precip, all.oxygen2, all.x=T, all.y=T)
vc.pdsi <- read.csv("VC_PDSI.csv", header=T)

#carbon.pdsi <- merge(vc.pdsi, all.carbon3, all.x=T, all.y=T)
#carbon.pdsi <- merge(vc.pdsi, all.oxygen2, all.x=T, all.y=T)

#oxygen.vpd <- merge(valles.vpd, all.oxygen2, all.x=T, all.y=T)

#####################################
#Carbon Correlations
c.fir.tmean <- cor(all.carbon3$dif.c.fir[45:72], cru.mean.temp[82:109,], method = "pearson")
c.spruce.tmean <- cor(all.carbon3$dif.c.spruce[45:72], cru.mean.temp[82:109,], method = "pearson")
c.pipo.tmean <- cor(all.carbon3$dif.c.pipo[45:72], cru.mean.temp[82:109,], method = "pearson")

c.fir.tmax <- cor(all.carbon3$dif.c.fir[45:72],cru.max.temp[82:109,], method = "pearson")
c.spruce.tmax <- cor(all.carbon3$dif.c.spruce[45:72],cru.max.temp[82:109,], method = "pearson")
c.pipo.tmax <- cor(all.carbon3$dif.c.pipo[45:72],cru.max.temp[82:109,], method = "pearson")

c.fir.tmin <- cor(all.carbon3$dif.c.fir[45:72],cru.min.temp[82:109,], method = "pearson")
c.spruce.tmin <- cor(all.carbon3$dif.c.spruce[45:72],cru.min.temp[82:109,], method = "pearson")
c.pipo.tmin <- cor(all.carbon3$dif.c.pipo[45:72],cru.min.temp[82:109,], method = "pearson")

c.fir.precip <- cor(all.carbon3$dif.c.fir[45:72], cru.precip[82:109,],  method = "pearson")
c.spruce.precip <- cor(all.carbon3$dif.c.spruce[45:72], cru.precip[82:109,], method = "pearson")
c.pipo.precip <- cor(all.carbon3$dif.c.pipo[45:72], cru.precip[82:109,], method = "pearson")

c.fir.vpd <- cor(all.carbon3$dif.c.fir[45:72], valles.vpd[81:108,], method = "pearson")
c.spruce.vpd <- cor(all.carbon3$dif.c.spruce[45:72], valles.vpd[81:108,], method = "pearson")
c.pipo.vpd <- cor(all.carbon3$dif.c.pipo[45:72], valles.vpd[81:108,], method = "pearson")
#############################
#oxygen correlations

o.fir.tmean <- cor(all.oxygen2$dif.o.fir[45:72],cru.mean.temp[82:109,], method = "pearson")
o.spruce.tmean <- cor(all.oxygen2$dif.o.spruce[45:72],cru.mean.temp[82:109,], method = "pearson")
o.pipo.tmean <- cor(all.oxygen2$dif.o.pipo[45:72],cru.mean.temp[82:109,], method = "pearson")

o.fir.tmax <- cor(all.oxygen2$dif.o.fir[45:72],cru.max.temp[82:109,], method = "pearson")
o.spruce.tmax <- cor(all.oxygen2$dif.o.spruce[45:72],cru.max.temp[82:109,], method = "pearson")
o.pipo.tmax <- cor(all.oxygen2$dif.o.pipo[45:72],cru.max.temp[82:109,], method = "pearson")

o.fir.tmin <- cor(all.oxygen2$dif.o.fir[45:72], cru.min.temp[82:109,], method = "pearson")
o.spruce.tmin <- cor(all.oxygen2$dif.o.spruce[45:72],cru.min.temp[82:109,], method = "pearson")
o.pipo.tmin <- cor(all.oxygen2$dif.o.pipo[45:72], cru.min.temp[82:109,],method = "pearson")

o.fir.precip <- cor(all.oxygen2$dif.o.fir[45:72], cru.precip[82:109,], method = "pearson")
o.spruce.precip <- cor(all.oxygen2$dif.o.spruce[45:72], cru.precip[82:109,], method = "pearson")
o.pipo.precip <- cor(all.oxygen2$dif.o.pipo[45:72], cru.precip[82:109,], method = "pearson")

o.fir.vpd <- cor(all.oxygen2$dif.o.fir[45:72], valles.vpd[81:108,], method = "pearson")
o.spruce.vpd <- cor(all.oxygen2$dif.o.spruce[45:72], valles.vpd[81:108,], method = "pearson")
o.pipo.vpd <- cor(all.oxygen2$dif.o.pipo[45:72], valles.vpd[81:108,], method = "pearson")

########################
#bar plots
#fir

par(mfrow=c(2,3), las=3,tck=0.025)
barplot(c.fir.tmean[,2:18] ,main = "c.fir.tmean", ylim=c(-0.5,0.5), col=ifelse(c.fir.tmean[,2:18] > 0.367 |  c.fir.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.fir.tmin [,2:18] ,main = "c.fir.tmin",ylim=c(-0.5,0.5), col=ifelse(c.fir.tmin[,2:18] > 0.367 |  c.fir.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.fir.tmax[,2:18] ,main = "c.fir.tmax",ylim=c(-0.5,0.5), col=ifelse(c.fir.tmax[,2:18] > 0.367 |  c.fir.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.fir.precip[,2:18] ,main = "c.fir.precip",ylim=c(-0.5,0.5), col=ifelse(c.fir.precip[,2:18] > 0.367 |  c.fir.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.fir.vpd[,2:15] ,main = "c.fir.vpd",ylim=c(-0.5,0.5), col=ifelse(c.fir.vpd[,2:15] > 0.367 |  c.fir.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)

#spruce
par(mfrow=c(2,3), las=3,tck=0.025)
barplot(c.spruce.tmean[,2:18] ,main = "c.spruce.tmean", ylim=c(-0.5,0.5), col=ifelse(c.spruce.tmean[,2:18] > 0.367 |  c.spruce.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.spruce.tmin [,2:18] ,main = "c.spruce.tmin",ylim=c(-0.5,0.5), col=ifelse(c.spruce.tmin[,2:18] > 0.367 |  c.spruce.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.spruce.tmax[,2:18] ,main = "c.spruce.tmax",ylim=c(-0.5,0.5), col=ifelse(c.spruce.tmax[,2:18] > 0.367 |  c.spruce.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.spruce.precip[,2:18] ,main = "c.spruce.precip",ylim=c(-0.5,0.5), col=ifelse(c.spruce.precip[,2:18] > 0.367 |  c.spruce.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.spruce.vpd[,2:15] ,main = "c.spruce.vpd",ylim=c(-0.5,0.5), col=ifelse(c.spruce.vpd[,2:15] > 0.367 |  c.spruce.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
#pipo
par(mfrow=c(2,3), las=3,tck=0.025)
barplot(c.pipo.tmean[,2:18] ,main = "c.pipo.tmean", ylim=c(-0.5,0.5), col=ifelse(c.pipo.tmean[,2:18] > 0.367 |  c.pipo.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.pipo.tmin [,2:18] ,main = "c.pipo.tmin",ylim=c(-0.5,0.5), col=ifelse(c.pipo.tmin[,2:18] > 0.367 |  c.pipo.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.pipo.tmax[,2:18] ,main = "c.pipo.tmax",ylim=c(-0.5,0.5), col=ifelse(c.pipo.tmax[,2:18] > 0.367 |  c.pipo.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.pipo.precip[,2:18] ,main = "c.pipo.precip",ylim=c(-0.5,0.5), col=ifelse(c.pipo.precip[,2:18] > 0.367 |  c.pipo.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(c.pipo.vpd[,2:15] ,main = "c.pipo.vpd",ylim=c(-0.5,0.5), col=ifelse(c.pipo.vpd[,2:15] > 0.367 |  c.pipo.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
#############################################
#oxygen isotopes

par(mfrow=c(2,3), las=3,tck=0.025)
barplot(o.fir.tmean[,2:18] ,main = "o.fir.tmean", ylim=c(-0.5,0.5), col=ifelse(o.fir.tmean[,2:18] > 0.367 |  o.fir.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.fir.tmin [,2:18] ,main = "o.fir.tmin",ylim=c(-0.5,0.5), col=ifelse(o.fir.tmin[,2:18] > 0.367 |  o.fir.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.fir.tmax[,2:18] ,main = "o.fir.tmax",ylim=c(-0.5,0.5), col=ifelse(o.fir.tmax[,2:18] > 0.367 |  o.fir.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.fir.precip[,2:18] ,main = "o.fir.precip",ylim=c(-0.5,0.5), col=ifelse(o.fir.precip[,2:18] > 0.367 |  o.fir.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.fir.vpd[,2:15] ,main = "o.fir.vpd",ylim=c(-0.5,0.5), col=ifelse(o.fir.vpd[,2:15] > 0.367 |  o.fir.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)

#spruce
par(mfrow=c(2,3), las=3,tck=0.025)
barplot(o.spruce.tmean[,2:18] ,main = "o.spruce.tmean", ylim=c(-0.5,0.5), col=ifelse(o.spruce.tmean[,2:18] > 0.367 |  o.spruce.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.spruce.tmin [,2:18] ,main = "o.spruce.tmin",ylim=c(-0.5,0.5), col=ifelse(o.spruce.tmin[,2:18] > 0.367 |  o.spruce.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.spruce.tmax[,2:18] ,main = "o.spruce.tmax",ylim=c(-0.5,0.5), col=ifelse(o.spruce.tmax[,2:18] > 0.367 |  o.spruce.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.spruce.precip[,2:18] ,main = "o.spruce.precip",ylim=c(-0.5,0.5), col=ifelse(o.spruce.precip[,2:18] > 0.367 |  o.spruce.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.spruce.vpd[,2:15] ,main = "o.spruce.vpd",ylim=c(-0.5,0.5), col=ifelse(o.spruce.vpd[,2:15] > 0.367 |  o.spruce.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
#pipo
par(mfrow=c(2,3), las=3,tck=0.025)
barplot(o.pipo.tmean[,2:18] ,main = "o.pipo.tmean", ylim=c(-0.5,0.5), col=ifelse(o.pipo.tmean[,2:18] > 0.367 |  o.pipo.tmean[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.pipo.tmin [,2:18] ,main = "o.pipo.tmin",ylim=c(-0.5,0.5), col=ifelse(o.pipo.tmin[,2:18] > 0.367 |  o.pipo.tmin[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.pipo.tmax[,2:18] ,main = "o.pipo.tmax",ylim=c(-0.5,0.5), col=ifelse(o.pipo.tmax[,2:18] > 0.367 |  o.pipo.tmax[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.pipo.precip[,2:18] ,main = "o.pipo.precip",ylim=c(-0.5,0.5), col=ifelse(o.pipo.precip[,2:18] > 0.367 |  o.pipo.precip[,2:18] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)
barplot(o.pipo.vpd[,2:15] ,main = "o.pipo.vpd",ylim=c(-0.5,0.5), col=ifelse(o.pipo.vpd[,2:15] > 0.367 |  o.pipo.vpd[,2:15] < -0.367, "blue", "white"))
abline(h = 0.367, lty="dashed", col="red", lwd=2)
abline(h = -0.367, lty="dashed", col="red", lwd=2)