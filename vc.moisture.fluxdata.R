#uploading merged Flux data files for all years for one site
vcm.all <- read.csv("vcm_gapfilled_all.csv", header=T)
vcp.all <- read.csv("vcp_gapfilled_all.csv", header=T)

names(vcm.all)
vcm.moisture <- vcm.all[,c(1:3, 14, 27)]
vcm.moisture[vcm.moisture == -9999] <- NA
summary(vcm.moisture)

vcm.moisture2 <- aggregate(vcm.moisture[,c("h", "vpd")], by=list(vcm.moisture$day, vcm.moisture$year), FUN="mean", na.rm=T)
names(vcm.moisture2)<-c("day", "year", "humid", "vpd")
head(vcm.moisture2)
summary(vcm.moisture2)

vcm.moisture3 <- aggregate(vcm.moisture[,c("h", "vpd")], by=list(vcm.moisture$day), FUN="mean", na.rm=T)
names(vcm.moisture3)<- c("day", "h", "vpd")
head(vcm.moisture3)
#some summary plots of humidity
vcm.h1<-loess(vcm.moisture2$h[vcm.moisture2$year== 2009] ~ vcm.moisture2$day[vcm.moisture2$year== 2009])
plot(vcm.moisture2$h[vcm.moisture2$year== 2009] ~ vcm.moisture2$day[vcm.moisture2$year== 2009], type="l")
lines(predict(vcm.h1), col="red", lwd=2)

vcm.h2<-loess(vcm.moisture2$h[vcm.moisture2$year== 2010] ~ vcm.moisture2$day[vcm.moisture2$year== 2010])
plot(vcm.moisture2$h[vcm.moisture2$year== 2010] ~ vcm.moisture2$day[vcm.moisture2$year== 2010], type="l")
lines(predict(vcm.h2), col="red", lwd=2)

vcm.h3<-loess(vcm.moisture2$h[vcm.moisture2$year== 2011] ~ vcm.moisture2$day[vcm.moisture2$year== 2011])
plot(vcm.moisture2$h[vcm.moisture2$year== 2011] ~ vcm.moisture2$day[vcm.moisture2$year== 2011], type="l")
lines(predict(vcm.h3), col="red", lwd=2)

vcm.h4<-loess(h ~ day, data=vcm.moisture3)
plot(h ~ day, data=vcm.moisture3, type="l")
lines(predict(vcm.h4), col="red", lwd=2)
#some summary plots of VPD
plot(vcm.moisture2$vpd[vcm.moisture2$year== 2009] ~ vcm.moisture2$day[vcm.moisture2$year== 2009], type="l")
plot(vcm.moisture2$vpd[vcm.moisture2$year== 2010] ~ vcm.moisture2$day[vcm.moisture2$year== 2010], type="l")
plot(vcm.moisture2$vpd[vcm.moisture2$year== 2011] ~ vcm.moisture2$day[vcm.moisture2$year== 2011], type="l")

vcm.vpd4<-loess(vpd ~ day, data=vcm.moisture3)
plot(vpd ~ day, data=vcm.moisture3, type="l")
lines(predict(vcm.vpd4), col="red", lwd=2)

plot(vpd ~ day, data=vcm.moisture3, type="l")
lines(predict(vcm.vpd4), col="red", lwd=2)
par(new=T)
plot(h ~ day, data=vcm.moisture3, type="l")
lines(predict(vcm.h4), col="blue", lwd=2)

###############################################################
#Pine Site
vcp.moisture <- vcp.all[,c(1:3, 14, 27)]
vcp.moisture[vcp.moisture == -9999] <- NA
summary(vcp.moisture)

vcp.moisture2 <- aggregate(vcp.moisture[,c("h", "vpd")], by=list(vcp.moisture$day, vcp.moisture$year), FUN="mean", na.rm=T)
names(vcp.moisture2)<-c("day", "year", "h", "vpd")
head(vcp.moisture2)
summary(vcp.moisture2)
head(vcp.moisture2)


vcp.moisture3 <- aggregate(vcp.moisture[,c("h", "vpd")], by=list(vcp.moisture$day), FUN="mean", na.rm=T)
names(vcp.moisture3)<- c("day", "h", "vpd")
head(vcp.moisture3)

#some summary plots of humidity
vcp.h1<-loess(vcp.moisture2$h[vcp.moisture2$year== 2009] ~ vcp.moisture2$day[vcp.moisture2$year== 2009])
plot(vcp.moisture2$h[vcp.moisture2$year== 2009] ~ vcp.moisture2$day[vcp.moisture2$year== 2009], type="l")
lines(predict(vcp.h1), col="red", lwd=2)

vcp.h2<-loess(vcp.moisture2$h[vcp.moisture2$year== 2010] ~ vcp.moisture2$day[vcp.moisture2$year== 2010])
plot(vcp.moisture2$h[vcp.moisture2$year== 2010] ~ vcp.moisture2$day[vcp.moisture2$year== 2010], type="l")
lines(predict(vcp.h2), col="red", lwd=2)

vcp.h3<-loess(vcp.moisture2$h[vcp.moisture2$year== 2011] ~ vcp.moisture2$day[vcp.moisture2$year== 2011])
plot(vcp.moisture2$h[vcp.moisture2$year== 2011] ~ vcp.moisture2$day[vcp.moisture2$year== 2011], type="l")
lines(predict(vcp.h3), col="red", lwd=2)

vcp.h4<-loess(h ~ day, data=vcp.moisture3)
plot(h ~ day, data=vcp.moisture3, type="l")
lines(predict(vcp.h4), col="red", lwd=2)
#some summary plots of VPD
plot(vcp.moisture2$vpd[vcp.moisture2$year== 2009] ~ vcp.moisture2$day[vcp.moisture2$year== 2009], type="l")
plot(vcp.moisture2$vpd[vcp.moisture2$year== 2010] ~ vcp.moisture2$day[vcp.moisture2$year== 2010], type="l")
plot(vcp.moisture2$vpd[vcp.moisture2$year== 2011] ~ vcp.moisture2$day[vcp.moisture2$year== 2011], type="l")

vcp.vpd4<-loess(vpd ~ day, data=vcp.moisture3)
plot(vpd ~ day, data=vcp.moisture3, type="l")
lines(predict(vcp.vpd4), col="red", lwd=2)

plot(vpd ~ day, data=vcp.moisture3, type="l")
lines(predict(vcp.vpd4), col="red", lwd=2)
par(new=T)
plot(h ~ day, data=vcp.moisture3, type="l")
lines(predict(vcp.h4), col="blue", lwd=2)