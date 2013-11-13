#uploading merged Flux data files for all years for one site
vcm.all <- read.csv("vcm_gapfilled_all.csv", header=T)
vcp.all <- read.csv("vcp_gapfilled_all.csv", header=T)

names(vcm.all)
summary(vcm.all$fc)

vcm.all[vcm.all == -9999] <- NA
vcp.all[vcp.all == -9999] <- NA

summary(vcm.all)

plot(vcm.all$fc~vcm.all$day)

vcm.fc<-vcm.all$fc
vcm.fc2<- vcm.fc *60*30
vcm.fc.3 <- vcm.fc2 / 1e6 * 12.01 / 1000
summary(vcm.fc.3)
#should now be in kg of carbon per meter squared per half hour

vcm.fc.4<-as.data.frame(vcm.all$year)
names(vcm.fc.4)<-c("year")
summary(vcm.fc.4)
vcm.fc.4$day<-vcm.all$day
vcm.fc.4$hrmin<-vcm.all$hrmin
summary(vcm.fc.4)
vcm.fc.4$fc<-vcm.fc.3
summary(vcm.fc.4)

vcm.fc.day <- aggregate(vcm.fc.4$fc, by=list(vcm.fc.4$day, vcm.fc.4$year), FUN="sum", na.rm=T)
summary(vcm.fc.day)
names(vcm.fc.day)<-c("day", "year", "fc")

vcm.fc.day2 <- aggregate(vcm.fc.4$fc, by=list(vcm.fc.4$day), FUN="sum", na.rm=T)
summary(vcm.fc.day2)
names(vcm.fc.day2)<-c("day", "fc")

vcm.fc.year <- aggregate(vcm.fc.day$fc, by=list(vcm.fc.day$year), FUN="sum", na.rm=T)
summary(vcm.fc.year)
names(vcm.fc.year)<-c("year", "fc")

plot(vcm.fc.day2, type="l", lwd=2, xlab="Day", ylab="fc (Carbon flux?) kgC/m2", main= "Upper Flux Tower Mean")

plot(vcm.fc.day$fc[vcm.fc.day$year==2007]~vcm.fc.day$day[vcm.fc.day$year==2007], type="l", lwd=1.5, xlab="Day", ylab="kg C/m2", col="blue", xlim=range(vcm.fc.day$day), ylim=range(vcm.fc.day$fc, na.rm=T), main="Upper Flux Tower")
par(new=T)
plot(vcm.fc.day$fc[vcm.fc.day$year==2008]~vcm.fc.day$day[vcm.fc.day$year==2008], type="l", lwd=1.5, xlab="Day", ylab="kg C/m2", col="red", xlim=range(vcm.fc.day$day), ylim=range(vcm.fc.day$fc, na.rm=T))
par(new=T)
plot(vcm.fc.day$fc[vcm.fc.day$year==2009]~vcm.fc.day$day[vcm.fc.day$year==2009], type="l", lwd=1.5, xlab="Day", ylab="kg C/m2", col="green", xlim=range(vcm.fc.day$day), ylim=range(vcm.fc.day$fc, na.rm=T))
par(new=T)
plot(vcm.fc.day$fc[vcm.fc.day$year==2010]~vcm.fc.day$day[vcm.fc.day$year==2010], type="l", lwd=1.5, xlab="Day", ylab="kg C/m2", col="black", xlim=range(vcm.fc.day$day), ylim=range(vcm.fc.day$fc, na.rm=T))
par(new=T)
plot(vcm.fc.day$fc[vcm.fc.day$year==2011]~vcm.fc.day$day[vcm.fc.day$year==2011], type="l", lwd=1.5, xlab="Day", ylab="kg C/m2", col="purple", xlim=range(vcm.fc.day$day), ylim=range(vcm.fc.day$fc, na.rm=T))
legend("bottomright", legend=c("2007","2008","2009", "2010", "2011"), lwd=2, col=c("blue", "red","green", "black", "purple"))
#Valles Caldera Pine Tower
#VCP

summary(vcp.all)

plot(vcp.all$fc~vcp.all$day)

vcp.fc<-vcp.all$fc
vcp.fc2<- vcp.fc *60*30
vcp.fc.3 <- vcp.fc2 / 1e6 * 12.01 / 1000
summary(vcp.fc.3)
#should now be in kg of carbon per meter squared per half hour

vcp.fc.4<-as.data.frame(vcp.all$year)
names(vcp.fc.4)<-c("year")
summary(vcp.fc.4)
vcp.fc.4$day<-vcp.all$day
vcp.fc.4$hrmin<-vcp.all$hrmin
summary(vcp.fc.4)
vcp.fc.4$fc<-vcp.fc.3
summary(vcp.fc.4)

vcp.fc.day <- aggregate(vcp.fc.4$fc, by=list(vcp.fc.4$day, vcp.fc.4$year), FUN="sum", na.rm=T)
summary(vcp.fc.day)
names(vcp.fc.day)<-c("day", "year", "fc")

vcp.fc.day2 <- aggregate(vcp.fc.4$fc, by=list(vcp.fc.4$day), FUN="sum", na.rm=T)
summary(vcp.fc.day2)
names(vcp.fc.day2)<-c("day", "fc")

vcp.fc.year <- aggregate(vcp.fc.day$fc, by=list(vcp.fc.day$year), FUN="sum", na.rm=T)
summary(vcp.fc.year)
names(vcp.fc.year)<-c("year", "fc")

plot(vcp.fc.day2, type="l", lwd=2,xlab="Day", ylab="fc (Carbon flux?) kgC/m2", main= "Lower Flux Tower Mean")


plot(vcp.fc.day$fc[vcp.fc.day$year==2009]~vcp.fc.day$day[vcp.fc.day$year==2009], type="l", lwd=2, xlab="Day", ylab="kg C/m2", col="green", xlim=range(vcp.fc.day$day), ylim=range(vcp.fc.day$fc, na.rm=T), main="Lower Flux Site")
par(new=T)
plot(vcp.fc.day$fc[vcp.fc.day$year==2010]~vcp.fc.day$day[vcp.fc.day$year==2010], type="l", lwd=2, xlab="Day", ylab="kg C/m2", col="black", xlim=range(vcp.fc.day$day), ylim=range(vcp.fc.day$fc, na.rm=T))
par(new=T)
plot(vcp.fc.day$fc[vcp.fc.day$year==2011]~vcp.fc.day$day[vcp.fc.day$year==2011], type="l", lwd=2, xlab="Day", ylab="kg C/m2", col="purple", xlim=range(vcp.fc.day$day), ylim=range(vcp.fc.day$fc, na.rm=T))
legend("topleft", legend=c("2009", "2010", "2011"), lwd=2, col=c("green", "black", "purple"))