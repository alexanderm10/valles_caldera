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
vcm.fc.day.sd <- aggregate(vcm.fc.4$fc, by=list(vcm.fc.4$day, vcm.fc.4$year), FUN="sd", na.rm=T)
names(vcm.fc.day.sd)<-c("day", "year", "sd.fc")

vcm.fc.day2 <- aggregate(vcm.fc.4$fc, by=list(vcm.fc.4$day), FUN="sum", na.rm=T)
summary(vcm.fc.day2)
names(vcm.fc.day2)<-c("day", "fc")
vcm.fc.day2.sd <- aggregate(vcm.fc.4$fc, by=list(vcm.fc.4$day), FUN="sd", na.rm=T)
names(vcm.fc.day2.sd)<-c("year", "sd.fc")

vcm.fc.year <- aggregate(vcm.fc.day$fc, by=list(vcm.fc.day$year), FUN="sum", na.rm=T)
summary(vcm.fc.year)
names(vcm.fc.year)<-c("year", "fc")
vcm.fc.year.sd <- aggregate(vcm.fc.day$fc, by=list(vcm.fc.day$year), FUN="sd", na.rm=T)
names(vcm.fc.year.sd)<-c("year", "sd.fc")

vcm.fc.year2<- merge(vcm.fc.year, vcm.fc.year.sd)

dim(vcm.fc.year)

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


plot(fc~year, data=vcm.fc.year, pch=16)
plot(fc~year, data=vcp.fc.year, pch=16)


nee.plot<- ggplot()  +
  # plotting total site basal area
  
  #geom_ribbon(data=model.mean.diff, aes(x=year, ymin=(vuf.mean.area - 1.96*vuf.sd.area), ymax=(vuf.mean.area + 1.96*vuf.sd.area)), alpha=0.15, fill="black")+
  geom_ribbon(data=model.mean.diff, aes(x=year, ymin=(vuf.mean.area - 1.96*vuf.sd.area), ymax=(vuf.mean.area + 1.96*vuf.sd.area)), alpha=0.15, color="black")+
  
  geom_line(data=model.mean.diff, aes(x=year, y=vuf.mean.area), size=1.5, color="black") +
  #geom_line(data= model.mean, aes(x=year, y=vuf.mean.area), size=1.5, colour="blue") +
  
  
  geom_point(data=vcm.fc.year2, aes(x=year, y=fc*-1, size=10, color="blue"))+
  #geom_point(data=current.mean, aes(x=2012, y=vuf.mean.area), size=4, colour="blue")+
  
  
  #geom_errorbar(data=vcm.fc.year2,aes(x=year, ymin=(fc - 1.96*sd.fc)*-1, ymax=(fc + 1.96*sd.fc)*-1, size=0.5))+
  #geom_errorbar(data=current.mean, aes(x=2012, ymin=(vuf.mean.area - 1.96*vuf.sd.area), ymax=(vuf.mean.area + 1.96*vuf.sd.area)), color="blue")+
  
  # all of that theme stuff you can just pre-set
  poster.theme
#theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12))
#scale_fill_discrete(name="Model", labels = c("nt.pipo.sum", "nt.piaz.sum", "nt.pine.spp", "nt.vcnp.sum", "nt.pine.dom.sum"))

# telling what colors to make the lines for species
#scale_color_manual(values=c("red", "blue", "orange", "green")) 
             
nee.plot+ggtitle("Productivity Comparison")+scale_y_continuous("kg Biomass m-2", limits=c(0,0.5))+scale_x_continuous(name="Year", limits=c(2000, 2011))
vcm.fc.year2

temp1<- merge(vcm.fc.year2, model.mean.diff)
temp1$perc<- temp1$vuf.mean.area/temp1$fc