all.carbon <- read.csv("valles_carbon_adj.csv", header=T)

o.fir <- read.csv("18O-fir.csv", header =T)
names(o.fir)<- c("year", "o.fir")

o.spruce <- read.csv("18O-spruce.csv", header =T)
names(o.spruce)<- c("year", "o.spruce")

o.pine <- read.csv("pine_o_iso.csv", header=T)
names(o.pine)<- c("year", "o.pine")

step.one <- merge(o.fir, o.spruce, all.x=T, all.y=T)
all.oxygen <- merge(step.one, o.pine, all.x=T, all.y=T)
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




################################################
#checking that the isotope series actually correlate with one another
#carbon
row.names(all.carbon)<- all.carbon$year

corr.rwl.seg(all.carbon[, 7:9], seg.length = 30, bin.floor=0)

#oxygen
corr.rwl.seg(all.oxygen[,2:4], seg.length=30, bin.floor=0)



#Basic time seres stuff

ccf(all.carbon$adj.c.fir, all.carbon$adj.c.fir, type="correlation",na.action=na.omit)
acf(all.carbon$adj.c.fir, type="correlation",na.action=na.omit)
pacf(all.carbon$adj.c.fir,na.action=na.omit)

ccf(all.carbon$adj.c.spruce, all.carbon$adj.c.spruce, type="correlation",na.action=na.omit)
acf(all.carbon$adj.c.spruce, type="correlation",na.action=na.omit)
pacf(all.carbon$adj.c.spruce,na.action=na.omit)

ccf(all.carbon$adj.c.pipo, all.carbon$adj.c.pipo, type="correlation",na.action=na.omit)
acf(all.carbon$adj.c.pipo, type="correlation",na.action=na.omit)
pacf(all.carbon$adj.c.pipo,na.action=na.omit)

#################
#oxygen series
ccf(all.oxygen$o.fir, all.oxygen$o.fir, type="correlation",na.action=na.omit)
acf(all.oxygen$o.fir, type="correlation",na.action=na.omit)
pacf(all.oxygen$o.fir,na.action=na.omit)

ccf(all.oxygen$o.spruce, all.oxygen$o.spruce, type="correlation",na.action=na.omit)
acf(all.oxygen$o.spruce, type="correlation",na.action=na.omit)
pacf(all.oxygen$o.spruce,na.action=na.omit)

ccf(all.oxygen$o.pine, all.oxygen$o.pine, type="correlation",na.action=na.omit)
acf(all.oxygen$o.pine, type="correlation",na.action=na.omit)
pacf(all.oxygen$o.pine,na.action=na.omit)



lm.fir<- lm(all.carbon$adj.c.fir[44:74]~o.fir$o.fir[44:74])
summary(lm.fir)

lm.spruce <- lm(all.carbon$adj.c.spruce[44:74]~o.spruce$o.spruce)
summary(lm.spruce)

lm.pine <- lm(all.carbon$adj.c.pipo[44:74]~o.pine$o.pine[22:52])
summary(lm.pine)

plot(all.carbon$adj.c.fir[44:74]~o.fir$o.fir[44:74], pch=16, col="blue", xlim=c(30,37), ylim=c(-23,-16), xlab= "∂18O", ylab = "∂13C", main="Dual isotopes 1981-2012")
abline(lm.fir, col="blue", lwd=4)
par(new=T)
plot(all.carbon$adj.c.spruce[44:74]~o.spruce$o.spruce, pch=16, col="red",xlim=c(30,37), ylim=c(-23,-16), xlab="", ylab="")
abline(lm.spruce, col="red", lwd=4)
par(new=T)
plot(all.carbon$adj.c.pipo[44:74]~o.pine$o.pine[22:52], pch=16, col="dark green", xlim=c(30,37), ylim=c(-23,-16), xlab="", ylab="")
abline(lm.pine, col="dark green", lwd=4)
legend("topleft", legend=c("Fir", "Spruce", "Pine"), pch=16, col=c("blue", "red", "dark green"))
legend("topright", legend=c("R = 0.19", "R = 0.51*","R = 0.58*"),lwd=4, col=c("blue", "red", "dark green"))




#Time Series
#Carbon
plot(adj.c.fir ~ year, data=all.carbon, type="l", lwd=3, col="blue", ylim=c(-25,-15), ylab="∂13C", xlab="Year")
par(new=T)
plot(adj.c.spruce ~ year, data=all.carbon, type="l", lwd=3, col="red", xlim=range(all.carbon$year),ylim=c(-25,-15), ylab="", xlab="")
par(new=T)
plot(adj.c.pipo ~ year, data=all.carbon, type="l", lwd=3, col="dark green", xlim=range(all.carbon$year),ylim=c(-25,-15), ylab="", xlab="")
#par(new=T)
#plot(carbon ~ year, data=c.asp, type="l", lwd=4, col="orange", xlim=range(c.fir$year),ylim=c(-25,-15))
#hold off on aspen for now
legend("topleft", legend=c("Fir", "Spruce", "Pine"), lwd=3, col=c("blue","red", "dark green"))
abline(v=2000, lty="dashed", lwd=2)
abline(v=2002, lty="dashed", lwd=2)
abline(v=2006, lty="dashed", lwd=2)
abline(v=1996, lty="dashed", lwd=2)
abline(v=2010, lty="dashed", lwd=2)
abline(v=1971, lty="dashed", lwd=2)
abline(v=1963, lty="dashed", lwd=2)
abline(v=1973, lty="dashed", lwd=2)



#oxygen
plot(o.fir ~ year, data=all.oxygen, type="l", lwd=4, col="blue", ylim=c(25,40), ylab= "∂18O", xlab="Year")
par(new=T)
plot(o.spruce ~ year, data=all.oxygen, type="l", lwd=4, col="red", xlim=range(all.oxygen$year), ylim=c(25,40), ylab="", xlab="")
par(new=T)
plot(o.pine~ year, data=all.oxygen, type="l", lwd=4, col="dark green", xlim=range(all.oxygen$year), ylim=c(25,40), ylab="", xlab="")
legend("topleft", legend=c("Fir", "Spruce", "pine"), lwd=4, col=c("blue","red", "dark green"))
abline(v=2000, lty="dashed", lwd=2)
abline(v=2002, lty="dashed", lwd=2)
abline(v=2006, lty="dashed", lwd=2)
abline(v=1996, lty="dashed", lwd=2)
abline(v=2010, lty="dashed", lwd=2)
abline(v=1971, lty="dashed", lwd=2)

#Vapor Pressure Defecit
valles.vpd <- read.csv("valles_vpd2.csv", header=T)
#merge carbon iso time series with valles.vpd

library(MASS)
valles.iso.vpd <- merge(valles.vpd, all.carbon, all.x=T, all.y=T)

summary(valles.iso.vpd)

iso.lm.spruce <- lm(adj.c.spruce ~ 1, data=valles.iso.vpd[79:108,], na.action=na.omit)
summary(iso.lm.spruce)

step.iso.spruce <- stepAIC(iso.lm.spruce, ~ jan+feb+mar+apr+may+jun+
                           jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.iso.spruce)

#fir
iso.lm.fir <- lm(adj.c.fir ~ 1, data=valles.iso.vpd[79:108,], na.action=na.omit)
summary(iso.lm.fir)

step.iso.fir <- stepAIC(iso.lm.fir, ~ jan+feb+mar+apr+may+jun+
                             jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.iso.fir)

#pipo
iso.lm.pipo <- lm(adj.c.pipo ~ 1, data=valles.iso.vpd[79:108,], na.action=na.omit)
summary(iso.lm.pipo)

step.iso.pipo <- stepAIC(iso.lm.pipo, ~ jan+feb+mar+apr+may+jun+
                             jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.iso.pipo)

#temperature
cru.mean.temp <- read.csv("VC_CRU_meanT.csv", header=T)
cru.max.temp <- read.csv("VC_CRU_Tmax.csv", header=T)
cru.min.temp <- read.csv("VC_CRU_Tmin.csv", header=T)

iso.mean <- merge(cru.mean.temp, all.carbon, all.x=T, all.y=T)
iso.max <- merge(cru.max.temp, all.carbon, all.x=T, all.y=T)
iso.min <- merge(cru.min.temp, all.carbon, all.x=T, all.y=T)

#comparing adjusted cabon isotopes to mean temperature
#fir
iso.mean.fir <- lm(adj.c.fir ~ 1, data=iso.mean[80:109,], na.action=na.omit)
summary(iso.mean.fir)

step.iso.mean.fir <- stepAIC(iso.mean.fir, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                          jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.mean.fir)

#spruce
iso.mean.spruce <- lm(adj.c.spruce ~ 1, data=iso.mean[80:109,], na.action=na.omit)
summary(iso.mean.spruce)

step.iso.mean.spruce <- stepAIC(iso.mean.spruce, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                               jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.mean.spruce)

#pipo
iso.mean.pipo <- lm(adj.c.pipo ~ 1, data=iso.mean[80:109,], na.action=na.omit)
summary(iso.mean.pipo)

step.iso.mean.pipo <- stepAIC(iso.mean.pipo, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                               jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.mean.pipo)

#comparing adjusted cabon isotopes to min temperature
#fir
iso.min.fir <- lm(adj.c.fir ~ 1, data=iso.min[80:109,], na.action=na.omit)
summary(iso.min.fir)

step.iso.min.fir <- stepAIC(iso.min.fir, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                              jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.min.fir)

#spruce
iso.min.spruce <- lm(adj.c.spruce ~ 1, data=iso.min[80:109,], na.action=na.omit)
summary(iso.min.spruce)

step.iso.min.spruce <- stepAIC(iso.min.spruce, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                                 jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.min.spruce)

#pipo
iso.min.pipo <- lm(adj.c.pipo ~ 1, data=iso.min[80:109,], na.action=na.omit)
summary(iso.min.pipo)

step.iso.min.pipo <- stepAIC(iso.min.pipo, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                               jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.min.pipo)

#comparing adjusted cabon isotopes to max temperature
#fir
iso.max.fir <- lm(adj.c.fir ~ 1, data=iso.max[80:109,], na.action=na.omit)
summary(iso.max.fir)

step.iso.max.fir <- stepAIC(iso.max.fir, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                              jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.max.fir)

#spruce
iso.max.spruce <- lm(adj.c.spruce ~ 1, data=iso.max[80:109,], na.action=na.omit)
summary(iso.max.spruce)

step.iso.max.spruce <- stepAIC(iso.max.spruce, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                                 jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.max.spruce)

#pipo
iso.max.pipo <- lm(adj.c.pipo ~ 1, data=iso.max[80:109,], na.action=na.omit)
summary(iso.max.pipo)

step.iso.max.pipo <- stepAIC(iso.max.pipo, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                               jul+aug+sep+oct+nov+dec+mam+mjj, direction="both")
summary(step.iso.max.pipo)

#precipiatation
cru.precip <- read.csv("VC_CRU_precip.csv", header=T)

iso.precip <- merge(cru.precip, all.carbon, all.x=T, all.y=T)

#comparing adjusted carbon isotopes with mean precipitation
#fir
iso.precip.fir <- lm(adj.c.fir ~ 1, data=iso.precip[80:109,], na.action=na.omit)
summary(iso.precip.fir)

step.iso.precip.fir <- stepAIC(iso.precip.fir, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                               jul+aug+sep+oct+nov+dec+cool.p+warm.p, direction="both")
summary(step.iso.precip.fir)

#spruce
iso.precip.spruce <- lm(adj.c.spruce ~ 1, data=iso.precip[80:109,], na.action=na.omit)
summary(iso.precip.spruce)

step.iso.precip.spruce <- stepAIC(iso.precip.spruce, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                                    jul+aug+sep+oct+nov+dec+cool.p+warm.p, direction="both")
summary(step.iso.precip.spruce)

#pipo
iso.precip.pipo <- lm(adj.c.pipo ~ 1, data=iso.precip[80:109,], na.action=na.omit)
summary(iso.precip.pipo)

step.iso.precip.pipo <- stepAIC(iso.precip.pipo, ~ poct+pnov+pdec+jan+feb+mar+apr+may+jun+
                                  jul+aug+sep+oct+nov+dec+cool.p+warm.p, direction="both")
summary(step.iso.precip.pipo)

#PDSI
vc.pdsi <- read.csv("VC_PDSI.csv", header=T)

iso.pdsi <- merge(vc.pdsi, all.carbon, all.x=T, all.y=T)

#comparing adjusted carbon isotopes with pdsi
#fir
iso.pdsi.fir <- lm(adj.c.fir ~ 1, data=iso.pdsi[128:158,], na.action=na.omit)
summary(iso.pdsi.fir)

step.iso.pdsi.fir <- stepAIC(iso.pdsi.fir, ~ poct+pnov+pdec+Jan+Feb+Mar+Apr+May+Jun+
                               Jul+Aug+Sep+Oct+Nov+Dec+cool.pdsi+warm.pdsi, direction="both")
summary(step.iso.pdsi.fir)

#spruce
iso.pdsi.spruce <- lm(adj.c.spruce ~ 1, data=iso.pdsi[128:158,], na.action=na.omit)
summary(iso.pdsi.spruce)

step.iso.pdsi.spruce <- stepAIC(iso.pdsi.spruce, ~ poct+pnov+pdec+Jan+Feb+Mar+Apr+May+Jun+
                                  Jul+Aug+Sep+Oct+Nov+Dec+cool.pdsi+warm.pdsi, direction="both")
summary(step.iso.pdsi.spruce)

#pipo
iso.pdsi.pipo <- lm(adj.c.pipo ~ 1, data=iso.pdsi[128:158,], na.action=na.omit)
summary(iso.pdsi.pipo)

step.iso.pdsi.pipo <- stepAIC(iso.pdsi.pipo, ~ poct+pnov+pdec+Jan+Feb+Mar+Apr+May+Jun+
                                Jul+Aug+Sep+Oct+Nov+Dec+cool.pdsi+warm.pdsi, direction="both")

summary(step.iso.pdsi.pipo)

#################################
#checking oxygen isotopes against vpd
valles.o.vpd <- merge(valles.vpd, all.oxygen, all.x=T, all.y=T)

summary(valles.o.vpd)

o.lm.fir <- lm(o.fir ~ 1, data=valles.o.vpd[79:108,], na.action=na.omit)
summary(o.lm.fir)

step.o.fir <- stepAIC(o.lm.fir, ~ jan+feb+mar+apr+may+jun+
                             jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.o.fir)

#spruce
o.lm.spruce <- lm(o.spruce ~ 1, data=valles.o.vpd[79:108,], na.action=na.omit)
summary(o.lm.spruce)

step.o.spruce <- stepAIC(o.lm.spruce, ~ jan+feb+mar+apr+may+jun+
                          jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.o.spruce)

#pipo
o.lm.pipo <- lm(o.pine ~ 1, data=valles.o.vpd[79:108,], na.action=na.omit)
summary(o.lm.pipo)

step.o.pipo <- stepAIC(o.lm.pipo, ~ jan+feb+mar+apr+may+jun+
                           jul+aug+sep+oct+nov+dec+oct.mar+apr.sept, direction="both")
summary(step.o.pipo)

###############################################
#looking at first differnces of the carbon isotopes