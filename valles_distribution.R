library(ggplot2)


#################################################################################################
# Graphing Size class distributions of species & dated stems
#################################################################################################
vuf.data <- read.csv("vuf_diams.csv", na.strings=c("", "NA", "*"))
vuf.data$plot <- as.factor(vuf.data$plot)
summary(vuf.data)

vlf.data <- read.csv("vlf_diams.csv", na.strings=c("", "NA", "*"))
vlf.data$plot <- as.factor(vlf.data$plot)
summary(vlf.data)

library(dplR)

vuf.dated <- read.csv("vuf_final_all.csv", header=T, row.names=1)
head(vuf.dated)


vlf.dated <- read.csv("vlf_final_all.csv", header=T, row.names=1)
head(vlf.dated)



# Subsetting only the sites & species I have full data for right now
#sites <- unique(establishment$Site)
#sites

#species <- unique(establishment$Spp)
#species

############
# subsetting trees belonging to completed sites
#tree.data2 <- tree.data[tree.data$Site %in% sites,]
#summary(tree.data2)
#dim(tree.data)
#dim(tree.data2)

# making a vector of trees that were dated (have stablishment)
#tree.id <- as.vector(unique(establishment$TreeID))
#tree.id[1:10]
#length(tree.id)

# Alternative approach depending on your data
vuf.id <- as.vector(colnames(vuf.dated))
vuf.id[1:10] # checking names
length(vuf.id) # checking size
vuf.id2<- unique(substr(vuf.id,1,6))
vuf.id2[1:10]

vlf.id <- as.vector(colnames(vlf.dated))
vlf.id[1:10] # checking names
length(vlf.id) # checking size
vlf.id2<- unique(substr(vlf.id,1,6))
vlf.id2[1:10]



# Making a binary column of whether the tree was dated or not
for(i in 1:length(vuf.data$id)){
	vuf.data$Dated[i] <- ifelse(vuf.data$id[i] %in% vuf.id2, "YES", "NO")
}
vuf.data$Dated <- as.factor(vuf.data$Dated) # making a factor (because sometimes goes weird)
vuf.data$Spp.Dated <- as.factor(paste(vuf.data$spp, vuf.data$Dated, sep=".")) # don't worry about this (something I"m playing with)
summary(vuf.data)

for(i in 1:length(vlf.data$id)){
  vlf.data$Dated[i] <- ifelse(vlf.data$id[i] %in% vlf.id2, "YES", "NO")
}
vlf.data$Dated <- as.factor(vlf.data$Dated) # making a factor (because sometimes goes weird)
vlf.data$Spp.Dated <- as.factor(paste(vlf.data$spp, vlf.data$Dated, sep=".")) # don't worry about this (something I"m playing with)
summary(vlf.data)

###########
# merging sites together into one file
summary(vlf.data)
summary(vuf.data)

all.valles <- rbind(vlf.data,vuf.data)
all.valles$site <- as.factor(substr(all.valles$id,1,3))
summary(all.valles)

############
# Reading in a file that has species names and colors
#group.col <- read.csv("GroupColors.csv")
#summary(group.col)

# making bins for your distribution
dbh.bins1 <- seq(0, max(all.valles$dbh, na.rm=T), 2) # 5 year bins based on the range of your trees
#dbh.bins2 <- c(seq(0, 40, 5), Inf) # 5 year bins that stop at 40 cm

# making subsetting species from master lsit that are actually in the data (gets off otherwise)
#spp.list.tree <- unique(tree.data2[!tree.data2$Site=="IRN", "Spp"])
#spp.col.tree <- spp.col[spp.col$Spp %in% spp.list.tree,]
#length(spp.list.tree)
#dim(spp.col.tree)

# Plotting species by size distribution
qplot(x=dbh, data=all.valles, geom="histogram", breaks=dbh.bins1, fill=spp) + facet_grid(site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") #+ scale_fill_manual(values=as.vector(spp.col.tree$Color))

# Plotting species by Dated or Not
qplot(x=dbh, data=all.valles, geom="histogram", breaks=dbh.bins1, fill=Dated) + facet_grid(site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "blue"))

# Plotting species by Dated or Not, removing saplings
#qplot(x=DBH, data=tree.data2[!tree.data2$Site=="IRN" & tree.data2$DBH>5,], geom="histogram", breaks=dbh.bins1, fill=Dated) + facet_grid(Site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "gray30"))

# Plotting species by Dated or Not, removing saplings & dead stuff
#qplot(x=DBH, data=tree.data2[!tree.data2$Site=="IRN" & tree.data2$DBH>5 & tree.data2$Live=="LIVE",], geom="histogram", breaks=dbh.bins1, fill=Dated) + facet_grid(Site ~ .) + theme(axis.line=element_line(color="black", size=0.5), panel.grid.major=element_blank(), panel.grid.minor= element_blank(), panel.border= element_blank(), panel.background= element_blank(), axis.text.x=element_text(angle=0, color="black", size=12), axis.text.y=element_text(color="black", size=12)) + scale_x_continuous(name="DBH") + ggtitle("Size Distribution") + scale_fill_manual(values=c("gray80", "gray30"))

#calculating the percent dated vs. undated in each bin of histogram
bins.2 <- seq(from=0, to=60, by=2) #2 indicates the length of the bin, in this case 2cm 
all.valles$bin2 <- cut(all.valles$dbh, breaks=c(bins.2))

summary(all.valles)

all.valles$bin2b <- substr(paste(all.valles$bin2),2,10)

l1 <- length(unique(all.valles$site))*length(unique(all.valles$bin2))
test <- as.data.frame(vector(length=l1))
test[,1] <- unique(all.valles$site)
test <- as.data.frame(test[sort(test[,1]),])
test[,2] <- unique(paste(all.valles$bin2))
names(test) <- c("site", "bin2")
for(i in unique(all.valles$site)){
  for(j in unique(all.valles$bin2)){
    test[test$bin2==j & test$site==i,"perc.dated"] <- length(all.valles[all.valles$bin2==j & all.valles$site==i & all.valles$Dated=="YES","Dated"])/length(all.valles[all.valles$bin2==j & all.valles$site==i,"Dated"])
  }
}

summary(test)
print(test)

library(reshape2)

fancy.table1 <- melt(test2, na.rm=F)

fancy.table2 <- dcast(fancy.table1, bin2~site)

###################################
#biomass calculations
#merge site measurements from VLF and VUF

vlf.dated2 <- vlf.dated
vlf.dated2$year <- row.names(vlf.dated)

vuf.dated2 <- vuf.dated
vuf.dated2$year <- row.names(vuf.dated)

all.dated <- merge(vlf.dated2, vuf.dated2, all.x=T, all.y=T)
all.dated[all.dated == 0] <- NA
row.names(all.dated) <- all.dated$year
#converting the Tsap measurements (currently in 1/100mm) to cm to match the DBH file we will use later
all.dated2 <- all.dated[,2:ncol(all.dated)]/1000

# Making data frame with potential core ID & DBH
ID <- as.data.frame(c(paste(all.valles$id, "A", sep=""), paste(all.valles$id, "B", sep=""), paste(all.valles$id, "C", sep="")))
names(ID) <- "ID"
for(i in unique(all.valles$id)){
  ID[substr(ID$ID, 1,6)==i, "DBH"] <- all.valles[all.valles$id==i,"dbh"]
}

# checking to make sure it worked
ID[substr(ID$ID, 1, 6)=="VUF151",]

summary(ID)

#subsetting the DBH list to match what cores were actually dated
core.names <- names(all.dated2)

ID2 <- ID[ID$ID %in% core.names,]

#order both ID2 and all.dated2 alphabetically so that they align
ID2 <- ID2[order(ID2$ID),]
all.dated2 <- all.dated2[order(row.names(all.dated2), decreasing=T),order(names(all.dated2))]
?order

#diameter reconstructions
dbh.recon <- all.dated2

for(j in seq_along(dbh.recon)){
  # inserting 2012 DBH
  dbh.recon[1,j] <- ID2[ID2$ID==names(dbh.recon[j]),"DBH"] 
  for(i in 2:(length(dbh.recon[,j]))){
    dbh.recon[i,j] <- ifelse(!is.na(all.dated2[i,j]), dbh.recon[i-1,j] - all.dated2[i-1,j], NA) # subtracting the previous year's growth from DBH to get that year's DBH
  }
}

#checking for negative diameters that will need to be removed or switch to inside out orientation (call Christy)
min(dbh.recon, na.rm=T)

############################
#applying allometric equations to the diameter reconstruction
#Jenkins 2003 (Pine)== AGB=Exp(-2.5356 + 2.4349  lna〖dbh〗 ) biomass ratio parameters. AGB is in kilograms (kg).
#Jenkins 2003 (Spruce)==AGB=Exp(-2.0773 + 2.3323  lna〖dbh〗 
#Navar-Chaidez== P. arizonica  AGB= 0.0572(DBH)^2.5569±0.055
#Tyson spruce == AGB=0.155±0.039(dbh)^2.334±0.063
#Tyson PIPO == AGB=0.054±0.008(dbh)^2.651±0.035
#Tyson general valles== AGB= 0.063±0.007(DBH)^2.615±0.028
#Tyson pine dominant == AGB= 0.0546±0.0085(DBH)^2.64±0.037
#tyson mixed conifer == AGN = 0.0961±0.020(DBH)^2.493±0.048

#put these values in a separate .csv to call upon for calculations

equations <- read.csv("allometric_eqtns.csv", header=T)
summary(equations)

dbh.recon.vlf <- dbh.recon[,substr(names(dbh.recon), 1, 3)=="VLF"]
dbh.recon.vuf <- dbh.recon[,substr(names(dbh.recon), 1, 3)=="VUF"]

write.csv(dbh.recon.vlf, "dbh.recon.vlf.csv")

dbh.recon.vlf <- read.csv("dbh.recon.vlf.csv")

jenkins.pine <- exp(equations[equations$model=="jenkins" & equations$spp=="pine", "beta0"] 
                    + equations[equations$model=="jenkins" & equations$spp=="pine", "beta1"]
                    * log(dbh.recon.vlf))
for(i in 1:length(jenkins.pine[,1])){
  jenkins.pine[i,"average.tree"]<- mean(jenkins.pine[i,1:length(dbh.recon.vlf)], na.rm=T) 
}
summary(jenkins.pine)

mean(jenkins.pine[,1], na.rm=T)
mean(jenkins.pine[1,], na.rm=T)

sum(jenkins.pine[1,1:10], na.rm=T)
ncol(jenkins.pine[1,1:10])
sum(jenkins.pine[1,1:10], na.rm=T)/ncol(jenkins.pine[1,1:10])


summary(jenkins.pine$average.tree)

warnings()
