#Patrick Derviche

#load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, spatial.tools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools,sdm, raster)

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")

#### 01
#### Occurence records


##GBIF

#Leptogorgia punicea
Leptogorgia.punicea.gbif <-gbif("Leptogorgia", species="punicea*")
Leptogorgia.punicea.gbif <- Leptogorgia.punicea.gbif[Leptogorgia.punicea.gbif$year>1950,]
Leptogorgia.punicea.gbif<-Leptogorgia.punicea.gbif[,c("lon","lat", "species")]
Leptogorgia.punicea.gbif <- na.omit (Leptogorgia.punicea.gbif)
dups2 <- duplicated(Leptogorgia.punicea.gbif[, c("lon", "lat")])
sum(dups2)
Leptogorgia.punicea.gbif <- Leptogorgia.punicea.gbif[!dups2, ]
summary(Leptogorgia.punicea.gbif)
reference<-rep("GBIF",nrow(Leptogorgia.punicea.gbif))
Leptogorgia.punicea.gbif<-cbind(Leptogorgia.punicea.gbif,reference)
Leptogorgia.punicea.gbif
write.table(Leptogorgia.punicea.gbif,"Leptogorgia.punicea.gbif.csv", sep=";", dec=".",row.names = F)

#Carijoa riisei
Carijoa.riisei.gbif <-gbif("Carijoa", species="riisei*")
Carijoa.riisei.gbif <- Carijoa.riisei.gbif[Carijoa.riisei.gbif$year>1950,]
Carijoa.riisei.gbif<-Carijoa.riisei.gbif[,c("lon","lat", "species")]
Carijoa.riisei.gbif <- na.omit (Carijoa.riisei.gbif)
dups2 <- duplicated(Carijoa.riisei.gbif[, c("lon", "lat")])
sum(dups2)
Carijoa.riisei.gbif <- Carijoa.riisei.gbif[!dups2, ]
summary(Carijoa.riisei.gbif)
reference<-rep("GBIF",nrow(Carijoa.riisei.gbif))
Carijoa.riisei.gbif<-cbind(Carijoa.riisei.gbif,reference)
Carijoa.riisei.gbif
write.table(Carijoa.riisei.gbif,"Carijoa.riisei.gbif.csv", sep=";", dec=".",row.names = F)


##OBIS
#Leptogorgia punicea
Leptogorgia.punicea.obis<-read.csv("Leptogorgia.punicea.obis.csv", h=T, sep=";", dec=".")
Leptogorgia.punicea.obis<-Leptogorgia.punicea.obis[Leptogorgia.punicea.obis$year >= 1950,]
Leptogorgia.punicea.obis<-Leptogorgia.punicea.obis[,c("decimalLongitude","decimalLatitude","scientificName")]
Leptogorgia.punicea.obis <- na.omit (Leptogorgia.punicea.obis)
reference<-rep("OBIS",nrow(Leptogorgia.punicea.obis))
Leptogorgia.punicea.obis<-cbind(Leptogorgia.punicea.obis,reference)
Leptogorgia.punicea.obis
write.table(Leptogorgia.punicea.obis,"Leptogorgia.punicea.obis.csv", sep=";", dec=".",row.names = F)
Leptogorgia.punicea.obis<-read.csv("Leptogorgia.punicea.obis.csv", sep = ";", dec = ".", h=T)

#Carijoa riisei
Carijoa.riisei.obis<-read.csv("Carijoa.riisei.obis.csv", h=T, sep=";", dec=".")
Carijoa.riisei.obis<-Carijoa.riisei.obis[Carijoa.riisei.obis$year >= 1950,]
Carijoa.riisei.obis<-Carijoa.riisei.obis[,c("decimalLongitude","decimalLatitude","scientificName")]
Carijoa.riisei.obis <- na.omit (Carijoa.riisei.obis)
reference<-rep("OBIS",nrow(Carijoa.riisei.obis))
Carijoa.riisei.obis<-cbind(Carijoa.riisei.obis,reference)
Carijoa.riisei.obis
write.table(Carijoa.riisei.obis,"Carijoa.riisei.obis.csv", sep=";", dec=".",row.names = F)
Carijoa.riisei.obis<-read.csv("Carijoa.riisei.obis.csv", sep = ";", dec = ".", h=T)


#Combine data
#Leptogorgia punicea

Leptogorgia.punicea <- rbind(Leptogorgia.punicea.obis, Leptogorgia.punicea.gbif)
dups2 <- duplicated(Leptogorgia.punicea[, c("lon", "lat")])
sum(dups2)
Leptogorgia.punicea <- Leptogorgia.punicea[!dups2, ]
Leptogorgia.punicea <- na.omit (Leptogorgia.punicea)
summary(Leptogorgia.punicea)
Leptogorgia.punicea
write.table(Leptogorgia.punicea,"Leptogorgia.punicea.csv", sep=";", dec=".",row.names = F)

#Carijoa riisei
Carijoa.riisei <- rbind(Carijoa.riisei.obis, Carijoa.riisei.gbif)
dups2 <- duplicated(Carijoa.riisei[, c("lon", "lat")])
sum(dups2)
Carijoa.riisei <- Carijoa.riisei[!dups2, ]
Carijoa.riisei <- na.omit (Carijoa.riisei)
summary(Carijoa.riisei)
Carijoa.riisei
write.table(Carijoa.riisei,"Carijoa.riisei.csv", sep=";", dec=".",row.names = F)


#Plot in map
data(wrld_simpl)
projection(wrld_simpl)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(wrld_simpl, axes=T)

e1<-extent(c(-100,-25, -40, 40))
atlantic <- crop(wrld_simpl,e1)
plot(atlantic, axes=T)
plot(atlanticpacific, axes=T)

points(Leptogorgia.punicea[,"lon"], Leptogorgia.punicea[,"lat"], pch= 20, col="red", cex=1)
points(Carijoa.riisei.ENM[,"lon"], Carijoa.riisei.ENM[,"lat"], pch= 20, col="green", cex=1)

#Add reference
coordinates (Leptogorgia.punicea) <- ~lon+lat
coordinates (Carijoa.riisei) <- ~lon+lat
projection(Leptogorgia.punicea) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
projection(Carijoa.riisei) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")


#### 02
#### Environmental variables

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Modelagem/arquivos R")
atlanticpacific<-stack("atlanticpacific")
plot(atlanticpacific)

library("usdm")
vif(atlanticpacific)
vifcor(atlanticpacific,th=0.9)
vifstep(atlanticpacific,th=10)

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
Leptogorgia.punicea<-read.csv("Leptogorgia.punicea.csv", sep = ";", dec = ".", h=T)
Carijoa.riisei<-read.csv("Carijoa.riisei.csv", sep = ";", dec = ".", h=T)

data(wrld_simpl)
plot(wrld_simpl, axes=T)
points(Leptogorgia.punicea[,"lon"], Leptogorgia.punicea[,"lat"], pch= 20, col="red", cex=1)
points(Carijoa.riisei[,"lon"], Carijoa.riisei[,"lat"], pch= 20, col="green", cex=1)


#Extract values from each pixel
env.var <- values(atlanticpacific)
#Extract coordinates de cada pixel
coord <- xyFromCell(atlanticpacific, 1:ncell(atlanticpacific))
#Combine values and coordinates
env.var <- cbind(coord, env.var)
colnames(env.var)[1:2] <- c("lon", "lat")
#Eliminate NAs
env.var.table <- na.omit(env.var)
#Rename columns
colnames(env.var.table)[1:2] <- c("lon", "lat")

#Leptogorgia punicea
valores.Leptogorgia.punicea <- extract(atlanticpacific, Leptogorgia.punicea[,1:2], cellnumbers=T)
data.Leptogorgia.punicea <- cbind (Leptogorgia.punicea[,1:4], valores.Leptogorgia.punicea)
duplicated(data.Leptogorgia.punicea[,"cells"])
dup <- which(duplicated(data.Leptogorgia.punicea[,"cells"]) == TRUE)
data.Leptogorgia.punicea <- data.Leptogorgia.punicea[-dup,] 
Leptogorgia.punicea.ENM<- na.omit(data.Leptogorgia.punicea)
sp<-rep("1",nrow(Leptogorgia.punicea.ENM))
Leptogorgia.punicea.ENM<-cbind(Leptogorgia.punicea.ENM,sp)
Leptogorgia.punicea.ENM
write.table(Leptogorgia.punicea.ENM,"Leptogorgia.punicea.ENM.csv", sep=";", dec="." ,row.names = F)
Leptogorgia.punicea.ENM

#Carijoa riisei
valores.Carijoa.riisei <- extract(atlanticpacific, Carijoa.riisei[,1:2], cellnumbers=T)
data.Carijoa.riisei <- cbind (Carijoa.riisei[,1:4], valores.Carijoa.riisei)
duplicated(data.Carijoa.riisei[,"cells"])
dup <- which(duplicated(data.Carijoa.riisei[,"cells"]) == TRUE)
data.Carijoa.riisei <- data.Carijoa.riisei[-dup,] 
Carijoa.riisei.ENM<- na.omit(data.Carijoa.riisei)
sp<-rep("1",nrow(Carijoa.riisei.ENM))
Carijoa.riisei.ENM<-cbind(Carijoa.riisei.ENM,sp)
Carijoa.riisei.ENM
write.table(Carijoa.riisei.ENM,"Carijoa.riisei.ENM.csv", sep=";", dec="." ,row.names = F)
Carijoa.riisei.ENM


#Final product
Leptogorgia.punicea.ENM <- read.table("Leptogorgia.punicea.ENM.csv", header = TRUE, sep=";", dec=".")
Leptogorgia.punicea <- read.table("Leptogorgia.punicea.csv", header = TRUE, sep=";", dec=".")
summary(Leptogorgia.punicea.ENM)
summary(Leptogorgia.punicea)

Carijoa.riisei.ENM <- read.table("Carijoa.riisei.ENM.csv", header = TRUE, sep=";", dec=".")
Carijoa.riisei <- read.table("Carijoa.riisei.csv", header = TRUE, sep=";", dec=".")
summary(Carijoa.riisei.ENM)
summary(Carijoa.riisei)

data(wrld_simpl)
plot(wrld_simpl, axes=T)
points(Leptogorgia.punicea.ENM[,"lon"], Leptogorgia.punicea.ENM[,"lat"], pch= 20, col="red", cex=1)
points(Carijoa.riisei.ENM[,"lon"], Carijoa.riisei.ENM[,"lat"], pch= 20, col="green", cex=1)
