#Patrick Derviche
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools, sdm)


memory.limit(size=78000)
setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")

#### Realized niche

#Ophiothela mirabilis
model.ensemble.Ophiothelamirabilis <-stack("ophiothelamirabilis")
evaluation.mode.Ophiothelamirabilisl<-read.csv("evaluation.model.csv", sep = ";", dec = ".", h=T)
binary.model.Ophiothelamirabilis<-model.ensemble.Ophiothelamirabilis>=mean(evaluation.mode.Ophiothelamirabilisl[,5])
projection(binary.model.Ophiothelamirabilis)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Leptogorgia punicea
evaluation.model.Leptogorgia.punicea <- read.csv("evaluation.model.Leptogorgia.punicea.csv", sep = ";", dec = ".", h=T)
Leptogorgia.punicea.ensemble <- stack("Leptogorgia.punicea.ensemble.gri")
binary.model.Leptogorgia.punicea<-Leptogorgia.punicea.ensemble>=mean(evaluation.model.Leptogorgia.punicea[,5])
projection(binary.model.Leptogorgia.punicea)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Carijo riisei

evaluation.model.Carijoa.riisei <- read.csv("evaluation.model.Carijoa.riisei.csv", sep = ";", dec = ".", h=T)
Carijoa.riisei.ensemble <- stack("Carijoa.riisei.ensemble.gri")
binary.model.Carijoa.riisei<-Carijoa.riisei.ensemble>=mean(evaluation.model.Carijoa.riisei[,5])
projection(binary.model.Carijoa.riisei)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"


#This function measures similarity in the geographic distribution of suitability scores from two ENMs. 
#It returns two metrics, I and D. These metrics are described in Warren et al. 2008.

library("ENMTools")

raster.overlap(binary.model.Ophiothelamirabilis, binary.model.Leptogorgia.punicea)
raster.overlap(binary.model.Ophiothelamirabilis, binary.model.Carijoa.riisei)

e1<-extent(c(-60,-25, -40, 5)) #S Atlantic
e2<-extent(c(-100,-50, 5, 40)) #N Atlantic
Om.South <- crop(binary.model.Ophiothelamirabilis,e1)
Lp.South <- crop(binary.model.Leptogorgia.punicea,e1)
Cr.South <- crop(binary.model.Carijoa.riisei,e1)
Om.North <- crop(binary.model.Ophiothelamirabilis,e2)
Lp.North <- crop(binary.model.Leptogorgia.punicea,e2)
Cr.North <- crop(binary.model.Carijoa.riisei,e2)

raster.overlap(Om.South, Lp.South)
raster.overlap(Om.North, Lp.North)

#Leptogorgia punicea 
#(Southern Atlantic: D = 0.350, I = 0.564, and RC =  0.545; 
# Northern Atlantic: D = 0.243, I = 0.476, and RC =  0.445)

raster.overlap(Om.South, Cr.South)
raster.overlap(Om.North, Cr.North)

#Carijoa riisei 
#(Southern Atlantic: D = 0.777, I = 0.793, and RC =  0.775; 
#  Northern Atlantic: D = 0.712, I = 0.765, and RC =  0.731)


######
######
###### Overlap

p <- overlay(binary.model.Leptogorgia.punicea, binary.model.Carijoa.riisei, fun=sum)
host<-stack(p)
oph<-stack(binary.model.Ophiothelamirabilis)
     

oph=oph+1
oph[oph<2] <- NA

host[host<1] <- NA
host[host>1] <- 1

y <- overlay(oph, host, fun=sum)
y<-stack(y)


plot(oph, col= "blue",legend=F)
plot(host, add = T, col= "yellow",legend=F)
plot(y, col= "green",legend=F, add=T)

######
######
###### Overlap calculating

### % Overlap host
oph
summary(oph)
#Pixels= 44864
host
summary(host)
#32944
y #Host
summary(y) #Host
#Pixels= 27023

### % Overlap carijoa
oph
summary(oph)
#Pixels= 44864
carijoa
summary(carijoa)
#32823
y #carijoa
summary(y) #carijoa
#Pixels= 27003


e1<-extent(c(-60,-25, -40, 5)) #S Atlantic
e2<-extent(c(-100,-50, 5, 40)) #N Atlantic

ophSouth <- crop(oph,e1)
hostSouth <- crop(host,e1)
ySouth <- crop(y,e1)

ophSouth
summary(ophSouth)
#194400 - 186608 = 7792
ySouth
summary(ySouth)
#194400 - 188092 = 6308
#Overlap % = 80.95%

ophNorth <- crop(oph,e2)
hostNorth <- crop(host,e2)
yNorth <- crop(y,e2)

ophNorth
summary(ophNorth)
#252000 - 226036 = 25964
yNorth
summary(yNorth)
#252000 - 233478 = 18522
#Overlap % = 71.34%

######
###### Figure

projection(oph)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(host)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(y)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/i")
coastline <- shapefile("GSHHS_i_L1.shp")
lakes <- shapefile("GSHHS_i_L2.shp")
setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/WDBII_shp/i")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/Figures/Fig 7")

#S Atlantic
e1<-extent(c(-60,-25, -40, 5)) #Atlantico Sul

ophS <- crop(oph,e1)
hostS <- crop(host,e1)
yS <- crop(y,e1)

coastlineS <- crop(coastline, e1)
lakesS <- crop(lakes, e1)
countriesS <- crop(countries, e1)
statesS <- crop(states, e1)


projection(ophS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(hostS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(yS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastlineS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakesS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countriesS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(statesS)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(ophS, col= "turquoise1",legend=F)
plot(hostS, add = T, col= "purple2",legend=F)
plot(coastlineS, col="white", border="black",  legend = F, add=T)
plot(statesS, col="gray", add=T)
plot(countriesS,col="black", add=T)
plot(yS, col= "red",legend=F, add=T)


#N Atlantic
e2<-extent(c(-100,-50, 5, 40)) #Atlantico Norte

ophN <- crop(oph,e2)
hostN <- crop(host,e2)
yN <- crop(y,e2)

coastlineN <- crop(coastline, e2)
lakesN <- crop(lakes, e2)
countriesN <- crop(countries, e2)
statesN <- crop(states, e2)

projection(coastlineN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakesN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countriesN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(statesN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(ophN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(hostN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(yN)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(ophN, col= "turquoise1",legend=F)
plot(hostN, add = T, col= "purple2",legend=F)
plot(coastlineN, col="white", border="black",  legend = F, add=T)
plot(statesN, col="gray", add=T)
plot(countriesN,col="black", add=T)
plot(yN, col= "red",legend=F, add=T)









