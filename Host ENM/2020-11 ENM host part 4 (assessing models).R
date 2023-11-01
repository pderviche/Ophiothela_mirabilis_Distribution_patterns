#Patrick Derviche

#load packages
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, spatial.tools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools, rJava, sdm)
#### 04
#### Assessing models

### 3.1 Leptogorgia punicea

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
atlanticpacific<-stack("atlanticpacific1")
Leptogorgia.punicea.presence <- read.table("Leptogorgia.punicea.ENM.csv", header = TRUE, sep=";", dec=".")
ENM.Leptogorgia.punicea<-stack("ENM.Leptogorgia.punicea")

#### 
#### Predicted probability of occurrence

valores <- extract(ENM.Leptogorgia.punicea, Leptogorgia.punicea.presence[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
p.p.o.Leptogorgia <- cbind (Leptogorgia.punicea.presence[,1:2], valores)
summary(p.p.o.Leptogorgia)
write.table(p.p.o.Leptogorgia,"p.p.o.Leptogorgia.csv", sep=";", dec=".",row.names = F)
boxplot(p.p.o.Leptogorgia$layer,
        xlab="Occurrence records", ylab ="Environmental suitability", 
        ylim=c(0,1), col = 'gray')


### Shapes Wessel and Spalding

#Abrir shapefiles
setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/f")
coastline <- shapefile("GSHHS_f_L1.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/i")
coastline <- shapefile("GSHHS_i_L1.shp")
lakes <- shapefile("GSHHS_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/WDBII_shp/i")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")

e1<-extent(c(-100,-25, -40, 40))
coastline <- crop(coastline,e1)
lakes <- crop(lakes,e1)
states <- crop(states,e1)
countries <- crop(countries,e1)

#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model.Leptogorgia.csv<-read.csv("evaluation.model.Leptogorgia.csv", sep = ";", dec = ".", h=T)
binary.model.Leptogorgia<-model.ensemble>=mean(evaluation.model.Leptogorgia.csv[,5])

projection(binary.model.Leptogorgia)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Leptogorgia, col=c("white","blue"),  legend = F)
plot(coastline, col="gray90", border="black",  legend = F, add=T)
plot(lakes,  legend = F, add=T)
plot(states, col="gray", add=T)
plot(countries, add=T)





















### 4.2 Carijoa riisei

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
atlanticpacific<-stack("atlanticpacific1")
Carijo.riisei.presence <- read.table("Carijo.riisei.ENM.csv", header = TRUE, sep=";", dec=".")
ENM.Carijo.riisei<-stack("ENM.Carijo.riisei")

#### 
#### Predicted probability of occurrence

valores <- extract(Leptogorgia.ENM, Carijo.riisei.presence[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
data <- cbind (Carijo.riisei.presence[,1:2], valores)
summary(p.p.o.Leptogorgia)
write.table(p.p.o.Leptogorgia,"p.p.o.Leptogorgia.csv", sep=";", dec=".",row.names = F)
boxplot(p.p.o.Leptogorgia$layer,
        xlab="Occurrence records", ylab ="Environmental suitability", 
        ylim=c(0,1), col = 'gray')


### Shapes Wessel and Spalding

#Abrir shapefiles
setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/f")
coastline <- shapefile("GSHHS_f_L1.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/i")
coastline <- shapefile("GSHHS_i_L1.shp")
lakes <- shapefile("GSHHS_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/WDBII_shp/i")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")

e1<-extent(c(-100,-25, -40, 40))
coastline <- crop(coastline,e1)
lakes <- crop(lakes,e1)
states <- crop(states,e1)
countries <- crop(countries,e1)

#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model.Leptogorgia.csv<-read.csv("evaluation.model.Leptogorgia.csv", sep = ";", dec = ".", h=T)
binary.model.Leptogorgia<-model.ensemble>=mean(evaluation.model.Leptogorgia.csv[,5])

projection(binary.model.Leptogorgia)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Leptogorgia, col=c("white","blue"),  legend = F)
plot(coastline, col="gray90", border="black",  legend = F, add=T)
plot(lakes,  legend = F, add=T)
plot(states, col="gray", add=T)
plot(countries, add=T)






















### 4.3 Mycale angulosa

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
atlanticpacific<-stack("atlanticpacific1")
Mycale.angulosa.presence <- read.table("Mycale.angulosa.ENM.csv", header = TRUE, sep=";", dec=".")
ENM.Mycale.angulosa<-stack("ENM.Mycale.angulosa")

#### 
#### Predicted probability of occurrence

valores <- extract(ENM.Mycale.angulosa, Mycale.angulosa.presence[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
data <- cbind (Mycale.angulosa.presence[,1:2], valores)
summary(p.p.o.Leptogorgia)
write.table(p.p.o.Leptogorgia,"p.p.o.Leptogorgia.csv", sep=";", dec=".",row.names = F)
boxplot(p.p.o.Leptogorgia$layer,
        xlab="Occurrence records", ylab ="Environmental suitability", 
        ylim=c(0,1), col = 'gray')


### Shapes Wessel and Spalding

#Abrir shapefiles
setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/f")
coastline <- shapefile("GSHHS_f_L1.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/i")
coastline <- shapefile("GSHHS_i_L1.shp")
lakes <- shapefile("GSHHS_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/WDBII_shp/i")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")

e1<-extent(c(-100,-25, -40, 40))
coastline <- crop(coastline,e1)
lakes <- crop(lakes,e1)
states <- crop(states,e1)
countries <- crop(countries,e1)

#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model.Leptogorgia.csv<-read.csv("evaluation.model.Leptogorgia.csv", sep = ";", dec = ".", h=T)
binary.model.Leptogorgia<-model.ensemble>=mean(evaluation.model.Leptogorgia.csv[,5])

projection(binary.model.Leptogorgia)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Leptogorgia, col=c("white","blue"),  legend = F)
plot(coastline, col="gray90", border="black",  legend = F, add=T)
plot(lakes,  legend = F, add=T)
plot(states, col="gray", add=T)
plot(countries, add=T)