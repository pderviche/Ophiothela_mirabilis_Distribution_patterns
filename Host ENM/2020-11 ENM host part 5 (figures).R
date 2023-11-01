#Patrick Derviche

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Figuras/Fig 7")

#### 05
#### Figures

### ENM
model.ensemble <-stack("ophiothelamirabilis")

### Shapes Wessel and Spalding

#Abrir shapefiles
setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/f")
coastline <- shapefile("GSHHS_f_L1.shp")
plot(coastline)

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/GSHHS_shp/i")
coastline <- shapefile("GSHHS_i_L1.shp")
lake.and.land <- shapefile("GSHHS_i_L2.shp")
plot(lake.and.land)
plot(coastline)

setwd("D:/Patrick Derviche/Documentos/LabBentos/2019 IC/WDBII_shp/i")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")
plot(countries)
plot(states, col="gray", add=T)

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Modelagem/Spalding 2007/data/commondata/data0")
spalding <- shapefile("meow_ecos_expl_clipped_expl.shp")
plot(spalding, add=T)
head(spalding)
str(spalding@data)
spalding@data  <- spalding@data %>%
  mutate(color = ifelse(ECOREGION == "ECOREGION", "red", "green"))
plot(spalding, col = spalding@data$color)

##### Figures
#S Atlantic
e1<-extent(c(-60,-25, -40, 5)) #Atlantico Sul

model.ensemble.south.atlantic <- crop(model.ensemble,e1)
coastline.south.atlantic <- crop(coastline, e1)
lake.and.land.south.atlantic <- crop(lake.and.land, e1)
countries.south.atlantic <- crop(countries, e1)
states.south.atlantic <- crop(states, e1)

projection(model.ensemble.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lake.and.land.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
colfunc(100)

plot(model.ensemble.south.atlantic, col= colfunc(100),xlim=c(-60,-20),ylim=c(-40, 5), zlim=c(-0.01,1))
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lake.and.land.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic,col="gray10", add=T)


#N Atlantic
e2<-extent(c(-100,-50, 5, 40)) #Atlantico Norte

model.ensemble.north.atlantic <- crop(model.ensemble,e2)
coastline.north.atlantic <- crop(coastline, e2)
lake.and.land.north.atlantic <- crop(lake.and.land, e2)
countries.north.atlantic <- crop(countries, e2)
states.north.atlantic <- crop(states, e2)

projection(model.ensemble.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lake.and.land.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(model.ensemble.north.atlantic, col= colfunc(100),xlim=c(-100,-50),ylim=c(5, 40), zlim=c(-0.01,1))
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic,col="gray10", add=T)
plot(lake.and.land.north.atlantic,  legend = F, add=T)


####
#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model<-read.csv("evaluation.model.csv", sep = ";", dec = ".", h=T)
binary.model<-model.ensemble>=mean(evaluation.model[,5])

#S Atlantic
binary.model.south.atlantic <- crop(binary.model,e1)
projection(binary.model.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.south.atlantic, col=c("white","red"),  legend = F)
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lake.and.land.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic, add=T)


#N Atlantic
binary.model.north.atlantic <- crop(binary.model,e2)
projection(binary.model.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.north.atlantic, col=c("white","red"),  legend = F)
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lake.and.land.north.atlantic,  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic, add=T)

#Atlantic
e<-extent(c(-120,-30, -40, 40)) #Atlantic

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Modelagem/arquivos R")


model.ensemble.atlantic <- crop(model.ensemble,e)
coastline.atlantic <- crop(coastline, e)
lake.and.land.atlantic <- crop(lake.and.land, e)
countries.atlantic <- crop(countries, e)
states.atlantic <- crop(states, e)

projection(model.ensemble)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lake.and.land.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(model.ensemble, col= colfunc(100), zlim=c(-0.01,1))
plot(coastline.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lake.and.land.atlantic,  legend = F, add=T)
plot(states.atlantic, col="gray", add=T)
plot(countries.atlantic, add=T)

#Graphical abstract
setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Modelagem/arquivos R")

model.ensemble<-stack("ophiothelamirabilis")
presence <- read.table("presence.csv", header = TRUE, sep=";", dec=".")
presence.atlantic <- read.table("presence - atlantic.csv", header = TRUE, sep=";", dec=".")
presence.pacific <- read.table("presence - pacific.csv", header = TRUE, sep=";", dec=".")


projection(model.ensemble)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

coastline.occurence <- crop(coastline,extent(c(-120,-20, -40,40)))
states.occurence <- crop(states,extent(c(-120,-20, -40,40)))
countries.occurence <- crop(countries,extent(c(-120,-20, -40,40)))


colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Modelagem/arquivos R")

plot(model.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))
plot(coastline.occurence, col="gray90", border="black",  add = T)
plot(states.occurence, col="gray", add=T)
plot(countries.occurence, add=T)

points(presence.atlantic[,"lon"], presence.atlantic[,"lat"], pch= 21, col="black", bg="red", cex=1.2)
points(presence.pacific[,"lon"], presence.pacific[,"lat"], pch= 21, col="black", bg="skyblue1", cex=1.2)

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Figuras/Graphical abstract")
dev.copy(jpeg,filename="plot.jpg");
png(filename="D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Figuras/Graphical abstract/graphicalabstract.png")

