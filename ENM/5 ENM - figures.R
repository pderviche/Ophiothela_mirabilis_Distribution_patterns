#Distribution patterns and habitat suitability of the non-native brittle star Ophiothela mirabilis Verrill, 1867 along the Western Atlantic 
#Patrick Derviche, Angeline Saucsen, Daphne Spier, Paulo Lana

#### 5. Figures

# 5.1. Open files
# raster
model.ensemble <-stack("ophiothelamirabilis")
# shapefiles  Wessel and Smith (1996) and  Spalding et al. (2007)
coastline <- shapefile("GSHHS_f_L1.shp")
coastline <- shapefile("GSHHS_i_L1.shp")
lakes <- shapefile("GSHHS_i_L2.shp")
countries <- shapefile("WDBII_border_i_L1.shp")
states <- shapefile("WDBII_border_i_L2.shp")
spalding <- shapefile("meow_ecos_expl_clipped_expl.shp")
plot(spalding, add=T)
head(spalding)
str(spalding@data)
spalding@data  <- spalding@data %>%
  mutate(color = ifelse(ECOREGION == "ECOREGION", "red", "green"))
plot(spalding, col = spalding@data$color)

# 5.2. S Atlantic
e1<-extent(c(-60,-25, -40, 5)) #Atlantico Sul
model.ensemble.south.atlantic <- crop(model.ensemble,e1)
coastline.south.atlantic <- crop(coastline, e1)
lakes.south.atlantic <- crop(lakes, e1)
countries.south.atlantic <- crop(countries, e1)
states.south.atlantic <- crop(states, e1)
projection(model.ensemble.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
colfunc(100)

plot(model.ensemble.south.atlantic, col= colfunc(100),xlim=c(-60,-20),ylim=c(-40, 5), zlim=c(-0.01,1))
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic,col="gray10", add=T)


# 5.3. N Atlantic
e2<-extent(c(-100,-50, 5, 40)) #Atlantico Norte
model.ensemble.north.atlantic <- crop(model.ensemble,e2)
coastline.north.atlantic <- crop(coastline, e2)
lakes.north.atlantic <- crop(lakes, e2)
countries.north.atlantic <- crop(countries, e2)
states.north.atlantic <- crop(states, e2)
projection(model.ensemble.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(model.ensemble.north.atlantic, col= colfunc(100),xlim=c(-100,-50),ylim=c(5, 40), zlim=c(-0.01,1))
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic,col="gray10", add=T)
plot(lakes.north.atlantic,  legend = F, add=T)

# 5.4. Binary maps
#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model<-read.csv("evaluation.model.csv", sep = ";", dec = ".", h=T)
model.ensemble <-stack("ophiothelamirabilis")
binary.model<-model.ensemble>=mean(evaluation.model[,5])

#S Atlantic
e1<-extent(c(-60,-25, -40, 5)) #Atlantico Sul
binary.model.south.atlantic <- crop(binary.model,e1)
projection(binary.model.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(binary.model.south.atlantic, col=c("white","red"),  legend = F)
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic, add=T)

#N Atlantic
e2<-extent(c(-100,-50, 5, 40)) #Atlantico Norte
binary.model.north.atlantic <- crop(binary.model,e2)
projection(binary.model.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(binary.model.north.atlantic, col=c("white","red"),  legend = F)
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.north.atlantic,  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic, add=T)

#Atlantic
e<-extent(c(-120,-30, -40, 40)) #Atlantic
model.ensemble.atlantic <- crop(model.ensemble,e)
coastline.atlantic <- crop(coastline, e)
lakes.atlantic <- crop(lakes, e)
countries.atlantic <- crop(countries, e)
states.atlantic <- crop(states, e)
projection(model.ensemble)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(model.ensemble, col= colfunc(100), zlim=c(-0.01,1))
plot(coastline.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.atlantic,  legend = F, add=T)
plot(states.atlantic, col="gray", add=T)
plot(countries.atlantic, add=T)

# 5.5. Graphical abstract
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

plot(model.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))
plot(coastline.occurence, col="gray90", border="black",  add = T)
plot(states.occurence, col="gray", add=T)
plot(countries.occurence, add=T)

points(presence.atlantic[,"lon"], presence.atlantic[,"lat"], pch= 21, col="black", bg="red", cex=1.2)
points(presence.pacific[,"lon"], presence.pacific[,"lat"], pch= 21, col="black", bg="skyblue1", cex=1.2)

dev.copy(jpeg,filename="plot.jpg")

#END