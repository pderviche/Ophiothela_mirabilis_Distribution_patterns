#Distribution patterns and habitat suitability of the non-native brittle star Ophiothela mirabilis Verrill, 1867 along the Western Atlantic 
#Patrick Derviche, Angeline Saucsen, Daphne Spier, Paulo Lana

#Packages
install.packages("pacman")
if(!require(pacman)) install.packages("pacman")
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, spatial.tools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools, gbif)

#### 1. Occurence records

# 1.1. Import data from GBIF
gbif.danae <-gbif("Ophiothela", species="danae*")
gbif.mirabilis <-gbif("Ophiothela", species="mirabilis*")
#Select records by year (from 1950)
gbif.danae <- gbif.danae[gbif.danae$year>1950,]
gbif.mirabilis <- gbif.mirabilis[gbif.mirabilis$year>1950,]
#Select columns 
gbif.danae<-gbif.danae[,c("lon","lat", "species")]
gbif.mirabilis<-gbif.mirabilis[,c("lon","lat", "species")]
#Eliminate NAs
gbif.danae <- na.omit (gbif.danae)
gbif.mirabilis <- na.omit (gbif.mirabilis)
gbif <- rbind(gbif.mirabilis, gbif.danae)
#Check for duplicate coordinates
dups2 <- duplicated(gbif[, c("lon", "lat")])
sum(dups2)
gbif <- gbif[!dups2, ]
summary(gbif)
summary(gbif.mirabilis)
summary(gbif.danae)
#Add new column
reference<-rep("GBIF",nrow(gbif))
gbif<-cbind(gbif,reference)
gbif
summary(gbif)
write.table(gbif,"gbif.csv", sep=";", dec=".",row.names = F)
gbif<-read.csv("gbif.csv", h=T, sep=";", dec=".")

# 1.2. Import data from OBIS
obis<-read.csv("obis_rough.csv", h=T, sep=";", dec=".")
summary(obis)
#Select records by year (from 1950)
obis<-obis[obis$date_year >= 1950,]
summary(obis)
str(obis)
#Select columns 
obis<-obis[,c("decimalLongitude","decimalLatitude","scientificName")]
summary(obis)
#Eliminate NAs
obis <- na.omit (obis)
summary(obis)
obis
#Change column names
colnames(obis)[1] <- "lon"
colnames(obis)[2] <- "lat"
colnames(obis)[3] <- "species"
#Add new column
reference<-rep("OBIS",nrow(obis))
obis<-cbind(obis,reference)
obis
summary(obis)
write.table(obis,"obis.csv", sep=";", dec=".",row.names = F)
obis<-read.csv("obis.csv", h=T, sep=";", dec=".")

# 1.3. Import occurrences from peer-reviewed papers
papers<-read.csv("papers.csv", sep = ";", dec = ".", h=T)
papers <- cbind (papers[,1:4])
papers

# 1.4. Combine data
ocor <- rbind(papers, obis, gbif)
#Check for duplicate coordinates
dups2 <- duplicated(ocor[, c("lon", "lat")])
sum(dups2)
#Eliminate duplicate
o.mirabilis <- ocor[!dups2, ]
#Eliminate NAs
o.mirabilis <- na.omit (o.mirabilis)
summary(o.mirabilis)
str(o.mirabilis)
write.table(o.mirabilis,"o.mirabilis.csv", sep=";", dec=".",row.names = F)

# 1.5. heck of occurrence records in Google Earth
o.mirabilis.check<-read.csv("o.mirabilis.check.csv", sep = ";", dec = ".", h=T)

# 1.6. Plot in map
summary(o.mirabilis.check)
#Add reference
datarecord <- o.mirabilis.check
coordinates (datarecord) <- ~lon+lat
datarecord
projection(datarecord) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
e<-extent(datarecord@bbox)
e
data(wrld_simpl)
projection(wrld_simpl)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(wrld_simpl, axes=T)
points(o.mirabilis.check[,"lon"], o.mirabilis.check[,"lat"], pch= 20, col="blue", cex=1)

#### 2. Environmental variables

# 2.1. Choosing the variables
library(sdmpredictors)
list_layers("Bio-ORACLE")
load_layers("BO_calcite") -> calcite
load_layers("BO_salinity") -> salinity
load_layers("BO_ph") -> ph
load_layers("BO_sstmean") -> sstmean
load_layers("BO_sstrange") -> sstrange
load_layers("BO2_chlomean_bdmax") -> chlomean_bdmax
#Pearson's coefficient 
layers_correlation(c("BO_calcite","BO_ph","BO_salinity",
                     "BO_sstmean","BO_sstrange","BO2_chlomean_bdmax"))
environmental.variables <-stack(c(calcite,salinity,ph,
                                  sstmean,sstrange,chlomean_bdmax))

# 2.2. Occurrence records limit
summary(presence)
e <- extent(-120,190,-40,40)
atlanticpacific <- crop(environmental.variables, e)
plot(atlanticpacific)
plot(calcite)
writeRaster(atlanticpacific, "atlanticpacific", format="raster",overwrite=TRUE)
atlanticpacific<-stack("atlanticpacific")

# 2.3. Extract values from each pixel
env.var <- values(atlanticpacific)
env.var
#Extract coordinates de cada pixel
coord <- xyFromCell(atlanticpacific, 1:ncell(atlanticpacific))
#Combine values and coordinates
env.var <- cbind(coord, env.var)
colnames(env.var)[1:2] <- c("lon", "lat")
#Eliminate NAs
env.var.table <- na.omit(env.var)
#Rename columns
colnames(env.var.table)[1:2] <- c("lon", "lat")
#Extend of the study are
nrow(env.var.table)

# 2.4.Transform table into raster
write.table(env.var, "climate.csv", sep = ";", dec = ".",row.names=F)
climate<-read.csv("climate.csv", sep = ";", dec = ".", h=T)
gridded(climate) <- ~lon+lat

# 2.5. Extract values from pixels
atlanticpacific <- stack("atlanticpacific.grd")
valores <- extract(atlanticpacific, o.mirabilis.check[,1:2], cellnumbers=T)
#Combinar valores e coordenadas (w.bioracle e datarecords) 
data <- cbind (o.mirabilis.check[,1:4], valores)
#Eliminar celulas duplicadas de modo a deixar apenas um ponto de ocorrencia por pixel
duplicated(data[,"cells"])
dup <- which(duplicated(data[,"cells"]) == TRUE)
data <- data[-dup,] 
#Eliminar NAs
presence<- na.omit(data)
#Add new column
sp<-rep("1",nrow(presence))
presence<-cbind(presence,sp)
presence
write.table(presence,"presence.csv", sep=";", dec="." ,row.names = F)

# 2.6. Environmental variables and occurence records
presence <- read.table("presence.csv", header = TRUE, sep=";", dec=".")
summary(presence)
plot(wrld_simpl, axes=T)
points(presence[,"lon"], presence[,"lat"], pch= 20, col="blue", cex=1)

#END