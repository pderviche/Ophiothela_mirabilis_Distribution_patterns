#Patrick Derviche
pacman::p_load(raster, rgdal, corrplot, vegan, psych, sp, dismo, mnormt, permute,
               kernlab, rJava, maptools, spatial.tools, jsonlite, rgeos, fmsb,
               prettymapr, maps, ggmap, GISTools, rJava,sdm)
memory.limit(size=78000)

#### 03
#### Ecological niche modelling

### 3.1 Leptogorgia punicea

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
atlanticpacific<-stack("atlanticpacific1")
Leptogorgia.punicea.ENM <- read.table("Leptogorgia.punicea.ENM.csv", header = TRUE, sep=";", dec=".")


#Spatializing the data frame
coordinates(Leptogorgia.punicea.ENM) <- ~lon+lat

#Preparing data and generating background
pre.model.Leptogorgia.punicea <- sdmData(formula= sp ~ BO_calcite + BO_salinity + BO_ph
                                 + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax,
                                 train = Leptogorgia.punicea.ENM, predictors = atlanticpacific, 
                                 bg=list(n=7000,method='gRandom',remove=TRUE))

#Adjusting the models
model.Leptogorgia.punicea <- sdm(formula= sp ~ BO_calcite + BO_salinity + BO_ph
                                       + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax, data=pre.model.Leptogorgia.punicea,
                                       methods=c("maxent","svm","gam"),
                                       replicatin=c("boot"), test.percent=30, n=70)

#Ensemble
Leptogorgia.punicea.ensemble <- ensemble(model.Leptogorgia.punicea, newdata=atlanticpacific, filename="leptogorgiapuniceaenm",
                           setting=list(method='weighted', stat='TSS',opt=2), overwrite=TRUE)

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(Leptogorgia.punicea.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))

writeRaster(Leptogorgia.punicea.ensemble, "Leptogorgia.punicea.ensemble", format="raster",overwrite=T)

#### Important

#Results (FALSE or TRUE in "sucess")
model.info.Leptogorgia.punicea<-getModelInfo(model.Leptogorgia.punicea)

#### 
#### Variable importance 
library(dplyr)
fun_model <-function(x,rmNA=F)  
{vi_model=getVarImp(model.Leptogorgia.punicea,id=x,wtest="test.dep")
return(as.data.frame(vi_model@varImportance[[3]]))}

#Aplicando a funcao
V_model.Leptogorgia.punicea<-model.info.Leptogorgia.punicea$modelID
M_model.Leptogorgia.punicea<-lapply(V_model.Leptogorgia.punicea, fun_model)
vi_model.Leptogorgia.punicea<-t(bind_cols(M_model.Leptogorgia.punicea))
colnames(vi_model.Leptogorgia.punicea)<-c("BO_calcite", "BO_salinity", "BO_ph",
                      "BO_sstmean", "BO_sstrange", "BO2_chlomean_bdmax")
vi.model.var.Leptogorgia.punicea<-colMeans(vi_model.Leptogorgia.punicea)
write.table(vi.model.var.Leptogorgia.punicea,"vi.model.var.Leptogorgia.punicea.csv",sep=";",dec=".",row.names = T)

#### 
#### Model evaluation

#Check "true" in success"
model.info.Leptogorgia.punicea<-getModelInfo(model.Leptogorgia.punicea)
write.table(model.info.Leptogorgia.punicea,"model.info.Leptogorgia.punicea.csv",sep=";",dec=".",row.names = T)


evaluation.model.Leptogorgia.punicea<-getEvaluation(model.Leptogorgia.punicea,wtest='test.dep',
                                stat=c('AUC','TSS', 'threshold', "deviance"),opt=2)

write.table(evaluation.model.Leptogorgia.punicea,"evaluation.model.Leptogorgia.punicea.csv", sep=";", dec=".",row.names = F)
boxplot(evaluation.model.Leptogorgia.punicea$AUC, evaluation.model.Leptogorgia.punicea$TSS)
summary(evaluation.model.Leptogorgia.punicea)
str(evaluation.model.Leptogorgia.punicea)

#### 
#### Predicted probability of occurrence

valores <- extract(Leptogorgia.punicea.ensemble, Leptogorgia.punicea.ENM[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
p.p.o.Leptogorgia.punicea <- cbind (Leptogorgia.punicea.ENM[,1:2], valores)
summary(p.p.o.Leptogorgia.punicea)
write.table(p.p.o.Leptogorgia.punicea,"p.p.o.Leptogorgia.punicea.csv", sep=";", dec=".",row.names = F)
boxplot(p.p.o.Leptogorgia.punicea$layer,
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

e1<-extent(c(-120,-25, -40, 40))
coastline <- crop(coastline,e1)
lakes <- crop(lakes,e1)
states <- crop(states,e1)
countries <- crop(countries,e1)

#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model.Leptogorgia.punicea <- read.csv("evaluation.model.Leptogorgia.punicea.csv", sep = ";", dec = ".", h=T)
Leptogorgia.punicea.ensemble <- stack("Leptogorgia.punicea.ensemble.gri")
leptogorgiapuniceaenm <- stack("leptogorgiapuniceaenm.grd")

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(Leptogorgia.punicea.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))
plot(leptogorgiapuniceaenm, axes=T, col= colfunc(100), zlim=c(-0.01,1))

binary.model.Leptogorgia.punicea<-Leptogorgia.punicea.ensemble>=mean(evaluation.model.Leptogorgia.punicea[,5])

projection(binary.model.Leptogorgia.punicea)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Figures
setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/Figures/Figures hosts")

#Atlantic
plot(binary.model.Leptogorgia.punicea, col=c("white","blue"),  legend = F)
plot(coastline, col="gray90", border="black",  legend = F, add=T)
plot(lakes,  legend = F, add=T)
plot(states, col="gray", add=T)
plot(countries, add=T)

v<-getValues(binary.model.Leptogorgia.punicea)
v
write.csv(v, file = "BinaryValuesLeptogorgiapunicea.csv",dec = ".", sep = ",", col.names = NA)

#S Atlantic
e1<-extent(c(-60,-25, -40, 5))

binary.model.Leptogorgia.punicea.south.atlantic <- crop(binary.model.Leptogorgia.punicea,e1)
coastline.south.atlantic <- crop(coastline, e1)
lakes.south.atlantic <- crop(lakes, e1)
countries.south.atlantic <- crop(countries, e1)
states.south.atlantic <- crop(states, e1)

projection(binary.model.Leptogorgia.punicea.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Leptogorgia.punicea.south.atlantic, axes=T, col= colfunc(100), zlim=c(-0.01,1))

plot(binary.model.Leptogorgia.punicea.south.atlantic, col=c("white","blue"),  legend = F)
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic,col="gray10", add=T)

#N Atlantic
e2<-extent(c(-100,-50, 5, 40)) 

binary.model.Leptogorgia.punicea.north.atlantic <- crop(binary.model.Leptogorgia.punicea,e2)
coastline.north.atlantic <- crop(coastline, e2)
lakes.north.atlantic <- crop(lakes, e2)
countries.north.atlantic <- crop(countries, e2)
states.north.atlantic <- crop(states, e2)

projection(binary.model.Leptogorgia.punicea.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Leptogorgia.punicea.north.atlantic, axes=T, col= colfunc(100), zlim=c(-0.01,1))

plot(binary.model.Leptogorgia.punicea.north.atlantic, col=c("white","blue"),  legend = F)
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.north.atlantic,  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic,col="gray10", add=T)























### 3.2 Carijoa riisei

setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/R/Host ENM")
atlanticpacific<-stack("atlanticpacific1")
Carijoa.riisei.ENM <- read.table("Carijoa.riisei.ENM.csv", header = TRUE, sep=";", dec=".")


#Spatializing the data frame
coordinates(Carijoa.riisei.ENM) <- ~lon+lat

#Preparing data and generating background
pre.model.Carijoa.riisei <- sdmData(formula= sp ~ BO_calcite + BO_salinity + BO_ph
                                         + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax,
                                         train = Carijoa.riisei.ENM, predictors = atlanticpacific, 
                                         bg=list(n=7000,method='gRandom',remove=TRUE))

#Adjusting the models
model.Carijoa.riisei <- sdm(formula= sp ~ BO_calcite + BO_salinity + BO_ph
                                 + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax, data=pre.model.Carijoa.riisei,
                                 methods=c("maxent","svm","gam"),
                                 replicatin=c("boot"), test.percent=30, n=70)

#Ensemble
Carijoa.riisei.ensemble <- ensemble(model.Carijoa.riisei, newdata=atlanticpacific, filename="carijoariiseienm",
                                    setting=list(method='weighted', stat='TSS',opt=2), overwrite=TRUE)

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(Carijoa.riisei.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))

writeRaster(Carijoa.riisei.ensemble, "Carijoa.riisei.ensemble", format="raster",overwrite=T)

#### Important

#Results (FALSE or TRUE in "sucess")
model.info.Carijoa.riisei<-getModelInfo(model.Carijoa.riisei)

#### 
#### Variable importance 
library(dplyr)
fun_model <-function(x,rmNA=F)  
{vi_model=getVarImp(model.Carijoa.riisei,id=x,wtest="test.dep")
return(as.data.frame(vi_model@varImportance[[3]]))}

#Aplicando a funcao
V_model.Carijoa.riisei<-model.info.Carijoa.riisei$modelID
M_model.Carijoa.riisei<-lapply(V_model.Carijoa.riisei, fun_model)
vi_model.Carijoa.riisei<-t(bind_cols(M_model.Carijoa.riisei))
colnames(vi_model.Carijoa.riisei)<-c("BO_calcite", "BO_salinity", "BO_ph",
                                     "BO_sstmean", "BO_sstrange", "BO2_chlomean_bdmax")
vi.model.var.Carijoa.riisei<-colMeans(vi_model.Carijoa.riisei)
write.table(vi.model.var.Carijoa.riisei,"vi.model.var.Carijoa.riisei.csv",sep=";",dec=".",row.names = T)


#### 
#### Model evaluation

#Check "true" in success"
model.info.Carijoa.riisei<-getModelInfo(model.Carijoa.riisei)
write.table(model.info.Carijoa.riisei,"model.info.Carijoa.riisei.csv",sep=";",dec=".",row.names = T)

evaluation.model.Carijoa.riisei<-getEvaluation(model.Carijoa.riisei,wtest='test.dep',
                                stat=c('AUC','TSS', 'threshold', "deviance"),opt=2)


write.table(evaluation.model.Carijoa.riisei,"evaluation.model.Carijoa.riisei.csv", sep=";", dec=".",row.names = F)
boxplot(evaluation.model.Carijoa.riisei$AUC, evaluation.model.Carijoa.riisei$TSS)
summary(evaluation.model.Carijoa.riisei)
str(evaluation.model.Carijoa.riisei)

#### 
#### Predicted probability of occurrence

valores <- extract(Carijoa.riisei.ensemble, Carijoa.riisei.ENM[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
p.p.o.Carijoa.riisei <- cbind (Carijoa.riisei.ENM[,1:2], valores)
summary(p.p.o.Carijoa.riisei)
write.table(p.p.o.Carijoa.riisei,"p.p.o.Carijoa.riisei.csv", sep=";", dec=".",row.names = F)
boxplot(p.p.o.Carijoa.riisei$layer,
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

e1<-extent(c(-120,-25, -40, 40))
coastline <- crop(coastline,e1)
lakes <- crop(lakes,e1)
states <- crop(states,e1)
countries <- crop(countries,e1)

#### Binary maps

#Creating objects for maps from the mean thresholds of max specificity and sensitivity
evaluation.model.Carijoa.riisei <- read.csv("evaluation.model.Carijoa.riisei.csv", sep = ";", dec = ".", h=T)
Carijoa.riisei.ensemble <- stack("Carijoa.riisei.ensemble.gri")
carijoariiseienm <- stack("carijoariiseienm.grd")

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(Carijoa.riisei.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))
plot(carijoariiseienm, axes=T, col= colfunc(100), zlim=c(-0.01,1))

binary.model.Carijoa.riisei<-Carijoa.riisei.ensemble>=mean(evaluation.model.Carijoa.riisei[,5])


projection(binary.model.Carijoa.riisei)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

#Figures
setwd("D:/Patrick Derviche/Documentos/LabBentos/Omirabilis/Journal of Sea Research/Figures/Figures hosts")

#Atlantic
plot(binary.model.Carijoa.riisei, col=c("white","blue"),  legend = F)
plot(coastline, col="gray90", border="black",  legend = F, add=T)
plot(lakes,  legend = F, add=T)
plot(states, col="gray", add=T)
plot(countries, add=T)

v<-getValues(binary.model.Carijoa.riisei)
v
write.csv(v, file = "BinaryValuesCarijoariisei.csv",dec = ".", sep = ",", col.names = NA)

#S Atlantic
e1<-extent(c(-60,-25, -40, 5)) #Atlantico Sul

binary.model.Carijoa.riisei.south.atlantic <- crop(binary.model.Carijoa.riisei,e1)
coastline.south.atlantic <- crop(coastline, e1)
lakes.south.atlantic <- crop(lakes, e1)
countries.south.atlantic <- crop(countries, e1)
states.south.atlantic <- crop(states, e1)

projection(binary.model.Carijoa.riisei.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.south.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

plot(binary.model.Carijoa.riisei.south.atlantic, col=c("white","blue"),  legend = F)
plot(coastline.south.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.south.atlantic,  legend = F, add=T)
plot(states.south.atlantic, col="gray", add=T)
plot(countries.south.atlantic,col="gray10", add=T)

#N Atlantic
e2<-extent(c(-100,-50, 5, 40)) 

binary.model.Carijoa.riisei.north.atlantic <- crop(binary.model.Carijoa.riisei,e2)
coastline.north.atlantic <- crop(coastline, e2)
lakes.north.atlantic <- crop(lakes, e2)
countries.north.atlantic <- crop(countries, e2)
states.north.atlantic <- crop(states, e2)

projection(binary.model.Carijoa.riisei.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(coastline.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(lakes.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(countries.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
projection(states.north.atlantic)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(binary.model.Carijoa.riisei.north.atlantic, axes=T, col= colfunc(100), zlim=c(-0.01,1))

plot(binary.model.Carijoa.riisei.north.atlantic, col=c("white","blue"),  legend = F)
plot(coastline.north.atlantic, col="gray90", border="black",  legend = F, add=T)
plot(lakes.north.atlantic,  legend = F, add=T)
plot(states.north.atlantic, col="gray", add=T)
plot(countries.north.atlantic,col="gray10", add=T)


