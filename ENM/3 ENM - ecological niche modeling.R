#Distribution patterns and habitat suitability of the non-native brittle star Ophiothela mirabilis Verrill, 1867 along the Western Atlantic 
#Patrick Derviche, Angeline Saucsen, Daphne Spier, Paulo Lana

#### 3. Ecological niche modelling
#My memory limit
memory.limit(size=78000)

# 3.1. Import files
atlanticpacific<-stack("atlanticpacific")
presence <- read.table("presence.csv", header = TRUE, sep=";", dec=".")
#Spatializing the data frame
coordinates(presence) <- ~lon+lat

# 3.2. Preparing data and generating background
pre.model <- sdmData(formula= sp ~ BO_calcite + BO_salinity + BO_ph
                     + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax,
                     train = presence, predictors = atlanticpacific, 
                     bg=list(n=7000,method='gRandom',remove=TRUE))

# 3.3. Adjusting the models
model <- sdm(formula= sp ~ BO_calcite + BO_salinity + BO_ph
             + BO_sstmean + BO_sstrange + BO2_chlomean_bdmax, data=pre.model,
             methods=c("maxent","svm","gam"),
             replicatin=c("boot"), test.percent=30, n=70)

# 3.4. Ensemble
model.ensemble <- ensemble(model, newdata=atlanticpacific, filename="ophiothelamirabilis",
                           setting=list(method='weighted', stat='TSS',opt=2), overwrite=TRUE)
colfunc <- colorRampPalette(c("forestgreen","yellow", "red","red4"))
plot(model.ensemble, axes=T, col= colfunc(100), zlim=c(-0.01,1))
writeRaster(model.ensemble, "model.ensemble", format="raster",overwrite=T)

#END