#Distribution patterns and habitat suitability of the non-native brittle star Ophiothela mirabilis Verrill, 1867 along the Western Atlantic 
#Patrick Derviche, Angeline Saucsen, Daphne Spier, Paulo Lana

#### 04. Assessing models

# 4.1. Import files
model.ensemble<-stack("ophiothelamirabilis")
atlanticpacific<-stack("atlanticpacific")
presence <- read.table("presence.csv", header = TRUE, sep=";", dec=".")

# 4.2. Variable importance 
library(dplyr)
fun_model <-function(x,rmNA=F)  
{vi_model=getVarImp(model,id=x,wtest="test.dep")
return(as.data.frame(vi_model@varImportance[[3]]))}
V_model<-model.info$modelID
M_model<-lapply(V_model, fun_model)
vi_model<-t(bind_cols(M_model))
colnames(vi_model)<-c("BO_calcite", "BO_salinity", "BO_ph",
                      "BO_sstmean", "BO_sstrange", "BO2_chlomean_bdmax")
vi_model_var<-colMeans(vi_model)
write.table(vi_model_var,"vi_model_var.csv",sep=";",dec=".",row.names = T)

# 4.3. Models assessment
#Results (FALSE or TRUE in "sucess")
getModelInfo(model)
model.info<-getModelInfo(model)
show(model)
#Check "true" in success"
model.info<-getModelInfo(model)
write.table(model.info,"model.info.csv",sep=";",dec=".",row.names = T)
evaluation.model<-getEvaluation(model,wtest='test.dep',
                                stat=c('AUC','TSS', 'threshold', "deviance"),opt=2)
write.table(evaluation.model,"evaluation.model.csv", sep=";", dec=".",row.names = F)
boxplot(evaluation.model$AUC, evaluation.model$TSS)
summary(evaluation.model)
str(evaluation.model)

# 4.4. Predicted probability of occurrence
values <- extract(model.ensemble, presence[,1:2], cellnumbers=T)
#Combine values and environmentally suitable habitat
data <- cbind (presence[,1:2], values)
#Eliminate duplicates
duplicated(data[,"cells"])
dup <- which(duplicated(data[,"cells"]) == TRUE)
data <- data[-dup,] 
#Eliminate NAs
predicted.probability.of.occurrence <- na.omit(data)
summary(predicted.probability.of.occurrence)
write.table(predicted.probability.of.occurrence,"predicted.probability.of.occurrence.csv", sep=";", dec=".",row.names = F)
boxplot(predicted.probability.of.occurrence$layer,
        xlab="Occurrence records", ylab ="Environmental suitability", ylim=c(0,1))

# 4.5. Current habitats distribution using occurrence records (Fig A2)
# Packages
library(ggplot2)
library(sciplot)
library(png)
# Files
environmental.correlation <- cbind (presence[,5:10], values)
environmental.correlation <- na.omit(environmental.correlation)
# Calcite
calcite <- data.frame(x=environmental.correlation$BO_calcite, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.calcite <- ggplot(calcite, aes(x=environmental.correlation$BO_calcite, y=environmental.correlation$layer)) +
   geom_point() #scatter chart 
p.1.calcite <- p.calcite + geom_smooth(method=lm) #trend line and confidence interval
p.1.calcite <- p.1.calcite +
                xlab("Mean calcite concentration") + ylab("Environmentally suitable")+
                ylim(0,1)
p.1.calcite
summary(environmental.correlation$BO_calcite)
# Salinity
salinity <- data.frame(x=environmental.correlation$BO_salinity, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.salinity <- ggplot(salinity, aes(x=environmental.correlation$BO_salinity, y=environmental.correlation$layer)) +
  geom_point() #scatter chart 
p.1.salinity <- p.salinity + geom_smooth(method=lm) #trend line and confidence interval
p.1.salinity <- p.1.salinity +
                xlab("Mean salinity") + ylab("Environmentally suitable")+
                ylim(0,1)
p.1.salinity
summary(environmental.correlation$BO_salinity)
# pH
pH <- data.frame(x=environmental.correlation$BO_ph, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.pH <- ggplot(pH, aes(x=environmental.correlation$BO_ph, y=environmental.correlation$layer)) +
  geom_point() #scatter chart 
p.1.pH <- p.pH + geom_smooth(method=lm) #trend line and confidence interval
p.1.pH <-p.1.pH +
        xlab("Mean pH") + ylab("Environmentally suitable")+
         ylim(0,1)
p.1.pH
summary(environmental.correlation$BO_ph)
# SST mean
sstmean <- data.frame(x=environmental.correlation$BO_sstmean, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.sstmean <- ggplot(sstmean, aes(x=environmental.correlation$BO_sstmean, y=environmental.correlation$layer)) +
  geom_point() #scatter chart 
p.1.sstmean <- p.sstmean + geom_smooth(method=lm) #trend line and confidence interval
p.1.sstmean <-p.1.sstmean +
              xlab("Mean sea surface temperature") + ylab("Environmentally suitable")+
              ylim(0,1)
p.1.sstmean
summary(environmental.correlation$BO_sstmean)
# SST range
sstrange <- data.frame(x=environmental.correlation$BO_sstrange, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.sstrange <- ggplot(sstrange, aes(x=environmental.correlation$BO_sstrange, y=environmental.correlation$layer)) +
  geom_point() #scatter chart 
p.1.sstrange <- p.sstrange + geom_smooth(method=lm) #trend line and confidence interval
p.1.sstrange <- p.1.sstrange +
                 xlab("Range sea surface temperature") + ylab("Environmentally suitable")+
                 ylim(0,1)
p.1.sstrange
summary(environmental.correlation$BO_sstrange)
# Chlorophyll
chlorophyll <- data.frame(x=environmental.correlation$BO2_chlomean_bdmax, y=environmental.correlation$layer) #ggplot2 requires the data to be stored in a data.frame
p.chlorophyll <- ggplot(chlorophyll, aes(x=environmental.correlation$BO2_chlomean_bdmax, y=environmental.correlation$layer)) +
  geom_point() #scatter chart 
p.1.chlorophyll <- p.chlorophyll + geom_smooth(method=lm) #trend line and confidence interval
p.1.chlorophyll <- p.1.chlorophyll +
                    xlab("Mean chlorophyll concentration at maximum depth") + ylab("Environmentally suitable")+
                    ylim(0,1)
p.1.chlorophyll
summary(environmental.correlation$BO2_chlomean_bdmax)

# 4.6. Function multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

p<-multiplot(p.1.calcite, p.1.salinity,p.1.sstrange, p.1.sstmean, p.1.chlorophyll, p.1.pH, cols=2)
ggsave(p, file="p1.png", width=18.2, height=16.7, dpi=500)

#END