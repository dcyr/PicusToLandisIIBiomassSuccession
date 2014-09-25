######################
######################
#######
#######   Exploration of Spp Establishment' sensitivity to climate
#######   Dominic Cyr
#######
#####################
######################
rm(list=ls())
#readDir <- paste("C:/Users/dcyr/Dropbox/Landis/SCF_IA_LANDIS")   ### read-only folder, cfl system
readDir <- ifelse(Sys.info()["sysname"]=="Linux",
                  paste("/media/dcyr/Windows7_OS/Dropbox/landis/SCF_IA_LANDIS"),
                  paste("C:/Dropbox/Landis/SCF_IA_LANDIS"))  ### read-only folder, on my laptop (windows or Linux)

processedDir <- paste(readDir, "Picus/processedOutputs", sep="/")
dir.create(processedDir)
wwd <- paste(processedDir, Sys.Date(), sep="/")
dir.create(wwd)
vegCodes <- read.csv(file=paste(readDir, "vegCodes.csv", sep="/"))
######################
picusOutputsDF <- read.csv(paste(readDir, "/Picus/picusOutputsDF.csv", sep="/"))  ### this .csv file is produced by 
picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
######################


attach(picusOutputsDF)

spp <- unique(unique(species))
landtypes <- unique(landtype)


timeBeforeBiomass <- list()

for (s in levels(picusOutputsDF$scenario)){
  xS <- subset(picusOutputsDF, subset=picusOutputsDF$scenario==s)
  xS <- droplevels(xS)
  timeBeforeBiomass[[s]] <- list()
  for (p in levels(xS$period)) { 
    xP <- subset(xS, subset=xS$period==p)
    xP <- droplevels(xP)
    timeBeforeBiomass[[s]][[p]] <- matrix(NA, nrow=length(spp), ncol=length(landtypes), dimnames=list(spp, landtypes))
    for (l in landtypes) { 
      xL <- subset(xP, subset=xP$landtype==l)     
      for (sp in spp) { 
        x <- subset(xL, subset=xL$species==sp)  
        timeBeforeBiomass[[s]][[p]][sp, l] <- x[min(which(x$BiomassAbove_kg_ha>0)), "Year"] - 2000
        print(paste(s, p, l, sp))
      }
    }
  write.csv(timeBeforeBiomass[[s]][[p]], file=paste(wwd, "/timeBeforeBiomass_", s,"_", p, ".csv", sep=""))
  }
}

