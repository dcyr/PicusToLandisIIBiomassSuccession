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

landtypes <- levels(landtype)
spp <- levels(species)
spp_landis <- character()
for (sp in spp) {
  spp_landis <- append(spp_landis, as.character(vegCodes[which(vegCodes$PICUS_name ==sp), "Code_LANDIS"]))
}

#################
########  The following loop creates matrices of time necessary for the first accumulation of biomass in each
########  combination of spp and landtypes. We consider using this as a proxy for setting  Species Establishment Probabilities.
#################

timeBeforeBiomass <- pEst <- list()

for (s in levels(picusOutputsDF$scenario)){ 
  xS <- subset(picusOutputsDF, subset=picusOutputsDF$scenario==s)
  xS <- droplevels(xS)
  timeBeforeBiomass[[s]] <- list()
  for (p in levels(xS$period)) {
    xP <- subset(xS, subset=xS$period==p)
    xP <- droplevels(xP)
    timeBeforeBiomass[[s]][[p]] <- pEst[[s]][[p]] <- matrix(NA, nrow=length(spp), ncol=length(landtypes), dimnames=list(spp_landis, landtypes))
    for (l in landtypes) {
      xL <- subset(xP, subset=xP$landtype==l)     
      for (sp in seq_along(spp)) {
        x <- subset(xL, subset=xL$species==spp[sp])  
        timeBeforeBiomass[[s]][[p]][spp_landis[sp], l] <- x[min(which(x$BiomassAbove_kg_ha>0)), "Year"] - 2000
        print(paste(s, p, l, spp_landis[sp]))
      }
    }
  prob <- round(pbinom(q=0, size=10, prob=1/timeBeforeBiomass[[s]][[p]], lower.tail=FALSE), 3)
  prob[is.na(prob)] <- 0
  pEst[[s]][[p]] <- prob
  write.csv(timeBeforeBiomass[[s]][[p]], file=paste(wwd, "/timeBeforeBiomass_", s,"_", p, ".csv", sep=""))
  write.csv(prob, file=paste(wwd, "/pEst_", s,"_", p, ".csv", sep="")) 
  }
}
save(pEst, file=paste(wwd, "/pEst.RData", sep=""))


# ##########
# ### Optional output
# ### figure PICUS vs pEst Landis
# ##########
# 
# x <- seq(from=1, to=300, by=0.5)
# #x <- 6
# y <- pbinom(q=0, size=10, prob=1/x, lower.tail=FALSE)
# 
# png(filename = paste(wwd,"/timeBeforeBiomassVSpEst.png", sep=""), width = 6, height = 4,
#     units = "in", pointsize = 8, bg = "white",
#     res = 300, restoreConsole = TRUE)
# 
#   plot(x, y, log="x", type="l", xlab="Time before accumulating AGB in PICUS (years)", ylab="Corresponding SEP in LANDIS-II (10-yr. timesteps)")
#   grid(equilogs=FALSE)  
# 
# dev.off()
# 
# ##########
# ##########
