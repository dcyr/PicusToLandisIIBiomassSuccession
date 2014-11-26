######################
######################
#######
#######   Extraction of maxANPP and maxBiomass from PICUS outputs
#######   (primarily depends on "picusOutputsDF.RData", produced by "PicusOutputsToDF.R")
#######   More information in markdown file "PicusToLandisGRwothParam.html" located here <<add url>>
#######
#######   Dominic Cyr
#######
#####################
######################
rm(list=ls())
#readDir <- paste("C:/Users/dcyr/Dropbox/Landis/SCF_IA_LANDIS")   ### read-only folder, cfl system
readDir <- ifelse(Sys.info()["sysname"]=="Linux",
                    paste("/media/dcyr/Windows7_OS/Dropbox/landis/SCF_IA_LANDIS"),
                    paste("C:/Dropbox/Landis/SCF_IA_LANDIS"))  ### read-only folder, on my laptop (windows or Linux)

#### location of various folders used for reading, writing, etc...
processedDir <- paste(readDir, "Picus/processedOutputs", sep="/")
dir.create(processedDir)
wwd <- paste(processedDir, Sys.Date(), sep="/")
dir.create(wwd)

######################
#### primary input
picusOutputsDF <- read.csv(paste(readDir, "/Picus/picusOutputsDF.csv", sep="/"))  ### this .csv file is produced by 
picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
######################
#### secondary input
vegCodes <- read.csv(file=paste(readDir, "vegCodes.csv", sep="/"))



######################
#### unit conversion constant
unitConvertionFactor <- 0.1 ## from kg/ha to g/m2



##### preparing the loops (another loop could be added to process multiple simulation areas
landtypes <- levels(picusOutputsDF$landtype)
spp <- levels(picusOutputsDF$species)
spp_landis <- character()
for (sp in spp) {
  spp_landis <- append(spp_landis, as.character(vegCodes[which(vegCodes$PICUS_name ==sp), "Code_LANDIS"]))
}

##### beginning of loops (another loop could be added to process multiple simulation areas)
growthParam <- list()
for (s in levels(picusOutputsDF$scenario)) { ## beginning of scenario loop   # s <- "Baseline" 
  growthParam[[s]] <- list()
  ###subsetting for the climate scenario
  picusOutputs <- subset(picusOutputsDF,
                         subset= picusOutputsDF$scenario == s,
                         select=c("period", "landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))
  picusOutputs <- droplevels(picusOutputs)
  
  for (p in levels(picusOutputs$period)) { ## beginning of period loop   #  p<- "Baseline 
    ## creating matrices filled with zeros
    maxBiomass <- maxANPP <- matrix(0, nrow=length(spp), ncol=length(landtypes), dimnames=list(spp_landis, landtypes))
    ## subsetting for the current species
    picusOutputs <- subset(picusOutputsDF,
                          subset= picusOutputsDF$scenario == s & picusOutputsDF$period == p,
                          select=c("landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))
    
    for (sp in seq_along(spp)) { ## beginning of species loop    # sp <- 8
      ## subsetting for the current species
      picusOutputsSp <- picusOutputs[picusOutputs$species==spp[sp],]
      
      ###############
      ######## computing and storing max Biomass
      ###############
      
      #######
      ### old method
      #######
      ### extracting maxBiomass using peak biomass
      ##maxB <- by(picusOutputsSp$BiomassAbove_kg_ha, picusOutputsSp$landtype, max, na.rm=TRUE)  
      
      #######
      ### new method
      #######
      #### Extracting max biomass using the average around which biomass oscillate after peak biomass, generally after 100 years post establishment
      subsample <- which(picusOutputsSp$Year>=2100)  ### year after which 
      maxB <- by(picusOutputsSp[subsample, "BiomassAbove_kg_ha"], picusOutputsSp[subsample, "landtype"], mean, na.rm=TRUE)
      
      
      ## Unit conversion, rounding, and storing
      maxB <- maxB*unitConvertionFactor  #unit conversion
      maxBiomass[spp_landis[sp], names(maxB)] <- round(as.numeric(maxB))
      ###############
      ###############
      
      ###############
      ######## computing and storing max ANPP
      ###############
      ######## Only one method should be commented out.
      
      ### method 1 - using max instant NPP
      #maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, max, na.rm=TRUE)  ### old method, no smoothing => max instant.ANPP
      
      ### method 2 - smoothing ANPP using a 10-y window
      #maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, function(x) max(runmean(x, k=10, endrule="NA", align="center"), na.rm=TRUE))#, na.rm=TRUE) 
      
      ### method 3 - using average NPP before first peak 
      maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, function(x) mean(x[1:which(x==max(x, na.rm=TRUE))], na.rm=TRUE))#, na.rm=TRUE) ## 
      
      ## Unit conversion, rounding, and storing
      maxA <- maxA*unitConvertionFactor #unit conversion
      maxANPP[spp_landis[sp], names(maxA)] <- round(as.numeric(maxA))
    }
    
    growthParam[[s]][[p]][["maxBiomass"]] <- maxBiomass
    growthParam[[s]][[p]][["maxANPP"]] <- maxANPP
    #####
    ### Optional: comment this out to produce .csv files
    #####
    write.csv(maxANPP, file=paste(paste(wwd,"/maxANPP_",s,"_",p,".csv", sep="")))
    write.csv(maxBiomass, file=paste(paste(wwd,"/maxBiomass_",s,"_",p,".csv", sep="")))
    #####
  }
}
save(growthParam, file=paste(paste(wwd,"/growthParam.RData",sep="")))


