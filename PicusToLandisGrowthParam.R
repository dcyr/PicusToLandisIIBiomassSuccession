######################
######################
#######
#######   Extraction of maxANPP and maxBiomass from PICUS outputs (depends on "PicusOutputsToDF.r")
#######   + SEP (depends on "PicusToLandisSEP.r")
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



landtypes <- levels(picusOutputsDF$landtype)
spp <- levels(picusOutputsDF$species)

growthParam <- list()
for (s in levels(picusOutputsDF$scenario)) { # s <- "Baseline" 
  growthParam[[s]] <- list()
  picusOutputs <- subset(picusOutputsDF,
                         subset= picusOutputsDF$scenario == s,
                         select=c("period", "landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))
  
  picusOutputs <- droplevels(picusOutputs)
  
  for (p in levels(picusOutputs$period)) { # p <- "Baseline"
    
    maxBiomass <- maxANPP <- matrix(0, nrow=length(spp), ncol=length(landtypes), dimnames=list(spp, landtypes))
    
    picusOutputs <- subset(picusOutputsDF,
                          subset= picusOutputsDF$scenario == s & picusOutputsDF$period == p,
                          select=c("landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))
    
    for (sp in spp) { # sp <- "Abies_balsamea"
      ## another subset
      picusOutputsSp <- picusOutputs[picusOutputs$species==sp,]
      ## computing and storing max Biomass
      maxB <- by(picusOutputsSp$BiomassAbove_kg_ha, picusOutputsSp$landtype, max, na.rm=TRUE)
      maxBiomass[sp, names(maxB)] <- round(as.numeric(maxB))
      ## computing and storing max ANPP
      maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, max, na.rm=TRUE)
      maxANPP[sp, names(maxA)] <- round(as.numeric(maxA))
    }
    growthParam[[s]][[p]][["maxBiomass"]] <- maxBiomass
    growthParam[[s]][[p]][["maxANPP"]] <- maxANPP
      #####
#     ##comment this out to produce .csv files
    write.csv(maxANPP, file=paste(paste(wwd,"/maxANPP_",s,"_",p,".csv", sep="")))
    write.csv(maxBiomass, file=paste(paste(wwd,"/maxBiomass_",s,"_",p,".csv", sep="")))
#     #####
  }
}

rm(list=(c("picusOutputs", "picusOutputsDF", "picusOutputsSp")))


#### configuration of landis Climate Change scenarios
landisCCScenarios <- list(RCP85 = list("0" = c("Baseline", "Baseline"),
                                      "10" = c("RCP85", "20112040"),
                                      "40" = c("RCP85", "20412070"),
                                      "70" = c("RCP85", "20712100")),
                          RCP45 = list("0" = c("Baseline", "Baseline"),
                                       "10" = c("RCP85", "20112040"),
                                       "40" = c("RCP85", "20412070"),
                                       "70" = c("RCP45", "20712100"))
                          )



#### assembling parameters according to landis format
pEst <- get(load(paste(processedDir,"pEst.RData", sep="/")))
biomassSuccessionDynamicParams <- list()

for (i in seq_along(landisCCScenarios)){# i <- 1
  y <- as.numeric(names(landisCCScenarios[[i]]))
  for (j in seq_along(y)) { # j <- 1 
    s <- landisCCScenarios[[i]][[j]][1] ### scenarioCC
    p <- landisCCScenarios[[i]][[j]][2] ### periodCC
    
    maxBiomass <- as.data.frame(growthParam[[s]][[p]][["maxBiomass"]])
    maxANPP <- as.data.frame(growthParam[[s]][[p]][["maxANPP"]])
    SEP <- as.data.frame(pEst[[s]][[p]])
    spp <- rep(rownames(maxBiomass), ncol(maxANPP))
    maxBiomass <- stack(maxBiomass)
    maxANPP <- stack(maxANPP)
    SEP <- stack(SEP)
    
    maxBiomass[is.na(maxBiomass)] <- 0
    maxANPP[is.na(maxANPP)] <- 0
    
     
    paramsTmp <- data.frame(year = rep(y[j], length(spp)),
                             landtype=maxBiomass$ind,
                             species = spp,
                             probEst = SEP$values,  ##### "probEst" To Be Attributed 
                             maxANPP = maxANPP$values,
                             maxB = maxBiomass$values)
    if(exists("params")) {
      params <- rbind(params, paramsTmp)      
    } else {
      params <- paramsTmp
    }
    rm(paramsTmp)
  }
  biomassSuccessionDynamicParams[[names(landisCCScenarios)[i]]] <- params
  rm(params)
}
  

###### assembling landis input file

for(i in seq_along(biomassSuccessionDynamicParams)){ # i<-1
  params <- biomassSuccessionDynamicParams[[i]]
  sName <- names(biomassSuccessionDynamicParams)[i]
  
  fileName <- paste(wwd,"/biomass-succession-dynamic-inputs_",sName, ".txt", sep="")
  
  ## intro section
  sink(fileName)
  cat('LandisData "Dynamic Input Data"')
  cat("\n")
  cat("\n")
  cat(paste(c(">>",colnames(params)), collapse="\t"))
  cat("\n")
  sink()  
  ## param table
  write.table(biomassSuccessionDynamicParams[[i]], file=fileName,
              append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t",
              quote=FALSE,
              eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
              )
}

