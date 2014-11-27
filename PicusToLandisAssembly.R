
######################
######################
#######
#######   Assembly of Landis Biomass succession dynamic input files
#######   Depends on "growthParam.RData" (produced by "PicusToLandisGrowthParam.r") and
#######   "pEst.RData" (produced by "PicusToLandisSEP.r")
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

processedDir <- paste(readDir, "Picus/processedOutputs", sep="/")
dir.create(processedDir)
wwd <- paste(processedDir, Sys.Date(), sep="/")
dir.create(wwd)


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



#### assembling parameter table according to landis format
growthParam <- get(load(paste(processedDir,"growthParam.RData", sep="/")))
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
    
    maxBiomass$ind <- as.factor(paste(substr(maxBiomass$ind, 1,3), substr(maxBiomass$ind, 4,4), sep="_") )
    
    paramsTmp <- data.frame(year = rep(y[j], length(spp)),
                            landtype= maxBiomass$ind,
                            species = spp,
                            probEst = SEP$values,  ##### "probEst" To Be Attributed 
                            maxANPP = round(maxANPP$values, 0), 
                            maxB = round(maxBiomass$values, 0))
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
              #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
              eol = "\n" #default line endings on windows system.
  )
}
