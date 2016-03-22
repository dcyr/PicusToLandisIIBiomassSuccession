
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

processedOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                             "/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession",
                             "C:/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")

### That assumes Picus outputs were processed on the same day
### (else, specify another folder containing formated Picus outputs)
setwd(paste(processedOutputDir, Sys.Date(), sep="/"))

### vegCodes is the species master list
### It indicates which species to look in picus output folders
require(RCurl)
require(dplyr)
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
######################
######################
############
x <- list.files(full.names=F)
x <- x[grep("growthParam_", x)]
#### subsample of folderNames
#folderNames <- folderNames[grep("Acadian", folderNames)]#"AM|BSE|BSW|BP"
areas <- unique(gsub("growthParam_|.RData", "", x))
timestep <- 10

#### configuration of landis Climate Change scenarios
landisCCScenarios <- list(RCP85 = list("0" = c("Baseline", "Baseline"),
                                       "10" = c("RCP85", "20112040"),
                                       "40" = c("RCP85", "20412070"),
                                       "70" = c("RCP85", "20712100")),
                          RCP45 = list("0" = c("Baseline", "Baseline"),
                                       "10" = c("RCP45", "20112040"),
                                       "40" = c("RCP45", "20412070"),
                                       "70" = c("RCP45", "20712100")),
                          RCP26 = list("0" = c("Baseline", "Baseline"),
                                       "10" = c("RCP26", "20112040"),
                                       "40" = c("RCP26", "20412070"),
                                       "70" = c("RCP26", "20712100")),
                          Baseline = list("0" = c("Baseline", "Baseline"))
                          )

for (a in areas) {


    #### assembling parameter table according to landis format
    growthParam <- get(load(paste0("growthParam_", a, ".RData")))
    if (timestep == 10) {
        sep <- get(load(paste0("sep_", a, ".RData")))
    }
    if (timestep == 5) {
        sep <- get(load(paste0("sep_", a, "_5yrsTS.RData")))
    }

    biomassSuccessionDynamicParams <- list()


    for (i in seq_along(landisCCScenarios)){# i <- 1
        y <- as.numeric(names(landisCCScenarios[[i]]))
        for (j in seq_along(y)) { # j <- 1
            s <- landisCCScenarios[[i]][[j]][1] ### scenarioCC
            p <- landisCCScenarios[[i]][[j]][2] ### periodCC

            maxBiomass <- as.data.frame(growthParam[[s]][[p]][["maxBiomass"]])
            maxANPP <- as.data.frame(growthParam[[s]][[p]][["maxANPP"]])
            pEst <- as.data.frame(sep[[s]][[p]])
            spp <- rep(rownames(maxBiomass), ncol(maxANPP))
            maxBiomass <- stack(maxBiomass)
            maxANPP <- stack(maxANPP)
            pEst <- stack(pEst)

            maxBiomass[is.na(maxBiomass)] <- 0
            maxANPP[is.na(maxANPP)] <- 0

            ## formating landtype names
            l <- as.character(maxBiomass$ind)
            maxBiomass$ind <- as.factor(paste(substr(l, 1, nchar(l)-1),
                                              substr(maxBiomass$ind, nchar(l),nchar(l)), sep="_") )
            rm(l)

            paramsTmp <- data.frame(year = rep(ifelse(j==1, y[j], y[j]+timestep), length(spp)),
                            landtype= maxBiomass$ind,
                            species = spp,
                            probEst = pEst$values,  ##### "probEst" To Be Attributed
                            maxANPP = round(maxANPP$values, 0),
                            maxB = round(maxBiomass$values, 0))


            #########################
            ######## applying cut-offs
            #########################
            tmp <-paramsTmp[,c("probEst", "maxANPP", "maxB")]

            ## if SEP == 0 then maxANPP & maxB == 0
            index <- which(tmp[,"probEst"] == 0)
            tmp[index,c("maxANPP", "maxB")] <- 0

            ## if SEP > 0 & maxANPP == 0 then maxANPP == 1,
            index <- which(tmp[,"probEst"] > 0 &
                               tmp[,"maxANPP"] == 0)
            tmp[index, "maxANPP"] <- 1

            ## if SEP > 0 & maxB == 0 then maxB == 1,
            index <- which(tmp[,"probEst"] > 0 &
                               tmp[,"maxB"] == 0)
            tmp[index, "maxB"] <- 10

            ## if maxB > 0 & maxANPP == 0, then maxANPP == 1
            index <- which(tmp[,"maxANPP"] == 0 &
                               tmp[,"maxB"] > 0 )

            tmp[index, "maxANPP"] <- 1
            tmp[index, "maxB"] <- 10

            ## if maxB == 0 & maxANPP > 0, then maxB == 10
            index <- which(tmp[,"maxB"] == 0 &
                               tmp[,"maxANPP"] > 0 )
            tmp[index ,"maxB"] <- 10

            paramsTmp[,c("probEst", "maxANPP", "maxB")] <- tmp
            ## identifying landtypes with no possible biomass
            landtypeZeroB <- paramsTmp %>% group_by(landtype) %>%
                summarize(maxB = max(maxB)) %>%
                filter(maxB == 0)

            if(nrow(landtypeZeroB)>0) {
                landtypeZeroB <- as.character(landtypeZeroB$landtype)
            } else {
                landtypeZeroB <- NULL
            }


            ## replacing zeros by non-zero values for the first species in each of these landtypes
            index <- which(paramsTmp$landtype %in% landtypeZeroB &
                               paramsTmp$species == spp[1])

            if(length(index) > 0) {
                paramsTmp[index, "probEst"] <- 0.001
                paramsTmp[index, "maxANPP"] <- 1
                paramsTmp[index, "maxB"] <- 10
            }
            #########################
            #########################

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
    ## str(biomassSuccessionDynamicParams)
    ###### assembling landis input file

    for(i in seq_along(biomassSuccessionDynamicParams)){ # i<-1
        params <- biomassSuccessionDynamicParams[[i]]
        sName <- names(biomassSuccessionDynamicParams)[i]

        if (timestep == 10) {
            fileName <- paste("biomass-succession-dynamic-inputs_", a, "_", sName, ".txt", sep="")
        }
        if (timestep == 5) {
            fileName <- paste("biomass-succession-dynamic-inputs_", a, "_", sName, "_5yrsTS.txt", sep="")
        }


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
}
### additionnal snipet to produce light matrix for BS-main-inputs
###
# x <- biomassSuccessionDynamicParams[[i]]
# landtypes <- unique(as.character(x$landtype))
# mat <- t(cbind(landtypes,
#              matrix(c("20%", "40%", "50%", "70%", "90%"),
                    # byrow=TRUE, ncol=5, nrow=length(landtypes)),
#                 1,
#                 landtypes,
#                 600))
# write.csv(mat, "shadeMatrix.csv", row.names = FALSE)
###