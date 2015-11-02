######################
######################
#######
#######   Extraction of maxANPP and maxBiomass from PICUS outputs
#######   (primarily depends on "picusOutputsDF.RData", produced by "PicusOutputsToDF.R")
#######   More information in markdown file "PicusToLandisGRwothParam.html" located here <<add url>>
#######
#######   Dominic Cyr
#######
######################
######################

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
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
######################
######################
############
x <- list.files(full.names=F)
x <- x[grep("picusOutputsDF_", x)]
#### subsample of folderNames
#areas <- unique(gsub("picusOutputsDF_|.csv", "", x))
areas <- "BSW"

####################
for (a in areas) {# a <- areas[1]
    ######################
    picusOutputsDF <- read.csv(paste0("picusOutputsDF_", a, ".csv"))  ### this .csv file is produced by
    picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
    #####################

    attach(picusOutputsDF)


    ######################
    #### unit conversion constant
    unitConvertionFactor <- 0.1 ## from kg/ha to g/m2


    ##### preparing the loops (another loop could be added to process multiple simulation areas
    landtypes <- levels(picusOutputsDF$landtype)
    spp <- levels(picusOutputsDF$species)
    spp_landis <- vegCodes[which(vegCodes[,a]==TRUE), "LandisCode"]
    spp_landis <- droplevels(spp_landis)
    spp_landis <- spp_landis[order(spp_landis)]

    ### spp list, some species in Landis use the same Picus reference (ex. Populus spp)
    tmp <- list()
    for (sp in spp) {
        tmp[[sp]] <- as.character(vegCodes[intersect(which(vegCodes$picusRef == sp),
                                                      which(vegCodes[,a] == 1)), "LandisCode"])
    }
    spp <- tmp

    ##### beginning of loops (another loop could be added to process multiple simulation areas)
    growthParam <- list()
    require(caTools)
    for (s in levels(picusOutputsDF$scenario)) { ## beginning of scenario loop   # s <- "Baseline"
      growthParam[[s]] <- list()
      ###subsetting for the climate scenario
      picusOutputs <- subset(picusOutputsDF,
                             subset= picusOutputsDF$scenario == s,
                             select=c("period", "landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))
      picusOutputs <- droplevels(picusOutputs)

      for (p in levels(picusOutputs$period)) { ## beginning of period loop   #  p<- "Baseline"
        ## creating matrices filled with zeros
        maxBiomass <- maxANPP <- matrix(0, nrow=length(spp_landis), ncol=length(landtypes), dimnames=list(spp_landis, landtypes))
        ## subsetting for the current species
        picusOutputs <- subset(picusOutputsDF,
                              subset= picusOutputsDF$scenario == s & picusOutputsDF$period == p,
                              select=c("landtype", "species", "Year", "BiomassAbove_kg_ha", "anpp"))

        for (sp in seq_along(spp)) { ## beginning of species loop    # sp <- 3

            picusRef <- names(spp)[sp]
            ## subsetting for the current species
            picusOutputsSp <- picusOutputs[picusOutputs$species==picusRef,]

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

            ###############
            ###############

            ###############
            ######## computing and storing max ANPP
            ###############
            ######## Only one method should be commented out.

            ### method 1 - using max instant NPP
            #maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, max, na.rm=TRUE)  ### old method, no smoothing => max instant.ANPP

            ### method 2 - smoothing ANPP using a 10-y window
            maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, function(x) max(runmean(x, k=10, endrule="NA", align="center"), na.rm=TRUE))#, na.rm=TRUE)

            ### method 3 - using average NPP before first peak
            #maxA <- by(picusOutputsSp$anpp, picusOutputsSp$landtype, function(x) mean(x[1:which(x==max(x, na.rm=TRUE))], na.rm=TRUE))#, na.rm=TRUE) ##

            ## Unit conversion, rounding, and storing
            maxA <- maxA*unitConvertionFactor #unit conversion

            for (i in spp[[sp]]) {
                maxBiomass[i, names(maxB)] <- round(as.numeric(maxB))
                maxANPP[i, names(maxA)] <- round(as.numeric(maxA))
                print(paste(a, s, p, i))
            }
        }

        growthParam[[s]][[p]][["maxBiomass"]] <- maxBiomass
        growthParam[[s]][[p]][["maxANPP"]] <- maxANPP

        #####
        ### Optional: comment this out to produce .csv files
        #####
        #write.csv(maxANPP, file=paste(paste("maxANPP_",s,"_",p,".csv", sep="")))
        #write.csv(maxBiomass, file=paste(paste("maxBiomass_",s,"_",p,".csv", sep="")))
        #####
      }
    }
    save(growthParam, file=paste0("growthParam_", a, ".RData"))
    rm(growthParam)
}

