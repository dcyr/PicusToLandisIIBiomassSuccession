######################
######################
#######
#######   Computation of Species Establishment Probabilities for Landis-II Biomass succession
#######   from formatted Picus outputs
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
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
######################
######################
############
x <- list.files(full.names=F)
x <- x[grep("picusOutputsDF_", x)]
#### subsample of folderNames
areas <- unique(gsub("picusOutputsDF_|.csv", "", x))
areas <- "AM"

for (a in areas) { # a <- areas[5]

    ######################
    picusOutputsDF <- read.csv(paste0("picusOutputsDF_", a, ".csv"))  ### this .csv file is produced by
    picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
    ######################

    attach(picusOutputsDF)

    landtypes <- levels(landtype)
    spp <- levels(species)

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
            timeBeforeBiomass[[s]][[p]] <- pEst[[s]][[p]] <- matrix(NA, nrow=length(spp_landis),
                                                                    ncol=length(landtypes),
                                                                    dimnames=list(spp_landis, landtypes))
            for (l in landtypes) {
                xL <- subset(xP, subset=xP$landtype==l)
                for (sp in seq_along(spp)) {
                    picusRef <- names(spp)[sp]
                    x <- subset(xL, subset=xL$species == picusRef)
                    for (i in spp[[sp]]) {
                        timeBeforeBiomass[[s]][[p]][i, l] <- x[min(which(x$BiomassAbove_kg_ha>0)), "Year"] - 2000
                        print(paste(a, s, p, l, spp_landis[sp]))
                    }
                }
            }
            prob <- round(pbinom(q=0, size=10, prob=1/timeBeforeBiomass[[s]][[p]], lower.tail=FALSE), 3)
            prob[is.na(prob)] <- 0
            pEst[[s]][[p]] <- prob
            #write.csv(timeBeforeBiomass[[s]][[p]], file=paste("timeBeforeBiomass_", s,"_", p, ".csv", sep=""))
            #write.csv(prob, file=paste("pEst_", s,"_", p, ".csv", sep=""))
        }
    }
    save(pEst, file=paste0("sep_", a, ".RData"))
    rm(pEst)
}

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
