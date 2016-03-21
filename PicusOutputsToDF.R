######################
######################
#######
#######   Extraction of AnppMax and BiomassMax from PICUS outputs
#######   Dominic Cyr
#######
######################
######################
rm(list=ls())

## From Anthony's dropbox
picusOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                         "/media/dcyr/Windows7_OS/Travail/SCF/Landis/Picus/PICUS DATA",
                         "C:/Travail/SCF/Landis/Picus/PICUS DATA")
processedOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                             "/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession",
                             "C:/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")
processedOutputDir <- paste(processedOutputDir, Sys.Date(), sep="/")

###
dir.create(processedOutputDir)
setwd(processedOutputDir)
rm(processedOutputDir)
readDir <- picusOutputDir ###  may be changed if the dataframe was created another day

### vegCodes is the species master list
### It indicates which species to look in picus output folders
require(RCurl)
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
######################
######################


######################
######################
## From Anthony's dropbox
areaFolders <- list.dirs(picusOutputDir, full.names=F, recursive=F)
#
areas <- strsplit(areaFolders, " ")
areas <- as.character(lapply(areas, function(x) gsub("DATA_", "", x[2])))
#areaSubsample <- areas
areaSubsample <- "SudStl"
#
folderNames <- areaFolders[areas %in% areaSubsample]
folderNames <- paste(picusOutputDir, folderNames, sep="/")
#


for (a in seq_along(folderNames)) {
    require(stringr)
    areaCode <- areaSubsample[a]
    zipFiles <- list.files(folderNames[a])
    spp <- unique(vegCodes[vegCodes[,areaCode]==1,"picusRef"])
    ## creating list for each quadrat
    picusOutputs <- list()
    ## fetching spp list
    for (i in seq_along(zipFiles)) {
        z <- zipFiles[i]

        info <- gsub(".zip", "", z)
        info <- strsplit(info, "_")
        s <- info[[1]][1]  ### nome of scenario
        s <- ifelse(s == "baseline", "Baseline", toupper(s))
        p <- ifelse(is.na(info[[1]][2]), "Baseline", info[[1]][2])
        ## zipfile full name
        z <-paste(folderNames[a], z, sep="/")
        #### creation of lists where dataframes are stored
        if (s %in% names(picusOutputs) == F) {
            picusOutputs[[s]] <- list()
        }

        picusOutputs[[s]][[p]] <- list()


        x <- unzip(z, list = TRUE)$Name

        for (sp in as.character(spp)) { # sp <- as.character(spp[9])
            #### fetching .csv files
            sppIndex <- agrep(sp, x)  ## fuzzy match
            deadwoodIndex <- grep("Deadwood", x)
            standIndex <- grep("Stand", x)
            #### read corresponding .csv files
            sppDeadwood <- x[intersect(sppIndex, deadwoodIndex)]
            sppStand <- x[intersect(sppIndex, standIndex)]

            for (j in seq_along(sppStand)) { #j <- 1
                standCSV <- sppStand[j]
                deadCSV <- sppDeadwood[j]
                time1 <- Sys.time()
                #
                standTmp <- read.csv(unz(z , filename = standCSV))
                if (nrow(standTmp)!=0)  {
                    deadwoodTmp <- read.csv(unz(z , filename = deadCSV))
                    standTmp <- standTmp[,c("Year","BiomassAbove_kg_ha")]
                    deadwoodTmp <- deadwoodTmp[,c("Year","DiedBiomassAbove_kg")]

                    tmp <- merge(standTmp, deadwoodTmp, by = "Year", all.x=TRUE)
                    tmp[is.na(tmp[,"DiedBiomassAbove_kg"]), "DiedBiomassAbove_kg"] <- 0
                    tmp[,"anpp"] <- diff(c(0,tmp[,"BiomassAbove_kg_ha"])) + tmp[,"DiedBiomassAbove_kg"]   #### ici je dois vérifier si les valeurs simulées correspondent à l'état au début ou à la fin du pas de temps
                    ###############################################
                    #################
                    landtype  <- str_extract(basename(standCSV), "[0-9]+")
                    tmp[,"landtype"] <- landtype
                    tmp[,"species"] <- sp
                    tmp[,"ecozone"] <- areaCode
                    tmp[,"scenario"] <- s
                    tmp[,"period"] <- p

                    if(exists("picusDF")) {
                        picusDF <- rbind(picusDF, tmp)
                    } else {
                        picusDF <- tmp
                    }
                    time2 <- Sys.time()
                }
                try(print(paste(areaCode, s, p, landtype, sp, as.character(round(time2-time1, 2)), "sec.")))

            }
            #### storing 1 dataframe per species
            picusOutputs[[s]][[p]][[sp]] <- picusDF[,c("ecozone", "scenario", "period", "landtype", "species","Year", "BiomassAbove_kg_ha", "DiedBiomassAbove_kg","anpp")]
            rm(picusDF)
        }
    }
    #save(picusOutputs, file = paste0("picusOutputs_", areaCode, ".RData"))

    for (s in names(picusOutputs)){
        for (p in names(picusOutputs[[s]]))  {
            if (exists("picusOutputsDF")) {
                picusOutputsDF  <- rbind(picusOutputsDF, do.call("rbind", picusOutputs[[s]][[p]]))
            } else {
                picusOutputsDF <- do.call("rbind", picusOutputs[[s]][[p]])
            }
        }
    }
    ### converting character vectors into factor (not really useful since it all goes into a .csv file)
    picusOutputsDF[,"ecozone"] <- as.factor(picusOutputsDF[,"ecozone"])
    picusOutputsDF[,"scenario"] <- as.factor(picusOutputsDF[,"scenario"])
    picusOutputsDF[,"period"] <- as.factor(picusOutputsDF[,"period"])
    picusOutputsDF[,"landtype"] <- as.factor(picusOutputsDF[,"landtype"])
    picusOutputsDF[,"species"] <- as.factor(picusOutputsDF[,"species"])

    write.csv(picusOutputsDF, paste0("picusOutputsDF_", areaCode, ".csv"), row.names=FALSE)
    rm(picusOutputsDF)
}


