######################
######################
#######
#######   Extraction of AnppMax and BiomassMax from PICUS outputs
#######   Dominic Cyr
#######
######################
######################
rm(list=ls())

picusOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                         "/media/dcyr/Windows7_OS/Travail/SCF/Landis/Picus/Outputs",
                         "C:/Travail/SCF/Landis/Picus/Outputs")
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
##### The following section, including the big loop, is dependent on a specific file structure (PICUS outputs produced by Anthony Taylor)
############
folderNames <- list.dirs(picusOutputDir, full.names=F, recursive=F)
#### subsample of folderNames
#folderNames <- folderNames[grep("Acadian", folderNames)]#"AM|BSE|BSW|BP"
outputInfo <- strsplit(folderNames, "_")
folderNames <- paste(picusOutputDir, folderNames, sep="/")
#### extracting simulation info from picus outputs folder names

### to compute parameters for all areas, use this:
#areas <- unique(rapply(outputInfo, function(x) x[1]))
### else, comment out previous line and specify which one(s) to compute
areas <- c("MC")

scenarios <- unique(rapply(outputInfo, function(x) x[2]))
periods <- unique(rapply(outputInfo, function(x) x[3]))

######################
######### This big loop read all csv files and gather information into a big dataframe
require(stringr)
#############
for (a in areas) { #a <- areas[1]
    picusOutputs <- list()
    picusOutputs <- list()

    folderArea <- folderNames[grep(a, folderNames)]

    spp <- unique(vegCodes[vegCodes[,a]==1,"picusRef"])

    for (i in seq_along(folderArea)) { # i <- 1

        s <- outputInfo[[i]][2]  ### nome of scenario
        p <- outputInfo[[i]][length(outputInfo[[i]])] ## name of period

        #### creation of lists where dataframes are stored
        if (s %in% names(picusOutputs) == F) {
            picusOutputs[[s]] <- list()
        }
        picusOutputs[[s]][[p]] <- list()
        ####
        x <- list.files(folderArea[i])

        for (sp in as.character(spp)) { # sp <- as.character(spp[9])

            #### fetching .csv files
            sppIndex <- grep(sp, x)
            deadwoodIndex <- grep("Deadwood", x)
            standIndex <- grep("Stand", x)
            #### read corresponding .csv files
            sppDeadwood <- x[intersect(sppIndex, deadwoodIndex)]
            sppStand <- x[intersect(sppIndex, standIndex)]

            for (j in seq_along(sppStand)) { #j <- 1
                time1 <- Sys.time()
                standTmp <- read.csv(paste(folderArea[i], sppStand[j], sep="/"))
                if (nrow(standTmp)!=0)  {
                    deadwoodTmp <- read.csv(paste(folderArea[i], sppDeadwood[j], sep="/"))
                    standTmp <- standTmp[,c("Year","BiomassAbove_kg_ha")]
                    deadwoodTmp <- deadwoodTmp[,c("Year","DiedBiomassAbove_kg")]

                    tmp <- merge(standTmp, deadwoodTmp, by = "Year", all.x=TRUE)
                    tmp[is.na(tmp[,"DiedBiomassAbove_kg"]), "DiedBiomassAbove_kg"] <- 0
                    tmp[,"anpp"] <- diff(c(0,tmp[,"BiomassAbove_kg_ha"])) + tmp[,"DiedBiomassAbove_kg"]   #### ici je dois vérifier si les valeurs simulées correspondent à l'état au début ou à la fin du pas de temps
                    ###############################################
                    #################
                    landtype  <- str_extract(sppStand[j], "[0-9]+")
                    tmp[,"landtype"] <- landtype
                    tmp[,"species"] <- sp
                    tmp[,"ecozone"] <- a
                    tmp[,"scenario"] <- s
                    tmp[,"period"] <- p

                    if(exists("picusDF")) {
                        picusDF <- rbind(picusDF, tmp)
                    } else {
                        picusDF <- tmp
                    }
                    time2 <- Sys.time()
                }
                print(paste(a, s, p, landtype, sp, as.character(round(time2-time1, 2)), "sec."))

            }
            #### storing 1 dataframe per species
            picusOutputs[[s]][[p]][[sp]] <- picusDF[,c("ecozone", "scenario", "period", "landtype", "species","Year", "BiomassAbove_kg_ha", "DiedBiomassAbove_kg","anpp")]
            rm(picusDF)
        }
    }

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

    write.csv(picusOutputsDF, paste0("picusOutputsDF_", a, ".csv"), row.names=FALSE)
    rm(picusOutputsDF)
}
######