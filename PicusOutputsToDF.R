######################
######################
#######
#######   Extraction of AnppMax and BiomassMax from PICUS outputs
#######   Dominic Cyr
#######
######################
######################
rm(list=ls())
beginTime <- Sys.time()
## From Anthony's dropbox
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession")
picusOutputDir <- "../PICUS DATA"

###
require(parallel)
require(data.table)

clusterN <- floor(detectCores()*.8) ### this script is I/O limited, I'm not even sure parallelizing is useful here
# clusterN <- floor(detectCores()*.8)  ### choose number of nodes to add to cluster.
sysName <- Sys.info()["sysname"]

processedOutputDir <- paste(getwd(), Sys.Date(), sep="/")

###
dir.create(processedOutputDir)

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
areaSubsample <- "QcCentral"
#
folderNames <- areaFolders[areas %in% areaSubsample]
folderNames <- paste(picusOutputDir, folderNames, sep="/")
#

for (a in seq_along(folderNames)) {
    #require(stringr)
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
        s <- ifelse(s == "baseline" | s == "Baseline", "Baseline", toupper(s))
        p <- ifelse(is.na(info[[1]][2]), "Baseline", info[[1]][2])
        ## zipfile full name
        z <-paste(folderNames[a], z, sep="/")
        #### creation of lists where dataframes are stored
        if (s %in% names(picusOutputs) == F) {
            picusOutputs[[s]] <- list()
        }
        x <- unzip(z, list = TRUE)$Name

        require(doSNOW)
        if (sysName=="Windows") {
            cl = makeCluster(clusterN, rscript="Rscript.exe", type='SOCK')
        }
        if (sysName=="Linux") {
            cl = makeCluster(clusterN)
        }

        registerDoSNOW(cl)
        t1 <- Sys.time()
        picusDF <- foreach (sp = as.character(spp)) %dopar% { # sp <- as.character(spp[9])
            require(stringr)
            require(foreach)
            
            #### fetching .csv files
            sppIndex <- agrep(sp, x)  ## fuzzy match
            deadwoodIndex <- grep("Deadwood", x)
            standIndex <- grep("Stand", x)
            #### read corresponding .csv files
            sppDeadwood <- x[intersect(sppIndex, deadwoodIndex)]
            sppStand <- x[intersect(sppIndex, standIndex)]

            df1 <- foreach(j = seq_along(sppStand), .combine="rbind") %dopar% {
                require(stringr)
                standCSV <- sppStand[j]
                deadCSV <- sppDeadwood[j]
                #time1 <- Sys.time()
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
                    tmp <- tmp[,c("ecozone", "scenario", "period", "landtype", "species","Year", "BiomassAbove_kg_ha", "DiedBiomassAbove_kg","anpp")]
                }
                #try(print(paste(areaCode, s, p, landtype, sp, as.character(round(time2-time1, 2)), "sec.")))
                return(tmp)
            }
            
            #### storing 1 dataframe per species
            return(df1)
        }
        stopCluster(cl)
        picusOutputs[[s]][[p]] <- picusDF
        rm(picusDF)
        t2 <- Sys.time()
        print(paste(s, p, round(t2-t1, 1)))
    }

    picusOutputsDF <- foreach(s = seq_along(picusOutputs), .combine = "rbind") %do% {
        foreach(p = seq_along(picusOutputs[[s]]), .combine = "rbind") %do% {
            do.call("rbind", picusOutputs[[s]][[p]])
        }
    }
   
    ### converting character vectors into factor (not that useful since it all goes into a .csv file)
    picusOutputsDF[,"ecozone"] <- as.factor(picusOutputsDF[,"ecozone"])
    picusOutputsDF[,"scenario"] <- as.factor(picusOutputsDF[,"scenario"])
    picusOutputsDF[,"period"] <- as.factor(picusOutputsDF[,"period"])
    picusOutputsDF[,"landtype"] <- as.factor(picusOutputsDF[,"landtype"])
    picusOutputsDF[,"species"] <- as.factor(picusOutputsDF[,"species"])

    write.csv(picusOutputsDF, paste0(processedOutputDir, "/picusOutputsDF_", areaCode, ".csv"), row.names=FALSE)
    
    rm(picusOutputsDF)
}


