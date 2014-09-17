######################
######################
#######
#######   Extraction of AnppMax and BiomassMax from PICUS outputs
#######   Dominic Cyr
#######
######################
######################
rm(list=ls())

#readDir <- paste("C:/Users/dcyr/Dropbox/Landis/SCF_IA_LANDIS")   ### read-only folder, cfl system
readDir <- ifelse(Sys.info()["sysname"]=="Linux",
                  paste("/media/dcyr/Windows7_OS/Dropbox/landis/SCF_IA_LANDIS"),
                  paste("C:/Dropbox/Landis/SCF_IA_LANDIS"))  ### read-only folder, on my laptop (windows or Linux)

vegCodes <- read.csv(file=paste(readDir, "vegCodes.csv", sep="/"))
######################
######################


######################
######################
##### The following section, including the big loop, is dependent on a specific file structure (PICUS outputs produced by Anthony Taylor)
############
folderNames <- list.dirs(paste(readDir, "Picus/Outputs", sep="/"), full.names=F, recursive=F)
outputInfo <- strsplit(folderNames, "_")
folderNames <- paste(readDir, "Picus/Outputs", folderNames, sep="/")

area <- unique(rapply(outputInfo, function(x) x[2]))
scenarios <- unique(rapply(outputInfo, function(x) x[3]))
periods <- unique(rapply(outputInfo, function(x) x[4]))
######################
######### This big loop read all csv files and gather informatino into a big dataframe
#############
require(stringr)
#############
########
picusOutputs <- list()
for (a in area) {
  picusOutputs[[a]] <- list()

  landtypeCodes <- read.csv(file=paste(readDir, "/", "landtypeCodes", area, ".csv", sep=""))  
  folderArea <- folderNames[grep(a, folderNames)]
  
  spp <- vegCodes[vegCodes[,a]==1,"PICUS_name"]
 
  for (i in seq_along(folderNames)) {
    
    s <- outputInfo[[i]][3]  ### nom du scénario
    p <- outputInfo[[i]][length(outputInfo[[i]])] ## nom de la période 
    
    #### creation of lists where dataframes are stored
    if (s %in% names(picusOutputs[[a]]) == F) {
      picusOutputs[[a]][[s]] <- list()  
    }         
    picusOutputs[[a]][[s]][[p]] <- list()
    #### 
    
    for (sp in as.character(spp)) { 
    

      
      #### fetching .csv files
      x <- list.files(paste(folderNames[i], sep="/"))
      sppIndex <- grep(sp, x)

        
      deadwoodIndex <- grep("Deadwood", x)
      standIndex <- grep("Stand", x)  
        
      #### indentifying .csv files
      
      #### read corresponding .csv files
      sppDeadwood <- x[intersect(sppIndex, deadwoodIndex)]  
      sppStand <- x[intersect(sppIndex, standIndex)]
    
      for (j in seq_along(sppStand)) { 
        time1 <- Sys.time()
        standTmp <- read.csv(paste(folderNames[i], sppStand[j], sep="/"))  
        if (nrow(standTmp)!=0)  {
          deadwoodTmp <- read.csv(paste(folderNames[i], sppDeadwood[j], sep="/")) 
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
          } else {picusDF <- tmp}
        time2 <- Sys.time()
        }
      print(paste(a, s, p, landtype, sp, as.character(round(time2-time1, 2)), "sec."))

      }
      #### storing 1 dataframe per species      
      picusOutputs[[a]][[s]][[p]][[sp]] <- picusDF[,c("ecozone", "scenario", "period", "landtype", "species","Year", "BiomassAbove_kg_ha", "DiedBiomassAbove_kg","anpp")]
      rm(picusDF)
    }
  }
}
######

#rm(picusOutputsDF)
##unlisting all dataframe and put them all in a big one
for (a in names(picusOutputs)){
  for (s in names(picusOutputs[[a]])){
    for (p in names(picusOutputs[[a]][[s]]))  {
      if (exists("picusOutputsDF")) {
        picusOutputsDF  <- rbind(picusOutputsDF, do.call("rbind", picusOutputs[[a]][[s]][[p]])) 
      } else {
        picusOutputsDF <- do.call("rbind", picusOutputs[[a]][[s]][[p]])
      }
    }
  }
}

picusOutputsDF[,"ecozone"] <- as.factor(picusOutputsDF[,"ecozone"])
picusOutputsDF[,"scenario"] <- as.factor(picusOutputsDF[,"scenario"])
picusOutputsDF[,"period"] <- as.factor(picusOutputsDF[,"period"])
picusOutputsDF[,"landtype"] <- as.factor(picusOutputsDF[,"landtype"])
picusOutputsDF[,"species"] <- as.factor(picusOutputsDF[,"species"])

processedDir <- paste(readDir, "picus/processedOutputs", sep="/")
dir.create(processedDir)
write.csv(picusOutputsDF, paste(processedDir, "picusOutputsDF.csv", sep="/"), row.names=FALSE)

#summary(picusOutputsDF)


  
