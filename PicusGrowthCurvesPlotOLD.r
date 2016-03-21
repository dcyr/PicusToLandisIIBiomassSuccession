######################
######################
#######
#######   Production of reference files for calibrating the growth curve parameter of Landis-II Biomass succession
#######   (depends on "picusOutputsDF.RData", produced by "PicusOutputsToDF.r")
#######   Dominic Cyr
#######
#####################
######################
rm(list=ls())
setwd("/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
processedDir <-  "..//processedOutputs"


### vegCodes is the species master list
### It indicates which species to look in picus output folders
require(RCurl)
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))

###
x <- list.files(processedDir)
x <- x[grep("picusOutputsDF", x)]
###
areas <- unique(gsub("picusOutputsDF_|.csv", "", x))




for (a in )
######################
picusOutputsDF <- read.csv(paste(readDir, "/Picus/picusOutputsDF.csv", sep="/"))  ### this .csv file is produced by
picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
######################
LandisSiteDir <- "C:/Travail/SCF/Landis/LandisGrowthCalib/LANDIS-II Site/output"






#
# #################### fetching landisSite outputs
# sims <- list.files(LandisSiteDir)
# #sims <- sims[grep("test", sims)]
# simInfo <- strsplit(sims, "_")
# spp <- rapply(simInfo, function(x) x[1])
# simLandtypes <- rapply(simInfo, function(x) gsub("[^0-9]", "", x[2]))
# ###################
# ###################
# landisSiteOutputs <- list()
# for (i in seq_along(sims))  { # i<-1
#  simFiles <- list.files(paste(LandisSiteDir, sims[i], sep="/"))
#  landisSiteOutputs[[sims[i]]] <- list()
#  for (j in seq_along(simFiles)) {  ## j<- 1
#    t1 <- Sys.time()
#    output <- read.csv(paste(LandisSiteDir, sims[i], simFiles[j], sep="/"))
#    output[,"species"] <- spp[i]
#    output[,"landtype"] <- simLandtypes[i]
#    replicate <- gsub("[^0-9]", "", simFiles[j])
#    output[,"replicate"] <- as.numeric(replicate)
#    landisSiteOutputs[[sims[i]]][[replicate]] <- output
#    print(paste(sims[i], simFiles[j], round(Sys.time()-t1, 3)))
#  }
# }
# ###################
# ## then, unlisting everything (it is faster to proceed that way)
# tmp <- list()
# for (i in seq_along(landisSiteOutputs)){
#   tmp[[i]] <- do.call("rbind", landisSiteOutputs[[i]])
#   print(i)
# }
# landisSiteOutputs <- do.call("rbind", tmp)
#
# landisSiteOutputs$landtype <- as.factor(landisSiteOutputs$landtype)
# landisSiteOutputs$species <- as.factor(landisSiteOutputs$species)
# ####################
# #save(landisSiteOutputs, file=paste(wwd, "landisSiteOutputs.rData", sep="/"))
# ####################
#
#
#
#
# #################
# ##### replicates subsamples
# #################
# replicateSubsample <- data.frame(shape = c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1, 1,
#                                            0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
#                                  r1 = c(50, 50, 50, 50, 50, 50, 50, 50,
#                                         50, 50, 50, 50, 50, 50, 50, 50))
# rownames(replicateSubsample) <- unique(spp)
#
#
#
#
# ####################
# ## To retrieve info (may have changed location)
# #landisSiteOutputs <- get(load("C:/Travail/SCF/Landis/LandisGrowthCalib/landisSiteOutputs.RData"))
# ######
#
# legend <- c(rep("fast", 25), rep("intermediate",25), rep("slow", 25))
# legend <- cbind(legend, colors=ifelse(legend=="fast", "darkred", ifelse(legend=="intermediate", "darkgreen", "darkblue")))
#
# ####################
# for (sp in unique(spp)) {  # sp <- "ABIE.BAL"
#   sppLatin <- as.character(vegCodes[which(vegCodes$Code_LANDIS==sp),"PICUS_name"])
#
#   dfRef <- subset(picusOutputsDF, subset=as.character(picusOutputsDF$species)==sppLatin &
#                  picusOutputsDF$period == "Baseline",
#                select= c("Year","landtype", "BiomassAbove_kg_ha"))
#   attach(dfRef)
#
#   LandisSiteSp <- subset(landisSiteOutputs, subset=as.character(landisSiteOutputs$species)==sp)
#   simLandtypes <- unique(LandisSiteSp$landtype)
#   #n <- length() ### number of landtypes to display (could be selected randomly, now the first ones are selected)
#   #colors <- rainbow(n, start=.7, end=.1)
#
#   Nlandtypes <- length(simLandtypes)
#   figRows <- ceiling(Nlandtypes/2)
#
#
#   fontRatio <- 1/(1-(figRows-3)/3)
#
#   png(filename = paste(wwd,"/picusGrowth_", sp, ".png", sep=""), width = 8, height =1.375*figRows+2.5,
#     units = "in", pointsize = 8, bg = "white",
#     res = 600, restoreConsole = TRUE)
#
#     par(mfrow=c(figRows,2))
#     par(mar=c(2,2,1,1))
#     par(oma=c(5,5,6,1))
#
#
#     for (i in 1:min(c(length(levels(simLandtypes)),length(simLandtypes)))) {#
#
#
#       lIndex <- which(landtype==as.character(simLandtypes[i]))
#       xl <- Year[lIndex]
#       yl <- BiomassAbove_kg_ha[lIndex]
#       lIndex <- which(yl > 0)
#       yl <- c(0,yl[lIndex])
#       xl <- seq(from=0, to=length(yl)-1, by=1)
#       plot(x=xl, y=yl, type="l", ylab="",lwd=2, col="black", cex=1.5*fontRatio, xlim=c(0,600))
#
#       yMax <- max(yl)
#       text(x=600, y=0.4*yMax, sp, adj=c(1,1), cex=1.5*fontRatio, col="darkred")
#       text(x=600, y=0.3*yMax, paste("landtype =",simLandtypes[i]), adj=c(1,1), cex=1.5*fontRatio, col="darkred")
#
#       ##fetching landisSite replicates
#       LandisSiteSpLT <- LandisSiteSp[which(LandisSiteSp$landtype==simLandtypes[i]),]
#       replicates <- unique(LandisSiteSpLT$replicate)
#       replicates <- sample(replicates, length(replicates))
#
#       threshold <- 100
#       meanNPPRef <- mean(diff(yl[0:threshold]))
#       MaxBiomass <- mean(yl[(threshold+1):length(yl)])
#       meanNPPRealized <- maxBiomassRealized <- numeric()
#       for (r in replicates){ # r <- 1
#         if (r >= replicateSubsample[sp,"r1"]) {
#
#           lIndex <- which(LandisSiteSpLT$replicate==r)
#           xl <- LandisSiteSpLT$Year[lIndex]
#           yl <- LandisSiteSpLT$Biomass[lIndex]*10
#           lines(x=xl, y=yl, lwd=.25, col=legend[r+1, "colors"])
#           meanNPPRealized <- append(meanNPPRealized, mean(diff(yl[1:threshold])))
#           maxBiomassRealized <- append(maxBiomassRealized, mean(yl[(threshold+1):(length(yl))]))
#         }
#       } ## j <- 1
#       grid()
#       #### compute averages
#       biasNPP <- 100*(mean(meanNPPRealized)-meanNPPRef)/meanNPPRef
#       biasMaxB <- 100*(mean(maxBiomassRealized)-MaxBiomass)/MaxBiomass
#       text(x=600, y=0.2*yMax, paste("Pre",threshold, "y. meanNPP biais: ", ifelse(biasNPP>0, "+", ""), round(biasNPP,1), "%", sep=""), adj=c(1,1), cex=1.5*fontRatio, col="darkred")
#       text(x=600, y=0.1*yMax, paste("Post", threshold, "y. av,biom. biais: ", ifelse(biasMaxB>0, "+", ""), round(biasMaxB,1), "%", sep=""), adj=c(1,1), cex=1.5*fontRatio, col="darkred")
#     }
#     mtext("Comparison between pure stands growth as simulated\nby Picus (bold lines) and Landis-II (50 replicates)",
#            side=3, line=1, outer=TRUE, cex=1.5)
#     mtext(expression(paste('Aboveground biomass (',kg/ha,')',sep='')),#expression(paste('Aboveground biomass (',g/m^2,')',sep=''))
#         side=2, line=1.5, outer=TRUE, cex=1.5)
#     mtext("Year",
#         side=1, line=2, outer=TRUE, cex=1.5)
#
#   dev.off()
# }






############## figure de toutes les courbes (tous les landtypes) pour chaque espèces

png(filename = paste(wwd,"/picusGrowth_all_spp.png", sep=""), width = 6, height = 4,
    units = "in", pointsize = 6, bg = "white",
    res = 600, restoreConsole = TRUE)

  par(mfrow=c(5,4))
  par(mar=c(2,2,1,1))
  par(oma=c(5,5,6,1))

  for (sp in levels(picusOutputsDF$species)) { # sp <- levels(picusOutputsDF$species)[1]
    df <- subset(picusOutputsDF, subset=picusOutputsDF$species==sp &
                                        picusOutputsDF$period == "Baseline",
                                select= c("Year","landtype", "BiomassAbove_kg_ha"))

    attach(df)






    plot(x=Year-2000, y=BiomassAbove_kg_ha, type="n", xlab="Year",# (from the moment of establishment)",
         main="")
    for (i in seq_along(levels(landtype))){ # i <- 1
      lIndex <- which(landtype==levels(landtype)[i])
      yl <- BiomassAbove_kg_ha[lIndex]
      #lIndex <- which(yl > 0)
      #yl <- c(0,yl[lIndex])
      xl <- Year[lIndex]-2000
      #xl <- seq(from=0, to=length(yl)-1, by=1)

      lines(x=xl, y=yl, lwd=0.2)

    }
    grid()
    text(x=max(Year)-2000, y=max(BiomassAbove_kg_ha), sp, adj=c(1,1))
    text(x=max(Year)-2000, y=0.90*max(BiomassAbove_kg_ha), "(Baseline)", adj=c(1,1))




  }
  mtext("Pure stand growth as simulated by Picus",
        side=3, line=2.5, outer=TRUE, cex=1.5)
  mtext("(*Each line represents a different landtype, N=80)",
      side=3, line=1, outer=TRUE, cex=1)
  mtext(expression(paste('Aboveground biomass (',kg/ha,')',sep='')),#expression(paste('Aboveground biomass (',g/m^2,')',sep=''))
        side=2, line=1.5, outer=TRUE, cex=1.5)
  mtext("Year",
        side=1, line=2, outer=TRUE, cex=1.5)
dev.off()

################







