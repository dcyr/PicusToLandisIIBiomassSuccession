rm(list = ls())
setwd("/media/dcyr/Win/Users/Dominic Cyr/Desktop/NorthShore/")

wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(raster)
require(RCurl)
require(stringr)
require(vegan)
require(reshape2)
require(ggplot2)
################################################################
################################################################
#################  Loading general inputs and initial conditions

readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
ecozones <- read.csv(text = getURL(paste(readURL, "ecoNames.csv", sep="/")))

###
areas <- "NorthShore"
a <- areas[1]
spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )

## loading landtypes
readURL <- "https://github.com/dcyr/LANDIS-II_IA_generalUseFiles/raw/master/LandisInputs/"
tmpFile <- tempfile()
url <- paste(readURL, a, "/landtypes_", a, ".tif", sep="")
download.file(url, tmpFile, method="wget")
landtypes <- raster("../landtypes.tif")

### loading initial biomass
dtaDir <- "../initialBiomass/"
biomassKnn <- stack(paste0(dtaDir, "/initBiomassKnnTonsPerHa-", a, "-", spp, ".tif"))
names(biomassKnn) <- paste0(spp, "_tonsPerHa")
# removing inactive pixels
biomassKnn[is.na(landtypes)] <- NA
################################################################
simInfo <- read.csv("../simInfo.csv")
simDir <- simInfo$simDir
simDir <- str_pad(simDir, max(nchar(simDir)), pad = 0)
## total biomass (Knn estimates)
biomassKnnTotal <- sum(biomassKnn)
biomassKnnTotal_mean <- mean(values(biomassKnnTotal), na.rm = T)
## initial proportion (Knn estimates)
biomassKnnProp <- biomassKnn/biomassKnnTotal
#xMatKnn <- values(biomassKnnProp)



################################################################
######

outputs <- list.files()
outputs <- outputs[grep("processedOutputs", outputs)]
simNum <- gsub("[^0-9]", "", outputs)

# available sims
require(dplyr)
simInfo <- simInfo %>%
    filter(simDir %in% as.numeric(simNum))


simID <- character()
for (i in 1:nrow(simInfo)) {
    simN <- str_pad(simInfo[i, "simDir"], 4, pad = "0")
    simID <- append(simID, simN)
    result <- get(load(paste0("processedOutputs_", simN, ".RData")))
    
    if (i == 1) {
        totalBiomass <- result$biomassTotal
        brayDissAbs <- result$brayDistAbs
        brayDissRel <- result$brayDistRel
    } else {
        totalBiomass <- stack(totalBiomass, result$biomassTotal)
        brayDissAbs <- stack(brayDissAbs, result$brayDistAbs)
        brayDissRel <- stack(brayDissRel, result$brayDistRel)
    }
    
}

names(totalBiomass) <- names(brayDissAbs) <- names(brayDissRel) <- simID
simID <-  gsub("X", "", simID)

### reformatting 'totalBiomass' for analysis and plotting 
totalBiomassDF <- rasterToPoints(totalBiomass)
colnames(totalBiomassDF)[3:ncol(totalBiomassDF)] <- simID
totalBiomassDF <- melt(as.data.frame(totalBiomassDF), id.vars = c("x", "y"),
                       variable.name = "simID", value.name = "biomassTotal_tonsPerHa")
rm(totalBiomass)

### reformatting 'brayDistAbs' and 'brayDistRel for analysis and plotting
brayDissAbsDF <- rasterToPoints(brayDissAbs)
colnames(brayDissAbsDF)[3:ncol(brayDissAbsDF)] <- simID
brayDissAbsDF <- melt(as.data.frame(brayDissAbsDF), id.vars = c("x", "y"),
                      variable.name = "simID", value.name = "brayDissAbs")
rm(brayDissAbs)

brayDissRelDF <- rasterToPoints(brayDissRel)
colnames(brayDissRelDF)[3:ncol(brayDissRelDF)] <- simID
brayDissRelDF <- melt(as.data.frame(brayDissRelDF), id.vars = c("x", "y"),
                      variable.name = "simID", value.name = "brayDissRel")
rm(brayDissRel)


#### summarizing outputs

require(dplyr)
totalBiomassSummary <- totalBiomassDF %>%
    group_by(simID) %>%
    summarise(totalBiomassMean_tonsPerHa = mean(biomassTotal_tonsPerHa))

brayDissAbsSummary <- brayDissAbsDF %>%
    group_by(simID) %>%
    summarise(brayDissAbs_mean = mean(brayDissAbs))

brayDissRelSummary <- brayDissRelDF %>%
    group_by(simID) %>%
    summarise(brayDissRel_mean = mean(brayDissRel))

simInfo[,"simID"] <- str_pad(simInfo[,"simDir"], 4, pad = "0")
calibSummary <- merge(simInfo, totalBiomassSummary)
calibSummary <- merge(calibSummary, brayDissAbsSummary)
calibSummary <- merge(calibSummary, brayDissRelSummary)








foo <- calibSummary %>%
    mutate(brayDistDiff = brayDist_mean-min(brayDist_mean))

colScale <- scale_color_gradientn(name = "diff", limits = c(0,0.25),
                                 colours = rev(c("lightblue", "gold2", "darkred")),
                                 values = c(0, 0.01, 1))


p <- ggplot(foo, aes(x = maxBiomassMultiplier, y = totalBiomassMean_tonsPerHa,
                              group = averageMaxBiomassTarget,
                              colour = brayDistDiff)) +
    geom_line(size = 0.5) +
    colScale +
    #scale_colour_discrete(palette = "Set1") +
    geom_hline(aes(yintercept=biomassKnnTotal_mean))


png(filename="totalBiomassCalibration.png",
    width = 6, height = 3, units = "in", res = 300, pointsize=8)

print(p +
          labs(y =  "Average total biomass (tons/ha)",
               x = "maxB multiplier (all species)",
               title = "Calibration of total biomass (all species)"))


>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0

simInfo[,"simID"] <- str_pad(simInfo[,"simDir"], 3, pad = "0")
calibSummary <- merge(simInfo, brayDistSummary)
calibSummary <- merge(calibSummary, totalBiomassSummary)


<<<<<<< HEAD
calibSummary <- calibSummary %>%
    mutate(brayDistDiff = brayDist_mean-min(brayDist_mean))

calibSummary <- calibSummary %>%
    group_by(maxBiomassMultiplier) %>%
    summarize(maxBTargetLabel = max(averageMaxBiomassTarget),
              ratio_max = brayDist_mean[which.max(averageMaxBiomassTarget)],
              brayDist_min = min(brayDist_mean),
              minBrayRatio = averageMaxBiomassTarget[which.min(brayDist_mean)]) %>%
    merge(calibSummary) %>%
    group_by(averageMaxBiomassTarget) %>%
    summarize(totalBiomassLabel = totalBiomassMean_tonsPerHa[which.max(maxBiomassMultiplier)]) %>%
    merge(calibSummary)
=======
p <- ggplot(calibSummary, aes(x = averageMaxBiomassTarget, y = brayDist_mean,
                              group = maxBiomassMultiplier,
                              colour = maxBiomassMultiplier)) +
    geom_line() +
    #scale_colour_discrete(palette = "Set1") +
    geom_vline(aes(xintercept=0.2169649), colour = "blue")


png(filename="ABIE.BAL_relPropCalibration.png",
    width = 6, height = 3, units = "in", res = 300, pointsize=8)
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0

print(p +
          labs(y = "Average Bray-Curtis dissimilarity",
               x = "maxB ABIE.BAL : maxB landtype (ratio)",
               title = "calibration of ABIE.BAL's relative importance"))

<<<<<<< HEAD

colScale <- scale_color_gradientn(name = "diff", limits = c(0,0.25),
                                 colours = rev(c("lightblue", "gold2", "darkred")),
                                 values = c(0, 0.1, 1))


p <- ggplot(calibSummary, aes(x = maxBiomassMultiplier, y = totalBiomassMean_tonsPerHa,
                              group = averageMaxBiomassTarget,
                              colour = averageMaxBiomassTarget)) +
    geom_line(size = 0.25) +
    geom_text(aes(x = max(maxBiomassMultiplier),
                  y = totalBiomassLabel,
                  label = averageMaxBiomassTarget),
              size = 2, hjust = 0, vjust = 0.5) +
    #colScale +
    #scale_colour_discrete(palette = "Set1") +
    geom_hline(aes(yintercept=biomassKnnTotal_mean))


png(filename="totalBiomassCalibration.png",
    width = 6, height = 4, units = "in", res = 300, pointsize=8)

print(p +
          labs(y =  "Average total biomass (tons/ha)",
               x = "maxB multiplier (all species)",
               title = "Calibration of total biomass",
               subtitle = "Each line represent one case of ABIE.BAL's relative potential target") +
          theme(legend.position="none"))



dev.off()


calibSummary <- calibSummary %>%
    group_by(maxBiomassMultiplier) %>%
    summarize(maxBTargetLabel = max(averageMaxBiomassTarget),
              ratio_max = brayDist_mean[which.max(averageMaxBiomassTarget)],
              brayDist_min = min(brayDist_mean),
              minBrayRatio = averageMaxBiomassTarget[which.min(brayDist_mean)]) %>%
    merge(calibSummary)
    


p <- ggplot(calibSummary, aes(x = averageMaxBiomassTarget, y = brayDist_mean,
                              group = maxBiomassMultiplier,
                              colour = maxBiomassMultiplier)) +
    geom_line() +
    #scale_colour_discrete(palette = "Set1") +
    geom_vline(aes(xintercept = 0.2169649), colour = "blue", linetype = 3, size = 0.5) +
    geom_vline(aes(xintercept = minBrayRatio), linetype = 3, size = 0.25) +
    geom_text(aes(x = maxBTargetLabel, y = ratio_max, label = maxBiomassMultiplier),
               size = 2, hjust = 0, vjust = 0) +
    geom_text(aes(x = minBrayRatio, y = brayDist_min, label = minBrayRatio),
              size = 1.5, hjust = 0, vjust = 0, angle = 270)


png(filename="ABIE.BAL_relPropCalibration.png",
    width = 6, height = 4, units = "in", res = 300, pointsize=8)

print(p +
          labs(y = "Average Bray-Curtis dissimilarity",
               x = "maxB ABIE.BAL : maxB landtype (ratio)",
               title = "Calibration of ABIE.BAL's relative importance",
               subtitle = "Comparision of absolute abundance (tons / ha)") +
          theme(legend.position="none"))



=======


>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
dev.off()


# #############################################################
# #############################################################
# #############################################################
# ###### maps !
<<<<<<< HEAD
# targetSubsample <- c(.2, .425, .675, 0.9)
=======
# targetSubsample <- seq(.3, .9, 0.15)
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
# maxBmultSubsample <- seq(0.5, 1, 0.1)
# 
# simInfoSubsample <- simInfo %>%
#     filter(averageMaxBiomassTarget %in% targetSubsample,
#            maxBiomassMultiplier %in% maxBmultSubsample)
# 
# unique(simInfo$averageMaxBiomassTarget)
# folderSubset <- simInfoSubsample$simDir
# 
# require(doSNOW)
# require(parallel)
<<<<<<< HEAD
# cl = makeCluster(4, outfile = "") ##
=======
# cl = makeCluster(6, outfile = "") ##
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
# registerDoSNOW(cl)
# 
# foreach(sp = spp) %dopar% {
#     require(raster)
#     require(ggplot2)
#     require(reshape2)
#     require(stringr)
<<<<<<< HEAD
# 
#     for (i in folderSubset) {#seq_along(simDir)) {
# 
=======
#     for (i in folderSubset) {#seq_along(simDir)) {
#         
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
#         target <- simInfo[i, "averageMaxBiomassTarget"]
#         outputDir <- paste0("../", simDir[i], "/output/biomass")
#         outputs <- list.files(outputDir)
#         outputs <- outputs[grep(paste(sp, collapse = "|"), outputs)]
#         outputs <- raster(paste(outputDir, outputs, sep = "/"))
#         if (i == folderSubset[1]) {
#             x <- outputs
#         } else {
#             x <- stack(x, outputs)
#         }
#     }
#     crs(x) <- crs(landtypes)
#     extent(x) <- extent(landtypes)
#     x[is.na(landtypes)] <- NA
<<<<<<< HEAD
# 
#     ### convert to tons per ha
#     x <- x / 100
# 
# 
#     ### identifying layers
#     layerNames <- paste(simInfoSubsample$averageMaxBiomassTarget,
#                         simInfoSubsample$maxBiomassMultiplier, sep = "_")
# 
#     ### computing differences between spinup and knn estimates
#     diff <- x - biomassKnn[[grep(sp, names(biomassKnn))]]
#     # mean differences
#     diffMean <-  apply(values(diff), 2, mean, na.rm = T)
#     names(diffMean) <- layerNames
#     # mean differences when present
#     diff[x == 0] <- NA
#     diffMeanPresent <-  apply(values(diff), 2, mean, na.rm = T)
#     diffSdPresent <- apply(values(diff), 2, sd, na.rm = T)
#     names(diffMeanPresent) <- names(diffSdPresent) <- layerNames
#     # # bring zero values back
#     # diff[x == 0] <- 0
#     
#     ## smoothing to increase contrast, better visual. of spatial patterns
#     diffSmoothed <- list()
#     for(j in 1:nlayers(diff)) {
#         diffSmoothed[[j]] <- focal(diff[[j]], matrix(1, ncol = 9, nrow = 9), mean, na.rm = T)
=======
#     
#     ### convert to tons per ha
#     x <- x / 100
#     
#     
#     ### identifying layers
#     layerNames <- paste(simInfoSubsample$averageMaxBiomassTarget,
#                         simInfoSubsample$maxBiomassMultiplier, sep = "_")
#     
#     
#     diff <- x - biomassKnn[[grep(sp, names(biomassKnn))]]
#     ## smoothing to increase contrast, better visual. of spatial patterns
#     diffSmoothed <- list()
#     for(j in 1:nlayers(diff)) {
#         diffSmoothed[[j]] <- focal(diff[[j]], matrix(1, ncol = 5, nrow = 5), mean, na.rm = T) 
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
#     }
#     rm(list = c("diff", "x"))
#     diffSmoothed <- stack(diffSmoothed)
#     diffSmoothed[is.na(landtypes)] <- NA
<<<<<<< HEAD
# 
# 
=======
#     
#     
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
#     df <- rasterToPoints(diffSmoothed)
#     df <- as.data.frame(df)
#     colnames(df)[3:ncol(df)] <-  layerNames
#     df <- melt(df, id.vars = c("x", "y"), variable.name = "layer")#, measure.vars = "biomassDiff_tonsPerHa")
<<<<<<< HEAD
# 
#     layerInfo <- str_split(df$layer, "_")
# 
#     df[, "spRelmaxB"] <- paste("ABIE.BAL maxB:maxBtotal =", as.numeric(lapply(layerInfo, function(x) x[1])))
#     df[, "maxBMultiplier"] <- paste("maxB multiplier =", as.numeric(lapply(layerInfo, function(x) x[2])))
#     
#     #############
#     diffMean <- data.frame(layer = factor(names(diffMeanPresent), levels = levels(df$layer)),
#                       bias = paste(round(diffMean, 1), "/",
#                                    round(diffMeanPresent, 1),
#                                    "+-", round(diffSdPresent, 1)))
# 
#     df <- merge(df, diffMean)
#     
#     
#     colScale <- scale_fill_gradient2(name = "diff",
#                                      low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0)
# 
=======
#     
#     layerInfo <- str_split(df$layer, "_")
#     
#     df[, "spRelmaxB"] <- paste("ABIE.BAL maxB:maxBtotal =", as.numeric(lapply(layerInfo, function(x) x[1])))
#     df[, "maxBMultiplier"] <- paste("maxB multiplier =", as.numeric(lapply(layerInfo, function(x) x[2])))
#     
#     colScale <- scale_fill_gradient2(name = "diff",
#                                      low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0)
#     
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
#     p <- ggplot(data = df, aes(x = x, y = y, fill = value)) +
#         theme_dark() +
#         geom_raster() +
#         coord_fixed() +
#         colScale +
#         facet_grid(spRelmaxB ~ maxBMultiplier) +
#         theme(axis.text = element_blank(),
<<<<<<< HEAD
#               axis.title = element_blank(),
#               axis.ticks = element_blank())
# 
#     
#     yMax <- layer_scales(p)$y$range$range[2]
#     xMax <- layer_scales(p)$x$range$range[2]
#    
#     ### don't work with new facetting (ggplot2 2.2.)
#     # ## creating 'label' data.frame that report average bias
#     # labelDF <-  data.frame(x = xMax, y = yMax,
#     #                        spRelmaxB = as.numeric(lapply(str_split(names(diffMean), "_"), function(x) x[1])),
#     #                        maxBMultiplier = as.numeric(lapply(str_split(names(diffMean), "_"), function(x) x[2])),
#     #                        meanBias = diffMean)
#     # labelDF[,"spRelmaxB"] <- paste("ABIE.BAL maxB:maxBtotal =",  labelDF[,"spRelmaxB"])
#     # labelDF[,"maxBMultiplier"] <- paste("maxB multiplier =",  labelDF[,"maxBMultiplier"])
#    
#     
# 
#     
#     png(filename = paste0("diffInit_", sp, ".png"),
#         width = 12, height = 10, units = "in", res = 600, pointsize=10)
# 
#         print(p +
#                   labs(title = paste0("Difference between initial biomass after LANDIS spinup and Knn estimations\n", sp)) +
#                   geom_text(aes(x = xMax, y = yMax,
#                                 label = "bias (global / when present)"),
#                             #data = labelDF,  ## don't work with ggplot2 2.2.0
#                             hjust = 1, size = 2.25, fontface = 1) +
#                   geom_text(aes(x = xMax, y = yMax - 20000,
#                                 label = bias),
#                             #data = labelDF,  ## don't work with ggplot2 2.2.0
#                             hjust = 1, size = 2.75, fontface = "bold"))
#                   
=======
#               #axis.text.y =  element_blank(),
#               axis.title = element_blank(),
#               axis.ticks = element_blank())#,
#     #axis.ticks = element_blank())
#     
#     
#     png(filename = paste0("diffInit_", sp, ".png"),
#         width = 12, height = 8, units = "in", res = 600, pointsize=10)
#     
#     print(p + labs(title = paste0("Difference between initial biomass after LANDIS spinup and Knn estimations\n", sp)))
#     
#     
>>>>>>> 7761088794a6198bec75a25aef6dc1081a34eac0
#     dev.off()
# }
# 
# stopCluster(cl)

