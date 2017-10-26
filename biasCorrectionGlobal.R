################################################################
################################################################
#################  This script should be ran right after the raw
#################  outputs have been processed by
#################  'biasCorrectionOutputProcess.R'
#################  
#################  It takes about one minute for a thousand simulations
rm(list = ls())
a <- "ALPAC"
###
setwd(paste("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/", a, sep = "/"))
wwd <- paste(paste(getwd(), Sys.Date(), sep = "/"))
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.

###
require(raster)
require(RCurl)
require(stringr)
require(vegan)
require(reshape2)
require(ggplot2)
require(data.table)
require(dplyr)

################################################################
################################################################
#################  Loading general inputs and initial conditions

readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
ecozones <- read.csv(text = getURL(paste(readURL, "ecoNames.csv", sep="/")))

###
spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )

## loading landtypes
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



# #################################################################################################
# #################################################################################################
# #################  calibration figures - First pass (total biomass and global composition)
# outputDir <- ifelse(Sys.info()["nodename"] == "dcyr-ThinkPad-X220",
#                     paste0("/media/dcyr/Seagate Backup Plus Drive/Sync/Sims/", a, "Calib/processedOutputs"),
#                     paste0("/media/dcyr/Data/Sims/", a, "Calib/processedOutputs"))
# ##
# outputs <- list.files(outputDir)
# outputs <- outputs[grep("biomassTotal", outputs)]
# simNum <- gsub("[^0-9]", "", outputs)
# 
# ############################################################
# ####### compiling results
# cl = makeCluster(clusterN, outfile = "") ##
# registerDoSNOW(cl)
# ##
# dfSummary <- foreach(i = 1:nrow(simInfo), .combine = "rbind") %dopar% {
#     require(raster)
#     require(stringr)
#     simN <- str_pad(simInfo[i, "simDir"], max(nchar(simNum)), pad = "0")
# 
#     biomassTotal <- raster(paste0(outputDir, "/biomassTotal_", simN, ".tif"))
#     distAbs <- raster(paste0(outputDir, "/distAbs_", simN, ".tif"))
#     distRel <- raster(paste0(outputDir, "/distRel_", simN, ".tif"))
# 
#     x <- data.frame(biomassTotalMean_tonsPerHa =  mean(values(biomassTotal), na.rm = T),
#                     brayDissAbs_mean =  mean(values(distAbs), na.rm = T),
#                     brayDissRel_mean =  mean(values(distRel), na.rm = T),
#                     simID = simN,
#                     simInfo[i, c("averageMaxBiomassTarget",
#                                  "spAnppMultiplier",
#                                  "spBiomassMultiplier",
#                                  "maxBiomassMultiplier",
#                                  "spinupMortalityFraction")]
#     )
# 
#     print(i)
#     return(x)
# }
# stopCluster(cl)
# save(dfSummary, file = "dfSummary.RData")





############################################################
####### Plots
dfSummary <- get(load("../results/dfSummary.RData"))

#######
require(plot3D)
width <- 4
height <- 3.75
labelRel = .6
titleRel = .75

#### grid lines for surface inter/extrapolation (careful with extrapolation because this is loess!!)
gridLines <- 100

xPred <- seq(min(dfSummary$spinupMortalityFraction),
             max(dfSummary$spinupMortalityFraction), length.out = gridLines)
yPred <- seq(min(dfSummary$averageMaxBiomassTarget),
             max(dfSummary$averageMaxBiomassTarget), length.out = gridLines)
xy <- expand.grid( x = xPred, y = yPred)

maxBmult <- unique(dfSummary$maxBiomassMultiplier)
maxBmult <- maxBmult[order(maxBmult)]

############################################################
####### Bray dissimilarities - Absolute Abundances
#######
zlim <- c(floor(min(dfSummary$brayDissAbs_mean)*10)/10-0.05,
             ceiling(max(dfSummary$brayDissAbs_mean)*10)/10 + 0.05)

fListAbs <- character()
for (i in maxBmult) {
    df <- dfSummary %>%
        filter(abs(dfSummary$maxBiomassMultiplier - i) < 0.0001)

    x <- df$spinupMortalityFraction
    y <- df$averageMaxBiomassTarget
    z <- df$brayDissAbs_mean

    ###
    fit <- loess(z ~ x + y, span = 0.4, control=loess.control(surface="direct"))
    ###

    zPred <- matrix(predict(fit, newdata = xy),
                    nrow = gridLines, ncol = gridLines)
    # fitted points for droplines to surface
    fitpoints <- predict(fit)

    fTitleAbs <- paste0("calibrationCompositionAbs_maxBmult",
                        ifelse(i == 1, "1.00", str_pad(i, "4", pad = "0", side = "right")),
                        ".png")

    fListAbs <- append(fListAbs, fTitleAbs)
    ############################################################
    png(filename = fTitleAbs,
        width = width, height = height, units = "in", res = 300, pointsize = 8)
    # scatter plot with regression plane
    scatter3D(x, y, z,
              zlim = zlim,
              xlim = range(xPred),
              ylim = range(yPred),
              #col = jet2.col(200),
              pch = 20, cex = 0.35,
              cex.lab = labelRel,
              cex.axis = labelRel,
              theta = 230, phi = 25,
              ticktype = "detailed",
              xlab = "\nspinup mortality fraction",
              ylab = "\nABIE.BAL maxB target ratio",
              zlab = "\nAverage dissimilarity",
              surf = list(x = xPred, y = yPred, z = zPred,
                          facets = NA, fit = fitpoints,
                          lwd = 0.15),
              colkey = list(#dist = 0.025,
                  cex.clab = labelRel,
                  cex.axis = labelRel),
              clab = c("Average", "dissimilarity"),
              clim = zlim,
              # clim = c(floor(min(zPred)*10)/10,
              #          ceiling(max(zPred)*10)/10),
              main = c("Landis-II Biomass Succession - Composition calibration",
                       paste0("(Absolute abundances; max biomass multiplier = ", i, ")")),

              cex.main = titleRel)

    dev.off()
}

require(animation)
oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.75, autobrowse = FALSE)
### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
im.convert(fListAbs, output = "calibrationCompositionAbs.gif",
           extra.opts = "", clean = F)


############################################################
### Bray dissimilarities - relative abundances
#######
zlim <- c(floor(min(dfSummary$brayDissRel_mean)*10)/10-0.05,
          ceiling(max(dfSummary$brayDissRel_mean)*10)/10 + 0.05)



maxBmult <- unique(dfSummary$maxBiomassMultiplier)
maxBmult <- maxBmult[order(maxBmult)]
fListRel <-  character()
for (i in maxBmult) {
    df <- dfSummary %>%
        filter(abs(dfSummary$maxBiomassMultiplier - i) < 0.0001)

    x <- df$spinupMortalityFraction
    y <- df$averageMaxBiomassTarget
    z <- df$brayDissRel_mean
    ###
    fit <- loess(z ~ x + y, span =  0.35, control=loess.control(surface="direct"))
    #
    zPred <- matrix(predict(fit, newdata = xy),
                    nrow = gridLines, ncol = gridLines)
    # fitted points for droplines to surface
    fitpoints <- predict(fit)


    fTitleRel <- paste0("calibrationCompositionRel_maxBmult",
                        ifelse(i == 1, "1.00", str_pad(i, "4", pad = "0", side = "right")),
                        ".png")

    fListRel <- append(fListRel, fTitleRel)
    ############################################################
    png(filename = fTitleRel,
        width = width, height = height, units = "in", res = 300, pointsize = 8)
    ### scatter plot with regression plane
    scatter3D(x, y, z,
              zlim = zlim,
              xlim = range(xPred),
              ylim = range(yPred),
              #col = jet2.col(200),
              pch = 20, cex = 0.35,
              cex.lab = labelRel,
              cex.axis = labelRel,
              theta = 230, phi = 25,
              ticktype = "detailed",
              xlab = "\nspinup mortality fraction",
              ylab = "\nABIE.BAL maxB target ratio",
              zlab = "\nAverage dissimilarity",
              surf = list(x = xPred, y = yPred, z = zPred,
                          facets = NA, fit = fitpoints,
                          lwd = 0.15),
              colkey = list(#dist = 0.025,
                  cex.clab = labelRel,
                  cex.axis = labelRel),
              clab = c("Average", "dissimilarity"),
              clim = zlim,
              main = c("Landis-II Biomass Succession - Composition calibration",
                       paste0("(Relative abundances; max biomass multiplier = ", i, ")")),
              cex.main = titleRel)

    dev.off()

}

require(animation)
oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.5, autobrowse = FALSE)
### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
im.convert(fListRel, output = "calibrationCompositionRel.gif",
           extra.opts = "", clean = F)




############################################################
### Total biomass
zlim <- range(dfSummary$biomassTotalMean_tonsPerHa - biomassKnnTotal_mean)

xPred <- seq(min(dfSummary$averageMaxBiomassTarget),
             max(dfSummary$averageMaxBiomassTarget), length.out = gridLines)
yPred <- seq(min(dfSummary$maxBiomassMultiplier),
             max(dfSummary$maxBiomassMultiplier), length.out = gridLines)
xy <- expand.grid( x = xPred, y = yPred)


fList <- character()
for (i in unique(dfSummary$spinupMortalityFraction)) {

    df <- dfSummary %>%
        filter(abs(spinupMortalityFraction - i) <= 0.0001)

    x <- df$averageMaxBiomassTarget
    y <- df$maxBiomassMultiplier
    z <- df$biomassTotalMean_tonsPerHa - biomassKnnTotal_mean

    # zRange <- ceiling(max(abs(range(z))) / 5 ) * 5
    # breaks <- seq(from = -zRange, to = zRange, length.out = 100)
    #
    # colNeg <- colorRampPalette(c("#BB4444", "#EE9988", "#808080"))(length(breaks)/2)
    # colPos <- colorRampPalette(c("#808080", "#77AADD", "#4477AA"))(length(breaks)/2)
    # colAll <- c(colNeg, colPos[-1])
    #
    #
    # cols <- colAll[findInterval(z, breaks)]
    ###
    fit <- loess(z ~ x + y, span = 0.2, control=loess.control(surface="direct"))
    ############################################################
    zPred <- matrix(predict(fit, newdata = xy),
                    nrow = gridLines, ncol = gridLines)
    # fitted points for droplines to surface
    fitpoints <- predict(fit)
    ############################################################
    fTitle <- paste0("calibrationTotalBiomass_SMF",
                     ifelse(i ==  0, "0.000", str_pad(i, "5", pad = "0", side = "right")),
                     ".png")

    fList <- append(fList, fTitle)

    png(filename = fTitle,
        width = width, height = height, units = "in", res = 300, pointsize = 8)
    # scatter plot with regression plane
    scatter3D(x, y, z,
              zlim = zlim,
              xlim = range(xPred),
              ylim = range(yPred),
              # zlim = c(floor(min(zPred)*10)/10,
              #          ceiling(max(zPred)*10)/10),
              # col = ramp.col(col = c("#BB4444", "#EE9988", "#808080", "#77AADD", "#4477AA"),
              #              n = 200, alpha = 1),
              col = gg.col(n = 50, alpha = 1),
              pch = 20, cex = 0.35,
              cex.lab = labelRel,
              cex.axis = labelRel,
              theta = 330, phi = 25,
              ticktype = "detailed",
              xlab = "\nABIE.BAL maxB target ratio",
              ylab = "\nmaxB multiplier",
              zlab = "\nResidual average total biomass",
              surf = list(x = xPred, y = yPred, z = zPred,
                          facets = NA, fit = fitpoints,
                          lwd = 0.15),
              colkey = list(cex.clab = labelRel,
                            cex.axis = labelRel),
              clab = c("Residual", "average biomass", "(tons/ha)"),
              clim = zlim,
              main = c("Landis-II Biomass Succession",
                       "Average total biomass calibration",
                       paste("spinup mortality fraction:",i)),
              cex.main = titleRel)

    dev.off()
}

require(animation)
oopt = ani.options(ani.dev="png", ani.type="png", interval = 0.66, autobrowse = FALSE)
### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
im.convert(c(fList[-(length(fList))], rep(fList[length(fList)], 5)), output = "calibrationTotalBiomass.gif",
           extra.opts = "", clean = F)

