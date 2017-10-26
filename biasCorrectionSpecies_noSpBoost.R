rm(list = ls())
a <- "ALPAC"
initDir <- paste("..", a, sep = "/")
###
outputDir <- ifelse(Sys.info()["nodename"] == "dcyr-ThinkPad-X220",
                    paste0("/media/dcyr/Seagate Backup Plus Drive/Sync/Sims/", a, "Calib"),
                    paste0("/media/dcyr/Data/Sims/", a, "Calib"))
###
setwd(paste("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/", a, sep = "/"))
wwd <- paste(paste(getwd(), Sys.Date(), sep = "/"))
dir.create(wwd)
setwd(wwd)
rm(wwd)
###


require(raster)
require(RCurl)
require(stringr)
require(vegan)
require(reshape2)
require(ggplot2)
require(data.table)
################################################################
################################################################
#################  Loading general inputs and initial conditions
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
ecozones <- read.csv(text = getURL(paste(readURL, "ecoNames.csv", sep="/")))
#
spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )

## loading landtypes
landtypes <- raster("../landtypes.tif")
### landtype resampled (for faster plotting)
r <- raster(ext = extent(landtypes),
            nrow = round(nrow(landtypes)/5),
            ncol = round(ncol(landtypes)/5),
            crs  = crs(landtypes)) 
landtypeResampled <- resample(landtypes, r, method = "ngb")


### loading initial biomass
biomassKnn <- stack(paste0("../initialBiomass/initBiomassKnnTonsPerHa-", a, "-", spp, ".tif"))

# removing inactive pixels
biomassKnn[is.na(landtypes)] <- NA
################################################################
simInfo <- read.csv("../simInfo.csv")
simDir <- simInfo$simDir
simDir <- str_pad(simDir, max(nchar(simDir)), pad = 0)
simInfo[,"simID"] <- simDir
simInfo <- simInfo[-(which(colnames(simInfo)=="species"))]

## mean biomass (Knn estimates)
x <- values(biomassKnn)
biomassKnnSpp_mean <- apply(x, 2, mean, na.rm = T)
x[x==0] <- NA
biomassKnnSppWhenPresent_mean <- apply(x, 2, mean, na.rm = T)
rm(x)
## total biomass (Knn estimates)
biomassKnnTotal <- sum(biomassKnn)
biomassKnnTotal_mean <- mean(values(biomassKnnTotal), na.rm = T)
biomassKnnSppWhenPresent_mean <- c(biomassKnnSppWhenPresent_mean, total = biomassKnnTotal_mean)
## initial proportion (Knn estimates)
biomassKnnProp <- biomassKnn/biomassKnnTotal
names(biomassKnn) <- paste0(spp, "_tonsPerHa")
names(biomassKnnProp) <- paste0(spp, "_relAbund")
#xMatKnn <- values(biomassKnnProp)



#################################################################################################
#################################################################################################
require(doSNOW)
require(parallel)
require(dplyr)
clusterN <-  max(1, floor(0.4*detectCores()))  ### choose number of nodes to add to cluster.

############################################################
#######  subsetting simulations
dfSummary <- get(load("../results/dfSummary.RData"))

### for QcNb, try imposing SMF == 0.018
### for ALPAC, try imposing SMF == 0.01
# dfSummarySbSample <- filter(dfSummary,
#                             spinupMortalityFraction %in% c(0.002, 0.005, 0.01, 0.02) &
#                                 maxBiomassMultiplier %in% c(0.4, 0.55, 0.7, .85, 1))
dfSummarySbSample <- filter(dfSummary,
                            spinupMortalityFraction %in% c(0.01) &
                                maxBiomassMultiplier %in% c(0.75))                                

index <- 1:nrow(dfSummarySbSample)

# # ### default procedure, minimize disssimilarity
# index <- unique(c(which.min(dfSummary$brayDissAbs_mean),
#                   which.min(dfSummary$brayDissRel_mean)))

### minimizin dissimilarity didn't work for SudStl, I chose manually based on figures.
# index <- which(dfSummarySbSample$maxBiomassMultiplier == 0.5 &
#                    dfSummarySbSample$spinupMortalityFraction == 0.01 &
#                    dfSummarySbSample$averageMaxBiomassTarget == 0.6)

               
simIDcorr <- as.character(dfSummarySbSample[index,"simID"])

# 
# simIDcorr <- as.character(dfSummary[index,"simID"])

simInfoSubsample <- simInfo %>%
    filter(simID %in% simIDcorr)

simInfoSubsample <- rbind(simInfoSubsample, simInfo %>%
                              filter(spinupMortalityFraction %in% 0.002,
                                     maxBiomassMultiplier == 1, 
                                     spBiomassMultiplier == 1))

simIdRef <- last(simInfoSubsample$simID)


# # ### first pass - North Shore
# maxBmult <- c(0.55, 0.7, 0.85, 1)
# AbTargetRatio <- c(0.2, 0.4, 0.6, 0.8)
# ### second pass - all species together on the same figure



#####################################################
#####################################################

SMF <- simInfoSubsample$spinupMortalityFraction  
maxBmult <- simInfoSubsample$maxBiomassMultiplier
AbTargetRatio <- simInfoSubsample$averageMaxBiomassTarget

smfString <- str_pad(SMF, 5, pad = "0", side = "right")


simInfoVar <- c("averageMaxBiomassTarget",
                "spBiomassMultiplier",
                "maxBiomassMultiplier",
                "spinupMortalityFraction")


############################################################
####### compiling results
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
##
biomassCalib <- foreach(i = 1:nrow(simInfoSubsample))  %dopar% { #
    require(raster)
    require(reshape2)
    require(dplyr)
    require(stringr)

    simN <- simInfoSubsample[i, "simID"]


    x <- stack(paste0(outputDir, "/", simN, "/output/biomass/biomass_", c(spp, "TotalBiomass"), "_0.tif"))

    x[sum(x) == 0] <- NA
    crs(x) <- crs(biomassKnn)
    extent(x) <- extent(biomassKnn)
    ## convert to tons per ha
    x <- x/100

    ## computing differences between spinup and reference
    x <- x - stack(biomassKnn, biomassKnnTotal)
    sppNames <- c(gsub("_tonsPerHa", "", names(biomassKnn)), "Total")
    ## computing exact statistics
    rasterValues <- values(x)
    rasterValues[rasterValues==0] <- NA
    mat <- apply(rasterValues, 2, mean, na.rm = T)
    mat <- data.frame(residualMean_tonsPerHa = mat,
                      redidualMean_proportion = mat/biomassKnnSppWhenPresent_mean)



    mat <- data.frame(simID = simN,
                      species = sppNames,
                      mat)
    rownames(mat) <- 1:nrow(mat)

    #if (simN %in% simInfoSubsample[,"simID"]) {
    ### lowering resolution for faster plotting
    x <- resample(x, landtypeResampled)
    x[is.na(landtypeResampled)] <- NA

    ### converting to long data.frame for ggplot2
    x <-  rasterToPoints(x)
    colnames(x)[3:ncol(x)] <- c(spp, "Total")

    x <- melt(as.data.frame(x),
              id.vars = c("x", "y"),
              variable.name = "species", value.name = "biomassResidual_tonsPerHa")

    x[, "simID"] <- simN
    ############################################################################################################

    ## rounding values to reduce file size
    x$biomassResidual_tonsPerHa <- round(x$biomassResidual_tonsPerHa, 2)
    x[,simInfoVar] <- simInfoSubsample[i, simInfoVar]
    #}


    return(list(map = x, summary = mat))

    print(paste(SMF[i], i))
}
stopCluster(cl)

names(biomassCalib) <- simInfoSubsample$simID
subSample <- simInfoSubsample$simID
### putting compiled results into tidy data.frame
extractElement <- function(x, element) {
    lapply(x, function(y) y[[element]])
}
#
biomassCalibSummary <- do.call(rbind, extractElement(biomassCalib, element = "summary"))
rownames(biomassCalibSummary) <- 1:nrow(biomassCalibSummary)
biomassCalibSummary <- merge(biomassCalibSummary, simInfo)
save(biomassCalibSummary, file = paste0("biomassCalibSummary.RData"))
#
biomassCalibMaps <- do.call(rbind, extractElement(biomassCalib[subSample], element = "map"))
rownames(biomassCalibMaps) <- 1:nrow(biomassCalibMaps)
save(biomassCalibMaps, file = paste0("biomassCalibMaps.RData"))


# ############################################################
# biomassCalibMaps <- get(load("../processedOutputs/biomassCalibMaps.RData"))
# biomassCalibSummary <- get(load("../processedOutputs/biomassCalibSummary.RData"))
# ############################################################

##
if(length(simIDcorr)>1) {  # then plot all species separately
    ######################################################
    ######   compute df for total
    for (sp in c(spp, "Total")) {#, spp)) {

        df <- biomassCalibMaps %>%
            filter(species == sp)

        dfBias <- biomassCalibSummary %>%
            filter(species == sp)
        df <- merge(df, dfBias, by = c("spinupMortalityFraction","maxBiomassMultiplier"))

        ########################
        #### Compute sp biases

        colScale <- scale_fill_gradient2(name = "bias (tons/ha)",
                                         low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0)

        yMax <- max(df$y)
        xMax <- max(df$x)
        ###########
        nMaxBSp <- length(unique(df$maxBiomassMultiplier))
        nMaxB <- length(unique(df$maxBiomassMultiplier))

        p <- ggplot(data = df, aes(x = x, y = y, fill = biomassResidual_tonsPerHa,
                                   label = paste(round(residualMean_tonsPerHa, 1), "tons/ha"))) +
            theme_dark() +
            geom_raster() +
            coord_fixed() +
            colScale +
            facet_grid(spinupMortalityFraction ~ maxBiomassMultiplier) +
            geom_text(aes(x = xMax, y = yMax,
                          label = "mean bias"),
                      hjust = 1, size = rel(3), fontface = 1) +
            geom_text(aes(x = xMax, y = yMax - 17000),
                      hjust = 1, size = rel(3), fontface = "bold") +#
            theme(axis.text = element_blank(),
                  axis.title = element_blank(),
                  axis.ticks = element_blank(),
                  title = element_text(size = rel(0.5*(0.66*nMaxB))),
                  strip.text = element_text(size = rel(0.5)),
                  legend.key.size = unit(0.15 * (0.66*nMaxB), "in"),
                  legend.text = element_text(size = rel(0.5 * (0.66*nMaxB)))) +
            labs(title = paste0("Difference between initial biomass after LANDIS spinup and Knn estimations\n", sp))



        ##
        pHeight <- nMaxBSp * 2.5
        pWidth <- (nMaxB+1) * 2
        #
        png(filename = paste0("initBias.png"),
            width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)

        print(p)

        dev.off()
    }
}




#############################################################
#############################################################
#############################################################
### summary tables, comparing with "no correction" simulation
#############################################################

if (length(simIDcorr) == 1) {

    # total biomass
    x <- stack(paste0(outputDir, "/", simIDcorr, "/output/biomass/biomass_", c(spp, "TotalBiomass"), "_0.tif"))
    xNoCorr <- stack(paste0(outputDir, "/", simIdRef, "/output/biomass/biomass_", c(spp, "TotalBiomass"), "_0.tif"))
    x[sum(x) == 0] <- NA
    xNoCorr[sum(xNoCorr) == 0] <- NA

    crs(x) <- crs(xNoCorr) <- crs(biomassKnn)
    extent(x) <- extent(xNoCorr) <- extent(biomassKnn)

    ### convert to tons per ha
    x <- x/100
    xNoCorr <- xNoCorr/100

    ### computing differences between spinup and reference
    xRef <- stack(biomassKnn, biomassKnnTotal)
    x <- x - xRef
    xNoCorr <- xNoCorr - xRef

    ### computing exact statistics
    # (biomass biases)
    rasterValues <- values(x)
    rasterValues[rasterValues==0] <- NA
    mat <- apply(rasterValues, 2, mean, na.rm = T)
    #
    rasterValues <- values(xNoCorr)
    rasterValues[rasterValues==0] <- NA
    mat2 <- apply(rasterValues, 2, mean, na.rm = T)

    mat <- data.frame(biasMean_tonsPerHa_Corr = mat,
                      biasMean_tonsPerHa_noCorr = mat2,
                      redidualMean_proportion_Corr = mat/biomassKnnSppWhenPresent_mean,
                      redidualMean_proportion_noCorr = mat2/biomassKnnSppWhenPresent_mean)
    ####
    write.csv(mat, file = "summaryTable_biomass.csv", row.names = T)

    ### diss index
    index <- which(dfSummary$simID %in% simInfoSubsample$simID)


    df <- data.frame(simID = dfSummary[index, "simID"],
                     distAbs = dfSummary[index, "brayDissAbs_mean"],
                     distRel = dfSummary[index, "brayDissRel_mean"])

    df <- merge(df, simInfo)
    write.csv(df, file = "summaryTable_diss.csv", row.names = F)

    ####
    ### copying bias corrected files to working directory
    file.copy(paste0(outputDir, "/", simIDcorr, "/biomass-succession-dynamic-inputs.txt"),
              paste0("biomass-succession-dynamic-inputs_", a, "_BiasCorrected.txt"), overwrite = T)
}




##########################################################################################################################
##########################################################################################################################
### One figure, all species (with correction and no correction)

## Limits are defined so that they emcompasse about 99% of bias values for total biomass when uncorrected
require(dplyr)
zLimits <-  biomassCalibMaps %>%
    filter(#simID == simIdRef &
               species == "Total")

zLimits <- quantile(zLimits$biomassResidual_tonsPerHa, c(0.025, 0.975))
# zLimits <- round(max(abs(zLimits))/10)*10
# zLimits <- c(-zLimits, zLimits)

zLimits <- c(floor(zLimits[1]/5)*5,
             ceiling(zLimits[2]/5)*5)


zLimits <- c(floor(min(zLimits)/5)*5,
             ceiling(max(zLimits)/5)*5)

titleList <- character()
if (length(simIDcorr) == 1) {
    for (i in c("Corr", "noCorr")) {
        if (i == "Corr") {
         ID <- first(simInfoSubsample$simID)
        }
        if (i == "noCorr") {
         ID <- simIdRef
        }



        df <- biomassCalibMaps %>%
         filter(simID == ID)

        dfBias <- biomassCalibSummary %>%
         filter(simID == ID)
        df <- merge(df, dfBias, by = "species")

        ########################
        #### Compute sp biases
        require(scales)
        colScale <- scale_fill_gradient2(name = "bias (tons/ha)",
                                         low = muted("blue"), mid = "white", high = muted("red"), midpoint = 0,
                                      #low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0,
                                      limits = zLimits)


        yMax <- max(df$y)
        xMax <- max(df$x)
        ###########

        p <- ggplot(data = df, aes(x = x, y = y, fill = biomassResidual_tonsPerHa,
                                label = paste(round(residualMean_tonsPerHa, 1), "tons/ha"))) +
         theme_dark() +
         geom_raster() +
         coord_fixed() +
         colScale +
         facet_wrap( ~ species) +
         geom_text(aes(x = xMax, y = yMax,
                       label = "mean bias"),
                   hjust = 1, size = rel(2.5), fontface = 1) +
         geom_text(aes(x = xMax, y = yMax - 35000),
                   hjust = 1, size = rel(2.5), fontface = "bold") +#
         theme(axis.text = element_blank(),
               axis.title = element_blank(),
               axis.ticks = element_blank(),
               title = element_text(size = rel(1)),
               strip.text = element_text(size = rel(0.75)),
               legend.key.size = unit(0.25, "in"),
               legend.text = element_text(size = rel(1))) +
         labs(title = paste0("Difference between initial biomass after LANDIS spinup and Knn estimations"),
              subtitle = ifelse(i == "noCorr", paste0("Without bias correction (SMF = ", SMF[2], ")"),
                                paste0("With bias correction (SMF = ", SMF[1], "; maxBmult: ", maxBmult[1], ")")))



        ##
        pHeight <- 10
        pWidth <- 10
        #
        fTitle <- paste0("initBias_", i, ".png")
        titleList <- append(titleList, fTitle)
        png(filename = fTitle,
         width = pWidth, height = pHeight, units = "in", res = 600, pointsize=10)

        print(p)

        dev.off()
    }
}
# 
# require(animation)
# oopt = ani.options(ani.dev="png", ani.type="png", interval = 2, autobrowse = FALSE)
# ### (Windows users may want to add):  ani.options(convert = 'c:/program files/imagemagick/convert.exe')
# im.convert(titleList, output = "initBias.gif",
#            extra.opts = "", clean = F)




                 
                 
                 
                 
                 
