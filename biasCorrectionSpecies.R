rm(list = ls())
a <- "NorthShore"
initDir <- paste("..", a, sep = "/")
###
outputDir <- ifelse(Sys.info()["nodename"] == "dcyr-ThinkPad-X220",
                    "/media/dcyr/Seagate Backup Plus Drive/Sync/Sims/NorthShoreCalib/",
                    "/media/dcyr/Data/Sims/NorthShoreCalib")
###
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/")
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


spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )

## loading landtypes
#readURL <- "https://github.com/dcyr/LANDIS-II_IA_generalUseFiles/raw/master/LandisInputs/"
#tmpFile <- tempfile()
#url <- paste(readURL, a, "/landtypes_", a, ".tif", sep="")
#download.file(url, tmpFile, method="wget")
landtypes <- raster(paste(initDir, "landtypes.tif", sep = "/"))
### landtype resampled (for faster plotting)
r <- raster(ext = extent(landtypes),
            nrow = round(nrow(landtypes)/5),
            ncol = round(ncol(landtypes)/5),
            crs  = crs(landtypes)) 
landtypeResampled <- resample(landtypes, r, method = "ngb")


### loading initial biomass
biomassKnn <- stack(paste0(initDir, "/initialBiomass/initBiomassKnnTonsPerHa-", a, "-", spp, ".tif"))
names(biomassKnn) <- paste0(spp, "_tonsPerHa")
# removing inactive pixels
biomassKnn[is.na(landtypes)] <- NA
################################################################
simInfo <- read.csv(paste0(initDir, "/simInfo.csv"))
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
#xMatKnn <- values(biomassKnnProp)



#################################################################################################
#################################################################################################
require(doSNOW)
require(parallel)
require(dplyr)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.

############################################################
#######  subsetting simulations

for (SMF in c(0.018)) {#c(0.01, 0.018, 0.025)) {#
    #SMF <- 0.018  
    smfString <- str_pad(SMF, 5, pad = "0", side = "right")
    
    # ### first pass - North Shore
    # maxBmult <- c(0.55, 0.7, 0.85, 1)
    # AbTargetRatio <- c(0.2, 0.4, 0.6, 0.8)
    
    ### second pass - North Shore
    maxBmult <- c(0.7)
    AbTargetRatio <- c(0.5)
    
    
    simInfoSubsample <- simInfo %>%
        filter(spinupMortalityFraction %in% SMF,
               maxBiomassMultiplier %in% maxBmult,
               averageMaxBiomassTarget %in% AbTargetRatio)
    simInfoVar <- c("averageMaxBiomassTarget",
                    "spBiomassMultiplier",
                    "maxBiomassMultiplier",
                    "spinupMortalityFraction")
    
    
    ############################################################
    ####### compiling results
    cl = makeCluster(clusterN, outfile = "") ##
    registerDoSNOW(cl)
    ##
    biomassCalib <- foreach(i = 1:nrow(simInfo))  %dopar% { #
        require(raster)
        require(reshape2)
        require(dplyr)
        require(stringr)

        simN <- simInfo[i, "simID"]


        x <- stack(paste0(outputDir, "/", simN, "/output/biomass/biomass_", c(spp, "TotalBiomass"), "_0.tif"))

        x[sum(x) == 0] <- NA
        crs(x) <- crs(biomassKnn)
        extent(x) <- extent(biomassKnn)
        ## convert to tons per ha
        x <- x/100
        
        ## computing differences between spinup and reference
        x <- x - stack(biomassKnn, biomassKnnTotal)
        
        ## computing exact statistics
        rasterValues <- values(x)
        rasterValues[rasterValues==0] <- NA
        mat <- apply(rasterValues, 2, mean, na.rm = T)
        mat <- data.frame(residualMean_tonsPerHa = mat,
                          redidualMean_proportion = mat/biomassKnnSppWhenPresent_mean)
            
        

        mat <- data.frame(simID = simN,
                          species = gsub("biomass|Biomass|_|0", "", rownames(mat)),
                          mat)
        rownames(mat) <- 1:nrow(mat)

        if (simN %in% simInfoSubsample[,"simID"]) {
            ### lowering resolution for faster plotting
            x <- resample(x, landtypeResampled)
            x[is.na(landtypeResampled)] <- NA
    
            ### converting to long data.frame for ggplot2
            x <-  rasterToPoints(x)
            colnames(x)[3:ncol(x)] <- c(spp, "total")
    
            x <- melt(as.data.frame(x),
                      id.vars = c("x", "y"),
                      variable.name = "species", value.name = "biomassResidual_tonsPerHa")
    
            x[, "simID"] <- simN
            ############################################################################################################

            ## rounding values to reduce file size
            x$biomassResidual_tonsPerHa <- round(x$biomassResidual_tonsPerHa, 2)
            x[,simInfoVar] <- simInfo[i, simInfoVar]
        }
        
        if (simInfo[i, "simID"] %in% simInfoSubsample[,"simID"]) {
            return(list(map = x, summary = mat))
        } else {
            return(list(summary = mat))
        }
        print(paste(SMF, i))
    }
    stopCluster(cl)
    names(biomassCalib) <- simInfo$simID
    subSample <- simInfoSubsample$simID
    ### putting compiled results into tidy data.frame
    extractElement <- function(x, element) {
        lapply(x, function(y) y[[element]])
    }
    #
    biomassCalibSummary <- do.call(rbind, extractElement(biomassCalib, element = "summary"))
    rownames(biomassCalibSummary) <- 1:nrow(biomassCalibSummary)
    save(biomassCalibSummary, file = paste0("biomassCalibSummary_", smfString, ".RData"))
    #
    biomassCalibMaps <- do.call(rbind, extractElement(biomassCalib[subSample], element = "map"))
    rownames(biomassCalibMaps) <- 1:nrow(biomassCalibMaps)
    save(biomassCalibMaps, file = paste0("biomassCalibMaps_", smfString, ".RData"))
    # 
    # #############################################################
    # #############################################################
    # #############################################################
    # ###### maps ! (uncomment for second pass)
    # biomassCalib <- get(load(paste0("biomassCalibMaps_", smfString, ".RData")))
    # #############################################################
    # 
    # ######################################################
    # ######################################################
    # ######################################################
    # ######################################################
    # ######   compute df for total
    # for (sp in c("total", spp)) {
    #     
    #     df <- biomassCalib %>%
    #         filter(species == sp)
    #     
    #     colScale <- scale_fill_gradient2(name = "bias (tons/ha)",
    #                                      low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0)
    #     
    #     
    #     p <- ggplot(data = df, aes(x = x, y = y, fill = biomassResidual_tonsPerHa)) +
    #         theme_dark() +
    #         geom_raster() +
    #         coord_fixed() +
    #         colScale +
    #         facet_grid(averageMaxBiomassTarget ~ maxBiomassMultiplier) +
    #         theme(axis.text = element_blank(),
    #               axis.title = element_blank(),
    #               axis.ticks = element_blank())
    #     
    #     yMax <- layer_scales(p)$y$range$range[2]
    #     xMax <- layer_scales(p)$x$range$range[2]
    #     
    #     png(filename = paste0("initBias_SMF",smfString, "_", sp, ".png"),
    #         width = 10, height = 10, units = "in", res = 600, pointsize=10)
    #     
    #     print(p +
    #               labs(title = paste0("Difference between initial biomass after LANDIS spinup and Knn estimations\n", sp)) +
    #               geom_text(aes(x = xMax, y = yMax,
    #                             label = "bias (global)"),
    #                         hjust = 1, size = 2.25, fontface = 1) +
    #               geom_text(aes(x = xMax, y = yMax - 20000,
    #                             label = biomassResidualMean_tonsPerHa),
    #                         hjust = 1, size = 2.75, fontface = "bold")
    #     )
    #     dev.off()
    # } 
    
}

#############################################################
#############################################################
#############################################################
### summary tables
#############################################################


