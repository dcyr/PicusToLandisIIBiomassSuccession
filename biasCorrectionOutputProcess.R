rm(list = ls())

areas <- "LSJ"
a <- areas[1]

ifelse(Sys.info()["nodename"] == "dcyr-ThinkPad-X220",
       setwd(paste0("/media/dcyr/Seagate Backup Plus Drive/Sync/Sims/", a, "Calib")),
       setwd(paste0("/media/dcyr/Data/Sims/", a, "Calib")))


wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

##################
require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.4*detectCores()))  ## 5 for LSJ, 10 for NorthShore...
print(paste(clusterN, "cores"))
##################


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

spp <- as.character(vegCodes[vegCodes[, a] == 1, "LandisCode"] )

## loading landtypes
landtypes <- raster("../landtypes.tif") ### online raster doesn't keep CRS when downloading using "wininet"

### loading initial biomass
dtaDir <- "../initialBiomass/"
biomassKnn <- stack(paste0(dtaDir, "/initBiomassKnnTonsPerHa-", a, "-", spp, ".tif"))
names(biomassKnn) <- paste0(spp, "_tonsPerHa")
# removing inactive pixels
biomassKnn[is.na(landtypes)] <- NA
################################################################
################################################################
#################  looping through simulation outputs


simInfo <- read.csv("../simInfo.csv")
simDir <- simInfo$simDir
simDir <- str_pad(simDir, max(nchar(simDir)), pad = 0)

## total biomass (Knn estimates)
biomassKnnTotal <- sum(biomassKnn)
biomassKnnTotal_mean <- mean(values(biomassKnnTotal), na.rm = T)
## initial proportion (Knn estimates)
biomassKnnProp <- biomassKnn/biomassKnnTotal

################################################################
################################################################
#################  function that computes dissimilarity between
#################  two communities
#x <- c(biomassKnnProp[[1:8]][i], xProp[[1:8]][i])
brayDistFnc <- function(x) { # both
    nLayers <- length(x)
    # i <- 9791
    if(anyNA(x)) {
        return(NA)
    } else {
        mat <- rbind(x[1:(nLayers/2)],
                     x[(nLayers/2+1):nLayers])
        return(vegdist(mat))
    }
}



# #######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)
tInit <- Sys.time()
nSims <- length(simDir)
file.copy("../simInfo.csv", to = getwd(), overwrite = T)
brayDist <- foreach(i = 771:length(simDir))  %dopar% { #) %dopar% { ##seq_along(simDir))  %dopar% { #) %dopar% { #
    require(raster)
    require(vegan)
    # target <- simInfo[i, "averageMaxBiomassTarget"]
    # spinupMortalityFraction <- simInfo[i,"spinupMortalityFraction"]
    # maxBmult <- simInfo[i,"maxBiomassMultiplier"]
    outputDir <- paste0("../", simDir[i], "/output/biomass")
    outputs <- list.files(outputDir)
    outputs <- outputs[grep(paste(spp, collapse = "|"), outputs)]
    x <- stack(paste(outputDir, outputs, sep = "/"))
    crs(x) <- crs(landtypes)
    extent(x) <- extent(landtypes)
    x[is.na(landtypes)] <- NA

    ### convert to tons per ha
    x <- x / 100
    
    ### total biomass
    biomassTotal <- sum(x)
    biomassTotal_mean <- mean(values(biomassTotal), na.rm = T)
    
    ###  compare with Knn estimates and compute metrics
    ## absolute abundances
    x <- stack(x, biomassKnn)
    ## proportions
    # x <- x/biomassTotal
    # x <- stack(x, biomassKnnProp)

    # computing bray diss from absolute abundances
    distAbs <- calc(x,  brayDistFnc)
    
    # computing proportions
    x <- x[[1:(nlayers(x)/2)]]/biomassTotal
    x <- stack(x, biomassKnnProp)
    # computing bray diss from relative abundances
    distRel <- calc(x,  brayDistFnc)
    
    # result <- list(biomassTotal = biomassTotal,
    #                brayDistAbs = distAbs,
    #                brayDistRel = distRel)
    
    writeRaster(biomassTotal, file = paste0("biomassTotal_", simDir[i], ".tif"), overwrite = T)
    writeRaster(distAbs, file = paste0("distAbs_", simDir[i], ".tif"), overwrite = T)
    writeRaster(distRel, file = paste0("distRel_", simDir[i], ".tif"), overwrite = T)
    meanProcessTime <- (Sys.time()-tInit)/i
    ETC <- Sys.time() + (nSims-i)*meanProcessTime
    print(paste("outputs", i, "of", nSims, "completed"))
    print(paste("estimated time at completion:", ETC))
    removeTmpFiles(h = 0.15)
}
stopCluster(cl)
#############################
#############################


