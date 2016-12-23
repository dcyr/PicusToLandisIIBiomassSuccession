rm(list = ls())
#setwd("C:/Users/Dominic Cyr/Desktop/NorthShore")
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/NorthShore")
os <- Sys.info()["sysname"]

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
download.file(url, tmpFile, method= ifelse(os == "Windows", "wininet", "wget"))
landtypes <- raster("../landtypes.tif") ### online raster doesn't keep CRS when downloading using "wininet"

### loading initial biomass
dtaDir <- "../initialBiomass"
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
brayDistFnc <- function(x) {
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


require(doSNOW)
require(parallel)
clusterN <-  max(1, floor(0.9*detectCores()))  ### choose number of nodes to add to cluster.
# #######
cl = makeCluster(clusterN, outfile = "") ##
registerDoSNOW(cl)

brayDist <- foreach(i = seq_along(simDir)) %dopar% {
    require(raster)
    require(vegan)
    target <- simInfo[i, "averageMaxBiomassTarget"]
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
    
    
    
    # xProp <- x/biomassTotal
    # xProp <- stack(xProp, biomassKnnProp)
    result <- list(biomassTotal = biomassTotal,
                   brayDist = calc(x,  brayDistFnc))
    print(i)
    return(result)
    
}
stopCluster(cl)
#############################
#############################

totalBiomass <- stack(lapply(brayDist, function(x) x$biomassTotal))
brayDist <- stack(lapply(brayDist, function(x) x$brayDist))

save(totalBiomass, file = "totalBiomass.RData")
save(brayDist, file = "brayDist.RData")