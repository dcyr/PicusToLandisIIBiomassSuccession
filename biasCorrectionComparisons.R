rm(list = ls())
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/NorthShore")
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
landtypes <- raster(tmpFile)

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
#xMatKnn <- values(biomassKnnProp)

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
    x <- stack(x, biomassKnn)
    # xProp <- x/biomassTotal
    # xProp <- stack(xProp, biomassKnnProp)
    result <- list(biomassTotal = biomassTotal,
                   brayDist = calc(x,  brayDistFnc))
    return(result)
    
}
stopCluster(cl)


#############################
#############################

BrayDist <- stack(lapply(brayDist, function(x) x$brayDist))
totalBiomass <- stack(lapply(brayDist, function(x) x$biomassTotal))

BrayDistMean <- apply(values(BrayDist), 2, mean, na.rm = T)
png(filename="BrayDist.png",
    width = 6, height = 4, units = "in", res = 300, pointsize=10)
    
    plot(x = simInfo$averageMaxBiomassTarget,
         y = BrayDistMean,
        type = "l", xlab = "ABIE.BAL targetted maxB Ratio",
        ylab = "Average Bray-Curtis dissimilarity")

dev.off()

totalBiomassMean <- apply(values(totalBiomass), 2, mean, na.rm = T)
png(filename="totalBiomass.png",
    width = 6, height = 4, units = "in", res = 300, pointsize=10)

plot(x = simInfo$averageMaxBiomassTarget,
     y = totalBiomassMean,
     type = "l", xlab = "ABIE.BAL targetted maxB Ratio",
     ylab = "Average total biomass (tons/ha)")
abline(h = biomassKnnTotal_mean)

dev.off()





simInfo$averageMaxBiomassTarget[which.min(BrayDistMean)]
folderSubset <- c(1,3,6:18,21,25,29) 


cl = makeCluster(clusterN, outfile = "") ## 
registerDoSNOW(cl)

foreach(sp = spp) %dopar% {
    require(raster)
    require(ggplot2)
    require(reshape2)
    for (i in folderSubset) {#seq_along(simDir)) {
        
        target <- simInfo[i, "averageMaxBiomassTarget"]
        outputDir <- paste0("../", simDir[i], "/output/biomass")
        outputs <- list.files(outputDir)
        outputs <- outputs[grep(paste(sp, collapse = "|"), outputs)]
        outputs <- raster(paste(outputDir, outputs, sep = "/"))
        if (i == folderSubset[1]) {
           x <- outputs
        } else {
            x <- stack(x, outputs)
        }
    }
    crs(x) <- crs(landtypes)
    extent(x) <- extent(landtypes)
    x[is.na(landtypes)] <- NA
        
    ### convert to tons per ha
    x <- x / 100
        
    diff <- x - biomassKnn[[grep(sp, names(biomassKnn))]]

    df <- rasterToPoints(diff)
    df <- as.data.frame(df)
    colnames(df)[3:ncol(df)] <-  paste("target", simInfo[folderSubset, "averageMaxBiomassTarget"], sep = "_")
    df <- melt(df, id.vars = c("x", "y"), variable.name = "layer")#, measure.vars = "biomassDiff_tonsPerHa")

    
    colScale <- scale_fill_gradient2(name = "diff",
                                     low = "#4477AA", mid = "white", high = "#BB4444", midpoint = 0) 
    
    p <- ggplot(data = df, aes(x = x, y = y, fill = value)) +
        theme_dark() +
        geom_raster() +
        coord_fixed() +
        colScale +
        facet_wrap(~ layer) +
        theme(axis.text = element_blank(),
              #axis.text.y =  element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank())#,
    #axis.ticks = element_blank())
    
        
    png(filename = paste0("diffInit_", sp, ".png"),
        width = 12, height = 10, units = "in", res = 600, pointsize=10)
        
        print(p)
        
        
    dev.off()
}
    
stopCluster(cl)



