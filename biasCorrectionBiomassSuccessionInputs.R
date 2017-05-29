################################################################
################################################################
#################   This script should be ran after the best correction
#################   has been set. The bias-corrected 'baseline' dynamic input should be in
#################   the '../results' folder.
#################
#################   However, it re-fetches the original files so that yan's path are restored
rm(list = ls())
a <- "SudStl"
timestep <- 5
#################   At this point, the spin-up mortality factor has to be set manually here
SMF <- 0.01

###
setwd(paste("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/", a, sep = "/"))
wwd <- paste(paste(getwd(), Sys.Date(), sep = "/"))
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(dplyr)
require(stringr)
require(RCurl)

### fetching multipliers from bias corrected file
dynamicInputsCorr <- paste0("../results/biomass-succession-dynamic-inputs_", a, "_BiasCorrected.txt")
x <- readLines(con = dynamicInputsCorr)
maxBmult <- x[last(grep("multiplier", x))+1]

maxBmult <- as.numeric(last(str_split(maxBmult, "\t")[[1]]))
ABIE.BAL_mult <- x[first(grep("ABIE.BAL", x))]
target <- as.numeric(str_split(ABIE.BAL_mult, "\t")[[1]][3])
ABIE.BAL_mult <- as.numeric(last(str_split(ABIE.BAL_mult, "\t")[[1]]))


scenarios <- c("Baseline", "RCP26", "RCP45", "RCP85")

readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/LandisInputs/"

for (s in scenarios) {
    dynamicInputs <- paste0(readURL, a, "/biomass-succession-dynamic-inputs_", a, "_", s,
                            ifelse(timestep == 5, "_5yrsTS.txt", ".txt"))
    tmpFile <- tempfile()
    download.file(dynamicInputs, tmpFile, method="wget")
    
    ## dynamic Inputs
    dynamicInputs <- read.table(tmpFile, skip = 1, comment.char = ">")
    ### applying global maxB correction    
    dynamicInputs[, 5:6] <-  round(dynamicInputs[, 5:6]*maxBmult)
    ### applying correction for ABIE.BAL
    index <- which(dynamicInputs[,3] == "ABIE.BAL")
    dynamicInputs[index, 5:6] <- round(dynamicInputs[index, 5:6]*ABIE.BAL_mult)
    
    ##### writing to file
    fileName <- paste0("biomass-succession-dynamic-inputs_", a, "_", s,
                       ifelse(timestep == 5, "_5yrsTS_BiasCorrected.txt","_BiasCorrected.txt"))
    
    sink(fileName)
    cat('LandisData "Dynamic Input Data"')
    cat("\r\n")
    cat("\r\n")
    cat(">> Warning - This is a bias-corrected file")
    cat("\r\n")
    cat("\r\n")
    cat(">> The following multiplier was applied to all species's original maxB and maxANPP")
    cat("\r\n")
    cat(paste0(c(">>", "multiplier"), collapse = "\t"))
    cat("\r\n")
    cat(paste0(c(">>", maxBmult), collapse = "\t"))
    cat("\r\n")
    cat("\r\n")
    cat(">> The following species' parameters (maxANPP and maxB) have also been modified")
    cat("\r\n")
    cat("\r\n")
    cat(paste0(c(">>", "species" , "\taverageMaxBiomassTarget", "finalMultiplier"), collapse = "\t"))
    cat("\r\n")
    cat(paste0(c(">>", "ABIE.BAL", target, "\t", ABIE.BAL_mult),  collapse = "\t"))
    cat("\r\n")
    cat("\r\n")
    cat(">>\tyear\tlandtype\tspecies\tprobEst\tmaxANPP\tmaxB")
    cat("\r\n")
    cat("\r\n")
    sink()
    ## param table
    write.table(dynamicInputs, file=fileName,
                append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t",
                quote=FALSE,
                #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                eol = "\r\n" #default line endings on windows system.
    )  
    
    ######  copying main inputs
    mainInputs <- paste0(readURL, a, "/biomass-succession-main-inputs_", a, "_", s, ".txt")
    tmpFile <- tempfile()
    download.file(mainInputs, tmpFile, method="wget")
    
    x <- readLines(con = tmpFile)
    x[grep("SpinupMortalityFraction", x)] <- paste("SpinupMortalityFraction", SMF)
    x[grep("Timestep", x)] <- paste("Timestep", timestep)
    
    ##### writing updated main inputs to file
    fileName <- paste0("biomass-succession-main-inputs_", a, "_", s,
                       ifelse(timestep == 5, "_5yrsTS_BiasCorrected.txt","_BiasCorrected.txt"))
    
    sink(fileName)
    writeLines(x)
    sink()
}


