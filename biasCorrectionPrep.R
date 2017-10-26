rm(list = ls())

a <- "ALPAC"
setwd(paste("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection", a, sep = "/"))
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(dplyr)
require(stringr)
### loading baseline inputs
baselineInputs <- read.table("../biomass-succession-dynamic-inputs.txt", skip = 1, comment.char = ">")
colnames(baselineInputs) <- c("year", "landtype", "species", "probEst", "maxANPP", "maxB")


maxBiomassRatios <- baselineInputs %>%
    filter(year == 0) %>%
    group_by(landtype) %>%
    summarise(maxB_landtype = max(maxB)) %>%
    merge(baselineInputs) %>%
    group_by(species) %>%
    summarise(maxBiomassRatio = mean(maxB)/mean(maxB_landtype))
maxBiomassRatios <- as.data.frame(maxBiomassRatios)


### these are targetted proportion of maxBiomass for given spp

#maxBiomassCoeff <- seq(0.3, 1, by = 0.1)
maxBiomassCoeff <- 1
# propMaxBiomassBoost <- list(ABIE.BAL = c(maxBiomassRatios[which(maxBiomassRatios$species == "ABIE.BAL"),
#                                                          "maxBiomassRatio"], seq(0.2, 1, 0.1)))
# propMaxBiomassBoost <- list(ABIE.BAL = c(maxBiomassRatios[which(maxBiomassRatios$species == "ABIE.BAL"),
#                                                           "maxBiomassRatio"], .6))
propMaxBiomassBoost <- list(BETU.PAP = c(maxBiomassRatios[which(maxBiomassRatios$species == "BETU.PAP"),
                                                          "maxBiomassRatio"]))
spinupMortalityFraction <- c(0, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.015, 0.02)


#     
#     mutate(maxB_sppMean = mean(maxB),
#               maxBiomassRatio = maxB/maxB_landtype)
# head(baselineInputs)

### computing correctionFactors
correctionFactors <- list()
for (i in seq_along(propMaxBiomassBoost)) {
    sp <- names(propMaxBiomassBoost)[i]
    corr <- propMaxBiomassBoost[[sp]] / as.numeric(maxBiomassRatios[maxBiomassRatios$species == sp, "maxBiomassRatio"])
    correctionFactors[[sp]] <- corr
}




files <- character()
### applying correctionFactors

for (k in maxBiomassCoeff) {
    intermediateInputs <- baselineInputs
    intermediateInputs[c("maxANPP", "maxB")] <- round(intermediateInputs[c("maxANPP", "maxB")] * k)
    
    for (i in seq_along(correctionFactors)) {
        sp <- names(correctionFactors)[i]
        spIndex <- which(intermediateInputs$species == sp)
        nPad <- max(nchar(propMaxBiomassBoost[[i]][-1]))+1
        if(nPad == -Inf) {
            nPad <- 4
        }
        
        for (j in seq_along(correctionFactors[[i]])) {
            
            ### applying species' correction factors
            corrFactor <- correctionFactors[[i]][j]
            
            finalInputs <- intermediateInputs

            finalInputs[spIndex, c("maxANPP", "maxB")] <- round(finalInputs[spIndex, c("maxANPP", "maxB")] *
                                                           corrFactor)
            
            target <- propMaxBiomassBoost[[i]][j]
            
            ##### writing to file
            fileName <- paste0("biomass-succession-dynamic-inputs_",
                               "sppRatio",
                               ifelse(target == 1, "1.00",
                                      str_pad(round(target,2), nPad, pad = "0", side = "right")),
                               "_maxBmult",
                               ifelse(k == 1, "1.00",
                                      str_pad(k, nPad, pad = "0", side = "right")),
                               ".txt")
            #
            files <- append(files, fileName)
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
            cat(paste0(c(">>", k), collapse = "\t"))
            cat("\r\n")
            cat("\r\n")
            cat(">> The following species' parameters (maxANPP and maxB) have also been modified")
            cat("\r\n")
            cat("\r\n")
            cat(paste0(c(">>", "species" , "\taverageMaxBiomassTarget", "finalMultiplier"), collapse = "\t"))
            cat("\r\n")
            cat(paste0(c(">>", sp, target, "\t", round(corrFactor, 4)),  collapse = "\t"))
            cat("\r\n")
            cat("\r\n")
            cat(paste0(c(">>",colnames(baselineInputs)), collapse="\t"))
            cat("\r\n")
            cat("\r\n")
            sink()
            ## param table
            write.table(finalInputs, file=fileName,
                        append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t",
                        quote=FALSE,
                        #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                        eol = "\r\n" #default line endings on windows system.
            )  
        }
    }
}


########################
########################
### generating simulation packages

sp <- "BETU.PAP"
nSims <- length(files) * length(spinupMortalityFraction)
tmp <- str_split(gsub(".txt|sppRatio|maxBmult", "", files), "_")
targetABIEBAL <- as.numeric(lapply(tmp, function(x) x[2]))
maxBiomassCoeff <- as.numeric(lapply(tmp, function(x) x[3]))


s <- sStart <- 0
for (f in seq_along(spinupMortalityFraction)) {
    smf <- spinupMortalityFraction[f]
    con <- file("../biomass-succession-main-inputs.txt")
    mainInputs <- readLines(con)
    index <- grep("SpinupMortalityFraction", mainInputs)
    mainInputs[index] <- paste("SpinupMortalityFraction", smf)
    
    mainInputFile <- paste0("biomass-succession-main-inputs_", smf, ".txt")
    sink(mainInputFile)
    cat(paste0(mainInputs, "\r\n"))
    sink()
    
    
    for (i in seq_along(files)) {
        
        corrFactor <- correctionFactors[[sp]][which(round(propMaxBiomassBoost[[sp]] - targetABIEBAL[i], 2) == 0)]
        
    
        simName <- str_pad(s, nchar(nSims), pad = "0")
        simDir <- paste0("../", simName)
        dir.create(simDir)
        file.copy("../scenario.txt", simDir, overwrite = T)
        file.copy(mainInputFile, paste(simDir, "biomass-succession-main-inputs.txt", sep = "/"), overwrite = T)
        file.copy(files[i], paste(simDir, "biomass-succession-dynamic-inputs.txt", sep = "/"), overwrite = T)
        
        df <- data.frame(simDir = simName, species = sp,
                         averageMaxBiomassTarget = targetABIEBAL[i],
                         spAnppMultiplier = corrFactor, 
                         spBiomassMultiplier = corrFactor,
                         maxBiomassMultiplier = maxBiomassCoeff[i],
                         spinupMortalityFraction = smf)
        if(s == sStart) {
            simInfo <- df
        } else {
            simInfo <- rbind(simInfo, df)
        }
        s <- s + 1  
    }
}
write.csv(simInfo, file = "../simInfo.csv", row.names = F)




