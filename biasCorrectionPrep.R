rm(list = ls())
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession/biasCorrection/NorthShore")
wwd <- paste(getwd(), Sys.Date(), sep = "/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

require(dplyr)
require(stringr)
### loading baseline inputs
baselineInputs <- read.table("../biomass-succession-dynamic-inputs.txt", skip = 1, comment.char = ">")
colnames(baselineInputs) <- c("year", "landtype", "species", "probEst", "maxANPP", "maxB")

### these are targetted proportion of maxBiomass for given spp

maxBiomassCoeff <- seq(0.5, 1, by = 0.05)
propMaxBiomassBoost <- list(ABIE.BAL = seq(0.2, 0.9, by = 0.025))


targetInputs <- baselineInputs ## may be the same, or different than baseline inputs

maxBiomassRatios <- baselineInputs %>%
    filter(year == 0) %>%
    group_by(landtype) %>%
    summarise(maxB_landtype = max(maxB)) %>%
    merge(baselineInputs) %>%
    group_by(species) %>%
    summarise(maxBiomassRatio = mean(maxB)/mean(maxB_landtype))
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
    intermediateInputs <- targetInputs
    intermediateInputs[c("maxANPP", "maxB")] <- intermediateInputs[c("maxANPP", "maxB")] * k
    
    for (i in seq_along(correctionFactors)) {
        sp <- names(correctionFactors)[i]
        nPad <- max(nchar(propMaxBiomassBoost[[i]]))
        
        for (j in seq_along(correctionFactors[[i]])) {
            
            ### applying species' correction factors
            corrFactor <- correctionFactors[[i]][j]
            
            finalInputs <- intermediateInputs
            finalInputs[c("maxANPP", "maxB")] <- round(finalInputs[finalInputs$species == sp, c("maxANPP", "maxB")] *
                                                           corrFactor)
            
            target <- propMaxBiomassBoost[[i]][j]
            ##### writing to file
            fileName <- paste0("biomass-succession-dynamic-inputs_",
                               str_pad(propMaxBiomassBoost[[i]][j], ifelse(target != 1, nPad, 0), pad = "0", side = "right"),
                               "_maxBk_",k, ".txt")
            #
            files <- append(files, fileName)
            sink(fileName)
            cat('LandisData "Dynamic Input Data"')
            cat("\n")
            cat("\n")
            cat(">> Warning - This is a bias-corrected file")
            cat("\n")
            cat("\n")
            cat(">> The following multiplier was applied to all species's original maxB and maxANPP")
            cat("\n")
            cat(paste0(c(">>", "multiplier"), collapse = "\t"))
            cat("\n")
            cat(paste0(c(">>", k), collapse = "\t"))
            cat("\n")
            cat("\n")
            cat(">> The following species' parameters (maxANPP and maxB) have also been modified")
            cat("\n")
            cat("\n")
            cat(paste0(c(">>", "species" , "\taverageMaxBiomassTarget", "finalMultiplier"), collapse = "\t"))
            cat("\n")
            cat(paste0(c(">>", sp, target, "\t", round(corrFactor[j], 3)),  collapse = "\t"))
            cat("\n")
            cat("\n")
            cat(paste0(c(">>",colnames(baselineInputs)), collapse="\t"))
            cat("\n")
            cat("\n")
            sink()
            ## param table
            write.table(finalInputs, file=fileName,
                        append=TRUE, row.names=FALSE, col.names=FALSE, sep="\t",
                        quote=FALSE,
                        #eol = "\r\n" #will produce Windows' line endings on a Unix-alike OS
                        eol = "\n" #default line endings on windows system.
            )
            
        }
    }
    
}







########################
########################
### generating simulation packages

sp <- "ABIE.BAL"
nSims <- length(files)
tmp <- str_split(gsub(".txt", "", files), "_")
targetABIEBAL <- as.numeric(lapply(tmp, function(x) x[2]))
maxBiomassCoeff <- as.numeric(lapply(tmp, function(x) x[4]))
correctionFactors
for (i in seq_along(files)) {
    
    corrFactor <- correctionFactors[[sp]][which(round(propMaxBiomassBoost[[sp]] - targetABIEBAL[i], 3) == 0)]
    
    simName <- str_pad(i, nchar(nSims), pad = "0")
    simDir <- paste0("../", simName)
    dir.create(simDir)
    file.copy("../scenario.txt", simDir, overwrite = T)
    file.copy(files[i], paste(simDir, "biomass-succession-dynamic-inputs.txt", sep = "/"), overwrite = T)
    
    df <- data.frame(simDir = simName, species = sp,
                     averageMaxBiomassTarget = targetABIEBAL[i],
                     spAnppMultiplier = corrFactor, 
                     spBiomassMultiplier = corrFactor,
                     maxBiomassMultiplier = maxBiomassCoeff[i])
    if(i == 1) {
        simInfo <- df
    } else {
        simInfo <- rbind(simInfo, df)
    }
    
}
write.csv(simInfo, file = "../simInfo.csv", row.names = F)



