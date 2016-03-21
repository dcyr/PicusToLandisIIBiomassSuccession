######################
######################
#######
#######   Summary statistics and figures related to Landis-II biomass succession parameters
#######   Dominic Cyr
#######
#####################
######################
rm(list=ls())


processedOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                             "/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession",
                             "C:/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")

### That assumes Picus outputs were processed on the same day
### (else, specify another folder containing formated Picus outputs)
wwd <- paste(processedOutputDir, Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)

### sometimes useful to change that...
options(scipen=4)
###

### vegCodes is the species master list
### It indicates which species to look in picus output folders
require(RCurl)
readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
ecoNames <- read.csv(text = getURL(paste(readURL, "ecoNames.csv", sep="/")))
######################
######################
############


#### subsample of folderNames
areas <- "MC"

for (a in areas) {
    x  <- list.files()
    x <- x[grep("biomass-succession-dynamic-inputs", x)]
    dynamicInputsFiles <- x[grep("RCP85|RCP45|RCP26", x)]
    for (i in seq_along(dynamicInputsFiles)) { # i <- 1
        df <- read.table(dynamicInputsFiles[i], skip=1, comment.char = ">")
        file <- rep(dynamicInputsFiles[i], nrow(df))
        if (i > 1){
            inputs <- rbind(cbind(file, df), inputs)
        } else {
            inputs <- cbind(file, df)
        }
    }
    colnames(inputs)[-1] <- c("year", "landtype", "species", "probEst", "maxANPP", "maxB")

    ############
    ######  Max biomass by landtypes and periods
    ############
    require(reshape2)
    require(dplyr)
    inputs2 <- melt(inputs, id=c("file", "year", "landtype", "species"), measure.vars=c("probEst", "maxANPP", "maxB"))

    for (s in seq_along(levels(inputs2$file)))  { # s <- 1
        dfTmp <- subset(inputs2, inputs2$file == levels(inputs2$file)[s] &
                            inputs2$variable == "maxB")
        dfMax <- dfTmp %>%
            group_by(year, landtype) %>%
            summarize(maxB = max(value, na.rm=TRUE))

        dfMax <- as.data.frame(dfMax)

        speciesMax <- character()
        for (i in 1:nrow(dfMax)) {
            index <- which(dfTmp$landtype==dfMax[i,"landtype"] &
                               dfTmp$year==dfMax[i,"year"] &
                               dfTmp$value==dfMax[i,"maxB"])
            speciesMax <- append(speciesMax, as.character(dfTmp[index, "species"])[1])
        }
        dfMax[, "species"] <-  factor(speciesMax, levels(dfTmp$species))

        scenario <- strsplit(as.character(unique(dfTmp$file)), "_")
        scenario <- scenario[[1]][3]
        scenario <- gsub(".txt", "", scenario)
        dfMax[,"scenario"] <- scenario

        if (s > 1){
            maxBiomByLandtype <- rbind(dfMax, maxBiomByLandtype)
        } else {
            maxBiomByLandtype <- dfMax
        }
    }
    #str(maxBiomByLandtype)
    maxBiomByLandtype <- maxBiomByLandtype[,c(5,1,2,3,4)]
    colnames(maxBiomByLandtype) <- c("scenario", "year", "landtype", "landtypeMaxB", "speciesMax")
    rownames(maxBiomByLandtype) <- 1:nrow(maxBiomByLandtype)
    maxBiomByLandtype[,"scenario"] <- as.factor(maxBiomByLandtype[,"scenario"])
    maxBiomByLandtype[,"landtype"] <- as.factor(maxBiomByLandtype[,"landtype"])
    maxBiomByLandtype[,"year"] <- as.factor(maxBiomByLandtype[,"year"])
    maxBiomByLandtype[,"speciesMax"] <- as.factor(maxBiomByLandtype[,"speciesMax"])

    df0 <- maxBiomByLandtype[maxBiomByLandtype$year == 0, c("year", "landtype", "landtypeMaxB", "speciesMax")]
    df80 <- maxBiomByLandtype[maxBiomByLandtype$year == 80,]

    df0 <- droplevels(df0)
    df80 <- droplevels(df80)
    ######## plot years 2000 to 2070 (one scenario)
    colScenarios <- c("black", "dodgerblue2", "goldenrod1", "red3")
    require(ggplot2)
    require(directlabels)

    options("scipen"=100, "digits"=4)
    p <- ggplot(df0, aes(landtype, landtypeMaxB, fill=year)) +
        geom_bar(stat="identity", position="dodge", fill = "black") +
        theme(axis.text=element_text(size=4,
                                     angle=-90)) +
        ylim(0, round(max(df0$landtypeMaxB*1.1),-3)) +
        ggtitle("Maximum biomass by landtype by period") +
        geom_text(aes(label=paste(speciesMax, landtypeMaxB)),
                  hjust=1, vjust=.5, angle=-90, size=2,
                  position = position_dodge(width=0.9))

    png(filename = paste0("maxBiomassByLandtype_0-70_", a, ".png"),
        width = 11, height = 6,
        units = "in", pointsize = 6, bg = "white",
        res = 300)

        print(p)

    dev.off()


    ######## plot years 2070 to 2099 (three scenarios)

    require(ggplot2)
    require(directlabels)

    p <- ggplot(df80, aes(landtype, landtypeMaxB, fill=scenario)) +
        geom_bar(stat="identity", position="dodge") +
        theme(axis.text=element_text(size=6,
                                         angle=-90)) +
        ylim(0, round(max(df0$landtypeMaxB*1.1),-3)) +
        scale_fill_manual(values = colScenarios[2:4]) +
        ggtitle("Maximum biomass by landtype by scenario for 2070 and beyond") +
        geom_text(aes(label=paste(speciesMax, landtypeMaxB)),
                  hjust=1, vjust=.5, angle=-90, size=1.5,
                  position = position_dodge(width=0.9))

    options("scipen"=100, "digits"=4)
    png(filename = paste("maxBiomassByLandtype_71-00_", a, ".png", sep=""),
        width = 11, height = 6,
        units = "in", pointsize = 6, bg = "white",
        res = 300)

        print(p)

    dev.off()
}
