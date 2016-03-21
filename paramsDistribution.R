rm(list=ls())
require(ggplot2)
processedOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
                             "/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession",
                             "C:/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")

### That assumes Picus outputs were processed on the same day
### (else, specify another folder containing formated Picus outputs)
#setwd(paste(processedOutputDir, Sys.Date(), sep="/"))
setwd(paste(processedOutputDir, Sys.Date(), sep="/"))

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
x <- list.files(full.names=F)
x <- x[grep("sep_", x)]
#### subsample of folderNames
#folderNames <- folderNames[grep("Acadian", folderNames)]#"AM|BSE|BSW|BP"
areas <- unique(gsub("growthParam_|sep_|.RData", "", x))

for (a in areas) {

    require(reshape2)
    timestep <- ifelse(grepl("5yrsTS", a), 5, 10)
    growthParam <- get(load(paste0("growthParam_", gsub("_.*", "", a), ".RData")))
    pEst <- get(load(paste0("sep_", a, ".RData")))

    i <- 1
    for (s in names(growthParam)){ # s <- names(growthParam)[2]
    for (p in names(growthParam[[s]])) { #p <- names(growthParam[[2]])[1]
        df <- melt(growthParam[[s]][[p]][["maxBiomass"]])
        colnames(df)[colnames(df)=="Var1"] <- "species"
        colnames(df)[colnames(df)=="Var2"] <- "landtype"
        colnames(df)[colnames(df)=="value"] <- "maxAGB"
        colnames(df)[colnames(df)=="value"] <- "maxAGB"
        df[,"maxANPP"] <- melt(growthParam[[s]][[p]][["maxANPP"]])$value
        df[is.na(df$maxANPP),"maxANPP"] <- 0
        df[is.na(df$maxAGB),"maxAGB"] <- 0
        df[,"SEP"] <- melt(pEst[[s]][[p]])$value
        df[,"period"] <- p
        df[,"scenario"] <- s
        df[,"ecozone"] <- a
        df[,c("maxANPP", "maxAGB")] <- df[,c("maxANPP", "maxAGB")]*10
        if (i ==1){
            params <- df
        } else {
            params <- rbind(params, df)
        }
        i <- i+1
        }
    }


    ## a little cleaning up
    params$period <- as.factor(params$period)
    params$scenario <- as.factor(params$scenario)
    params$landtype <- as.factor(params$landtype)
    params$ecozone <- as.factor(params$ecozone)

    params[,"paramSet"] <- paste(params$scenario, params$period)
    params[params[,"paramSet"] == "Baseline Baseline","paramSet"] <- "Baseline"
    params$paramSet <- factor(params$paramSet, levels=c("RCP85 20712100",
                                                        "RCP45 20712100",
                                                        "RCP26 20712100",
                                                        "RCP85 20412070",
                                                        "RCP45 20412070",
                                                        "RCP26 20412070",
                                                        "RCP85 20112040",
                                                        "RCP45 20112040",
                                                        "RCP26 20112040",
                                                        "Baseline"))
    # maxBiomass convert kg/ha to tons/ha
    params[, "maxAGB"] <- params[, "maxAGB"]/1000
    ##  maxANPP - convert kg/ha*yr to g/m*yr
    params[, "maxANPP"] <- params[, "maxANPP"]/10
    ##
    for (i in c("SEP", "maxANPP", "maxAGB")) { # i <- "maxAGB"

        # p <- qplot(species, eval(parse(text=i)), data = params, fill=paramSet, geom = "boxplot", colour="white") + coord_flip()
        p <- ggplot(data = params, aes(x=species, y=eval(parse(text=i)), fill=paramSet)) +
            geom_boxplot(outlier.colour = NULL, lwd=0.2, outlier.size = 0.5) +
            coord_flip() +
            theme(legend.position="top", legend.direction="horizontal", legend.text=element_text(size=6)) +
            guides(fill = guide_legend(reverse = TRUE)) +
            scale_fill_discrete("")

        if (i == "SEP") {
            p <- p + ylim(0,1)
        }


        png(filename = paste("ParamDistrib_", i, "_", a, ".png", sep=""),
            width = 10, height =6,
            units = "in", pointsize = 8, bg = "white",
            res = 300)

            print(p + labs(title = paste0("Distribution of ", i, " among land types (N=", length(levels(params$landtype)),")\n",
                                         ecoNames[ecoNames$code==a,"name"]),
                           y=ifelse(i=="SEP", paste0("Species Establishment Probability (per ", timestep, "-yrs time step)"),
                                    ifelse(i=="maxANPP",
                                           expression(paste("maxANPP", (g %.% m^-2 %.% yr^-1))),
                                           expression(paste("maxBiomass", (tons %.% ha^-1))))),
                           x=""))


        dev.off()
    }
}
