######################
######################
#######
#######   Plotting of raw Picus growth curves.
#######   Dominic Cyr
#######
#####################
######################
rm(list=ls())
setwd("~/Travail/SCF/Landis/Picus/PicusToLandisIIBiomassSuccession")
wwd <- paste(getwd(), Sys.Date(), sep="/")
dir.create(wwd)
setwd(wwd)
rm(wwd)
# processedOutputDir <- ifelse(Sys.info()["sysname"]=="Linux",
#                              "/media/dcyr/Windows7_OS/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession",
#                              "C:/Travail/Git/LandisScripts/PicusToLandisIIBiomassSuccession")


### vegCodes is the species master list
### It indicates which species to look in picus output folders
require(RCurl)
require(plyr)
require(dplyr)
require(ggplot2)

readURL <- "https://raw.githubusercontent.com/dcyr/LANDIS-II_IA_generalUseFiles/master/"
vegCodes <- read.csv(text = getURL(paste(readURL, "vegCodes.csv", sep="/")))
ecoNames <- read.csv(text = getURL(paste(readURL, "ecoNames.csv", sep="/")))
###
x <- list.files()
x <- x[grep("picusOutputsDF", x)]
###
areas <- unique(gsub("picusOutputsDF_|.csv", "", x))

### full file names
x <- paste(getwd(), x, sep="/")


for (a in seq_along(areas)) {
    areaCode <- areas[a]
    areaName <- ecoNames[which(ecoNames$code == areaCode), "name"]
    ### fetching picus outputs
    picusOutputsDF <- read.csv(x[grep(areaCode, x)])
    ### tidying up a bit
    picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
    df <- picusOutputsDF %>%
        mutate(simID = as.numeric(as.factor(paste0(ecozone,scenario,period,landtype,species))),
               AGB_tonsPerHa = BiomassAbove_kg_ha/1000,
               period_scenario = paste(scenario, period),
               species =  gsub("_", " ", species)) %>%
        mutate(period_scenario = gsub("Baseline ", "", period_scenario))

    ### filtering out some species (for poster illustration)
    df <- filter(df, species %in% c("Larix laricina", "Picea rubens",
                                    "Pinus resinosa", "Pinus strobus",
                                    "Quercus rubra", "Thuja occidentalis", "Tsuga canadensis") == F)

    ## shuffling simID for plotting
    ## (if you don't do that, some colors hide the ones just before)
    uniqueSimID <- unique(df$simID)
    df$simID <- match(df$simID, sample(uniqueSimID))
    ## renaming/reordering  levels
    df$period_scenario <- factor(df$period_scenario)#, levels = unique(c("Baseline",
                                                     #           "RCP26 20112040",
                                                      #          "RCP45 20112040",
                                                       #         "RCP85 20112040",
                                                        #        "RCP85 20412070",
                                                         #       "RCP26 20712100",
                                                          #      "RCP45 20712100",
                                                           #     "RCP85 20712100"))

    periodLevelNames <- c("Baseline", "2011-2040", "2041-2070", "2071-2100")
    names(periodLevelNames) <- c("Baseline", "20112040", "20412070", "20712100")
    df$period  <- factor(periodLevelNames[as.character(df$period)], levels = periodLevelNames)

    ## renaming levels for nicer plotting
    levelPS <- levels(df$period_scenario)
    newLevels <- c("Baseline",
                   paste(substring(levelPS[-1], 1,10),
                         substring(levelPS[-1], 11,14), sep="-"))
    df$period_scenario <- mapvalues(df$period_scenario, levelPS, newLevels)


    ylim <- c(0, 50*ceiling(quantile(df$AGB_tonsPerHa, .99)/50))

    ### assigning colors for climate change scenarios
    colScenarios <- c("black", "dodgerblue2", "goldenrod1", "red3")
    ## plotting
    p <- ggplot(df, aes(x = Year-2000, y = AGB_tonsPerHa, group = simID, col = scenario)) +
        theme_grey(base_size = 10) +
        #theme_dark(base_size = 10) +
        geom_line(lwd = 0.1, alpha=0.5) +
        scale_colour_manual(values = colScenarios) +
        facet_grid(species ~ period) +
        guides(col = guide_legend(override.aes = list(size = 1, alpha = 1))) +
        labs(title = paste("Pure stand growth as simulated by Picus\nin ",
                           length(unique(df$landtype)), " land types - ",
                           "Saguenay - Lac St-Jean", "\n", sep=""),
             y=expression(paste("Aboveground biomass ", (tons %.% ha^-1), "\n", sep="")),
             x="\nYear",
             caption = "Some species were left out for ") +
        scale_x_continuous(breaks = c(0, 100, 200))



    png(filename = paste0("picusGrowth_", areaCode, ".png"),
        #width = 1800, height = (160*length(unique(df$species))+200), units = "px", pointsize = 16,
        width = 8, height = (0.85*length(unique(df$species))+1), units = "in", res = 300,
        bg = "white")

        print(p + scale_y_continuous(limits = ylim) +
                  theme(plot.caption = element_text(size = rel(0.8)),
                        axis.text.x = element_text(size=8, angle = 45, hjust = 1),
                        axis.text.y = element_text(size=8),
                        strip.text.x = element_text(size=9),
                        strip.text.y = element_text(size=6)))

    dev.off()

}





