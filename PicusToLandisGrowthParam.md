---
title: "PicusToLandisGrowthParam"
author: "Dominic Cyr"
date: "Tuesday, November 25, 2014"
output: html_document
---

# CodeBook for [*PicusToLandisGrowthParam.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisGrowthParam.R)

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## General description

This is documentation for [*PicusToLandisGrowthParam.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisGrowthParam.R), which yields growth parameters (**maxANPP** and **maxBiomass**) from previously formatted PICUS outputs.

- **maxANPP** is the maximum aboveground net primary productivity in grams per sq-meters per year. It can only be achieved in free growth conditions, i.e. in absence of inter- or intra-specific competition.
- **maxBiomass** is the maximum biomass than one species can attain on a given site, in grams per sq-meters.

More details about those parameters, how the participate to succession as simulated by LANDIS-II and how they interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.

## Primary input
A data table *picusOutputsDF.csv* produced by the script [*PicusOutputsToDF.R*](insert URL), which contains all necessary information to compute **maxANPP** and **maxBiomass**, for every combinations of species and landtypes. That .csv file may contain information from one or more study area, climate scenario, and period.

The location of *picusOutputsDF.csv* must be specified and can be assigned to variable **processedDir**.


```r
summary(picusOutputsDF)
```

```
##  ecozone           scenario            period           landtype      
##  BSE:2242154   Baseline: 324203   20112040: 383392   4234   :  28785  
##                RCP26   : 383963   20412070: 383584   4254   :  28782  
##                RCP45   : 383876   20712100:1150975   4255   :  28781  
##                RCP85   :1150112   Baseline: 324203   4263   :  28768  
##                                                      4265   :  28762  
##                                                      4235   :  28761  
##                                                      (Other):2069515  
##                 species             Year      BiomassAbove_kg_ha
##  Pinus_banksiana    : 144000   Min.   :2000   Min.   :     0    
##  Picea_glauca       : 143995   1st Qu.:2075   1st Qu.: 47461    
##  Populus_tremuloides: 143995   Median :2150   Median : 93860    
##  Betula_papyrifera  : 143991   Mean   :2150   Mean   : 88011    
##  Picea_mariana      : 143755   3rd Qu.:2225   3rd Qu.:119416    
##  Abies_balsamea     : 143709   Max.   :2299   Max.   :446763    
##  (Other)            :1378709                                    
##  DiedBiomassAbove_kg      anpp       
##  Min.   :    0       Min.   : -9275  
##  1st Qu.: 2393       1st Qu.:  2230  
##  Median : 3751       Median :  3932  
##  Mean   : 3952       Mean   :  4232  
##  3rd Qu.: 5228       3rd Qu.:  5563  
##  Max.   :36446       Max.   :123461  
## 
```

### Seconday input
A few additionnal information contained in *vegCodes.csv* allows for a smoother processing of multiple species and simulation areas. That table contains the scientific names, common names, LANDIS names, etc., and the species to include in each simulation areas (as binary variables).

```r
head(vegCodes)  ### first lines of table 'vegCodes.csv'
```

```
##   Code      Common.name                    Scientific.name Farrar.1995.
## 1  100           Spruce                              Picea           95
## 2  101     Black spruce          Picea mariana (Mill.) BSP          106
## 3  102       Red spruce                 Picea rubens Sarg.          104
## 4  103  Norway spruce *            Picea abies (L.) Karst.          108
## 5  104 Engelmann spruce Picea engelmannii Parry ex Engelm.          100
## 6  105     White spruce         Picea glauca (Moench) Voss          102
##   Life.form Code_LANDIS Veg_rep    PICUS_name BSE
## 1      <NA>        <NA>      NA          <NA>   0
## 2     ST-MT    PICE.MAR       0 Picea_mariana   1
## 3        MT    PICE.RUB       0  Picea_rubens   0
## 4        LT        <NA>      NA          <NA>   0
## 5 LT (-VLT)    PICE.ENG       0          <NA>   0
## 6        MT    PICE.GLA       0  Picea_glauca   1
```

## Primary ouput
A structured R list *growthParam.RData*  containing the extracted growth parameters **maxANPP** and **maxBiomass**. Its hierarchical structure should facilitate the use of loops in downstream analysis.

That object is saved in the folder **wwd**, and overwrite any existing file of the same name. A new folder is generated one is generated every day using current date.


There are currently 2 implemented methods for computing **maxBiomass**, and three for computing **maxANPP**. Only one should be commented out in the code, else, the last one will overwrite any previous results.

* __maxBiomass__
  + **Old method:** The maximum value of accumulated biomass in Picus is computed and used as maxBiomass. Using that method, the maxBiomass is generally based on the peak biomass that is reached by the initial cohort, which generally results in an overestimation of maxBiomass as intended for LANDIS-II. 
  + **New method (preferred method):** The average value after a given amount of time (typically 100 or 150 years) is computed and used as maxBiomass. That method seems to allow for an almost perfect fit of LANDIS growth curves with PICUS outputs when establishment is not limited by light conditions.
  
* __maxANPP__
  + **Method 1:** The maximum instant (yearly) increment in biomass is computed from PICUS output and used as maxANPP.
  + **Method 2 (preferred method):** Simular to method 1, but the annual increment in biomass is smoothed using a moving average before extracting the maximum value.
  + **Method 3:** The average NPP before reaching the maximum biomass is computed and used as maxANPP. (That method departs a bit from the intended definitino of the parameter, but seems to produce values that are closer than what's reported in the litterature.)


```r
picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
str(growthParam)  ### Shows the structure of the list produced 
```

```
## Error: object 'growthParam' not found
```

### Optional outputs
Wide tables can be produced for each parameter, scenario, and period, which allow for better readibility. They are stored in .csv files in the **wwd** folder.


Here is an example of all outputs that can be produced:


```r
list.files(wwd)
```

```
## [1] "biomass-succession-dynamic-inputs_RCP26.txt"
## [2] "biomass-succession-dynamic-inputs_RCP45.txt"
## [3] "biomass-succession-dynamic-inputs_RCP85.txt"
```



