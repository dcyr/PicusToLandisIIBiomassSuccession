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

- **maxANPP** is the maximum aboveground net primary productivity in grams per sq-meters per year. It can be achieved in free growth conditions, i.e. in absence of inter- or intra-specific competition.
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
##  BSE:2070590   Baseline: 302406   20112040: 360987   4225   :  28182  
##                RCP26   : 365246   20412070: 363716   4205   :  28154  
##                RCP45   : 363440   20712100:1043481   4223   :  28107  
##                RCP85   :1039498   Baseline: 302406   4175   :  28102  
##                                                      4413   :  28066  
##                                                      4011   :  27999  
##                                                      (Other):1901980  
##                 species             Year      BiomassAbove_kg_ha
##  Pinus_banksiana    : 142200   Min.   :2000   Min.   :     0    
##  Betula_papyrifera  : 139394   1st Qu.:2075   1st Qu.: 35933    
##  Populus_tremuloides: 138847   Median :2150   Median : 81325    
##  Thuja_occidentalis : 137709   Mean   :2150   Mean   : 77241    
##  Pinus_strobus      : 136786   3rd Qu.:2225   3rd Qu.:110892    
##  Pinus_resinosa     : 136691   Max.   :2299   Max.   :388526    
##  (Other)            :1238963                                    
##  DiedBiomassAbove_kg      anpp      
##  Min.   :    0       Min.   :-6185  
##  1st Qu.: 1784       1st Qu.: 1552  
##  Median : 3305       Median : 3423  
##  Mean   : 3432       Mean   : 3682  
##  3rd Qu.: 4780       3rd Qu.: 5085  
##  Max.   :28266       Max.   :95158  
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
  + **Old method:** The maximum value of accumulated biomass in Picus is computed and used as maxBiomass.
  + **New method:** The average value after a given amount of time (typically 100 or 150 years) is computed and used as maxBiomass. This method seems to allow for an almost perfect fit when establishment is not limited by light conditions.
  
* __maxANPP__
  + **Method 1:** The maximum instant (yearly) increment in biomass is computed from PICUS output and used as maxANPP.
  + **Method 2:** Simular to method 1, but the annual increment in biomass is smoothed using a moving average before extracting the maximum value.
  + **Method 3:** The average NPP before reaching the maximum biomass is computed and used as maxANPP. (That method departs a bit from the intended definitino of the parameter, but seems to produce values that are closer than what's reported in the litterature.)


```r
picusOutputsDF$landtype <- as.factor(picusOutputsDF$landtype)
str(growthParam)  ### Shows the structure of the list produced 
```

```
## List of 4
##  $ Baseline:List of 1
##   ..$ Baseline:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 3680 0 3723 9062 5135 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 237 0 89 73 374 0 176 539 482 426 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##  $ RCP26   :List of 1
##   ..$ 20712100:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 3879 10921 7166 12492 5510 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 208 663 289 505 507 254 187 647 445 550 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##  $ RCP45   :List of 1
##   ..$ 20712100:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 3696 12257 7361 12720 5411 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 272 832 317 415 478 433 265 537 419 558 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##  $ RCP85   :List of 3
##   ..$ 20112040:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 3946 10084 6968 12318 5390 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 328 497 314 435 539 326 281 531 445 538 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   ..$ 20412070:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 3599 12431 7398 12967 5396 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 224 843 323 519 456 527 266 742 322 531 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   ..$ 20712100:List of 2
##   .. ..$ maxBiomass: num [1:16, 1:79] 2056 12616 6113 12067 4747 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
##   .. ..$ maxANPP   : num [1:16, 1:79] 79 1015 237 434 304 ...
##   .. .. ..- attr(*, "dimnames")=List of 2
##   .. .. .. ..$ : chr [1:16] "ABIE.BAL" "ACER.RUB" "ACER.SAH" "BETU.ALL" ...
##   .. .. .. ..$ : chr [1:79] "4011" "4012" "4013" "4014" ...
```

### Optional outputs
Wide tables can be produced for each parameter, scenario, and period, which allow for better readibility. They are stored in .csv files in the **wwd** folder.


Here is an example of all outputs that can be produced:


```r
list.files(wwd)
```

```
##  [1] "growthParam.RData"                      
##  [2] "maxANPP_Baseline_Baseline.csv"          
##  [3] "maxANPP_RCP26_20712100.csv"             
##  [4] "maxANPP_RCP45_20712100.csv"             
##  [5] "maxANPP_RCP85_20112040.csv"             
##  [6] "maxANPP_RCP85_20412070.csv"             
##  [7] "maxANPP_RCP85_20712100.csv"             
##  [8] "maxBiomass_Baseline_Baseline.csv"       
##  [9] "maxBiomass_RCP26_20712100.csv"          
## [10] "maxBiomass_RCP45_20712100.csv"          
## [11] "maxBiomass_RCP85_20112040.csv"          
## [12] "maxBiomass_RCP85_20412070.csv"          
## [13] "maxBiomass_RCP85_20712100.csv"          
## [14] "pEst.RData"                             
## [15] "pEst_Baseline_Baseline.csv"             
## [16] "pEst_RCP26_20712100.csv"                
## [17] "pEst_RCP45_20712100.csv"                
## [18] "pEst_RCP85_20112040.csv"                
## [19] "pEst_RCP85_20412070.csv"                
## [20] "pEst_RCP85_20712100.csv"                
## [21] "timeBeforeBiomass_Baseline_Baseline.csv"
## [22] "timeBeforeBiomass_RCP26_20712100.csv"   
## [23] "timeBeforeBiomass_RCP45_20712100.csv"   
## [24] "timeBeforeBiomass_RCP85_20112040.csv"   
## [25] "timeBeforeBiomass_RCP85_20412070.csv"   
## [26] "timeBeforeBiomass_RCP85_20712100.csv"
```



