---
title: "PicusOutputsToDF"
author: "Dominic Cyr"
date: "Tuesday, November 25, 2014"
output: word_document
---

# CodeBook for *PicusOutputsToDF.R*

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## General description

This is documentation for [*PicusOutputsToDF.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusOutputsToDF.R), which yields a formatted data frame from PICUS outputs, which is saved on disc as *picusOutputsDF.csv*. That file is the primary input for [*PicusToLandisGrowthParam.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisGrowthParam.R) and [*PicusToLandisSEP.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisSEP.R), which can be used to compute Landis-II growth parameters (**maxANPP** and **maxBioamss**) and species establishment probabilities (**SEP**), respectively.

More details about those parameters, how the participate to succession as simulated by LANDIS-II and how they interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.


## Primary inputs
Outputs files from PICUS, which consist in various compartiment of living and dead biomass (in different files), for one or several species and landtypes, contained in .csv files.

Those pairs of files (living and dead biomass) are contained in folders that correspond to different simulation areas, climate scenarios, and period.

Here is an example of those folders:


```r
folderNames <- list.dirs(paste(readDir, "Picus/Outputs", sep="/"), full.names=F, recursive=F) 
folderNames
```

```
## [1] "YLT_BSE_Baseline"       "YLT_BSE_RCP26_20712100"
## [3] "YLT_BSE_RCP45_20712100" "YLT_BSE_RCP85_20112040"
## [5] "YLT_BSE_RCP85_20412070" "YLT_BSE_RCP85_20712100"
```

Each folder contains many .csv files. Here's an example of those files:

```r
folderNames <- paste(readDir, "Picus/Outputs", folderNames, sep="/")
x <- list.files(paste(folderNames[1], sep="/"))
x[1:10]
```

```
##  [1] "Abies_balsamea4011XDeadwood_PICUS IA Project_16642.csv"
##  [2] "Abies_balsamea4011XStand_PICUS IA Project_16642.csv"   
##  [3] "Abies_balsamea4012XDeadwood_PICUS IA Project_16643.csv"
##  [4] "Abies_balsamea4012XStand_PICUS IA Project_16643.csv"   
##  [5] "Abies_balsamea4013XDeadwood_PICUS IA Project_16644.csv"
##  [6] "Abies_balsamea4013XStand_PICUS IA Project_16644.csv"   
##  [7] "Abies_balsamea4014XDeadwood_PICUS IA Project_16645.csv"
##  [8] "Abies_balsamea4014XStand_PICUS IA Project_16645.csv"   
##  [9] "Abies_balsamea4015XDeadwood_PICUS IA Project_16646.csv"
## [10] "Abies_balsamea4015XStand_PICUS IA Project_16646.csv"
```

Here are summaries of one of these file pairs.


```r
summary(read.csv(paste(folderNames[1], x[1], sep="/"))) ### Summary of Dead biomass Picus raw outputs
```

```
##       Year            Species     DiedCount_ha DiedVolume_ha  
##  Min.   :2010   Balsam Fir:290   Min.   : 24   Min.   :0.006  
##  1st Qu.:2082                    1st Qu.:224   1st Qu.:4.017  
##  Median :2154                    Median :260   Median :4.470  
##  Mean   :2154                    Mean   :255   Mean   :4.427  
##  3rd Qu.:2227                    3rd Qu.:294   3rd Qu.:5.075  
##  Max.   :2299                    Max.   :380   Max.   :6.600  
##   DiedDBH_avg    DiedBiomassAbove_kg DiedVol_barkbeetle    StCnt_ha   
##  Min.   : 1.50   Min.   :   8        Min.   :0          Min.   :  24  
##  1st Qu.: 6.30   1st Qu.:3241        1st Qu.:0          1st Qu.:4818  
##  Median : 6.71   Median :3637        Median :0          Median :5238  
##  Mean   : 6.84   Mean   :3592        Mean   :0          Mean   :4796  
##  3rd Qu.: 7.15   3rd Qu.:4079        3rd Qu.:0          3rd Qu.:5480  
##  Max.   :12.36   Max.   :5275        Max.   :0          Max.   :5964  
##   StVolume_ha       StDBH_avg     StBiomass_kg    StVol_gt10cm 
##  Min.   :  0.01   Min.   :1.50   Min.   :    8   Min.   : 0.0  
##  1st Qu.: 86.74   1st Qu.:6.65   1st Qu.:69891   1st Qu.:63.2  
##  Median : 92.91   Median :6.74   Median :73280   Median :68.5  
##  Mean   : 84.29   Mean   :6.76   Mean   :68432   Mean   :62.5  
##  3rd Qu.: 96.34   3rd Qu.:6.85   3rd Qu.:75666   3rd Qu.:71.0  
##  Max.   :101.09   Max.   :9.16   Max.   :79947   Max.   :74.9  
##   StVol_gt20cm    MgmtCount MgmtVolume_ha  MgmtDBH_avg MgmtBiomassAbove
##  Min.   : 0.0   Min.   :0   Min.   :0     Min.   :0    Min.   :0       
##  1st Qu.:17.1   1st Qu.:0   1st Qu.:0     1st Qu.:0    1st Qu.:0       
##  Median :18.1   Median :0   Median :0     Median :0    Median :0       
##  Mean   :16.6   Mean   :0   Mean   :0     Mean   :0    Mean   :0       
##  3rd Qu.:19.5   3rd Qu.:0   3rd Qu.:0     3rd Qu.:0    3rd Qu.:0       
##  Max.   :22.7   Max.   :0   Max.   :0     Max.   :0    Max.   :0
```

```r
summary(read.csv(paste(folderNames[1], x[2], sep="/"))) ### Summary of Living biomass Picus raw outputs
```

```
##       Year            Species      StemNumber     LAI_m2_m2   
##  Min.   :2000   Balsam Fir:300   Min.   :   0   Min.   :0.00  
##  1st Qu.:2075                    1st Qu.:2146   1st Qu.:4.21  
##  Median :2150                    Median :2486   Median :4.54  
##  Mean   :2150                    Mean   :2364   Mean   :4.35  
##  3rd Qu.:2224                    3rd Qu.:2744   3rd Qu.:4.85  
##  Max.   :2299                    Max.   :3243   Max.   :6.07  
##  BiomassTotal_kg_ha BiomassAbove_kg_ha  Roots_kg_ha    Foliage_kg_ha 
##  Min.   :    24     Min.   :    0      Min.   :    0   Min.   :   0  
##  1st Qu.: 70964     1st Qu.:34663      1st Qu.: 9367   1st Qu.:5533  
##  Median : 76352     Median :36393      Median : 9826   Median :5980  
##  Mean   : 72858     Mean   :34973      Mean   : 9430   Mean   :5720  
##  3rd Qu.: 81519     3rd Qu.:38644      3rd Qu.:10418   3rd Qu.:6382  
##  Max.   :110376     Max.   :47072      Max.   :12524   Max.   :7986  
##    Stem_kg_ha     Twigs_kg_ha   CarbonTotal_kg    Volume_m3   
##  Min.   :    0   Min.   :   0   Min.   :    0   Min.   : 0.0  
##  1st Qu.:24459   1st Qu.:4604   1st Qu.:22876   1st Qu.:44.0  
##  Median :25771   Median :4852   Median :24030   Median :46.3  
##  Mean   :24624   Mean   :4629   Mean   :23089   Mean   :43.5  
##  3rd Qu.:27110   3rd Qu.:5107   3rd Qu.:25513   3rd Qu.:48.6  
##  Max.   :33491   Max.   :6322   Max.   :30939   Max.   :54.3  
##  TotalGrossGrowth_m3 AnnualIncrement_m3 RemovedVolume_m3  BasalArea_m2 
##  Min.   :   0        Min.   :0.00       Min.   :0.00     Min.   : 0.0  
##  1st Qu.: 303        1st Qu.:4.20       1st Qu.:4.16     1st Qu.:12.7  
##  Median : 659        Median :4.83       Median :4.70     Median :13.5  
##  Mean   : 668        Mean   :4.68       Mean   :4.51     Mean   :13.0  
##  3rd Qu.:1031        3rd Qu.:5.60       3rd Qu.:5.29     3rd Qu.:14.4  
##  Max.   :1403        Max.   :7.55       Max.   :6.74     Max.   :17.5  
##  SeedProduction_N_ha   DBH_avg_cm    Height_avg_cm  DomHeight_cm 
##  Min.   :       0    Min.   : 0.00   Min.   :  0   Min.   :   0  
##  1st Qu.: 6020684    1st Qu.: 6.33   1st Qu.:462   1st Qu.:1498  
##  Median : 6415810    Median : 6.78   Median :500   Median :1560  
##  Mean   :13845353    Mean   : 6.63   Mean   :477   Mean   :1411  
##  3rd Qu.:30204695    3rd Qu.: 7.09   3rd Qu.:527   3rd Qu.:1579  
##  Max.   :34586286    Max.   :12.10   Max.   :682   Max.   :1649  
##  Height_Lorey_cm DBH_avg_Basal_cm  DBHRange_cm    AvgTemp_deg     
##  Min.   :   0    Min.   : 0.00    Min.   : 0.0   Min.   :-1.4635  
##  1st Qu.: 950    1st Qu.: 7.93    1st Qu.:28.9   1st Qu.:-0.5162  
##  Median : 987    Median : 8.34    Median :30.8   Median : 0.0928  
##  Mean   : 924    Mean   : 8.12    Mean   :28.7   Mean   : 0.2179  
##  3rd Qu.:1027    3rd Qu.: 8.71    3rd Qu.:32.2   3rd Qu.: 0.7076  
##  Max.   :1095    Max.   :13.21    Max.   :38.8   Max.   : 2.9918  
##    SumPrec_mm  
##  Min.   : 703  
##  1st Qu.: 832  
##  Median : 860  
##  Mean   : 863  
##  3rd Qu.: 904  
##  Max.   :1027
```

The script is dependant on that file structure. It will to be adapted should there be modifications in the file structure.

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

## Primary outputs
A tidy data table *picusOutputsDF.csv* produced by the script [*PicusOutputsToDF.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusOutputsToDF.R), which contains all necessary information to compute **maxANPP**, **maxBiomass**, and species establishment coefficients (**SEP**) for every combinations of species and landtypes. That .csv file may contain information from one or more study area, climate scenario, and period.


```r
summary(picusOutputsDF)
```

```
## Length  Class   Mode 
##      0   NULL   NULL
```



