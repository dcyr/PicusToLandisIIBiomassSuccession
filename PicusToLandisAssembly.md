---
title: "PicusToLandisAssembly"
author: "Dominic Cyr"
date: "Tuesday, November 25, 2014"
output: word_document
---

# CodeBook for *PicusToLandisAssembly.R*

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## General description

This is documentation for [*PicusToLandisAssembly.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisAssembly.R), which takes the information contained in *growthParam.RData* and *pEst.RData*, and format them into .txt files ready to be used as [LANDIS-II](http://www.landis-ii.org/) [*Biomass Succesion*](http://www.landis-ii.org/extensions/biomass-succession) dynamic inputs. Those two R objects are produced by [PicusToLandisGrowthParams.R](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisGrowthParam.R) and [PicusToLandisSEP.R](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisSEP.R), respectively.

This script allows to customize scenario, as parameters can be updated at any time steps. The assembly is based on the following list:


```r
landisCCScenarios  ### example of the list on which the assembly of LANDIS-II biomass succession dynamic input file is based.
```

```
## $RCP85
## $RCP85$`0`
## [1] "Baseline" "Baseline"
## 
## $RCP85$`10`
## [1] "RCP85"    "20112040"
## 
## $RCP85$`40`
## [1] "RCP85"    "20412070"
## 
## $RCP85$`70`
## [1] "RCP85"    "20712100"
## 
## 
## $RCP45
## $RCP45$`0`
## [1] "Baseline" "Baseline"
## 
## $RCP45$`10`
## [1] "RCP85"    "20112040"
## 
## $RCP45$`40`
## [1] "RCP85"    "20412070"
## 
## $RCP45$`70`
## [1] "RCP45"    "20712100"
```
In the latter example, only the period after time step "70" differs between those two scenarios

More details about those parameters, how the participate to succession as simulated by LANDIS-II and how they interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.



