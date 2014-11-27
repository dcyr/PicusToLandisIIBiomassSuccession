---
title: "PicusToLandisSEP"
author: "Dominic Cyr"
date: "Tuesday, November 25, 2014"
output: html_document
---

# CodeBook for *PicusToLandisSEP.R*

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## General description

This is documentation for [*PicusToLandisSEP.R*](insertURL), which yields species establishment probabilities (**SEP**) from previously formatted PICUS outputs.

- **SEP** is the probability of a cohort to successfully establish on a site during one time step, given that seeds reach it and that light conditions are adequate. It can range from 0 to 1.

More details about this parameter, how it participate to succession as simulated by LANDIS-II and how it interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.

## Primary input
A data table *picusOutputsDF.csv* produced by the script [*PicusOutputsToDF.R*](insert URL), which contains all necessary information to compute **SEP**, for every combinations of species and landtypes. That .csv file may contain information from one or more study area, climate scenario, and period.

The location of *picusOutputsDF.csv* must be specified and can be assigned to variable **processedDir**.

```{r}
summary(picusOutputsDF)
```

### Seconday input
A few additionnal information contained in *vegCodes.csv* allows for a smoother processing of multiple species and simulation areas. That table contains the scientific names, common names, LANDIS names, etc., and the species to include in each simulation areas (as binary variables).
```{r}
head(vegCodes)  ### first lines of table 'vegCodes.csv'
```


## Primary ouput
A structured R list *pEST.RData*  containing the extracted SEPs. Its hierarchical structure should facilitate the use of loops in downstream analysis.

That object is saved in the folder **wwd**, and overwrite any existing file of the same name. A new folder is generated one is generated every day using current date.


## Methodological details

We assume that SEP for LANDIS-II is directly linked with the time necessary to accumulate aboveground in biomass in PICUS. That interval is sensitive to climate and soil.
We thus take the time necessary to accumulate aboveground biomass (tbb), and suppose that the probability of establishment at any time step in LANDIS-II is equal 1/tbb.

Then, we consider the establishment of a cohort as a Bernouilli trial conducted every year during a time step. If the time step is 10 years, we compute the probability of having more than zero success (1 or more) in 10 consecutive trials.

The SEP varies as a function of time before accumulating biomass as followed: 

```{r}
x <- seq(from=1, to=300, by=0.5)
y <- pbinom(q=0, size=10, prob=1/x, lower.tail=FALSE)
plot(x, y, log="x", type="l",
     main="Species Establishment Probabilities (SEP) in LANDIS-II as a function
     of time before accumulating above-ground biomass in PICUS", 
     xlab="Time before accumulating AGB in PICUS (years)",
     ylab="Corresponding SEP in LANDIS-II (10-yr. timesteps)")
  grid(equilogs=FALSE)  
```




