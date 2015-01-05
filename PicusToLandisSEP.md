---
title: "PicusToLandisSEP"
author: "Dominic Cyr"
date: "Tuesday, November 25, 2014"
output: html_document
---

# CodeBook for *PicusToLandisSEP.R*

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

## General description

This is documentation for [*PicusToLandisSEP.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusToLandisSEP.R), which yields species establishment probabilities (**SEP**) from previously formatted PICUS outputs.

- The **SEP** is the probability of a given's species cohort to successfully establish on a given landtype during one time step, granted that seeds reach it and that light conditions are adequate. It can range from 0 to 1.

More details about this parameter, how it participate to succession as simulated by LANDIS-II and how it interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.

## Primary input
A data table *picusOutputsDF.csv* produced by the script [*PicusOutputsToDF.R*](http://github.com/dcyr/Landis-II-SCF/blob/master/PicusOutputsToDF.R), which contains all necessary information to compute **SEP**, for every combinations of species and landtypes. That .csv file may contain information from one or more study area, climate scenario, and period.

The location of *picusOutputsDF.csv* must be specified and can be assigned to variable **processedDir**.


```r
summary(picusOutputsDF)
```

```
## Error: erreur d'évaluation de l'argument 'object' lors de la sélection d'une méthode pour la fonction 'summary' : Error: object 'picusOutputsDF' not found
```

### Seconday input
A few additionnal information contained in *vegCodes.csv* allows for a smoother processing of multiple species and simulation areas. That table contains the scientific names, common names, LANDIS names, etc., and the species to include in each simulation areas (as binary variables).

```r
head(vegCodes)  ### first lines of table 'vegCodes.csv'
```

```
## Error: erreur d'évaluation de l'argument 'x' lors de la sélection d'une méthode pour la fonction 'head' : Error: object 'vegCodes' not found
```


## Primary ouput
A structured R list *pEST.RData*  containing the extracted SEPs. Its hierarchical structure should facilitate the use of loops in downstream analysis.

That object is saved in the folder **wwd**, and overwrite any existing file of the same name. A new folder is generated one is generated every day using current date.


## Methodological details


The Species Establishment Probability (SEP) is the probability of a given's species cohort to successfully establish on a given land type during one time step, granted that seeds reach it and that light conditions are adequate. It can range from 0 to 1.

That parameter must therefore be specified for every combinations of species by land types. It is also one that is almost impossible to ground entirely into empirical data, as the experimental conditions necessary to document such probabilities are almost impossible to obtain in the real world, at least not for the entire range of species and land types that we wish to simulate with LANDIS-II.

That parameter is also scale-sensitive, which often limit their portability from one simulation project to another.
Typically, three approaches have been used to set those parameters' values. In increasing order of subjectivity, they are:
+ Expert knowledge
+ Forest inventory data
+  Gap models / Stand-level physiological models

Even the use of lower-level physiological models implies some level of subjectivity. First, there are many models which can yields considerably different results. Choosing one over another is not always easy to justify and is often a matter of accessibility.  Secondly, those model usually won't provide the probabilities that are needed in the most direct way. Some additional assumptions may be required.

Luckily, we can take advantage of the stand-level simulations conducted with PICUS, even though it doesn't yield a SEP per se. 

First, and most importantly, we assume that SEP for LANDIS-II is directly linked with the time necessary to accumulate aboveground in biomass in PICUS. That interval is sensitive to climate and soil.

We then take the time necessary to accumulate aboveground biomass (t) in PICUS and, for our purpose of translating that information into a SEP for LANDIS-II, consider it as the result of a random process associated with an annual probability of 1/t.

We thus consider the establishment of a cohort as a Bernouilli trial conducted every year during a time step. If the time step is 10 years, we compute the probability of having more than zero success (1 or more) in 10 consecutive trials.

The SEP varies as a function of time before accumulating biomass as followed:


```r
x <- seq(from=1, to=300, by=0.5)
y <- pbinom(q=0, size=10, prob=1/x, lower.tail=FALSE)
plot(x, y, log="x", type="l",
     main="Species Establishment Probabilities (SEP) in LANDIS-II as a function
     of time before accumulating above-ground biomass in PICUS", 
     xlab="Time before accumulating AGB in PICUS (years)",
     ylab="Corresponding SEP in LANDIS-II (10-yr. timesteps)")
  grid(equilogs=FALSE)  
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 




