---
title: "README.md"
author: "Dominic Cyr"
date: "Thursday, November 27, 2014"
output: html_document
---

# General description

In this repository you will find R scripts that allow to generate [LANDIS-II](http://www.landis-ii.org/)'s [Biomass Succession extension](http://www.landis-ii.org/extensions/biomass-succession) dynamic inputs from the ouputs of stand-level forest simulator [PICUS](http://www.wabo.boku.ac.at/en/waldbau/forschung/fachgebiete/waldoekosystemmodellierung/dynamische-oekosystemmodelle/picus/).

One of the current study's objective is to emulate species growth as simulated by PICUS, upscale stand-level growth to landscape level using LANDIS-II, and integrate processes that occur at those scales such as fire, seed dispersal, and landscape-level forest management, etc.

More specifically, that involves fetching PICUS outputs, passing them to a first script that format those into a tidy .csv file, which is then used to generate Species Establishment Probabilities (__SEP__) and growth parameters (__maxANPP__ and __maxBiomass__. Those parameter set are then finally assembled into the appropriate format for LANDIS-II, according to user-specified scenarios.

The scripts allow to automate the processing of multiple species, climate scenarios, periods, and simulation areas at once.

Note that some secondary (required) inputs and optional are not shown on this figure.
![plot of chunk PicusToLandisWorkflow](figure/PicusToLandisWorkflow.png)


More details about each steps can be found in the markdown files (.md) that are associated with each script.

Other useful scripts may be added soon.

More details about those parameters, how the participate to succession as simulated by LANDIS-II and how they interact with each other, are available in [Scheller et al. 2004](http://landscape.forest.wisc.edu/PDF/Scheller_Mladenoff2004_EM.pdf), as well as in the latest [Biomass Succession documentation](http://www.landis-ii.org/extensions/biomass-succession) for an updated version.


