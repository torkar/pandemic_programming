[![DOI](https://zenodo.org/badge/257594337.svg)](https://zenodo.org/badge/latestdoi/257594337)
# Replication package for the manuscript *Pandemic Programming*

The replication package also consists of a more user friendly overview of the structural equation modeling. The latest version can be viewed [here](https://github.com/torkar/pandemic_programming/blob/master/Rmd/Analysis-for-PP.html).

## Project structure

```
. 
+-- Rmd/                        # `RMarkup`
|   +-- Analysis for PP.Rmd     # The `Rmd`, which you can knit in `RStudio`
|   +-- Analysis-for-PP.html    # The html generated from the `Rmd once knitted
+-- analysis/                   # Executable `R` scripts
|   +-- SEM.R                   # Main analysis script for confirmatory factor analysis and structural 
|                               # equation modeling (see also `Rmd/`)
|   +-- BDAMLMs.R               # Analysis script with Bayesian multilevel models (reviewers might want 
|                               # to have more details; here's plenty of it)
+-- data/                       # Data
|   +-- export_2020-04-16.csv   # All data in `CSV` format
+-- replication/                # If you want to redo the study yourself
|   +-- cleaning instructions/  # Instructions for how to clean/recode questionnaire data
    |   +-- clean.docx          # Cleaning instructions
    |   +-- recode.xlsx         # Recode instructions
    +-- Questionnaires/
    |   +-- *.pdf               # Questionnaire in 12 languages
+-- pandemic_programming.Rproj  # Open this in `RStudio` workspace to set working directory
+-- README.md                   # This file
```

## Where to start?

If you are interested in ...

* the code for our the main analysis in the paper then check `analysis/SEM.R`
* asking questions given a posterior probability dsitribution see `analysis/BDAMLMs.R`
* getting your hands on the data, please see the `data/` directory
* if you want to replicate this study, please see `replication/`
