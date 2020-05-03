[![DOI](https://zenodo.org/badge/257594337.svg)](https://zenodo.org/badge/latestdoi/257594337)
# Replication package for the manuscript *Pandemic Programming*

The replication package also consists of a more user friendly overview of the structural equation modeling. The latest version can be viewed [here](https://torkar.github.io/pandemic_programming/).

## Project structure

```
. 
+-- Rmd/                        # 
|   +-- Analysis for PP.Rmd     # The `Rmd`, which you can knit in `RStudio`
+-- analysis/                   # 
|   +-- SEM.R                   # Main analysis script for confirmatory factor analysis and structural 
|                               # equation modeling (see also `Rmd/`)
|   +-- BDAMLMs.R               # Analysis script with Bayesian multilevel models (reviewers might want 
|                               # to have more details; here's plenty of it)
+-- data/
|   +-- export_2020-04-16.csv   # All data in `CSV` format
+-- docs/
|   +-- index.html              # The html generated from the `Rmd` once knitted
+-- replication/                # 
|   +-- cleaning instructions/  # Instructions for how to clean/recode questionnaire data
    |   +-- clean.docx          # Cleaning instructions
    |   +-- recode.xlsx         # Recode instructions
    +-- Questionnaires/
    |   +-- *.pdf               # Questionnaire in 12 languages
+-- README.md                   # This file
+-- pandemic_programming.Rproj  # Open this in `RStudio` workspace to set working directory
```

## Where to start?

If you are interested in ...

* the code for our the main analysis in the paper then check `analysis/SEM.R`
* running the code then please have a look at `Rmd/` instead, where an `Rmd` file can be executed
* the `html` when running the `Rmd` file see [here](https://torkar.github.io/pandemic_programming/)
* asking questions given a posterior probability dsitribution see `analysis/BDAMLMs.R`
* getting your hands on the data, please see the `data/` directory
* if you want to replicate this study, please see `replication/`
