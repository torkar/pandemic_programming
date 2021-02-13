[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4533684.svg)](https://doi.org/10.5281/zenodo.4533684)
# Replication package for the manuscript *Pandemic Programming*

The replication package also consists of a more user friendly document of the structural equation modeling. The latest version can be viewed [https://torkar.github.io/pandemic_programming/](https://torkar.github.io/pandemic_programming/).

## Project structure

```
. 
+-- Rmd/                        # 
|   +-- Analysis for PP.Rmd     # The `Rmd`, which you can knit in `RStudio`
+-- analysis/                   # 
|   +-- SEM.R                   # Main analysis script for confirmatory factor analysis and structural 
|   |                           # equation modeling (see also `Rmd/`)
|   +-- BDAMLMs.R               # Analysis script with Bayesian multilevel models (reviewers might want 
|                               # to have more details; here's plenty of it)
+-- covid-19-recoding/          # Code for testing H1 and H2 and creating the histograms in the manuscript,
|                               # and export script to move all variable recoding if needed
+-- data/
|   +-- export_2020-04-16.csv   # All data in `CSV` format
+-- docs/
|   +-- index.html              # The html generated from the `Rmd` once knitted
+-- replication/                # 
|   +-- cleaning instructions/  # Instructions for how to clean/recode questionnaire data
    |   +-- clean.docx          # Cleaning instructions
    |   +-- recode.xlsx         # Recode instructions
    +-- Questionnaires/
        +-- *.pdf               # Questionnaire in 12 languages
+-- README.md                   # This file
+-- pandemic_programming.Rproj  # Open this in `RStudio` workspace to set working directory
```

## Where to start?

If you are interested in ...

* the code for our the main analysis in the paper then check `analysis/SEM.R`
* running the code then please have a look at `Rmd/` instead, where an `Rmd` file can be executed
* the `html` when running the `Rmd`, the file is published [here](https://torkar.github.io/pandemic_programming/)
* asking questions given a posterior probability distribution see `analysis/BDAMLMs.R`
* getting your hands on the data, please see the `data/` directory
* if you want to replicate this study, please see `replication/`
