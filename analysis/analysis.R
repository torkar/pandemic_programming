library(brms)
library(data.table)
options(mc.cores = parallel::detectCores()) # check num cores
rstan_options(auto_write = TRUE) # only recompile models if neccessary

d <- fread("data/", header=TRUE, sep=",", quote="\"", 
           strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", 
           na.strings=c("", "null", -99), stringsAsFactors=FALSE)

# check all vars and make ordered factor if needed

# We have, as a first step, the following predictors
# age real
# cohabitants integer
# covid status 
# disability
# gender
# education
# org size
# social isolation

# let's start with a simple intercept null model M0
m0 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1,
          data = d, family = cumulative)