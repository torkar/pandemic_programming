library(brms)
library(data.table)
options(mc.cores = parallel::detectCores()) # check num cores
rstan_options(auto_write = TRUE) # only recompile models if neccessary

# Added by Sebastian
# d <- fread("../data/export_2020-04-16.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", "NA", -99), stringsAsFactors=FALSE)

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE)#, header=TRUE, sep=",", quote="\"", 
           #strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", 
           #na.strings=c("", "null", -99), stringsAsFactors=FALSE)

# check all vars and make ordered factor if needed

# We have, as a first step, the following predictors
# age real
# cohabitants integer
# covid status 
# disability 1/0
# gender 0-4
# education mo()
# org size mo()
# social isolation mo()

# let's start with a simple intercept null model M0
m0 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1,
          data = d, family = cumulative)

# Add varying intercept and check that we have a diff in out of sample pred
m1 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + (1 |p| Language),
          data = d, family = cumulative)

loo_compare(loo(m0), loo(m1))