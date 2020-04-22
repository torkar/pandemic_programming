library(brms)
library(data.table)
library(dplyr)
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
# disability 0,..,2
# gender 0-4
# education mo()
# org size mo()
# social isolation mo()

d$age_s <- scale(d$Age)
d$disabilities_c <- d$Disabilities + 1
d$covidstatus_c <- d$COVIDStatus + 1
d$education_c <- d$Education + 1

d$WHO5S1 <- d$WHO5S1 + 1
d$WHO5S2 <- d$WHO5S2 + 1
d$WHO5S3 <- d$WHO5S3 + 1
d$WHO5S4 <- d$WHO5S4 + 1
d$WHO5S5 <- d$WHO5S5 + 1


# let's start with a simple intercept null model M0
m0 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1,
          data = d, family = cumulative)

# Add varying intercept and check that we have a diff in out of sample pred
m1 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + (1 |p| Language),
          data = d, family = cumulative)

loo_compare(loo(m0), loo(m1))
# the carying intercept, according to language, makes a tremendous impact
#    elpd_diff se_diff
# m1   0.0       0.0  
# m0 -80.7      22.1

# i.e., 
# > -80.7 + c(-1,1) * 22.1 * 2.576
# [1] -137.6296  -23.7704
# in short, on the 99%-level (z-score 2.576) there's clearly a difference.

# 

p <- get_prior(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + mo(covidstatus_c) +
                 mo(disabilities_c) + Gender + mo(education_c) + mo(OrganizationSize) + 
                 mo(Isolation) + (1 |p| Language),
               data = d, family = cumulative)

p$prior[2] <- "lkj(2)"
p$prior[c(5,30,55,80,105)] <- "normal(0,1)"
p$prior[c(15,40,65,90,115)] <- "normal(0,5)"
p$prior[c(22,47,72,97,122)] <- "weibull(2,1)"
p$prior[c(25,50,75,100,125)] <- "dirichlet(2,2,2,2,2)" #covid status
p$prior[c(26,51,76,101,126)] <- "dirichlet(2,2)" # disabilities
p$prior[c(27,52,77,102,127)] <- "dirichlet(2,2,2,2)" # education
p$prior[c(28,53,78,103,128)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(29,54,79,104,129)] <- "dirichlet(2,2,2,2,2)" # org size

# We'll use a number of predictors now
m2 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + mo(covidstatus_c) +
            mo(disabilities_c) + Gender + mo(education_c) + mo(OrganizationSize) + 
            mo(Isolation) + (1 |p| Language),
          data = d, family = cumulative, prior = p)



