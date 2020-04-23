library(brms)
library(data.table)
library(dplyr)
options(mc.cores = parallel::detectCores()) # check num cores

# Added by Sebastian
# d <- fread("../data/export_2020-04-16.csv", header=TRUE, sep=",", quote="\"", 
# strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", 
# na.strings=c("", "null", "NA", -99), stringsAsFactors=FALSE)

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE, 
           na.strings=c("", "null", -99))

### cfa and sem

# a Confirmatory Factor Analysis (CFA) model
model_cfa <- '  WHO5S_lat =~ WHO5S1 + WHO5S2 + WHO5S3 + WHO5S4 + WHO5S5
  PERFS_lat =~ HPQS1 + HPQS2 + HPQS3 + HPQS4 + HPQS5 + HPQS6 + HPQS7 + HPQS8 + PerS1item
  WHO5B_lat =~ WHO5B1 + WHO5B2 + WHO5B3 + WHO5B4 + WHO5B5
  PERFB_lat =~ HPQB1 + HPQB2 + HPQB3 + HPQB4 + HPQB5 + HPQB6 + HPQB7 + HPQB8 + PerB1item
  Erg_lat =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6            
  DP_lat =~ DP1 + DP2 + DP3 + DP4 + DP5
'

# a Structural Equation Model (SEM)
model_sem <- '# measurement model
  WHO5S_lat =~ WHO5S1 + WHO5S2 + WHO5S3 + WHO5S4 + WHO5S5
  PERFS_lat =~ HPQS1 + HPQS2 + HPQS3 + HPQS4 + HPQS5 + HPQS6 + HPQS7 + HPQS8 + PerS1item
  WHO5B_lat =~ WHO5B1 + WHO5B2 + WHO5B3 + WHO5B4 + WHO5B5
  PERFB_lat =~ HPQB1 + HPQB2 + HPQB3 + HPQB4 + HPQB5 + HPQB6 + HPQB7 + HPQB8 + PerB1item
  Erg_lat =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6            
  DP_lat =~ DP1 + DP2 + DP3 + DP4 + DP5             
  
  # structural model (regressions)  
  DP_lat ~ Age + Disabilities + Education
  FearResilience ~ DP_lat + Isolation + AdultCohabitants + ChildCohabitants + COVIDStatus + Disabilities + Education + Age
  Erg_lat ~ DP_lat + Age + Disabilities + Education + Isolation + AdultCohabitants + ChildCohabitants + COVIDStatus + YearsOfWorkFromHomeExperience
  WHO5S_lat ~ WHO5B_lat + Erg_lat + DP_lat + FearResilience  + COVIDStatus + AdultCohabitants + Disabilities + Education
  PERFS_lat ~ PERFB_lat + Erg_lat + DP_lat + FearResilience + YearsOfWorkFromHomeExperience + ChildCohabitants + Disabilities + Education
'

# fit Confirmatory Factor Analysis (CFA) with uncertainty propagated
bcfa(model_cfa, data=d)

# fit Structural Equation Model (SEM) with uncertainty propagated
bsem(model_mlm, data=d, sample=500, test="none") 

################################################################################
#
# MLM models á la Bayes
#
################################################################################
# We have, as a first step, the following predictors
d$age_s <- scale(as.integer(d$Age))
d$disabilities_c <- as.integer(d$Disabilities)
d$covidstatus_c <- as.integer(d$COVIDStatus) +1
d$education_c <- as.integer(d$Education)
d$AdultCohabitants_s <- scale(as.numeric(d$AdultCohabitants))
d$ChildCohabitants_s <- scale(as.numeric(d$ChildCohabitants))
d$YearsOfExperience_s <- scale(as.numeric(d$YearsOfExperience))
d$YearsOfWorkFromHomeExperience_s <- scale(as.numeric(d$YearsOfWorkFromHomeExperience))
d$OrganizationSize_c <- as.numeric(d$OrganizationSize)

# let's start with modelling WHO5S*, i.e., the following outcomes
d$WHO5S1 <- as.numeric(d$WHO5S1)
d$WHO5S2 <- as.numeric(d$WHO5S2)
d$WHO5S3 <- as.numeric(d$WHO5S3)
d$WHO5S4 <- as.numeric(d$WHO5S4)
d$WHO5S5 <- as.numeric(d$WHO5S5)

# a simple intercept null model M0
m0 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1,
          data = d, family = cumulative)

# add varying intercept and check that we have a diff in out of sample pred
m1 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + (1 |p| Language),
          data = d, family = cumulative)

loo_compare(loo(m0), loo(m1))
# the varying intercept, according to language, makes a tremendous impact
#    elpd_diff se_diff
# m1   0.0       0.0  
# m0 -80.7      22.1

# i.e., 
# > -80.7 + c(-1,1) * 22.1 * 2.576
# [1] -137.6296  -23.7704
# in short, on the 99%-level (z-score 2.576) there's clearly a difference.

# prior predictive checks
# pull default priors for our model
p <- get_prior(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + Gender + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Language),
               data = d, family = cumulative)

# set our own sane priors
p$prior[2] <- "lkj(2)" # broad correlation matrix prior
p$prior[c(5,35,65,95,125)] <- "normal(0,1)"
p$prior[c(20,50,80,110,140)] <- "normal(0,5)"
p$prior[c(27,57,87,117,147)] <- "weibull(2,1)"
p$prior[c(30,60,90,120,150)] <- "dirichlet(2,2,2,2,2)" #covid status
p$prior[c(31,61,91,121,151)] <- "dirichlet(2,2)" # disabilities
p$prior[c(32,62,92,122,152)] <- "dirichlet(2,2,2,2)" # education
p$prior[c(33,63,93,123,153)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(34,64,94,124,154)] <- "dirichlet(2,2,2,2,2)" # org size
# The priors above results in a nearly uniform prior on the outcome scale.
# Run the below model with sample_prior="only" and then use, e.g., 
# pp_check(model, resp = "WHO5S1", type="hist") etc. to check this

# Set up our model using WHO5S* as outcomes and add predictors
m2 <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + 
            mo(covidstatus_c) + mo(disabilities_c) + Gender + mo(education_c) + 
            mo(OrganizationSize_c) + mo(Isolation) + AdultCohabitants_s + 
            ChildCohabitants_s + YearsOfExperience_s + 
            YearsOfWorkFromHomeExperience_s + (1 |p| Language),
          data = d, family = cumulative, prior = p)
# diagnostics \widehat{R} < 1.01, neff > 0.1, and traceplots all look good
#
# so we have a good basic model, let's see later if adding more predictors will
# allow us to get better out of sample predictions
#
# For now, we want to look into the following additional concepts
# (Outcome ~ predictors):
#
# Disaster Preparedness ~ Age, Disabilities, Education, Country, Gender   
# 
# Fear - Disaster Preparedness, Isolation, AdultCoinhabitants, 
# ChildCoinhabitants, COVIDStatus, Country, Disabilities, Education, Age
# 
# Ergonomics - Disaster Preparedness,  Age, Disabilities, Education, Country, 
# Gender,  Isolation, AdultCoinhabitants, ChildCoinhabitants, COVIDStatus, 

# We design three models DP, F, and E, and conduct inferences
# Our outcomes for DP are DP1,..,DP5 which are all Likert scale 1,..,5, i.e.,
# indicating that a cumulative or acat likelihood should be used.

bform <- bf(mvbind(DP1,DP2,DP3,DP4,DP5) ~ 1 + age_s + mo(disabilities_c) + 
               mo(education_c) + Country + Gender)

p_DP <- get_prior(bform,
                  data = d, family = cumulative)

p_DP$prior[c(3,72,141,210,279)] <- "normal(0,1)"
p_DP$prior[c(65,134,203,272,341)] <- "weibull(2,1)"
p_DP$prior[c(70,139,208,277,346)] <- "dirichlet(2,2)"
p_DP$prior[c(71,140,209,278,347)] <- "dirichlet(2,2,2,2)"

m_DP <- brm(bform, data = d, family = cumulative, prior = p)

