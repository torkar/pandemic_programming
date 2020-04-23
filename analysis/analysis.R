library(brms)
library(data.table)
library(dplyr)
options(mc.cores = parallel::detectCores()) # check num cores

# Added by Sebastian
# d <- fread("../data/export_2020-04-16.csv", header=TRUE, sep=",", quote="\"", 
# strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", 
# na.strings=c("", "null", "NA", -99), stringsAsFactors=FALSE)

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE, 
           na.strings=c("", "null", "NA", -99))
# cleanup some stuff
d$age_s <- scale(as.integer(d$Age))
d$disabilities_c <- as.integer(d$Disabilities)
d$covidstatus_c <- as.integer(d$COVIDStatus) +1
d$education_c <- as.integer(d$Education)
d$AdultCohabitants_s <- scale(as.numeric(d$AdultCohabitants))
d$ChildCohabitants_s <- scale(as.numeric(d$ChildCohabitants))
d$YearsOfExperience_s <- scale(as.numeric(d$YearsOfExperience))
d$YearsOfWorkFromHomeExperience_s <- scale(as.numeric(d$YearsOfWorkFromHomeExperience))
d$OrganizationSize_c <- as.numeric(d$OrganizationSize)

d[, 11:47] <- as.data.frame(sapply(d[, 11:47], as.numeric))

################################################################################
#
# cfa and sem
#
################################################################################
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
bcfa(model_cfa, data = d, n.chains = 4)#, dp=p_cfa)

# fit Structural Equation Model (SEM) with uncertainty propagated
# set sane priors
p_mlm <- dpriors(model_mlm)
p_mlm[c(1,2,3,4)] <- "normal(0,5)"

bsem(model_mlm, data = d, test = "none", n.chains = 4, dp = p_mlm) 

################################################################################
#
# MLM models á la Bayes
#
################################################################################
# Start with WHO5S*

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
# m0 -80.4      22.1

# i.e., 
# > -80.4 + c(-1,1) * 22.1 * 2.576
# [1] -137.3296  -23.4704
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
p$prior[c(27,53,81,109,137)] <- "weibull(2,1)"
p$prior[c(28,56,84,112,140)] <- "dirichlet(2,2,2,2,2)" #covid status
p$prior[c(29,57,85,113,141)] <- "dirichlet(2,2)" # disabilities
p$prior[c(30,58,86,114,142)] <- "dirichlet(2,2,2,2)" # education
p$prior[c(31,59,87,115,143)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(32,60,88,116,144)] <- "dirichlet(2,2,2,2,2)" # org size
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
################################################################################
#
# DP model
#
################################################################################
# For now, we want to look into the following additional concepts
# (Outcome ~ predictors):
#
# Disaster Preparedness ~ Age, Disabilities, Education, Country, Gender   
#
# Our outcomes for DP are DP1,..,DP5 which are all Likert scale 1,..,5, i.e.,
# indicating that a cumulative or acat likelihood should be used.

bform <- bf(mvbind(DP1,DP2,DP3,DP4,DP5) ~ 1 + age_s + mo(disabilities_c) + 
               mo(education_c) + (1 |c| Country) + (1 |g| Gender))

p_DP <- get_prior(bform,
                  data = d, family = cumulative)

p_DP$prior[2] <- "lkj(2)"
p_DP$prior[c(6,22,38,54,70)] <- "normal(0,1)"
p_DP$prior[c(10,26,42,58,74)] <- "normal(0,5)"
p_DP$prior[c(15,31,47,63,79)] <- "weibull(2,1)"
p_DP$prior[c(20,36,52,68,84)] <- "dirichlet(2,2)"
p_DP$prior[c(21,37,53,69,85)] <- "dirichlet(2,2,2,2)"

m_DP <- brm(bform, data = d, family = cumulative, prior = p_DP)

################################################################################
#
# Fear model
#
################################################################################
# Fear ~ Disaster Preparedness, Isolation, AdultCoinhabitants, 
# ChildCoinhabitants, COVIDStatus, Country, Disabilities, Education, Age

################################################################################
#
# Ergonomics model
#
################################################################################
# Ergonomics ~ Disaster Preparedness,  Age, Disabilities, Education, Country, 
# Gender,  Isolation, AdultCoinhabitants, ChildCoinhabitants, COVIDStatus,

bform <- bf(mvbind(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6) ~ 1 + mo(DP1) + mo(DP2) + 
              mo(DP3) + mo(DP4) + mo(DP5) + age_s + mo(disabilities_c) + 
              mo(education_c) +  mo(Isolation) + AdultCohabitants_s + 
              ChildCohabitants_s + mo(covidstatus_c) + (1 |c| Country) + (1 |g| Gender)) 

p_ERG <- get_prior(bform, data = d, family = cumulative)
p_ERG$prior[c(2)] <- "lkj(2)"
p_ERG$prior[c(6,39,72,105,138,171)] <- "normal(0,1)"
p_ERG$prior[c(19,52,85,118,151,184)] <- "normal(0,5)"
p_ERG$prior[c(25,58,91,124,157,190)] <- "weibull(2,1)"
p_ERG$prior[c(30,63,96,129,162,195)] <- "dirichlet(2,2,2,2,2)" #covid
p_ERG$prior[c(31,64,97,130,163,196)] <- "dirichlet(2,2,2)" # disabilities
p_ERG$prior[c(32:36,65:69,98:102,131:135,164:168,197:201)] <- "dirichlet(2,2,2,2,2)" #DP
p_ERG$prior[c(37,70,103,136,169,202)] <- "dirichlet(2,2,2,2,2)" #edu
p_ERG$prior[c(38,71,104,137,170,203)] <- "dirichlet(2,2,2)" # isolation

m_ERG <- brm(bform, data = d, family = cumulative, prior = p_ERG)

