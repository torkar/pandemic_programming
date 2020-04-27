library(blavaan)
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
d$disabilities_c <- as.integer(d$Disabilities) + 1
d$covidstatus_c <- as.integer(d$COVIDStatus) + 1
d$education_c <- as.integer(d$Education) + 1
d$AdultCohabitants_s <- scale(as.numeric(d$AdultCohabitants) + 1)
d$ChildCohabitants_s <- scale(as.numeric(d$ChildCohabitants) + 1)
d$YearsOfExperience_s <- scale(as.numeric(d$YearsOfExperience)) #actually months
d$YearsOfWorkFromHomeExperience_s <- scale(as.numeric(d$YearsOfWorkFromHomeExperience))
d$OrganizationSize_c <- as.numeric(d$OrganizationSize)

d[, 11:47] <- as.data.frame(sapply(d[, 11:47], as.numeric))

################################################################################
#
# cfa and sem
#
################################################################################
# data shuffling for the SEM
df <- data.frame(DeltaW1 = d$WHO5S1-d$WHO5B1 + 6)
df$DeltaW2 <- d$WHO5S2-d$WHO5B2 + 6
df$DeltaW3 <- d$WHO5S3-d$WHO5B3 + 6
df$DeltaW4 <- d$WHO5S4-d$WHO5B4 + 6 
df$DeltaW5 <- d$WHO5S5-d$WHO5B5 + 6
df$DeltaP1 <- d$HPQS1-d$HPQB1 +5
df$DeltaP2 <- d$HPQS2-d$HPQB2 +5
df$DeltaP3 <- d$HPQS3-d$HPQB3 +5
df$DeltaP4 <- d$HPQS4-d$HPQB4 +5
df$DeltaP5 <- d$HPQS5-d$HPQB5 +5
df$DeltaP6 <- d$HPQS6-d$HPQB6 +5
df$DeltaP7 <- d$HPQS7-d$HPQB7 +5
df$DeltaP8 <- d$HPQS8-d$HPQB8 +7
df$Erg1 <- d$Erg1
df$Erg2 <- d$Erg2
df$Erg3 <- d$Erg3
df$Erg4 <- d$Erg4
df$Erg5 <- d$Erg5
df$Erg6 <- d$Erg6
df$DP1 <- d$DP1
df$DP2 <- d$DP2
df$DP3 <- d$DP3
df$DP4 <- d$DP4
df$DP5 <- d$DP5
df$disabilities_c <- d$disabilities_c
df$education_c <- d$education_c
df$isolation_c <- d$Isolation
df$covidstatus_c <- d$covidstatus_c
df$fear_s <- d$FearResilience
df$country <- d$Country
# continuous 
df$age_s <- d$age_s
df$adultcohab_s <- d$AdultCohabitants_s
df$childcohab_s <- d$ChildCohabitants_s
df$yearsofworkfromhomeexp_s <- d$YearsOfWorkFromHomeExperience_s

df <- df[complete.cases(df),]

df[,c("DeltaW1",
      "DeltaW2",
      "DeltaW3",
      "DeltaW4",
      "DeltaW5",
      "DeltaP1",
      "DeltaP2",
      "DeltaP3",
      "DeltaP4",
      "DeltaP5",
      "DeltaP6",
      "DeltaP7",
      "DeltaP8",
      "Erg1",
      "Erg2",
      "Erg3",
      "Erg4",
      "Erg5",
      "Erg6",
      "DP1",
      "DP2",
      "DP3",
      "DP4",
      "DP5",
      "disabilities_c",
      "education_c",
      "isolation_c",
      "covidstatus_c")] <-
  lapply(df[,c("DeltaW1",
               "DeltaW2",
               "DeltaW3",
               "DeltaW4",
               "DeltaW5",
               "DeltaP1",
               "DeltaP2",
               "DeltaP3",
               "DeltaP4",
               "DeltaP5",
               "DeltaP6",
               "DeltaP7",
               "DeltaP8",
               "Erg1",
               "Erg2",
               "Erg3",
               "Erg4",
               "Erg5",
               "Erg6",
               "DP1",
               "DP2",
               "DP3",
               "DP4",
               "DP5",
               "disabilities_c",
               "education_c",
               "isolation_c",
               "covidstatus_c")], ordered)

model_cfa <- '
  DeltaWellbeing =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
  DeltaPerformance =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5 + DeltaP6 + DeltaP8
  Erg =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
  DP =~ DP1 + DP2 + DP3 + DP4 + DP5
'

model_sem <- '# measurement model
  DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
  DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
  Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
  DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5

  # structural model (regressions)
  DP_l ~ age_s + disabilities_c + education_c
  fear_s ~ DP_l + isolation_c + adultcohab_s + childcohab_s + covidstatus_c + disabilities_c + education_c + age_s
  Erg_l ~ DP_l + age_s + disabilities_c + education_c + isolation_c + adultcohab_s + childcohab_s + covidstatus_c + yearsofworkfromhomeexp_s
  DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c + education_c
  DeltaPerformance_l ~ Erg_l + DP_l + fear_s + yearsofworkfromhomeexp_s + childcohab_s + disabilities_c + education_c
'
################################################################################
#
# Use lavaan w/o uncertainty propagation
#
################################################################################
l_cfa <- cfa(model_cfa, data = df)
summary(l_cfa)

l_sem <- sem(model_sem, data = df)
summary(l_sem)

################################################################################
#
# MLM models á la Bayes. Start with WHO5S
#
################################################################################

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

# The priors above result in a nearly uniform prior on the outcome scale.
# Run the below model with sample_prior="only" and then use, e.g., 
# pp_check(model, resp = "WHO5S1", type="hist") etc. to check this

# Set up our model using WHO5S* as outcomes and add predictors
m_WHO5S <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + Gender + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Language),
               data = d, family = cumulative, prior = p)
# diagnostics \widehat{R} < 1.01, neff > 0.1, and traceplots all look good
#
# so we have a good basic model, let's see later if adding more predictors will
# allow us to get better out of sample predictions
################################################################################
#
# WHO5B model
#
################################################################################

p <- get_prior(mvbind(WHO5B1, WHO5B2, WHO5B3, WHO5B4, WHO5B5) ~ 1 + age_s + 
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
# pp_check(model, resp = "WHO5B1", type="hist") etc. to check this

# Set up our model using WHO5B* as outcomes and add predictors
m_WHO5B <- brm(mvbind(WHO5B1, WHO5B2, WHO5B3, WHO5B4, WHO5B5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + Gender + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Language),
               data = d, family = cumulative, prior = p)

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

p_DP <- get_prior(bform, data = d, family = cumulative)

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

bform <- bf(fear_s ~ 1 + mo(DP1) + mo(DP2) + mo(DP3) + mo(DP4) + mo(DP5) + 
              mo(isolation_c) + childcohab_s + mo(covidstatus_c) + 
              mo(disabilities_c) + mo(education_c) + age_s +
              (1 | country))

p_DP <- get_prior(bform, data = df)

p_DP$prior[1] <- "normal(0,2)"
p_DP$prior[13] <- "normal(0,5)"
p_DP$prior[c(14,17)] <- "weibull(2,1)"
p_DP$prior[18] <- "dirichlet(2,2,2,2,2)"
p_DP$prior[19] <- "dirichlet(2,2)"
p_DP$prior[20:25] <- "dirichlet(2,2,2,2)"
p_DP$prior[26] <- "dirichlet(2,2,2)"

m_fear <- brm(bform, data = df, prior = p_DP, sample_prior = "only")
pp_check(m_fear, type = "dens_overlay", nsamples = 100) 
# Clearly our priors are spread out and nearly uniform on the outcome, i.e.,
# we are a bit sceptical towards extreme values but that's ok.

m_fear <- brm(bform, data = df, prior = p_DP, control = list(adapt_delta=0.95))

################################################################################
#
# Ergonomics model
#
################################################################################
bform <- bf(mvbind(Erg1, Erg2, Erg3, Erg4, Erg5, Erg6) ~ 1 + mo(DP1) + mo(DP2) + 
              mo(DP3) + mo(DP4) + mo(DP5) + age_s + mo(disabilities_c) + 
              mo(education_c) +  mo(Isolation) + AdultCohabitants_s + 
              ChildCohabitants_s + mo(covidstatus_c) + (1 |c| Country) + 
              (1 |g| Gender)) 

p_ERG <- get_prior(bform, data = d, family = cumulative)
p_ERG$prior[c(2)] <- "lkj(2)"
p_ERG$prior[c(6,39,72,105,138,171)] <- "normal(0,1)"
p_ERG$prior[c(19,52,85,118,151,184)] <- "normal(0,5)"
p_ERG$prior[c(25,58,91,124,157,190)] <- "weibull(2,1)"
p_ERG$prior[c(30,63,96,129,162,195)] <- "dirichlet(2,2,2,2,2)" #covid
p_ERG$prior[c(31,64,97,130,163,196)] <- "dirichlet(2,2)" # disabilities
p_ERG$prior[c(32:36,65:69,98:102,131:135,164:168,197:201)] <- "dirichlet(2,2,2,2)" #DP
p_ERG$prior[c(37,70,103,136,169,202)] <- "dirichlet(2,2,2,2)" #edu
p_ERG$prior[c(38,71,104,137,170,203)] <- "dirichlet(2,2,2)" # isolation

m_ERG <- brm(bform, data = d, family = cumulative, prior = p_ERG, 
             control = list(adapt_delta=.95))

################################################################################
#
# TODO: Productivity models 
#
################################################################################
# We could build one large model, but in this case let's split it into two
# since then we can use the posterior to look at the differences before/after

# This is what Paul proposes, but this is a latent model which we need
# to translate into Stan-ish language
# HPQS ~ HPQB+DP+ERG+FearResilience+WHO5S+RemoteExperience
# HPQB ~ age_s + gender_s + education_c + OrganizationSize_c + Disabilities_c + 
#   YearsOfExperience_c + Fulltime
