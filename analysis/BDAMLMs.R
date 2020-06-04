library(brms)
library(data.table)
library(dplyr)
library(sjmisc)
options(mc.cores = parallel::detectCores()) # check num cores

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE, 
           na.strings=c("", "null", "NA", -99))

# cleanup some stuff
df <- data.frame(
  gender_b = ifelse(d$Gender == "Female", 1, 0), # code as binary
  
  country = d$Country,
  # covidstatus is Likert 0,..,5, we need to make it 1,..,6 (reg. for lavaan)
  # covidstatus_o = d$COVIDStatus + 1,
  covidstatus_o = d$COVIDStatus,
  isolation_o = d$Isolation, #ok
  exp_s = scale(d$YearsOfExperience), # scale var, i.e., (x -\bar{x})/sd(x)
  homeexp_s = scale(d$YearsOfWorkFromHomeExperience),
  fulltime_b = d$Fulltime, # binary already
  # edu_o = d$Education + 1, # ok
  edu_o = d$Education,
  orgsize_o = d$OrganizationSize, # already starts at 1
  # disab_o = d$Disabilities + 1,
  disab_o = d$Disabilities,
  adultcohab_o = d$AdultCohabitants + 1,
  childcohab_o = d$ChildCohabitants + 1,
  dev_b = d$RolesIncludeDeveloper, # binary already
  fear = d$FearResilience, # already 'basically' centered
  
  # Age is a tricky one. From the questionnaire it's binned into 11 categories,
  # so we can either say it's nominal, ordered, or scale it.
  # I would argue that it can't be nominal since there exists an order, so then
  # we need to decide between ordered categorical or scale it
  # Either way works, and for sampling purposes it might even be better to
  # scale it (if we conduct MCMC later).
  age_s = scale(d$Age),
  
  # Our outcomes (ordered categorical) seem to be ok
  # WHO5S[1:6]
  # WHO5B[1:6]
  # HPQB1[1:8]
  # HPQS1[1:8]
  # DP[1:5]
  # Erg[1:6]
  #
  # but we need to look at the differences reg. before/after corona. This
  # concerns our outcomes wellbeing and productivity only.
  # Make sure that the scale starts at 1 (req for lavaan)
  DeltaW1 = (d$WHO5S1 - d$WHO5B1) + 6,
  DeltaW2 = (d$WHO5S2 - d$WHO5B2) + 6,
  DeltaW3 = (d$WHO5S3 - d$WHO5B3) + 6,
  DeltaW4 = (d$WHO5S4 - d$WHO5B4) + 5,
  DeltaW5 = (d$WHO5S5 - d$WHO5B5) + 6,
  DeltaP1 = (d$HPQS1 - d$HPQB1 ) + 5,
  DeltaP2 = (d$HPQS2 - d$HPQB2) + 5,
  DeltaP3 = (d$HPQS3 - d$HPQB3) + 5,
  DeltaP4 = (d$HPQS4 - d$HPQB4) + 5,
  DeltaP5 = (d$HPQS5 - d$HPQB5) + 5,
  DeltaP6 = (d$HPQS6 - d$HPQB6) + 5,
  DeltaP7 = (d$HPQS7 - d$HPQB7) + 5,
  DeltaP8 = (d$HPQS8 - d$HPQB8) + 7,
  # now also add the other two outcome categories (all checked)
  DP1 = d$DP1,
  DP2 = d$DP2,
  DP3 = d$DP3,
  DP4 = d$DP4,
  DP5 = d$DP5,
  ERG1 = d$Erg1,
  ERG2 = d$Erg2,
  ERG3 = d$Erg3,
  ERG4 = d$Erg4,
  ERG5 = d$Erg5,
  ERG6 = d$Erg6
)

df <- df[complete.cases(df), ]

# Next make all our latent variables ordered
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
      "ERG1",
      "ERG2",
      "ERG3",
      "ERG4",
      "ERG5",
      "ERG6",
      "DP1",
      "DP2",
      "DP3",
      "DP4",
      "DP5")] <-
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
               "ERG1",
               "ERG2",
               "ERG3",
               "ERG4",
               "ERG5",
               "ERG6",
               "DP1",
               "DP2",
               "DP3",
               "DP4",
               "DP5")], ordered)



# DWell must have predictors DP, Fear and ERG 
# DPerf must have predictors  DP, Fear and ERG 
# Fear must have predictor DP
#
# DWell =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5
# DPerf =~ DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + 
#   DeltaP8
# DP    =~ DP1 + DP2 + DP3 + DP4 + DP5
# ERG   =~ ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + ERG6
# 
# DP    ~ covidstatus_o + edu_o + disab_o + adultcohab_o
# 
# ERG   ~ gender_b + homeexp_s + disab_o + adultcohab_o + childcohab_o
# 
# fear  ~ DP +
#   gender_b + covidstatus_o + isolation_o + edu_o + orgsize_o + 
#   disab_o + childcohab_o + dev_b
# 
# DWell ~ ERG + fear + DP + covidstatus_o + exp_s + age_s
# 
# DPerf ~ ERG + fear + DP + age_s + disab_o + adultcohab_o
# 
# DWell ~ DPerf

# We'll need 6 models (see immediately above)

# Use all predictors for fear ~ and any remaining in DP ~
bf_fear <- bf(fear ~ 1 + 
                mo(covidstatus_o) + mo(isolation_o) + mo(edu_o) + 
                mo(orgsize_o) + mo(disab_o) + mo(childcohab_o) + 
                mo(adultcohab_o) + (1 | dev_b) + (1 | country) + (1 | gender_b)
              )

p <- get_prior(bf_fear, data = df, family = skew_normal)

p$prior[1] <- "normal(0,4)"
p$prior[2] <- "normal(0,0.2)"
p$prior[10] <- "normal(0,2)"
p$prior[11] <- "weibull(2,1)"
p$prior[18] <- "weibull(2,1)"
p$prior[19] <- "dirichlet(2,2,2,2,2,2,2,2,2)" #adultlcohab
p$prior[c(20,23)] <- "dirichlet(2,2,2,2)" # childcohab, edu
p$prior[c(21,25)] <- "dirichlet(2,2,2,2,2)" #covidstatus, orgsize
p$prior[22] <- "dirichlet(2,2)" # disab
p$prior[24] <- "dirichlet(2,2,2)" #isolation

m_fear_prior <- brm(bf_fear, family = skew_normal, data = df, prior = p, 
                    chains = 1, sample_prior = "only")

pp_check(m_fear_prior, nsamples = 100) # so spread out between -10 and 10

m_fear <- brm(bf_fear, family = skew_normal, data = df, prior = p,
              control =  list(adapt_delta=0.97))
pp_check(m_fear_prior) # looks good
# Only covidstatus and disab sign on 89%

bf_dwell <- bf(mvbind(DeltaW1,DeltaW2,DeltaW3,DeltaW4,DeltaW5) ~ 
                 homeexp_s + mo(disab_o) + mo(adultcohab_o) + mo(childcohab_o) +
                 mo(covidstatus_o) + mo(isolation_o) + mo(edu_o) + 
                 mo(orgsize_o) + exp_s + age_s + 
                 (1| gender_b) + (1|dev_b) + (1 | country))

p <- get_prior(bf_dwell, data = df, family = cumulative)
p$prior[c(3,39,75,111,146)] <- "normal(0,0.15)"
p$prior[c(14,50,86,122,157)] <- "normal(0,3)"
p$prior[c(25,61,97,132,168)] <- "weibull(2,1)"
p$prior[c(32,68,104,139,175)] <- "dirichlet(2,2,2,2,2,2,2,2,2)"# adultcohab
p$prior[c(33,69,105,140,176)] <- "dirichlet(2,2,2,2)" #childcohab
p$prior[c(34,70,106,141,177)] <- "dirichlet(2,2,2,2,2)" # covidstatus
p$prior[c(35,71,107,142,178)] <- "dirichlet(2,2)" #disab
p$prior[c(36,72,108,143,179)] <- "dirichlet(2,2,2,2)" #edu
p$prior[c(37,73,109,144,180)] <- "dirichlet(2,2,2)" #isolation
p$prior[c(38,74,110,145,181)] <- "dirichlet(2,2,2,2,2)" #orgsize

m_dwell_prio <- brm(bf_dwell, family = cumulative, data = df, prior = p, 
                    chains = 1, sample_prior = "only")

pp_check(m_dwell_prio, resp = "DeltaW1", type = "bars", nsamples = 100) # all outcomes have good est

m_dwell <- brm(bf_dwell, family = cumulative, data = df, prior = p)

pp_check(m_dwell, resp = "DeltaW1", type = "bars", nsamples = 100)
summary(m_dwell)
# sign on 95%
# DeltaW2_age_s pos (see plots/)
# DeltaW5_age_s pos (see plots/)
# DeltaW5_covidstatus_o neg
# DeltaW4_childcohab_o neg
# DeltaW3_covidstatus_o neg
# Also, for country, sd(DeltaW3) = 0.41, compared to sd(DeltaW2) = 0.32

bf_dperf <- bf(mvbind(DeltaP1,DeltaP2,DeltaP3,DeltaP4,DeltaP5,DeltaP6,DeltaP8) ~ 
                 homeexp_s + mo(disab_o) + mo(adultcohab_o) + mo(childcohab_o) +
                 mo(covidstatus_o) + mo(isolation_o) + mo(edu_o) + mo(orgsize_o) + 
                 exp_s + age_s +
                 (1 | gender_b) + (1 | dev_b) + (1 | country))

p <- get_prior(bf_dperf, data = df, family = cumulative)

p$prior[c(3,36,70,104,138,172,206)] <- "normal(0,0.1)"
p$prior[c(14,47,81,115,149,183,217)] <- "normal(0,3)"
p$prior[c(22,56,90,124,158,192,228)] <- "weibull(2,1)"
p$prior[c(29,63,97,131,165,199,235)] <- "dirichlet(2,2,2,2,2,2,2,2,2)" # adultcohab
p$prior[c(30,64,98,132,166,200,236)] <- "dirichlet(2,2,2,2)" # childcohab
p$prior[c(31,65,99,133,167,201,237)] <- "dirichlet(2,2,2,2,2)" # covidstatus
p$prior[c(32,66,100,134,168,202,238)] <- "dirichlet(2,2)" # disab
p$prior[c(33,67,101,135,169,203,239)] <- "dirichlet(2,2,2,2)" # edu
p$prior[c(34,68,102,136,170,204,240)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(35,69,103,137,171,205,241)] <- "dirichlet(2,2,2,2,2)" # orgsize

m_dpref_prio <- brm(bf_dperf, data = df, family = cumulative, prior = p, 
                    chains = 1, sample_prior = "only")

pp_check(m_dpref_prio, resp = "DeltaP1", type = "bars")

m_dpref <- brm(bf_dperf, data = df, family = cumulative, prior = p)

# Posterior predictive checks. Go through DeptaP1,...DeltaP6, and P8, if needed
pp_check(m_dpref, resp = "DeltaP1", type = "bars") 
# It all looks good.
# sign effects
# DeltaP2_age_s  neg
# DeltaP3_moadultcohab_o neg
# DeltaP6_moadultcohab_o neg

# A DP model

bf_dp <- bf(mvbind(DP1,DP2,DP3,DP4,DP5) ~
              mo(covidstatus_o) + mo(edu_o) + mo(disab_o) + mo(adultcohab_o) +
              (1|g|gender_b) + (1|d|dev_b) + (1|c|country)
            )

# 111 parameters to estimate
p <- get_prior(bf_dp, data = df, family = cumulative)

p$prior[2] <- "lkj(2)"
p$prior[c(7,28,49,70,91)] <- "normal(0,0.5)"
p$prior[c(12,33,54,75,96)] <- "normal(0,5)"
p$prior[c(17,38,59,80,101)] <- "weibull(2,1)"
p$prior[c(24,45,66,87,108)] <- "dirichlet(2,2,2,2,2,2,2,2,2)" #adultcohab
p$prior[c(25,46,67,88,109)] <- "dirichlet(2,2,2,2,2)" # covidstatus
p$prior[c(26,47,68,89,110)] <- "dirichlet(2,2)" # disab
p$prior[c(27,48,69,90,111)] <- "dirichlet(2,2,2,2)" #edu

m_dp_prio <- brm(bf_dp, data = df, family = cumulative, sample_prior = "only", 
                 chains = 1, prior = p)
pp_check(m_dp_prio, resp = "DP1", type = "bars")

m_dp <- brm(bf_dp, data = df, family = cumulative, prior = p, 
            control = list(adapt_delta=0.95))
pp_check(m_dp, resp = "DP1", type = "bars")
# sign effect
# DP5_moedu_o neg
# DP4_mocovidstatus_o pos
# DP5_moadultcohab_o  pos

######
######

conditional_effects(m_dp, resp = "DP5", effects = c("edu_o", "adultcohab_o"), type = "bars", categorical = TRUE)
conditional_effects(m_dp, resp = "DP5", effects = c("covidstatus_o"), type = "bars")


