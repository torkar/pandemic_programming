library(brms)
library(data.table)
library(dplyr)
library(sjmisc)
options(mc.cores = parallel::detectCores()) # check num cores

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE, 
           na.strings=c("", "null", "NA", -99))
# cleanup some stuff
d$age_s <- scale(as.integer(d$Age))
d$AdultCohabitants_s <- scale(as.numeric(d$AdultCohabitants) + 1)
d$ChildCohabitants_s <- scale(as.numeric(d$ChildCohabitants) + 1)
d$YearsOfExperience_s <- scale(as.numeric(d$YearsOfExperience)) #actually months
d$YearsOfWorkFromHomeExperience_s <- scale(as.numeric(d$YearsOfWorkFromHomeExperience))
d$OrganizationSize_c <- as.numeric(d$OrganizationSize)
d$fear_s <- d$FearResilience

d[, 11:47] <- as.data.frame(sapply(d[, 11:47], as.numeric))


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
# So we shoul dmake use of varying intercepts for sure, and Country and Gender
# are good candidates (Language can partly be explained by Country and we 
# avoid strong collinearity this way).

# prior predictive checks
# pull default priors for our model
p <- get_prior(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Country) +
                 (1 |g| Gender),
               data = d, family = cumulative)

# set our own sane priors
p$prior[2] <- "lkj(2)" # broad correlation matrix prior
p$prior[c(6,33,60,87,114)] <- "normal(0,1)"
p$prior[c(17,44,71,98,125)] <- "normal(0,5)"
p$prior[c(23,50,77,104,131)] <- "weibull(2,1)"
p$prior[c(28,55,82,109,136)] <- "dirichlet(2,2,2,2,2)" #covid status
p$prior[c(29,56,83,110,137)] <- "dirichlet(2,2)" # disabilities
p$prior[c(30,57,84,111,138)] <- "dirichlet(2,2,2,2)" # education
p$prior[c(31,58,85,112,139)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(32,59,86,113,140)] <- "dirichlet(2,2,2,2,2)" # org size

# The priors above result in a nearly uniform prior on the outcome scale.
# Run the below model with sample_prior="only" and then use, e.g., 
# pp_check(model, resp = "WHO5S1", type="hist") etc. to check this

# Set up our model using WHO5S* as outcomes and add predictors
m_WHO5S <- brm(mvbind(WHO5S1, WHO5S2, WHO5S3, WHO5S4, WHO5S5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Country) + 
                 (1 |g| Gender),
               data = d, family = cumulative, prior = p, 
               control = list(adapt_delta=0.95))
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
                 mo(covidstatus_c) + mo(disabilities_c) +
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Country) + 
                 (1 |g| Gender),
               data = d, family = cumulative)

# set our own sane priors
p$prior[2] <- "lkj(2)" # broad correlation matrix prior
p$prior[c(6,33,60,87,114)] <- "normal(0,1)"
p$prior[c(17,44,71,98,125)] <- "normal(0,5)"
p$prior[c(23,50,77,104,131)] <- "weibull(2,1)"
p$prior[c(28,55,82,109,136)] <- "dirichlet(2,2,2,2,2)" #covid status
p$prior[c(29,56,83,110,137)] <- "dirichlet(2,2)" # disabilities
p$prior[c(30,57,84,111,138)] <- "dirichlet(2,2,2,2)" # education
p$prior[c(31,58,85,112,139)] <- "dirichlet(2,2,2)" # isolation
p$prior[c(32,59,86,113,140)] <- "dirichlet(2,2,2,2,2)" # org size
# The priors above results in a nearly uniform prior on the outcome scale.
# Run the below model with sample_prior="only" and then use, e.g., 
# pp_check(model, resp = "WHO5B1", type="hist") etc. to check this

# Set up our model using WHO5B* as outcomes and add predictors
m_WHO5B <- brm(mvbind(WHO5B1, WHO5B2, WHO5B3, WHO5B4, WHO5B5) ~ 1 + age_s + 
                 mo(covidstatus_c) + mo(disabilities_c) + 
                 mo(education_c) + mo(OrganizationSize_c) + mo(Isolation) + 
                 AdultCohabitants_s + ChildCohabitants_s + YearsOfExperience_s + 
                 YearsOfWorkFromHomeExperience_s + (1 |p| Country) + 
                 (1 |g| Gender),
               data = d, family = cumulative, prior = p, 
               control = list(adapt_delta=.95))

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

m_DP <- brm(bform, data = d, family = cumulative, prior = p_DP, 
            control = list(adapt_delta=0.95))

################################################################################
#
# Fear model
#
################################################################################
# TODO: Check execution below since formulas changed 2020-04-28
bform <- bf(fear_s ~ 1 + mo(DP1) + mo(DP2) + mo(DP3) + mo(DP4) + mo(DP5) + 
              mo(Isolation) + ChildCohabitants_s + mo(covidstatus_c) + 
              mo(disabilities_c) + mo(education_c) + age_s +
              (1 | Country) + (1 | Gender))

p_DP <- get_prior(bform, data = d)

p_DP$prior[1] <- "normal(0,2)"
p_DP$prior[13] <- "normal(0,5)"
p_DP$prior[c(14,19)] <- "weibull(2,1)"
p_DP$prior[20] <- "dirichlet(2,2,2,2,2)" # covid
p_DP$prior[21] <- "dirichlet(2,2)" # disabilities
p_DP$prior[22:27] <- "dirichlet(2,2,2,2)" # education and DP
p_DP$prior[28] <- "dirichlet(2,2,2)" #isolation

m_fear <- brm(bform, data = d, prior = p_DP, sample_prior = "only")
pp_check(m_fear, type = "dens_overlay", nsamples = 100) 
# Clearly our priors are spread out and nearly uniform on the outcome, i.e.,
# we are a bit sceptical towards extreme values but that's ok.

m_fear <- brm(bform, data = d, prior = p_DP, control = list(adapt_delta=0.99))

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

# # A simple model
# bform <- bf(mvbind(HPQS1,HPQS2,HPQS3,HPQS4,HPQS5,HPQS6,HPQS7,HPQS8) ~ 1 + 
#               fear_s + YearsOfWorkFromHomeExperience_s + age_s + Gender +
#               mo(education_c) + mo(OrganizationSize_c) + mo(disabilities_c) +
#               YearsOfExperience_s + Fulltime)
# 
# # add varying intercept on Gender and Country
# bform_vi <- bf(mvbind(HPQS1,HPQS2,HPQS3,HPQS4,HPQS5,HPQS6,HPQS7,HPQS8) ~ 1 + 
#                  fear_s + YearsOfWorkFromHomeExperience_s + age_s +
#                  mo(education_c) + mo(OrganizationSize_c) + mo(disabilities_c) +
#                  YearsOfExperience_s + Fulltime + (1 | Gender) + (1 | Country))
# 
# foo <- brm(bform, data = d, family = cumulative)
# bar <- brm(bform_vi, data = d, family = cumulative)
# 
# loo_compare(loo(foo), loo(bar))


# ################################################################################
# #
# # Junk
# #
# ################################################################################
# ################################################################################
# #
# # cfa and sem
# #
# ################################################################################
# # data shuffling for the SEM
# df <- data.frame(DeltaW1 = d$WHO5S1-d$WHO5B1 + 6)
# df$DeltaW2 <- d$WHO5S2-d$WHO5B2 + 6
# df$DeltaW3 <- d$WHO5S3-d$WHO5B3 + 6
# df$DeltaW4 <- d$WHO5S4-d$WHO5B4 + 6 
# df$DeltaW5 <- d$WHO5S5-d$WHO5B5 + 6
# df$DeltaP1 <- d$HPQS1-d$HPQB1 +5
# df$DeltaP2 <- d$HPQS2-d$HPQB2 +5
# df$DeltaP3 <- d$HPQS3-d$HPQB3 +5
# df$DeltaP4 <- d$HPQS4-d$HPQB4 +5
# df$DeltaP5 <- d$HPQS5-d$HPQB5 +5
# df$DeltaP6 <- d$HPQS6-d$HPQB6 +5
# df$DeltaP7 <- d$HPQS7-d$HPQB7 +5
# df$DeltaP8 <- d$HPQS8-d$HPQB8 +7
# df$Erg1 <- d$Erg1
# df$Erg2 <- d$Erg2
# df$Erg3 <- d$Erg3
# df$Erg4 <- d$Erg4
# df$Erg5 <- d$Erg5
# df$Erg6 <- d$Erg6
# df$DP1 <- d$DP1
# df$DP2 <- d$DP2
# df$DP3 <- d$DP3
# df$DP4 <- d$DP4
# df$DP5 <- d$DP5
# df$disabilities_c <- d$disabilities_c
# df$education_c <- d$education_c
# df$isolation_c <- d$Isolation
# df$covidstatus_c <- d$covidstatus_c
# df$fear_s <- d$FearResilience
# df$country <- d$Country # droplevels() later if cutting data
# df$gender <- d$Gender
# # continuous 
# df$age_s <- d$age_s
# df$adultcohab_s <- d$AdultCohabitants_s
# df$childcohab_s <- d$ChildCohabitants_s
# df$yearsofworkfromhomeexp_s <- d$YearsOfWorkFromHomeExperience_s
# 
# df <- df[complete.cases(df),]
# 
# df$country <- droplevels(df$country)
# 
# df[,c("DeltaW1",
#       "DeltaW2",
#       "DeltaW3",
#       "DeltaW4",
#       "DeltaW5",
#       "DeltaP1",
#       "DeltaP2",
#       "DeltaP3",
#       "DeltaP4",
#       "DeltaP5",
#       "DeltaP6",
#       "DeltaP7",
#       "DeltaP8",
#       "Erg1",
#       "Erg2",
#       "Erg3",
#       "Erg4",
#       "Erg5",
#       "Erg6",
#       "DP1",
#       "DP2",
#       "DP3",
#       "DP4",
#       "DP5")] <-
#   lapply(df[,c("DeltaW1",
#                "DeltaW2",
#                "DeltaW3",
#                "DeltaW4",
#                "DeltaW5",
#                "DeltaP1",
#                "DeltaP2",
#                "DeltaP3",
#                "DeltaP4",
#                "DeltaP5",
#                "DeltaP6",
#                "DeltaP7",
#                "DeltaP8",
#                "Erg1",
#                "Erg2",
#                "Erg3",
#                "Erg4",
#                "Erg5",
#                "Erg6",
#                "DP1",
#                "DP2",
#                "DP3",
#                "DP4",
#                "DP5")], ordered)
# 
# ################################################################################
# #
# # SEM
# #
# ################################################################################
# 
# # Unfortunately this does not converge if we're using >2 genders
# # Let's focus on male/female since that's the majority of our sample, i.e., 
# # where our variability exists (18 cases removed)
# 
# # For now, pick only F and M
# df <- df[df$gender == "Male" | df$gender == "Female",]
# 
# # code female as 1
# df$genderF <- ifelse(df$gender == "Female", 1, 0) # Female == 1, Male == 0
# 
# model_cfa <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5'
# 
# l_cfa <- cfa(model_cfa, data = df, missing = "pairwise")
# summary(l_cfa, fit.measures=TRUE)
# 
# model_sem <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5
# 
#   # structural model (regressions)
#   DP_l ~ disabilities_c + education_c + genderF
#   fear_s ~ DP_l + isolation_c + covidstatus_c + disabilities_c + genderF
#   Erg_l ~ DP_l + age_s + childcohab_s + yearsofworkfromhomeexp_s + genderF
#   DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c + genderF
#   DeltaPerformance_l ~ Erg_l + genderF
#   DeltaWellbeing_l ~ DeltaPerformance_l 
# '
# 
# l_sem <- sem(model_sem, data = df)
# summary(l_sem, fit.measures = TRUE)
#
# # Add countries. We pick only the top-6 countries.
# sort(prop.table(table(df$country)))
# # If we pick top-6, i.e., countries with >1% of the sample size, while still
# # having enough variance, we get,
# # South Korea, United States, Italy, Brazil, Russia, and Germany
# 
# # Pick only rows with the top-6 countries (1223 observations)
# df <- df[df$country == "Korea, South" | df$country == "United States" | 
#            df$country == "Italy" | df$country == "Brazil" | 
#            df$country == "Russia" | df$country == "Germany",]
# 
# # make sure we drop levels otherwise R won't follow
# df$country <- droplevels(df$country) 
# df$DeltaW3 <- droplevels(df$DeltaW3) # 11 disappeared so remove it
# 
# # Create K-1 categories among the top-6 countries
# res <- model.matrix(~country, data = df)[, -1] # Brazil is the intercept
# 
# # We go from 1,..,5 starting with Germany (Brazil is intercept)
# colnames(res) <- paste("C", 1:5, sep="") # C1,..,C15 as column names
# 
# # Add the matrix to our df
# df <- cbind(df, res)
# 
# model_sem <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5
# 
#   # structural model (regressions)
#   DP_l ~ disabilities_c + education_c + genderF + C1 + C2 + C3 + C4 + C5
#   fear_s ~ DP_l + isolation_c + covidstatus_c + disabilities_c + genderF + C1 + C2 + C3 + C4 + C5
#   Erg_l ~ DP_l + age_s + childcohab_s + yearsofworkfromhomeexp_s + genderF + C1 + C2 + C3 + C4 + C5
#   DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c + genderF + C1 + C2 + C3 + C4 + C5
#   DeltaPerformance_l ~ Erg_l + genderF + C1 + C2 + C3 + C4 + C5
#   DeltaWellbeing_l ~ DeltaPerformance_l 
# '
# 
# l_sem <- sem(model_sem, data = df)
# summary(l_sem, fit.measures = TRUE)

# model_cfa <- '
#   DeltaWellbeing =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5 + DeltaP6 + DeltaP8
#   Erg =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP =~ DP1 + DP2 + DP3 + DP4 + DP5
# '
# 
# model_sem <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5
# 
#   # structural model (regressions)
#   DP_l ~ age_s + disabilities_c + education_c
#   fear_s ~ DP_l + isolation_c + adultcohab_s + childcohab_s + covidstatus_c + disabilities_c + education_c + age_s
#   Erg_l ~ DP_l + age_s + disabilities_c + education_c + isolation_c + adultcohab_s + childcohab_s + covidstatus_c + yearsofworkfromhomeexp_s
#   DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c + education_c
#   DeltaPerformance_l ~ Erg_l + DP_l + fear_s + yearsofworkfromhomeexp_s + childcohab_s + disabilities_c + education_c
# '
# ################################################################################
# #
# # Use lavaan w/o uncertainty propagation
# # 
# ################################################################################
# l_cfa <- cfa(model_cfa, data = df)
# summary(l_cfa)
# 
# l_sem <- sem(model_sem, data = df)
# summary(l_sem, fit.measures = TRUE)
# # All diagnostics passed
# # CFI > 0.97 Comparative Fit Index
# # RMSEA < 0.05 Root Mean Square Error of Approximation
# # SRMR < 0.05 Standardized Root Mean Square Residual
# 
# #                         Estimate  Std.Err  z-value  P(>|z|)
# # DP_l:
# #   age_s                 0.031    0.021    1.444    0.149
# # fear_s:
# #   adultcohab_s          0.075    0.045    1.658    0.097
# #   childcohab_s          0.084    0.047    1.779    0.075
# #   education_c          -0.115    0.059   -1.945    0.052
# #   age_s                -0.005    0.054   -0.087    0.930
# # Erg_l:
# #   disabilities_c       -0.015    0.057   -0.262    0.793
# #   education_c           0.009    0.025    0.373    0.709
# #   isolation_c           0.033    0.051    0.644    0.519
# #   adultcohab_s         -0.033    0.021   -1.589    0.112
# #   covidstatus_c        -0.047    0.031   -1.539    0.124
# # DeltaWellbeing_l:
# #   education_c          -0.023    0.030   -0.757    0.449
# # DeltaPerformance_l:
# #   DP_l                 -0.029    0.043   -0.665    0.506
# #   fear_s                0.003    0.009    0.308    0.758
# #   yrsfwrkfrmhmx_       -0.026    0.018   -1.412    0.158
# #   childcohab_s          0.010    0.016    0.601    0.548
# #   disabilities_c        0.077    0.045    1.705    0.088
# #   education_c          -0.003    0.020   -0.134    0.894
# 
# # Let's try a model where we drop the above factors
# model_sem <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5
# 
#   # structural model (regressions)
#   DP_l ~ disabilities_c + education_c
#   fear_s ~ DP_l + isolation_c + covidstatus_c + disabilities_c
#   Erg_l ~ DP_l + age_s + childcohab_s + yearsofworkfromhomeexp_s
#   DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c
#   DeltaPerformance_l ~ Erg_l
# '
# 
# l_sem <- sem(model_sem, data = df)
# summary(l_sem, fit.measures = TRUE)
# # lavaan WARNING: exogenous variable(s) declared as ordered in data: 
# # disabilities_c education_c isolation_c covidstatus_c
# # So we need to dummy code this also...
# 
# # Squared Multiple Correlations
# # – A variable should be dropped if its R^2  is relatively lower than the others
# # Identification of variables with squared multiple correlations relatively 
# # lower than others could be automated by identifying variables two standard 
# # deviations below the mean squared multiple correlations values.
# 
# # get squared multiple correlations
# rSquared = inspect(l_sem, "rsquare")
# # extract R^2 for IV
# nLatentVariables <- 4
# xVar = rSquared[1:(length(rSquared) - nLatentVariables)]
# # which should be excluded?
# names(xVar[xVar < (mean(xVar) - (2 * sd(xVar)))])
# # returns
# # fear_s
# # in this case
# 
# model_sem <- '# measurement model
#   DeltaWellbeing_l =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5 
#   DeltaPerformance_l =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP8
#   Erg_l =~ Erg1 + Erg2 + Erg3 + Erg4 + Erg5 + Erg6                      
#   DP_l =~ DP1 + DP2 + DP3 + DP4 + DP5
# 
#   # structural model (regressions)
#   DP_l ~ disabilities_c + education_c
#   fear_s ~ DP_l + isolation_c + covidstatus_c + disabilities_c
#   Erg_l ~ DP_l + age_s + childcohab_s + yearsofworkfromhomeexp_s
#   DeltaWellbeing_l ~ Erg_l + DP_l + fear_s  + covidstatus_c + adultcohab_s + disabilities_c
#   DeltaPerformance_l ~ Erg_l
#   DeltaWellbeing_l ~ DeltaPerformance_l
# '
# 
# l_sem <- sem(model_sem, data = df)
# summary(l_sem, fit.measures = TRUE)
