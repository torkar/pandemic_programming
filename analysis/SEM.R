library(lavaan)
library(data.table)
library(semPlot)

d <- fread("data/export_2020-04-16.csv", stringsAsFactors=TRUE, 
           na.strings=c("", "null", "NA", -99))

# TODO
# First, we will check that all variables are correctly coded
# Second, we will run a confirmatory factor analysis
# Third,  we will check correlation among covariates, and remove vars with 
# strong collinearity
# Fourth, we will, in iterations, check the predictive power of all exogeneous
# covariates (predictors), and remove any that have no or very little predictive
# power (need to do that due to ordered predictors)
# Fifth, we'll check the SEM diagnostics to check that the model can be trusted

################################################################################
#
# Data check
#
################################################################################

# Gender has way too few data points outside male/female so keep only those
# Remove 18 cases (16 "Not answering", and 2 "Other")
d <- d[d$Gender == "Female" | d$Gender == "Male", ]

# Let's move variables that we have checked to a new df so we always have the
# original data untouched. All predictors in small letters, suffixes are
# _n = nominal, _o = ordered, _s = scaled, _b = binary

df <- data.frame(
  gender_b = ifelse(d$Gender == "Female", 1, 0),
  
  # Country has way too many categories ($K=57$), so we'll use Language instead 
  # (K=12)
  
  # Language is coded as factors, i.e., nomial categorical w/ $K=12$ levels
  # we need to replace it by a set of $K-1$ dummy variables
  lang_n = model.matrix(~Language, data = d)[, -1], # Arabic is the intercept
  
  # covidstatus is Likert 0,..,5, we need to make it 1,..,6 (reg. for lavaan)
  covidstatus_o = d$COVIDStatus + 1,
  
  isolation_o = d$Isolation,
  
  exp_s = scale(d$YearsOfExperience),
  homeexp_s = scale(d$YearsOfWorkFromHomeExperience),
  fulltime_b = d$Fulltime,
  
  # Age is a tricky one. From the questionnaire it's binned into 11 categories,
  # so we can either say it's nominal, ordered, or scale it.
  # I would argue that it can't be nominal since there exists an order, so then
  # we need to decide between ordered categorical or scale it
  # Either way works, and for sampling purposes it might even be better to
  # scale it
  age_s = scale(d$Age),
  
  edu_o = d$Education + 1,
  
  orgsize_o = d$OrganizationSize, # alrewady >= 1
  
  disab_o = d$Disabilities + 1,
  adultcohab_o = d$AdultCohabitants + 1,
  childcohab_o = d$ChildCohabitants + 1,
  
  dev_b = d$RolesIncludeDeveloper,
  
  fear = d$FearResilience, # alrady basically scaled
  
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
  DeltaW1 = (d$WHO5B1 - d$WHO5S1) + 6,
  DeltaW2 = (d$WHO5B2 - d$WHO5S2) + 6,
  DeltaW3 = (d$WHO5B3 - d$WHO5S3) + 6,
  DeltaW4 = (d$WHO5B4 - d$WHO5S4) + 6,
  DeltaW5 = (d$WHO5B5 - d$WHO5S5) + 6,
  DeltaP1 = (d$HPQB1 - d$HPQS1 ) + 4,
  DeltaP2 = (d$HPQB2 - d$HPQS2) + 5,
  DeltaP3 = (d$HPQB3 - d$HPQS3) + 5,
  DeltaP4 = (d$HPQB4 - d$HPQS4) + 5,
  DeltaP5 = (d$HPQB5 - d$HPQS5) + 5,
  DeltaP6 = (d$HPQB6 - d$HPQS6) + 5,
  DeltaP7 = (d$HPQB7 - d$HPQS7) + 5,
  DeltaP8 = (d$HPQB8 - d$HPQS8) + 56,
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
# Remove row 2059 which contains mostly NAs
# df <- df[-2059,]

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

################################################################################
#
# All predictors have been checked for non-identifiability against all outcomes
#
################################################################################

################################################################################
#
# Confirmatory factor analysis using lavaan
#
################################################################################

# After =~ we list the exogenous covariates that models the latent variable on 
# lhs:
# DWell, is our latent variable of wellbeing (before/after)
# DPerf, is our latent var of performance (before/after)
# DP, latent variable for disaster preparedness
# ERG, latent variable for ergonomics

# Our measurement model
m_cfa <- '
  DWell =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5
  DPerf =~ DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + 
           DeltaP7 + DeltaP8
  DP    =~ DP1 + DP2 + DP3 + DP4 + DP5
  ERG   =~ ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + ERG6
'

l_cfa <- cfa(m_cfa, data = df)

# Summary shows that $\chi^2$, CFI/TLI, SRMR look ok. RMSEA is slightly above 
# threshold but that is not strange considering our sample size n=1377
summary(l_cfa, fit.measures=TRUE)

# check factor loadings
estVals <- parameterEstimates(l_cfa, standardized = TRUE)
estVals[estVals$op == "=~", "est"]
estVals[estVals$op == "=~", "pvalue"]
# NA (i.e., not available) for some variables, but those are the marker vars

# are all factor loadings significant?
pValues <- na.omit(estVals[estVals$op == "=~", "pvalue"])
print(
  paste0(
    if(sum(pValues < .05) != length(pValues)){"NOT "}, 
    "All factor loadings significant"))

# factor intercorrelations from the confirmatory factor analysis
inspect(l_cfa, "cor.lv")
#        DWell  DPerf  DP     ERG   
# DWell  1.000                     
# DPerf  0.571  1.000              
# DP    -0.044 -0.115  1.000       
# ERG   -0.270 -0.312  0.356  1.000

semPaths(l_cfa, rotation = 2, whatLabels = 'est', nDigits = 1,
         nCharNodes = 0, residuals = FALSE, exoCov = FALSE)


################################################################################
#
# Structural equation modelling using lavaan
#
################################################################################

# structural (regression) model
m_full <- '
  DWell =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5
  DPerf =~ DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + 
           DeltaP7 + DeltaP8
  DP    =~ DP1 + DP2 + DP3 + DP4 + DP5
  ERG   =~ ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + ERG6
  
  DP    ~ gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + exp_s + homeexp_s + fulltime_b + age_s +
          edu_o + orgsize_o + disab_o + adultcohab_o + childcohab_o + dev_b
  
  ERG   ~ gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + exp_s + homeexp_s + fulltime_b + age_s +
          edu_o + orgsize_o + disab_o + adultcohab_o + childcohab_o + dev_b
  
  fear  ~ DP +
          gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + exp_s + homeexp_s + fulltime_b + age_s +
          edu_o + orgsize_o + disab_o + adultcohab_o + childcohab_o + dev_b
  
  DWell ~ ERG +
          gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + exp_s + homeexp_s + fulltime_b + age_s +
          edu_o + orgsize_o + disab_o + adultcohab_o + childcohab_o + dev_b
  
  DPerf ~ ERG +
          gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + exp_s + homeexp_s + fulltime_b + age_s +
          edu_o + orgsize_o + disab_o + adultcohab_o + childcohab_o + dev_b

  DWell ~ DPerf  
'

l_full <- sem(m_full, data = df)
summary(l_full, fit.measures = TRUE) 
# now SRMR is slightly above 0.05 but SRMR < .08 is generally considered OK
# (Hu & Bentler, 1999), and we know the reason for this, i.e., n>>200.
# The output clearly indicates that some exogenous covariates should be removed

# We'll remove all covariates with p>0.1, but we'll leave Language for now due
# to the narrative
m_sem <- '
  DWell =~ DeltaW1 + DeltaW2 + DeltaW3 + DeltaW4 + DeltaW5
  DPerf =~  DeltaP1 + DeltaP2 + DeltaP3 + DeltaP4 + DeltaP5  + DeltaP6 + DeltaP7 + DeltaP8
  DP    =~ DP1 + DP2 + DP3 + DP4 + DP5
  ERG   =~ ERG1 + ERG2 + ERG3 + ERG4 + ERG5 + ERG6
  
  DP    ~ lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + adultcohab_o
  
  ERG   ~ lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          exp_s + adultcohab_o + childcohab_o
  
  fear  ~ DP +
          gender_b + 
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + isolation_o + 
          disab_o + childcohab_o + dev_b
  
  DWell ~ ERG +
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          covidstatus_o + age_s 
  
  DPerf ~ ERG +
          lang_n.LanguageChinese + lang_n.LanguageEnglish + 
          lang_n.LanguageFrench + lang_n.LanguageItalian + 
          lang_n.LanguageJapanese + lang_n.LanguageKorean + 
          lang_n.LanguagePersian +  lang_n.LanguagePortuguese + 
          lang_n.LanguageRussian + lang_n.LanguageSpanish + 
          lang_n.LanguageTurkish + 
          age_s + adultcohab_o

  DWell ~ DPerf  
'

l_sem <- sem(m_sem, data = df)
summary(l_sem, fit.measures = TRUE)
# SRMR < .08 is generally considered a good fit (Hu & Bentler, 1999)
