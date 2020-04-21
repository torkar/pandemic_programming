# set working directory (see https://stackoverflow.com/a/35842119)
dir = tryCatch({
  # script being sourced
  getSrcDirectory()[1]
}, error = function(e) {
  # script being run in RStudio
  dirname(rstudioapi::getActiveDocumentContext()$path)
})
setwd(dir)

library(data.table)

################################################################################

# function definitions
split_os_vars <- function(df) {
  for (i in 1:22) {
    column_name <- paste0("OS", i)
    column_yes <- paste0("OS", i, "_y")
    column_helpful <- paste0("OS", i, "_h")
    df[[column_yes]] <- as.integer(df[[column_name]] == "yes" | df[[column_name]] == "both")
    df[[column_helpful]] <- as.integer(df[[column_name]] == "helpful" | df[[column_name]] == "both")
    df[[column_name]] <- NULL
  }
  df
}

get_highest_covid_status <- function(df) {
  result <- rep(NA, nrow(df))
  for (i in 1:nrow(df)) {
    max_status = 0;
    for (status_number in 5:0) {
      status_set <- as.logical(subset(df[i], select=paste0("COVIDStatus", status_number)))
      if (is.na(status_set)) {
        status_set <- FALSE # for some languages, NA is set instead of 0
      }
      if (status_set) {
        max_status <- status_number
        break
      }
    }
    result[i] <- max_status
  }
  result
}

clean_child_cohabitants <- function(df) {
  child_cohabitants <- df$Children
  child_cohabitants[child_cohabitants < 0] <- NA  # some particpants provided a negative value...
  child_cohabitants
}

get_adult_cohabitants <- function(df) {
  adult_cohabitants <- df$Coinhabitants - df$Children - 1  # the respondent themselve has to be substracted
  adult_cohabitants[adult_cohabitants < 0] <- NA
  adult_cohabitants
}

clean_countries <- function(df) {
  country <- df$Country
  country[country == "Russia and Germany"] <- NA
  country
}

################################################################################

recoded <- fread("data/English_recoded.csv", header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", -99), stringsAsFactors=FALSE)

other_languages <- c("Turkish", "French", "Japanese", "Spanish", "Russian", "Italian", "Korean", "Portuguese", "Chinese", "Arabic", "Persian")
for (other_language in other_languages) {
  #print(other_language)
  recoded_new <- fread(paste0("data/", other_language, "_recoded.csv"), header=TRUE, sep=",", quote="\"", strip.white=TRUE, showProgress=TRUE, encoding="UTF-8", na.strings=c("", "null", -99), stringsAsFactors=FALSE)
  # fix IDs
  recoded_new$ID <- 1:nrow(recoded_new)
  recoded <- rbind(recoded, recoded_new)
  rm(recoded_new)
}
n <- nrow(recoded)
n
# 2225

# fix country names
recoded$Country <- clean_countries(recoded)

table_countries <- sort(table(recoded$Country, useNA="ifany"), decreasing=TRUE)
table_countries[table_countries >= 20]
# Germany         Russia         Brazil          Italy  United States   Korea, South        Belgium 
#     505            366            272            173             99             81             77 
# China         Turkey          India          Japan          Spain           Iran        Austria 
#    76             66             55             53             52             40             29 
# Canada    Switzerland United Kingdom           <NA> 
#     27             20             20             20 

sum(table_countries[table_countries < 20])
# 194 (Other)

# drop organizational support variables
for (i in 1:22) {
  recoded[[paste0("OS", i)]] <- NULL
}

# select highest COVID status
recoded$COVIDStatus <- get_highest_covid_status(recoded)
table(recoded$COVIDStatus)
#    0    1    2    3    4    5 
# 1853  279   50   33    3    7 

# get number of adult cohabitants
recoded$AdultCohabitants <- get_adult_cohabitants(recoded)
recoded$ChildCohabitants <- clean_child_cohabitants(recoded)
recoded$Coinhabitants <- NULL
recoded$Children <- NULL

# calculate FearResilience score
# weights taken from page 293 of http://cogprints.org/5245/1/Fear%26_Resilience_Bracha_%26_Burkle_Final_PDF.pdf
fr_weights <- c(5, 10, 5, 1, 1, 1, 5, -10, -3, 1)
FearResilience <- rep(0, n)
for (fr_number in 1:10) {
  FearResilience <- FearResilience + recoded[[paste0("FR", fr_number)]] * fr_weights[fr_number]
  recoded[[paste0("FR", fr_number)]] <- NULL
}
recoded$FearResilience <- FearResilience*0.1  # factor 0.1 to reduce variance

# recode some of the scales
# Isolation
recoded$Isolation <- recoded$Isolation + 1 # 1...4
# DP
for (i in 1:5) {
  recoded[[paste0("DP", i)]] <- recoded[[paste0("DP", i)]] + 1  # 1...5 
}
# Erg
erg_mapping <- "-3=1; -2=2; -1=3; 1=4; 2=5; 3=6; else=NA"
for (i in 1:6) {
  recoded[[paste0("Erg", i)]] <- as.integer(car::recode(unlist(recoded[[paste0("Erg", i)]]), erg_mapping))  # 1...6
}
# WHO5B
for (i in 1:5) {
  recoded[[paste0("WHO5B", i)]] <- recoded[[paste0("WHO5B", i)]] + 1  # 1...6
}
# WHO5S
for (i in 1:5) {
  recoded[[paste0("WHO5S", i)]] <- recoded[[paste0("WHO5S", i)]] + 1  # 1...6
}
# HPQB
for (i in 1:7) {
  recoded[[paste0("HPQB", i)]] <- recoded[[paste0("HPQB", i)]] + 1  # 1...5
}
# HPQB8
hpq8_mapping <- "-3=1; -2=2; -1=3; 0=4; 1=5; 2=6; 3=7; else=NA" 
recoded$HPQB8 <- as.integer(car::recode(recoded$HPQB8, hpq8_mapping))  # 1...7
# HPQS
for (i in 1:7) {
  recoded[[paste0("HPQS", i)]] <- recoded[[paste0("HPQS", i)]] + 1  # 1...5
}
# HPQS8
recoded$HPQS8 <- as.integer(car::recode(recoded$HPQS8, hpq8_mapping))  # 1...7

# export the merged dataset
DATE <- "2020-04-16"
write.table(recoded, file=paste0("data/export_", DATE, ".csv"), sep=",", col.names=TRUE, row.names=FALSE, na="NA", quote=TRUE, qmethod="double", fileEncoding="UTF-8")
