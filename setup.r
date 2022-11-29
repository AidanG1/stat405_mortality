library(tidyverse)
library(RSQLite)
library(grid)
library(knitr)

db <<- dbConnect(SQLite(), "data/mortality.sqlite")
initRegExp(db)
query <<- function(q) {
    req <- dbSendQuery(db, q)
    results <- dbFetch(req)
    dbClearResult(req)
    results
}


manners <<- list(
    "1" = "Accident",
    "2" = "Suicide",
    "3" = "Homicide",
    "4" = "Pending Investigation",
    "5" = "Could Not Determine",
    "6" = "Self-Inflicted",
    "7" = "Natural",
    "8" = "Not Specified"
)

ages <<- list(
    "1" = "Under 1 hour",
    "2" = "1-23 hours",
    "3" = "1 day",
    "4" = "2 days",
    "5" = "3 days",
    "6" = "4 days",
    "7" = "5 days",
    "8" = "6 days",
    "9" = "7-13 days",
    "10" = "14-20 days",
    "11" = "21-27 days",
    "12" = "1 month",
    "13" = "2 months",
    "14" = "3 months",
    "15" = "4 months",
    "16" = "5 months",
    "17" = "6 months",
    "18" = "7 months",
    "19" = "8 months",
    "20" = "9 months",
    "21" = "10 months",
    "22" = "11 months",
    "23" = "1 year",
    "24" = "2 years",
    "25" = "3 years",
    "26" = "4 years",
    "27" = "5-9 years",
    "28" = "10-14 years",
    "29" = "15-19 years",
    "30" = "20-24 years",
    "31" = "25-29 years",
    "32" = "30-34 years",
    "33" = "35-39 years",
    "34" = "40-44 years",
    "35" = "45-49 years",
    "36" = "50-54 years",
    "37" = "55-59 years",
    "38" = "60-64 years",
    "39" = "65-69 years",
    "40" = "70-74 years",
    "41" = "75-79 years",
    "42" = "80-84 years",
    "43" = "85-89 years",
    "44" = "90-94 years",
    "45" = "95-99 years",
    "46" = "100-104 years",
    "47" = "105-109 years",
    "48" = "110-114 years",
    "49" = "115-119 years",
    "50" = "120-124 years",
    "51" = "124+ years",
    "52" = "Age not stated"
)

educations <<- list(
    "1" = "8th grade or less",
    "2" = "9-12th grade, no diploma",
    "3" = "High school graduate or GED",
    "4" = "Some college credit, but no degree",
    "5" = "Associate degree",
    "6" = "Bachelor's degree",
    "7" = "Master's degree",
    "8" = "Doctorate or professional degree",
    "9" = "Unknown"
)

places <<- list(
    "1" = "Hospital - Inpatient",
    "2" = "Hospital - Outpatient",
    "3" = "Hospital - Dead on Arrival",
    "4" = "Decedent's home",
    "5" = "Hospice facility",
    "6" = "Nursing home/long term care",
    "7" = "Other",
    "9" = "Place of death unknown"
)

races <<- list(
    "0" = "Other (Puerto Rico Only)",
    "1" = "White",
    "2" = "Black",
    "3" = "American Indian",
    "4" = "Asian or Pacific Islander"
)
