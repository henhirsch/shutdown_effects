# clean environment
rm(list=ls())

# load packages
library(tidyverse)

# import data
crs_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/CR Data/CRs_number_length_duration_5.csv")

reviews_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/OIRA Review Data/1981_to_2023.csv")

shutdowns_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/Shutdown Data/shutdown_2.csv")

# remove rows with NA values
crs <- crs_0[complete.cases(crs_0), ]
shutdowns_0 <- shutdowns_0[complete.cases(shutdowns_0), ]

# format columns as.Date (initially: crs_0 in MM/DD/YY, reviews_0 in YYYY-MM-DD [default], shutdowns_0 in DD-Mon-YY)
crs$enactment_date <- as.Date(crs$enactment_date, format = "%m/%d/%y")
crs$expiration_date <- as.Date(crs$expiration_date, format = "%m/%d/%y")
reviews_0$date_received <- as.Date(reviews_0$date_received)
reviews_0$date_completed <- as.Date(reviews_0$date_completed)
reviews_0$date_published <- as.Date(reviews_0$date_published)
shutdowns_0$date_funding_ended <- as.Date(shutdowns_0$date_funding_ended, format = "%d-%b-%y")
shutdowns_0$date_funding_restored <- as.Date(shutdowns_0$date_funding_restored, format = "%d-%b-%y")

# format columns as.factor
reviews_0$agency_code <- as.factor(reviews_0$agency_code)
reviews_0$stage <- as.factor(reviews_0$stage)
reviews_0$ES <- as.factor(reviews_0$ES)
reviews_0$legal_deadline <- as.factor(reviews_0$legal_deadline)
reviews_0$decision <- as.factor(reviews_0$decision)
reviews_0$major <- as.factor(reviews_0$major)
shutdowns_0$shutdown_procedures_followed <- as.factor(shutdowns_0$shutdown_procedures_followed)

# remove columns
crs <- crs[ , c("enactment_date", "expiration_date")]
reviews <- reviews_0[ , c("agency_code", "stage", "ES", "date_received", "legal_deadline", "date_completed", "decision", "major")]
shutdowns <- shutdowns_0[ , c("date_funding_ended", "date_funding_restored", "shutdown_procedures_followed")]

# arrange data frames by date
crs <- crs %>% arrange(enactment_date)
reviews <- reviews %>% arrange(date_received)
shutdowns <- shutdowns %>% arrange(date_funding_ended)

# hello, this is a test