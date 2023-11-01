# clean environment
rm(list=ls())

# load packages
library(tidyverse)
library(ivs)
library(zoo)

# import data
crs_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/CR Data/CRs_number_length_duration_5.csv")

reviews_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/OIRA Review Data/1981_to_2023.csv")

shutdowns_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/Shutdown Data/shutdown_2.csv")

trends_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/Google Trends Data/all_google_data(normalized).csv", skip = 1)

complete_daily_0 <- read.csv("/Users/henryhirsch/Henry/Work/2023/Regulatory Studies Center/projects/5. Effects of Govt. Shutdowns on Rule Reviewing/data sets/Google Trends Data/complete_daily_data.csv")

# remove rows with NA values
crs <- crs_0[complete.cases(crs_0), ]
shutdowns_0 <- shutdowns_0[complete.cases(shutdowns_0), ]

# modify column names
colnames(trends_0) <- c("month_start", "intensity")
colnames(complete_daily_0) <- c("x", "date", "value")

# format columns as.Date (initially: crs_0 in MM/DD/YY, reviews_0 in YYYY-MM-DD [default], shutdowns_0 in DD-Mon-YY)
crs$enactment_date <- as.Date(crs$enactment_date, format = "%m/%d/%y")
crs$expiration_date <- as.Date(crs$expiration_date, format = "%m/%d/%y")
reviews_0$date_received <- as.Date(reviews_0$date_received)
reviews_0$date_completed <- as.Date(reviews_0$date_completed)
reviews_0$date_published <- as.Date(reviews_0$date_published)
shutdowns_0$date_funding_ended <- as.Date(shutdowns_0$date_funding_ended, format = "%d-%b-%y")
shutdowns_0$date_funding_restored <- as.Date(shutdowns_0$date_funding_restored, format = "%d-%b-%y")
trends_0$month_start <- as.Date(as.yearmon(trends_0$month_start))
complete_daily_0$date <- as.Date(complete_daily_0$date)

# format columns as.factor
reviews_0$agency_code <- as.factor(reviews_0$agency_code)
reviews_0$stage <- as.factor(reviews_0$stage)
reviews_0$ES <- as.factor(reviews_0$ES)
reviews_0$legal_deadline <- as.factor(reviews_0$legal_deadline)
reviews_0$decision <- as.factor(reviews_0$decision)
reviews_0$major <- as.factor(reviews_0$major)
shutdowns_0$shutdown_procedures_followed <- as.factor(shutdowns_0$shutdown_procedures_followed)

# remove columns
crs <- crs[ , c("enactment_date", "expiration_date", "duration_in_days")]
reviews <- reviews_0[ , c("agency_code", "stage", "ES", "date_received", "legal_deadline", "date_completed", "decision", "major")]
shutdowns <- shutdowns_0[ , c("date_funding_ended", "date_funding_restored", "shutdown_procedures_followed")]
complete_daily <- complete_daily_0[ , c("date", "value")]

# arrange data frames by date
crs <- crs %>% arrange(enactment_date)
reviews <- reviews %>% arrange(date_received)
shutdowns <- shutdowns %>% arrange(date_funding_ended)

# create month_end column for trends data frame
trends <- trends_0
trends <- trends %>%
  mutate(month_end = ceiling_date(month_start, "month") - days(1))

# create shutdown and cr interval data frames
cr_intervals <- data.frame(
  interval = interval(crs$enactment_date, crs$expiration_date))
shutdown_intervals <- data.frame(
  interval = interval(shutdowns$date_funding_ended, shutdowns$date_funding_restored))

# create all_dates_interval
earliest_date <- min(shutdowns$date_funding_ended)
all_dates_interval <- interval(earliest_date, Sys.Date())

# create iv intervals
shutdowns <- shutdowns %>%
   mutate(
     date_funding_restored_plus1 = date_funding_restored + days(1),
     iv_interval = iv(date_funding_ended, date_funding_restored_plus1)
   )

# create govt_open_iv_intervals using iv complements
govt_open_iv_intervals <- as.data.frame(iv_set_complement(shutdowns$iv_interval, lower = earliest_date, upper = Sys.Date()))
colnames(govt_open_iv_intervals) <- c("iv_interval")

# create shutdown_iv_interval data frame
shutdown_iv_intervals <- as.data.frame(shutdowns$iv_interval)
colnames(shutdown_iv_intervals) <- c("iv_interval")

# add govt_status column to both iv_interval data frame
govt_open_iv_intervals <- govt_open_iv_intervals %>%
  mutate(govt_status = factor("open", levels = c("open", "closed")))
shutdown_iv_intervals <- shutdown_iv_intervals %>%
  mutate(govt_status = factor("closed", levels = c("open", "closed")))

# create standard start and end date columns from iv_interval columns
govt_open_iv_intervals <- govt_open_iv_intervals %>%
  mutate(
    start_date = iv_start(iv_interval),
    end_date = (iv_end(iv_interval) - days(1))
  )

shutdown_iv_intervals <- shutdown_iv_intervals %>%
  mutate(
    start_date = iv_start(iv_interval),
    end_date = (iv_end(iv_interval) - days(1))
  )

# create govt_open and govt_closed data frames
govt_open <- as.data.frame(govt_open_iv_intervals[ , c("govt_status", "start_date", "end_date")])
govt_closed <- as.data.frame(shutdown_iv_intervals[ , c("govt_status", "start_date", "end_date")])

# add date interval columns to govt_open, got_closed, and trends
govt_open <- govt_open %>%
  mutate(date_interval = interval(start_date, end_date))
govt_closed <- govt_closed %>%
  mutate(date_interval = interval(start_date, end_date))
trends <- trends %>% 
  mutate(date_interval = interval(month_start, month_end))

# convert trends intensity variable to dummy
trends <- trends %>% 
  mutate(todummy = as.numeric(intensity))

trends$todummy[is.na(trends$todummy)] <- 0

trends <- trends %>%
   mutate(aware = ifelse(todummy == 0, 0, 1))

# combine govt_open and govt_closed into one data frame
govt_behavior <- bind_rows(govt_closed, govt_open)

# arrange rows according to date_interval column
govt_behavior <- govt_behavior %>%
  arrange(date_interval)

# create date_interval
crs <- crs %>%
  mutate(
    date_interval = interval(enactment_date, expiration_date))

reviews <- reviews %>%
  mutate(
    date_interval = interval(date_received, date_completed))

# create column that indicates the number of days in each interval
govt_behavior$days_in_interval <- time_length(govt_behavior$date_interval, unit = "days")
reviews$days_in_interval <- time_length(reviews$date_interval, unit = "days")
# ^ should 1 day be subtracted? See shutdown webpage: "*Days are counted from the first day to the last full day that the government was shut down. The date the Public Law was signed is not included because that law opened the government when it went into effect."

# figure out how many reviews were completed during each interval
# use %within% from lubridate? or use data.table to do non-equi update join?

# # create start date for awareness window
# govt_behavior <- govt_behavior %>%
#   mutate(
#     start_awareness = start_date - 7)

# plot daily google trends data
line1 <- ggplot(complete_daily, aes(x = date, y = value)) + 
  geom_line(color = "red") + 
  labs(title="Google Searches for 'Government Shutdown'",
       x="Date",
       y="Search Prevalence") +
  theme_minimal()

line1

# summary statistics for complete_daily
summary(complete_daily$value)
