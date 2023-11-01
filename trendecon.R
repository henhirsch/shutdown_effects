# clear environment
rm(list=ls())

# install packages
library(gtrendsR)
library(trendecon)

data <- ts_gtrends(
  keyword = c("government shutdown"),
  time = "all",
  geo     = "US"
)

data_consistent <- ts_gtrends_mwd(c("government shutdown"),
                                  geo = "US")
