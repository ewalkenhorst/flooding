#import buyout data
library(readr)
library(dplyr)
buyouts_v2 <- read.csv("https://www.fema.gov/api/open/v2/HazardMitigationAssistanceProjects.csv")
ar_buyouts <- filter(buyouts_v2, state=="Arkansas") %>%
  arrange(-programFy)
