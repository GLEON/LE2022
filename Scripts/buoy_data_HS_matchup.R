## Review Phycocyanin data

# Load Libraries
library(tidyverse)
library(ggplot2)

# Load data
buoy <- read.csv('Data/Buoy/daily_buoy.csv')

AVIRIS <- read.csv('Data/AVIRIS/AVIRIS_stats_buoy.csv')
DESIS <- read.csv('Data/DESIS/DESIS_stats_buoy.csv')
PRISMA <- read.csv('Data/PRISMA/PRISMA_stats_buoy.csv')

dates <- c(as.Date(as.character(AVIRIS$date),format="%Y%m%d"),
           as.Date(as.character(DESIS$date),format="%Y-%m-%d"),
           as.Date(as.character(PRISMA$date),format="%Y_%m_%d"))

# Define hyperspectral scene dates

Buoy_subset <- buoy %>%
  filter(as.Date(sampledate) %in% dates)

write.csv(Buoy_subset,"Data/Buoy/daily_buoy_HS_matchup.csv")
       