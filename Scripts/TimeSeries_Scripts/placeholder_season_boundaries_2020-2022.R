# This script is for calculating placeholder season definitions for 2020-2022.
# Eventually, these season definitions will be updated based on data from Robin Rowher.
# This is just so we have some season definitions for 2020-2022 for the time series figure in our ESA poster.
# I am going to take the average season boundary from 2010-2019 and use those boundaries as placeholders for 2020-2022.
# July 17 2023
# AGS

# Load packages
pacman::p_load(tidyverse, lubridate)

# First, load in the existing season definitions
seasons <- read.csv("MetabolismModel/seasons_from_Robins_paper.csv")

# Remove years pre-2010 (to try and take away climate change variations and just focus on the more recent years that are more likely to be closer to 2020-2022 conditions)
seasons <- filter(seasons, Year > 2009)

# Make a long format
seasons_long <- seasons %>% pivot_longer(Spring:Ice.on, names_to = "season", values_to = "date")

# Convert dates to day of year so it's easier to average
seasons_long_doy <- seasons_long %>% mutate(doy = yday(date))

# Calculate average doy for each season boundary
season_boundaries_avg <- seasons_long_doy %>% 
  group_by(season) %>%
  summarize(avg_boundary = mean(doy))
# Running into an issue averaging doy for the season boundary "Ice-on", since some of those dates are in January and some are in December. Could just eyeball this and say the cutoff is January 1st because that seems pretty close to the average.

# We actually have ice-on and ice-off dates in the cleaned_ice_cover.csv dataset, so we will use these actual boundaries for the cutoffs between Ice-on and Spring and between Fall and Ice-on.

# So here are the season boundaries we'll use for 2020-2022:
# Spring: 86.4 (86) is the averaged date, but instead we'll use the actual date from cleaned_ice_cover.csv
# Clearwater: 135.5 (136)
# Early.Summer: 163.7 (164)
# Late.Summer: 190.0 (190)
# Fall: 287.4 (287)
# Ice.on: 1 (1) is the initial estimated date, but instead we'll use the actual date from cleaned_ice_cover.csv

# Used this website: https://asd.gsfc.nasa.gov/Craig.Markwardt/doy2022.html to add these dates for 2020-2022 to the seasons_from_Robins_paper_long.csv file so now they can be integrated into our time series figure script.