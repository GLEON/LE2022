# Analysis of cyan data (.csv files from Vince Moriatry)
# July 2023
# AGS

pacman::p_load(tidyverse, lubridate)

# Read in whole lake data
cyan_lake <- read.csv(file = "Data/CyAN/Mendota_CyAN_TimeSeries.csv")

# Read in data clipped to the buoy - work with this for now
cyan_buoy <- read.csv(file = "Data/CyAN/MendotaVP_CyAN_TimeSeries.csv")

# Add day, month, year, and doy columns
cyan_buoy <- cyan_buoy %>% 
  mutate(sampledate = ymd(ImageDate),
         day = day(sampledate),
         month = month(sampledate),
         year = year(sampledate),
         doy = yday(sampledate))
cyan_buoy$year <- as.character(cyan_buoy$year)

# Plot with a different line for each year
cyan_buoy %>%
  ggplot(aes(x = doy, y = AvgCA, col = year)) + 
  geom_line(stat = "identity") +
  theme_classic() +
  labs(x = "Day of year", y = "Average CA", title = "")

# Plot across all years
cyan_buoy %>%
  ggplot(aes(x = sampledate, y = AvgCA)) + 
  geom_line(stat = "identity") +
  theme_classic() +
  labs(x = "Date", y = "Average CA", title = "")

# Ask Vincent if he can extract the values pre-2016 (if we are interested in putting this in our phenology figures and if we are going pre-2016 with those figures)

# These are weekly averages - do we have a need for the daily values? If so, could we ask him to run for us?

# Other questions for him if we end up including this in the paper: 
#   - how exactly did he clip around the buoy
#   - should we remove data points with high %NA values (see last column)?
#   - make sure we have the units right for CI and CA