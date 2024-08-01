# Trying out some time series seasonal decomposition on the metabolism variables
# June 2024
# AGS

pacman::p_load(tidyverse, patchwork, lubridate, forecast)

# Load metabolism data
metabolism <- read_csv('Data/Metabolism_Model_Output/SimResultsMatrix_MetabData_run05oct23.csv')
metabolism <- metabolism %>%
  mutate(doy = yday(SimDate)) %>%
  mutate(year = year(SimDate)) %>%
  select(SimDate, EpiR_mgC_L, EpiNPP_mgC_L) # select columns of interest

# ice <- read_csv('Data/IceCover/cleaned_ice_cover.csv')
# ice <- ice %>%
#   mutate(year = year(Date)) %>%
#   mutate(doy = yday(Date))
# 
# metabolism_ice <- merge(metabolism, ice, by = c("year", "doy"))
# metabolism<- metabolism_ice %>%
  #filter(Ice == "No") %>%
  #filter(EpiNPP_mgC_L < 0.5) %>% # this was the cutoff I used in the time series plotting for removing outliers
  
# Make vector of R values
R_vector <- metabolism$EpiR_mgC_L
#R_vector <- metabolism$EpiNPP_mgC_L

# Convert into a time series object
# The ts() function will convert a numeric vector into an R time series object. The format is ts(vector, start=, end=, frequency=) where start and end are the times of the first and last observation and frequency is the number of observations per unit time (1=annual, 4=quartly, 12=monthly, etc.).
R_ts <- ts(R_vector, start = c(2010, 1, 1), end = c(2022, 12, 31), frequency = 365)

# Plot time series
plot(R_ts)

# Seasonal decomposition - version 1
# A time series with additive trend, seasonal, and irregular components can be decomposed using the stl() function.
fit <- stl(R_ts, s.window="period")
plot(fit)

# Seasonal decomposition - version 2
# A seasonal time series, in addition to the trend and random components, also has a seasonal component. Decomposing a seasonal time series means separating the time series into these three components. In R we can use the decompose() function to estimate the three components of the time series.
decomp <- decompose(R_ts)
plot(decomp)

# Additional plots
monthplot(R_ts)
seasonplot(R_ts)

# NPP has high outliers, but it does weird things when I remove the outliers (perhaps it doesn't like data gaps?)

# Try with the 16S microbial data next? But how will it deal with a time series dataset where not every single day has a data point, as we have for the metabolism data I worked with above?