# Create table of in-situ conditions at hyperspectral dates
# For hyperspectral correlation analysis
# September 2024 (updated February 2025)
# AGS

pacman::p_load(tidyverse, patchwork, lubridate)

# Load in data that is going to be in this table
metabolism <- read_csv('Data/Metabolism_Model_Output/SimResultsMatrix_MetabData_run05oct23.csv')
metabolism <- metabolism %>%
  mutate(doy = yday(SimDate)) %>%
  mutate(year = year(SimDate))

ice <- read_csv('Data/IceCover/cleaned_ice_cover.csv')
ice <- ice %>%
  mutate(year = year(Date)) %>%
  mutate(doy = yday(Date))

metabolism_ice <- merge(metabolism, ice, by = c("year", "doy"))
metabolism_filter <- metabolism_ice %>%
  filter(Ice == "No") %>%
  filter(EpiNPP_mgC_L < 0.5) %>%
  select(c(year, doy, EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m))

buoy_TC <- read_csv('Data/Buoy/daily_buoy_tempcorr_clean.csv')
buoy_NTC <- read_csv('Data/Buoy/daily_buoy_clean.csv')

buoy_TC <- buoy_TC %>%
  mutate(doy = yday(sampledate)) %>%
  dplyr::rename("year" = year) %>%
  select(c(year, doy, avg_chlor_rfu_corr, avg_phyco_rfu_corr, avg_fdom_corr))

buoy_NTC <- buoy_NTC %>%
  mutate(doy = yday(sampledate)) %>%
  dplyr::rename("year" = year) %>%
  select(c(year, doy, avg_chlor_rfu, avg_phyco_rfu, avg_fdom, avg_turbidity))

# Add in the season boundaries by first creating an empty data frame of all possible dates
empty <- data.frame(date = seq(as.Date("01-01-2000", format = "%m-%d-%Y"), as.Date("12-31-2022", format = "%m-%d-%Y"), by = "day")) %>%
  mutate(doy = yday(date),
         year = year(date))
seasons <- read.csv('MetabolismModel/seasons_from_Robins_paper_long_withplaceholders.csv') %>%
  select(-placeholder) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"))
DATA <- left_join(empty, seasons) %>%
  fill(season, .direction="down")


# Join all of the data together into one dataframe
DATA <- left_join(DATA, metabolism_filter, by = c("year", "doy"))
DATA <- left_join(DATA, buoy_TC, by = c("year", "doy"))
DATA <- left_join(DATA, buoy_NTC, by = c("year", "doy"))

DATA$date <- as.factor(DATA$date)

# CONSTRAINING Y-AXES

DATA$avg_turbidity[DATA$avg_turbidity > 20] <- NA
DATA$avg_phyco_rfu_corr[DATA$avg_phyco_rfu_corr > 6] <- NA
DATA$avg_chlor_rfu_corr[DATA$avg_chlor_rfu_corr > 10] <- NA

DATA_scenes <- DATA %>%
  subset(date %in% c("2020-06-02", 
                     "2020-12-25",
                     "2021-06-11",
                     "2021-06-15",
                     "2021-08-15",
                     "2021-09-05",
                     "2021-11-02",
                     "2021-12-13",
                     "2022-05-13",
                     "2022-06-02",
                     "2022-06-16",
                     "2022-06-23",
                     "2022-07-27",
                     "2022-08-31",
                     "2022-10-10"))

# There was an NA for avg_fdom and avg_fdom_corr for the 2022-06-02 scene due to flags/sensor issues, so we are going to replace those NAs with the avg_fdom and avg_fdom_corr values from the day before, 2022-06-01
DATA %>%
  subset(date %in% c("2022-06-01")) %>%
  select(c(date, avg_fdom, avg_fdom_corr))

DATA_scenes$avg_fdom[DATA_scenes$date == "2022-06-02"] <- 5.82
DATA_scenes$avg_fdom_corr[DATA_scenes$date == "2022-06-02"] <- 5.694159

# Temperature correction didn't seem to work for 2021-11-02 scene. Go back and look into this.

write.csv(DATA_scenes, "Data/hyperspectral_dates_conditions_v4.csv")
