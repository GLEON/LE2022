# Script for making the temperature correction for chlorophyll-a, phycocyanin, and fDOM in buoy data,
# and for removing outliers particularly for chlorophyll-a, phycocyanin, fDOM, and turbidity.
#     Input is 'Data/Buoy/daily_buoy.csv' created in "buoy_data_retrieval.R"
#     Output will be 'Data/Buoy/daily_buoy_clean.csv' (once we finalize this script)
# August 2024
# AGS

# Load libraries
pacman::p_load(tidyverse, patchwork, lubridate)

# Load data
# daily_buoy.csv was created in the script "buoy_data_retrieval.R" and contains the daily averaged values for buoy parameters
buoy <- read_csv('Data/Buoy/daily_buoy.csv')
buoy <- buoy %>%
  mutate(doy = yday(sampledate)) %>% # add column for doy
  rename("year" = year4) # rename "year" column
buoy$year <- as.factor(buoy$year) # make "year" a factor for easier plotting

#----------------------------------------------------------------
# Temperature correction for chlorophyll-a, phycocyanin, and fDOM
#----------------------------------------------------------------

## First, we need the in situ water temperature on these dates and at 0.5m depth. 

# We'll load in the buoy temperature data ("North Temperate Lakes LTER: High Frequency Water Temperature Data - Lake Mendota Buoy 2006 - current") version knb-lter-ntl.130.31 where data ends at 2022-12-31 which is the same date our buoy data ends that we are using for this project.
# (there are now more updated versions of the dataset with 2023 and 2024 data but we don't need to use those because the hyperspectral scenes we are using end in 2022)

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/130/31/fa18315c045509540a7ac4f596fa2167" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")
dt1 <- read.csv(infile1, header = F, skip = 1 ,sep = ",", quot = '"', 
                col.names = c("year4", "sampledate", "daynum", "depth", "wtemp", "flag_wtemp"), 
                check.names = TRUE)
unlink(infile1) 

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
if (class(dt1$year4)=="factor") dt1$year4 <-as.numeric(levels(dt1$year4))[as.integer(dt1$year4) ]     
if (class(dt1$year4)=="character") dt1$year4 <-as.numeric(dt1$year4)                                  
# attempting to convert dt1$sampledate dateTime string to R date structure (date or POSIXct)          
tmpDateFormat<-"%Y-%m-%d"
tmp1sampledate<-as.Date(dt1$sampledate,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(nrow(dt1[dt1$sampledate != "",]) == length(tmp1sampledate[!is.na(tmp1sampledate)])){dt1$sampledate <- tmp1sampledate } else {print("Date conversion failed for dt1$sampledate. Please inspect the data and do the date conversion yourself.")}                                                               
if (class(dt1$daynum)=="factor") dt1$daynum <-as.numeric(levels(dt1$daynum))[as.integer(dt1$daynum) ]
if (class(dt1$daynum)=="character") dt1$daynum <-as.numeric(dt1$daynum)
if (class(dt1$depth)=="factor") dt1$depth <-as.numeric(levels(dt1$depth))[as.integer(dt1$depth) ]     
if (class(dt1$depth)=="character") dt1$depth <-as.numeric(dt1$depth)
if (class(dt1$wtemp)=="factor") dt1$wtemp <-as.numeric(levels(dt1$wtemp))[as.integer(dt1$wtemp) ]     
if (class(dt1$wtemp)=="character") dt1$wtemp <-as.numeric(dt1$wtemp)
if (class(dt1$flag_wtemp)!="factor") dt1$flag_wtemp<- as.factor(dt1$flag_wtemp)

# Turn flagged values into NA
daily_temp <- dt1 %>% 
  mutate(wtemp = case_when(flag_wtemp != "" ~ as.numeric(NA),
                           T ~ wtemp)) %>%  
  filter(!is.na(wtemp))

# Keep just the values at 0.5m depth (depth where the ysi was measuring chlorophyll-a, phycocyanin, fDOM, and turbidity)
daily_temp_0.5m <- daily_temp %>%
  filter(depth == 0.5)

# Visualize temperatures across dates
ggplot(daily_temp_0.5m, aes(x = sampledate, y = wtemp)) +
  geom_point()

# Visualize temperatures across doys
ggplot(daily_temp_0.5m, aes(x = daynum, y = wtemp)) +
  geom_point() +
  facet_wrap(~year4, ncol = 4)
# potential temperature outliers to remove in 2009 and 2012 - come back to

## Second, let's join the temperature data to our buoy dataframe according to the same-date matchups.
daily_temp_0.5m <- daily_temp_0.5m %>% select(c(sampledate, wtemp))
buoy <- left_join(buoy, daily_temp_0.5m, by = "sampledate")

## Third, let's add new columns for the temperature-corrected chlorophyll-a, phycocyanin, and fDOM values
rho = -0.01 # value determined from Watras et al. 2011 and Watras et al. 2017
Tr = 20 # reference T, 20C

buoy_corrected <- buoy %>%
  mutate(avg_chlor_rfu_corr = avg_chlor_rfu/(1+rho*(wtemp-Tr)), # chlorophyll
         avg_phyco_rfu_corr = avg_phyco_rfu/(1+rho*(wtemp-Tr)), # phycocyanin
         avg_fdom_corr = avg_fdom/(1+rho*(wtemp-Tr))) # fdom

## Fourth, evaluate the temperature correction and how it looks like it worked

# chlorophyll
p1 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_chlor_rfu, avg_chlor_rfu_corr)) %>%
  pivot_longer(avg_chlor_rfu:avg_chlor_rfu_corr, names_to = "parameter") %>%
  filter(year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 4, scales = "free_y")
ggsave("Figures/temp_corr/chl_temp_corr.png", p1, width = 12, height = 8, units = 'in', device = "png", dpi=100)

# phycocyanin
p2 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_phyco_rfu, avg_phyco_rfu_corr)) %>%
  pivot_longer(avg_phyco_rfu:avg_phyco_rfu_corr, names_to = "parameter") %>%
  filter(year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 4, scales = "free_y")
ggsave("Figures/temp_corr/phyco_temp_corr.png", p2, width = 12, height = 8, units = 'in', device = "png", dpi=100)

# fDOM
p3 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_fdom, avg_fdom_corr)) %>%
  pivot_longer(avg_fdom:avg_fdom_corr, names_to = "parameter") %>%
  filter(year %in% c(2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 1, scales = "free_y")
ggsave("Figures/temp_corr/fdom_temp_corr.png", p3, width = 12, height = 8, units = 'in', device = "png", dpi=100)

# Decision point: Do these temperature corrections look good? Do we know for sure that fDOM needs to be corrected? 
  
#----------------------------------------------------------------
# Outlier detection and removal
#----------------------------------------------------------------

# Right now this code is written for the un-temperature-corrected values - will change once we decide to move forward with the temperature-corrected values

# Plot our four variables of interest across the years
buoy %>% 
  pivot_longer(avg_chlor_rfu:scaled_phyco_rfu, names_to = "parameter") %>% 
  filter(parameter %in% c("avg_chlor_rfu", "avg_phyco_rfu", "avg_fdom", "avg_turbidity")) %>%
  #filter(year %in% c("2019", "2020", "2021", "2022")) %>% # just look at the last four years of data
  ggplot(aes(x = sampledate, y = value)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

# Plot our four variables of interest across the seasons
buoy %>% 
  pivot_longer(avg_chlor_rfu:scaled_phyco_rfu, names_to = "parameter") %>% 
  filter(parameter %in% c("avg_chlor_rfu", "avg_phyco_rfu", "avg_fdom", "avg_turbidity")) %>%
  #filter(year %in% c("2019", "2020", "2021", "2022")) %>% # just look at the last four years of data
  ggplot(aes(x = doy, y = value, color = year)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

# Proposed outlier removal based on just looking at those last four years of data
buoy_clean <- buoy
buoy_clean$avg_phyco_rfu[buoy_clean$avg_chlor_rfu >= 10000] <- NA
buoy_clean$avg_phyco_rfu[buoy_clean$avg_phyco_rfu >= 5000] <- NA
buoy_clean$avg_turbidity[buoy_clean$avg_turbidity >= 20] <- NA

# Plot our four variables of interest across the seasons, now with the cleaned version
buoy_clean %>% 
  pivot_longer(avg_chlor_rfu:scaled_phyco_rfu, names_to = "parameter") %>% 
  filter(parameter %in% c("avg_chlor_rfu", "avg_phyco_rfu", "avg_fdom", "avg_turbidity")) %>%
  #filter(year %in% c("2019", "2020", "2021", "2022")) %>%
  ggplot(aes(x = doy, y = value, color = year)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1)