# Script for making the temperature correction for chlorophyll-a, phycocyanin, and fDOM in buoy data,
# and for removing outliers particularly for chlorophyll-a, phycocyanin, fDOM, and turbidity.
#     Input is 'Data/Buoy/daily_buoy.csv' created in "buoy_data_retrieval.R"
#     Output will be 'Data/Buoy/daily_buoy_clean.csv' (once we finalize this script)
# August 2024
# AGS

# Load libraries
pacman::p_load(tidyverse, patchwork, lubridate)

# Load data
# Load daily data - need to go to website to update link... 
# https://lter.limnology.wisc.edu/dataset/north-temperate-lakes-lter-high-frequency-data-meteorological-dissolved-oxygen-chlorophyll-p
daily_raw = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-ntl.129.35&entityid=cba9ed12834b8f315d6b10675bb60c5a")

# make sure data values are NA when flagged, preserving as many points as possible
daily_long = daily_raw %>% 
  pivot_longer(cols = starts_with("avg"), names_prefix = "avg_", names_to = "variable", values_to = "avg") %>% 
  pivot_longer(cols = starts_with("flag"), names_prefix = "flag_avg_", names_to = "var_flag", values_to = "flag") %>% 
  filter(variable == var_flag) %>% 
  mutate(avg = case_when(flag != "" ~ as.numeric(NA),
                         T ~ avg)) %>%  
  select(-var_flag, -flag) %>% 
  filter(!is.na(avg))

#visualize data to check
ggplot(daily_long) +
  geom_point(aes(x = sampledate, y = avg))+
  facet_wrap(~variable, scales = "free")

#recreate wide table
daily_buoy = daily_long %>% 
  pivot_wider(names_from = variable, values_from = avg, names_prefix = "avg_") 

buoy <- daily_buoy %>%
  mutate(doy = yday(sampledate)) %>% # add column for doy
  rename("year" = year4) # rename "year" column
buoy$year <- as.factor(buoy$year) # make "year" a factor for easier plotting

#----------------------------------------------------------------
# Temperature correction for chlorophyll-a, phycocyanin, and fDOM
#----------------------------------------------------------------
# Visualize temperatures across dates
ggplot(buoy, aes(x = doy, y = avg_do_wtemp)) +
  geom_point() +
  facet_wrap(~year, ncol = 4)

# Add new columns for the temperature-corrected chlorophyll-a, phycocyanin, and fDOM values
rho = -0.01 # value determined from Watras et al. 2011 and Watras et al. 2017
Tr = 20 # reference T, 20C

buoy_corrected <- buoy %>%
  mutate(avg_chlor_rfu_corr = avg_chlor_rfu/(1+rho*(avg_do_wtemp-Tr)), # chlorophyll
         avg_phyco_rfu_corr = avg_phyco_rfu/(1+rho*(avg_do_wtemp-Tr)), # phycocyanin
         avg_fdom_corr = avg_fdom/(1+rho*(avg_do_wtemp-Tr))) # fdom

## Fourth, evaluate the temperature correction and how it looks like it worked

# chlorophyll
p1 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_chlor_rfu, avg_chlor_rfu_corr)) %>%
  pivot_longer(avg_chlor_rfu:avg_chlor_rfu_corr, names_to = "parameter") %>%
  filter(year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 4, scales = "free_y")
#ggsave("Figures/temp_corr/chl_temp_corr.png", p1, width = 12, height = 8, units = 'in', device = "png", dpi=100)

# phycocyanin
p2 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_phyco_rfu, avg_phyco_rfu_corr)) %>%
  pivot_longer(avg_phyco_rfu:avg_phyco_rfu_corr, names_to = "parameter") %>%
  filter(year %in% c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 4, scales = "free_y")
#ggsave("Figures/temp_corr/phyco_temp_corr.png", p2, width = 12, height = 8, units = 'in', device = "png", dpi=100)

# fDOM
p3 <- buoy_corrected %>%
  select(c(year, doy, sampledate, avg_fdom, avg_fdom_corr)) %>%
  pivot_longer(avg_fdom:avg_fdom_corr, names_to = "parameter") %>%
  filter(year %in% c(2019, 2020, 2021, 2022)) %>%
  ggplot(aes(x = doy, y = value, color = parameter)) +
  geom_point() +
  facet_wrap(~year, ncol = 1, scales = "free_y")
#ggsave("Figures/temp_corr/fdom_temp_corr.png", p3, width = 12, height = 8, units = 'in', device = "png", dpi=100)

#----------------------------------------------------------------
# Outlier detection and removal
#----------------------------------------------------------------

buoy <- buoy_corrected 

# Plot our four variables of interest across the years
buoy %>% 
  pivot_longer(avg_chlor_rfu:avg_spec_cond, names_to = "parameter") %>% 
  filter(parameter %in% c("avg_chlor_rfu", "avg_phyco_rfu", "avg_fdom", "avg_turbidity")) %>%
  filter(year %in% c("2019", "2020", "2021", "2022")) %>% # just look at the last four years of data
  ggplot(aes(x = sampledate, y = value)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

# Remove outlier fDOM points and create cleaned version of the file
buoy_clean <- buoy
buoy_clean$avg_fdom_corr[buoy_clean$avg_fdom_corr <= 3] <- NA # remove the three fDOM outliers
# make negatives zero
buoy_clean$avg_phyco_rfu_corr[buoy_clean$avg_phyco_rfu_corr < 0] <- 0 
buoy_clean$avg_chlor_rfu_corr[buoy_clean$avg_chlor_rfu_corr < 0] <- 0 

# Remove outlier fDOM points and create cleaned version of the file
buoy_clean <- buoy
buoy_clean$avg_fdom[buoy_clean$avg_fdom <= 3] <- NA # remove the three fDOM outliers
# make negatives zero
buoy_clean$avg_phyco_rfu[buoy_clean$avg_phyco_rfu < 0] <- 0 
buoy_clean$avg_chlor_rfu[buoy_clean$avg_chlor_rfu < 0] <- 0 

buoy_clean <- buoy_clean %>% 
  select(c(year, sampledate, doy, avg_chlor_rfu, avg_phyco_rfu, avg_fdom, avg_turbidity)) %>%
  subset(year %in% c(2019, 2020, 2021, 2022))
buoy_clean %>% 
  pivot_longer(avg_chlor_rfu:avg_turbidity, names_to = "parameter") %>% 
  filter(parameter %in% c("avg_chlor_rfu", "avg_phyco_rfu", "avg_fdom", "avg_turbidity")) %>%
  ggplot(aes(x = sampledate, y = value)) +
  geom_point() +
  facet_wrap(~parameter, scales = "free_y", ncol = 1)

write.csv(buoy_clean, "Data/Buoy/daily_buoy_tempcorr_clean.csv")

write.csv(buoy_clean, "Data/Buoy/daily_buoy_clean.csv")
