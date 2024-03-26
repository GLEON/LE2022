# Creation of a table of environmental conditions during the hyperspectral scenes
#   right now, just makes a table that matches the hyperspectral dates exactly
#   but could expand to take data from the nearest date
# January 2024
# AGS

pacman::p_load(tidyverse, patchwork, lubridate)

### Load in all of the data and make sure they all have doy and year columns ###

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
  filter(EpiNPP_mgC_L < 0.5)

microbes <- read_csv('Data/Microbial/Cyanobacteria_rel_abund_16S.csv')

rs <- read_csv('Data/Sentinel2/S2_Red_RedEdge1.csv')
rs <- rs %>%
  mutate(doy = yday(Date),
         year = year(Date))
# There are two red edge outliers that we want to remove so we can better visualize the trends on this figure. We are going to replace those values with NA.
rs$Red.RedEdge1.r[rs$Red.RedEdge1.r >= 1.2] <- NA
rs$Red.RedEdge1.r[rs$Red.RedEdge1.r <= 0.6] <- NA

landsat <- read_csv('Data/Landsat/LS8_Buoy100m.csv')
landsat <- landsat %>%
  mutate(doy = yday(DATE_ACQUIRED),
         year = year(DATE_ACQUIRED))

buoy <- read_csv('Data/Buoy/daily_buoy.csv')
buoy <- buoy %>%
  mutate(doy = yday(sampledate)) %>%
  rename("year" = year4)
# There are two really high phycocyanin outliers that we want to remove (they're above 20000 RFU) so we can better visualize the trends on this figure. We are going to replace those values with NA.
buoy$avg_phyco_rfu[buoy$avg_phyco_rfu >= 20000] <- NA

cyan <- read_csv('Data/CyAN/Mendota_Daily.csv')
cyan <- cyan %>%
  mutate(doy = yday(ImageDater)) %>%
  mutate(year = year(ImageDater))

hs_scenes <- read.csv('Data/hs_dates.csv')
hs_scenes <- hs_scenes %>%
  mutate(newdate = mdy(date)) %>%
  mutate(doy = yday(newdate)) %>%
  mutate(year = year(newdate)) %>%
  select(-date)

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
DATA <- left_join(DATA, microbes, by = c("year", "doy"))
DATA <- left_join(DATA, rs, by = c("year", "doy"))
DATA <- left_join(DATA, landsat, by = c("year", "doy"))
DATA <- left_join(DATA, buoy, by = c("year", "doy"))
DATA <- left_join(DATA, cyan, by = c("year", "doy"))
DATA <- left_join(DATA, hs_scenes, by = c("year", "doy"))

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(date, season, EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m, Cyanobacteria_rel_abund_16S, avg_phyco_rfu, scaled_phyco_rfu, avg_chlor_rfu, scaled_chlor_rfu, avg_fdom, avg_turbidity, Red.RedEdge1.r, ST_B10, AvgCA, MaxCA)

hs_scenes <- hs_scenes %>%
  rename(date = newdate)

joined <- left_join(hs_scenes, DATA_long, by = "date") %>%
  mutate(sensorid = case_when(sensorid == 1 ~ "AVIRIS",
                              sensorid == 2 ~ "DESIS",
                              sensorid == 3 ~ "PRISMA")) %>%
  subset(date!="2022-04-14")

#write.csv(joined, "hyperspectral_dates_conditions.csv", row.names = F, quote=T)
#write.csv(joined, "Data/hyperspectral_dates_conditions_v2.csv", row.names = F, quote=T)

# Make figure of the data in this dataframe that looks similar to the time series plot 
#   so we can visualize conditions across hyperspectral dates

joined_long <- joined %>%
  pivot_longer(EpiNPP_mgC_L:MaxCA, names_to = "variable", values_to = "value")

joined_long$date <- as.character(joined_long$date)

# Fix order of facets
joined_long$variable <- factor(joined_long$variable, 
                               levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", "avg_chlor_rfu",
                                          "avg_phyco_rfu",
                                          "Cyanobacteria_rel_abund_16S", 
                                          "AvgCA", "MaxCA", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(avg_phyco_rfu = "Average surface phycocyanin fluorescence (RFUs)",
    avg_chlor_rfu = "Average surface chlorophyll fluorescence (RFUs)",
    AvgCA = "Average Cyanobacterial Abundance (cells/mL)",
    MaxCA = "Maximum Cyanobacterial Abundance (cells/mL)",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Epilimnion Gross Primary Production (GPP) (mgC/L/d)",
    EpiR_mgC_L = "Epilimnion Respiration (R) (mgC/L/d)", 
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)"))

hc <- ggplot(joined_long) +
  geom_point(aes(x = date, y = value, color = season, shape = sensorid), size = 3) +
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ylab("Value") +
  xlab("Hyperspectral scene date") +
  theme_bw(base_size = 15) +
  labs(color = "Season") +
  scale_x_discrete(guide = guide_axis(angle = 45))

#ggsave("hyperspectral_scene_conditions.png", hc, width = 10, height = 10, units = 'in', device = "png", dpi=1000)