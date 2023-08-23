# Creation of time series figure for ESA poster
# July 2023
# AGS (build off of code from Adrianna)

pacman::p_load(tidyverse, patchwork, lubridate)

### Load in all of the data and make sure they all have doy and year columns ###

metabolism <- read_csv('Data/Metabolism_Model_Output/SimResultsMatrix_MetabData_run12jan23.csv')
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

# Add a column for NEP (NPP-R)
DATA <- DATA %>%
  mutate(EpiNEP_mgC_L = EpiNPP_mgC_L-EpiR_mgC_L)

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiNEP_mgC_L, EpiR_mgC_L, EpiNPP_mgC_L, Cyanobacteria_rel_abund_16S, avg_phyco_rfu, avg_chlor_rfu, Red.RedEdge1.r, ST_B10) %>%
  pivot_longer(EpiNEP_mgC_L:ST_B10, names_to = "variable", values_to = "value")
    
### Time series plot ###

## Preliminary plot set-up

# Fix order of facets
DATA_long$variable <- factor(DATA_long$variable, levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", "EpiNEP_mgC_L",
                                                            "avg_chlor_rfu", "avg_phyco_rfu", "Cyanobacteria_rel_abund_16S", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(avg_chlor_rfu = "Average surface chlorophyll fluorescence (RFU)", 
    avg_phyco_rfu = "Average surface phycocyanin fluorescence (RFU)",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Modeled epilimnion Net Primary Production (NPP) (mgC/L/d)",
    EpiR_mgC_L = "Modeled epilimnion Respiration (R) (mgC/L/d)", 
    EpiNEP_mgC_L = "Modeled epilimnion Net Ecosystem Production (NEP) (mgC/L/d)",
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)"))

# Fix order of seasons in legend
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

## Create the plot
timeseries <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 1) +
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  #scale_color_viridis_d() +
  scale_color_manual(values=c("#999999", "#440154", "#3b528b","#21918c","#5ec962","#fde725")) +
  ylab("Value") +
  xlab("Day of year") +
  theme_bw(base_size = 19) +
  labs(color = "Season") +
  guides(color = guide_legend(override.aes = list(size = 4))) +
  theme(panel.grid.major = element_blank(),
        legend.title = element_text(size = 19),
        legend.text = element_text(size = 19))
  

#ggsave("Figures/time_series/time_series_plot_v2.png", timeseries, width = 13, height = 14, units = 'in', device = "png")





# Old code using patchwork instead of faceting
 
npp <- ggplot(metabolism_filter)+
    geom_point(aes(x = doy, y = EpiNPP_mgC_L, color = year), size = 0.5)+
  scale_color_viridis_c() +
    ylab("NPP (mgC/L/day)")+
    xlab("")+
    theme_bw()

resp <- ggplot(metabolism_filter)+
  geom_point(aes(x = doy, y = EpiR_mgC_L, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("R (mgC/L/day)")+
  xlab("")+
  theme_bw()

cyano <-ggplot(microbes) +
  geom_point(aes(x = doy, y = Cyanobacteria_rel_abund_16S, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("Cyanobacterial rel. abund. (%)")+
  xlab("")+
  theme_bw()

rededge <- ggplot()+
  geom_point(data = rs, aes(x = doy, y = Red.RedEdge1.r, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("Sentinel Red Band Ratio")+
  xlab("")+
  theme_bw()

tempband <- ggplot()+
  geom_point(data = landsat, aes(x = doy, y = ST_B10, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("Landsat Thermal Band")+
  xlab("")+
  theme_bw()

phycocyanin <- ggplot()+
  geom_point(data = buoy, aes(x = doy, y = avg_phyco_rfu, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("Buoy Phycocyanin (RFU)")+
  xlab("")+
  theme_bw()

chla <- ggplot()+
  geom_point(data = buoy, aes(x = doy, y = avg_chlor_rfu, color = year), size = 0.5)+
  scale_color_viridis_c() +
  ylab("Buoy Chlorophyll-a (RFU)")+
  xlab("Day of year")+
  theme_bw()

npp/resp/cyano/phycocyanin/chla/rededge/tempband

#ggsave("figures/doy.png", width = 10, height = 12, units = 'in')