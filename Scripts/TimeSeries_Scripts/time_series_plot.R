# Creation of time series figure 
# July 2023, edited June 2024, August 2024
# AGS (build off of code from Adrianna)

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
  dplyr::rename("year" = year4)
# There are two really high phycocyanin outliers that we want to remove (they're above 20000 RFU) so we can better visualize the trends on this figure. We are going to replace those values with NA.
buoy$avg_phyco_rfu[buoy$avg_phyco_rfu >= 20000] <- NA

cyan <- read_csv('Data/CyAN/Mendota_Daily.csv')
cyan <- cyan %>%
  mutate(doy = yday(ImageDater)) %>%
  mutate(year = year(ImageDater))

# Add in the season boundaries by first creating an empty data frame of all possible dates
empty <- data.frame(date = seq(as.Date("01-01-2000", format = "%m-%d-%Y"), as.Date("12-31-2022", format = "%m-%d-%Y"), by = "day")) %>%
  mutate(doy = yday(date),
         year = year(date))
seasons <- read.csv('MetabolismModel/seasons_from_Robins_paper_long_withplaceholders.csv') %>%
  select(-placeholder) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"))
DATA <- left_join(empty, seasons) %>%
  fill(season, .direction="down")

# write csv for Bryan for the RF analysis
# write.csv(DATA, "dates_seasons.csv", row.names = F, quote=T)

# Join all of the data together into one dataframe
DATA <- left_join(DATA, metabolism_filter, by = c("year", "doy"))
DATA <- left_join(DATA, microbes, by = c("year", "doy"))
DATA <- left_join(DATA, rs, by = c("year", "doy"))
DATA <- left_join(DATA, landsat, by = c("year", "doy"))
DATA <- left_join(DATA, buoy, by = c("year", "doy"))
DATA <- left_join(DATA, cyan, by = c("year", "doy"))

# Add a column for NEP (NPP-R)
DATA <- DATA %>%
  mutate(EpiNEP_mgC_L = EpiNPP_mgC_L-EpiR_mgC_L)
# didn't end up including in the plot because it looked a lot like GPP/NPP

##################################################
### Time series plot - 2023 version - 1 column ###

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiR_mgC_L, EpiNPP_mgC_L, Cyanobacteria_rel_abund_16S, scaled_phyco_rfu, scaled_chlor_rfu, Red.RedEdge1.r, ST_B10, MaxCA) %>%
  pivot_longer(EpiR_mgC_L:MaxCA, names_to = "variable", values_to = "value")

# Fix order of facets
DATA_long$variable <- factor(DATA_long$variable, levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", 
                                                            "scaled_chlor_rfu", "scaled_phyco_rfu", "Cyanobacteria_rel_abund_16S", "MaxCA", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(scaled_chlor_rfu = "Average surface chlorophyll fluorescence (relative fluorescence units)", 
    scaled_phyco_rfu = "Average surface phycocyanin fluorescence (relative fluorescence units)",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Modeled epilimnion Gross Primary Production (GPP) (mgC/L/d)",
    EpiR_mgC_L = "Modeled epilimnion Respiration (R) (mgC/L/d)", 
    MaxCA = "Maximum Cyanobacterial abundance from CyAN (cells/mL)",
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)"))

# Fix order of seasons in legend
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

# Create the plot
timeseries <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 2.2) +
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  #scale_color_viridis_d() +
  #scale_color_manual(values=c("#999999", "#440154", "#3b528b","#21918c","#5ec962","#fde725")) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ylab("Value") +
  xlab("Day of year") +
  theme_bw(base_size = 32) +
  labs(color = "Season") +
  guides(color = guide_legend(override.aes = list(size = 5.5))) +
  theme(panel.grid.major = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 22),
        legend.text = element_text(size = 33),
        legend.title = element_text(size = 33),
        strip.text = element_text(face = "bold"))

# Save the plot
ggsave("Figures/time_series/time_series_plot_v7.png", timeseries, width = 19, height = 23, units = 'in', device = "png", dpi=1000)

##################################################
### Time series plot - 2023 version - 2 column ###

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiR_mgC_L, EpiNPP_mgC_L, Cyanobacteria_rel_abund_16S, avg_phyco_rfu, Red.RedEdge1.r, ST_B10) %>%
  pivot_longer(EpiR_mgC_L:ST_B10, names_to = "variable", values_to = "value")

# Fix order of facets
DATA_long$variable <- factor(DATA_long$variable, levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", 
                                                            "avg_phyco_rfu", "Cyanobacteria_rel_abund_16S", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(avg_phyco_rfu = "Average surface phycocyanin fluorescence (RFUs)",
    Cyanobacteria_rel_abund_16S = "Relative abund. of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Epilimnion Gross Primary Production (GPP) (mgC/L/d)",
    EpiR_mgC_L = "Epilimnion Respiration (R) (mgC/L/d)", 
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)"))

# Fix order of seasons in legend
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

# Create the plot
timeseries <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 2) +
  facet_wrap(.~variable, ncol = 2, scales = "free_y", labeller = facet_names) +
  #scale_color_viridis_d() +
  #scale_color_manual(values=c("#999999", "#440154", "#3b528b","#21918c","#5ec962","#fde725")) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ylab("Value") +
  xlab("Day of year") +
  theme_bw(base_size = 27) +
  labs(color = "Season") +
  guides(color = guide_legend(override.aes = list(size = 5.5))) +
  theme(panel.grid.major = element_blank(),
        legend.position = "right",
        axis.text.y = element_text(size = 22),
        legend.text = element_text(size = 22),
        legend.title = element_text(size = 22),
        strip.text = element_text(face = "bold", size = 23))

# Save the plot
ggsave("Figures/time_series/time_series_plot_presentation_v1.png", timeseries, width = 23, height = 9, units = 'in', device = "png", dpi=1000)

##################################################
### Time series plot - 2024 version - 1 column ###

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiR_mgC_L, EpiNPP_mgC_L, Cyanobacteria_rel_abund_16S, scaled_phyco_rfu, scaled_chlor_rfu, Red.RedEdge1.r, ST_B10, avg_fdom, avg_turbidity) %>%
  pivot_longer(EpiR_mgC_L:avg_turbidity, names_to = "variable", values_to = "value")

# Fix order of facets
DATA_long$variable <- factor(DATA_long$variable, levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", 
                                                            "scaled_chlor_rfu", "scaled_phyco_rfu", "avg_fdom", "avg_turbidity", "Cyanobacteria_rel_abund_16S", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(scaled_chlor_rfu = "Average surface chlorophyll fluorescence (relative fluorescence units)", 
    scaled_phyco_rfu = "Average surface phycocyanin fluorescence (relative fluorescence units)",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Modeled epilimnion Gross Primary Production (GPP) (mgC/L/d)",
    EpiR_mgC_L = "Modeled epilimnion Respiration (R) (mgC/L/d)", 
    avg_fdom = "Average surface fDOM (units?)",
    avg_turbidity = "Average surface turbidity (units?)",
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)"))

# Fix order of seasons in legend
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
DATA_long$year <- as.character(DATA_long$year)
# Create the plot
timeseries <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 2.2) +
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  #scale_color_viridis_d() +
  #scale_color_manual(values=c("#999999", "#440154", "#3b528b","#21918c","#5ec962","#fde725")) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ylab("Value") +
  xlab("Day of year") +
  theme_bw(base_size = 32) +
  labs(color = "Season") +
  guides(color = guide_legend(override.aes = list(size = 5.5))) +
  theme(panel.grid.major = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 22),
        legend.text = element_text(size = 33),
        legend.title = element_text(size = 33),
        strip.text = element_text(face = "bold"))

# Save the plot
ggsave("Figures/time_series/time_series_plot_v8.png", timeseries, width = 19, height = 23, units = 'in', device = "png", dpi=500)

# Note: fDOM and turbidity sensors were only on the buoy from 2019-2022 so they have less data on this figure than the other buoy parameters. 
# Need to look into why the fDOM magnitude seems to differ from year-to-year - might have to do with the temperature correction that we still need to do on the fluorescence data - wait and hear about that correction - and the chlorophyll and phycocyanin fluorescence facets will also change once we do the temperature correction