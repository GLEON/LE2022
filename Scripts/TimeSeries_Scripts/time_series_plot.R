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
ice<- ice %>%
  mutate(year = year(Date)) %>%
  mutate(doy = yday(Date))

metabolism_ice <- merge(metabolism, ice, by = c("year", "doy"))
metabolism_filter<- metabolism_ice %>%
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

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiNPP_mgC_L, EpiR_mgC_L, Cyanobacteria_rel_abund_16S, avg_phyco_rfu, avg_chlor_rfu, Red.RedEdge1.r, ST_B10) %>%
  pivot_longer(EpiNPP_mgC_L:ST_B10, names_to = "variable", values_to = "value")
    
### Time series plot ###

# Preliminary plot set-up
facet_names <- as_labeller(
  c(avg_chlor_rfu = "Average surface chlorophyll RFU", 
    avg_phyco_rfu = "Average surface phycocyanin RFU",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria",
    EpiNPP_mgC_L = "Epilimnion Net Primary Production (NPP) (mgC/L)",
    EpiR_mgC_L = "Epilimnion Respiration (R) (mgC/L)", 
    Red.RedEdge1.r = "Red.RedEdge1.r",
    ST_B10 = "ST_B10"))
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
  
# Create the plot
timeseries <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 0.75) +
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  scale_color_viridis_d() +
  ylab("Value") +
  xlab("Day of year") +
  theme_bw(base_size = 17) +
  labs(color = "Season") +
  guides(color = guide_legend(override.aes = list(size = 3.5))) +
  theme(panel.grid.major = element_blank(),
        legend.title = element_text(size = 17),
        legend.text = element_text(size = 17))

#ggsave("Figures/time_series/time_series_plot_v1.png", timeseries, width = 13, height = 12, units = 'in', device = "png")





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