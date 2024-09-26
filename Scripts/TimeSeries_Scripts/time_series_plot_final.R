# Creation of time series figure 
# July 2023, edited June 2024, August 2024
# AGS (build off of code from Adrianna)
# Final time series plot for paper

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
#rs$Red.RedEdge1.r[rs$Red.RedEdge1.r >= 1.2] <- NA
#rs$Red.RedEdge1.r[rs$Red.RedEdge1.r <= 0.6] <- NA

landsat <- read_csv('Data/Landsat/LS8_Buoy100m.csv')
landsat <- landsat %>%
  mutate(doy = yday(DATE_ACQUIRED),
         year = year(DATE_ACQUIRED))

buoy <- read_csv('Data/Buoy/daily_buoy_tempcorr_clean.csv')

buoy <- buoy %>%
  mutate(doy = yday(sampledate)) %>%
  dplyr::rename("year" = year)

# There are two really high phycocyanin outliers that we want to remove (they're above 20000 RFU) so we can better visualize the trends on this figure. We are going to replace those values with NA.
#buoy$avg_phyco_rfu[buoy$avg_phyco_rfu >= 20000] <- NA

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

# ADD a facet with the hyperspectral dates
# I want a vertical line (can make it thick) in a new panel to represent the hyperspectral scene dates, these dates are already in the dataframe as the date column, but I want to just show the 15 hyperspectral scene dates as lines colored by the season they fall into. So I should make another column for these other dates, perhaps? date_hyper?

str(DATA)

DATA <- DATA %>%
  mutate(date_hyper = case_when(date == "2020-06-02" ~ 2020,
                                date == "2020-12-25" ~ 2020,
                                date == "2021-06-11" ~ 2021,
                                date == "2021-06-15" ~ 2021,
                                date == "2021-08-15" ~ 2021,
                                date == "2021-09-05" ~ 2021,
                                date == "2021-11-02" ~ 2021,
                                date == "2021-12-13" ~ 2021,
                                date == "2022-05-13" ~ 2022,
                                date == "2022-06-02" ~ 2022,
                                date == "2022-06-16" ~ 2022,
                                date == "2022-06-23" ~ 2022,
                                date == "2022-07-27" ~ 2022,
                                date == "2022-08-31" ~ 2022,
                                date == "2022-10-10" ~ 2022),
         date_hyper_doy = case_when(date == "2020-06-02" ~ 154,
                                date == "2020-12-25" ~ 360,
                                date == "2021-06-11" ~ 162,
                                date == "2021-06-15" ~ 166,
                                date == "2021-08-15" ~ 227,
                                date == "2021-09-05" ~ 248,
                                date == "2021-11-02" ~ 306,
                                date == "2021-12-13" ~ 347,
                                date == "2022-05-13" ~ 133,
                                date == "2022-06-02" ~ 153,
                                date == "2022-06-16" ~ 167,
                                date == "2022-06-23" ~ 174,
                                date == "2022-07-27" ~ 208,
                                date == "2022-08-31" ~ 243,
                                date == "2022-10-10" ~ 283))

# CONSTRAINING Y-AXES

DATA$avg_turbidity[DATA$avg_turbidity > 20] <- NA
DATA$avg_phyco_rfu_corr[DATA$avg_phyco_rfu_corr > 6] <- NA
DATA$avg_chlor_rfu_corr[DATA$avg_chlor_rfu_corr > 10] <- NA

##################################################
### Time series plot - 2024 version - 1 column ###

# Remove unnecessary columns and make long format for plotting
DATA_long <- DATA %>% 
  select(doy, year, season, EpiR_mgC_L, EpiNPP_mgC_L, Cyanobacteria_rel_abund_16S, avg_phyco_rfu_corr, avg_chlor_rfu_corr, Red.RedEdge1.r, ST_B10, avg_fdom_corr, avg_turbidity) %>%
  pivot_longer(EpiR_mgC_L:avg_turbidity, names_to = "variable", values_to = "value")

# Fix order of facets
DATA_long$variable <- factor(DATA_long$variable, levels = c("EpiNPP_mgC_L", "EpiR_mgC_L", 
                                                            "avg_chlor_rfu_corr", "avg_phyco_rfu_corr", "avg_fdom_corr", "avg_turbidity", "Cyanobacteria_rel_abund_16S", "Red.RedEdge1.r", "ST_B10"))

# Rename columns to more descriptive facet labels
facet_names <- as_labeller(
  c(avg_chlor_rfu_corr = "Average surface chlorophyll fluorescence (Relative Fluorescence Units)", 
    avg_phyco_rfu_corr = "Average surface phycocyanin fluorescence (Relative Fluorescence Units)",
    Cyanobacteria_rel_abund_16S = "Relative abundance of Cyanobacteria in 16S data (%)",
    EpiNPP_mgC_L = "Epilimnion Net Primary Production (NPP) (mgC/L/d)",
    EpiR_mgC_L = "Epilimnion Respiration (R) (mgC/L/d)", 
    avg_fdom_corr = "Average surface fDOM (Relative Fluorescence Units)",
    avg_turbidity = "Average surface turbidity (Formazin Nephelometric Units)",
    Red.RedEdge1.r = "Sentinel-2 Red/Red-Edge 1 ratio",
    ST_B10 = "Landsat 8 surface temperature (Kelvin)",
    date_hyper = "Dates of hyperspectral scenes"))

# Remove ice-on dates
DATA_long <- DATA_long %>%
  filter(season != "Ice-on")

# Fix order of seasons in legend
DATA_long$season <- factor(DATA_long$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
DATA_long$season <- factor(DATA_long$season, levels = c("Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
DATA_long$year <- as.character(DATA_long$year)

# Remove DOY before 50
DATA_long <- DATA_long %>%
  filter(doy >= 50)

# Create the plot
p1 <- ggplot(DATA_long) +
  geom_point(aes(x = doy, y = value, color = season), size = 1.6) + 
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  #scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  #ylab("Value") +
  ylab("") +
  xlab("Day of year") +
  labs(color = "Lake season") +
  theme_bw(base_size = 32) +
  xlim(c(60, 365)) +
  guides(color = guide_legend(override.aes = list(size = 5.5))) +
  theme(panel.grid.major = element_blank(),
        legend.position = "top",
        axis.text.y = element_text(size = 22),
        legend.text = element_text(size = 33),
        legend.title = element_text(size = 33),
        strip.text = element_text(face = "bold")) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) 
  
# tag_facet(p1, x = -Inf, y = -Inf, 
#             vjust = 0,
#             open = "(", close = ")",
#             tag_pool = LETTERS)

# Subplot of just hyperspectral data
DATA_long_hyper <- DATA %>% 
  select(doy, season, date_hyper) %>%
  filter(date_hyper %in% c("2020", "2021", "2022"))  %>%
  pivot_longer(date_hyper, names_to = "variable", values_to = "value")

DATA_long_hyper$season <- factor(DATA_long_hyper$season, levels = c("Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
p2 <- ggplot(DATA_long_hyper) +
  geom_vline(aes(xintercept = doy, color = season), linewidth = 1) + 
  facet_wrap(.~variable, ncol = 1, scales = "free_y", labeller = facet_names) +
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ylab("") +
  xlab("Day of year") +
  labs(color = "Lake season") +
  theme_bw(base_size = 32) +
  xlim(c(60, 365)) +
  guides(color = guide_legend(override.aes = list(size = 5.5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.text.y = element_text(size = 22),
        legend.text = element_text(size = 33),
        legend.title = element_text(size = 33),
        strip.text = element_text(face = "bold"))

p3 <- p1 / plot_spacer() / p2 + plot_layout(heights = c(15, -0.35, 1)) 


# Save the plot
ggsave("Figures/time_series/time_series_plot_v11.png", p3, width = 19, height = 25, units = 'in', device = "png", dpi=200)

# Note: fDOM and turbidity sensors were only on the buoy from 2019-2022
# Need to look into why the fDOM magnitude seems to differ from year-to-year - might have to do with the temperature correction that we still need to do on the fluorescence data - wait and hear about that correction