# Re-making correlations between CyAN metrics and buoy phycocyanin, 
# using our most updated buoy data (has been temp corrected and outliers have been removed)
# November 2024
# AGS

# needs to be cleaned up

pacman::p_load(tidyverse, patchwork, lubridate)

# Load in CyAN data
cyan <- read_csv("Data/CyAN/Mendota_Daily.csv")
cyan <- cyan %>%
  mutate(doy = yday(ImageDater)) %>%
  mutate(year = year(ImageDater)) %>%
  subset(year < 2023)

# Load in buoy data
buoy <- read_csv('Data/Buoy/daily_buoy_tempcorr_clean.csv')
buoy <- buoy %>%
  mutate(doy = yday(sampledate)) %>%
  dplyr::rename("year" = year)

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
DATA <- left_join(DATA, buoy, by = c("year", "doy"))
cyan <- left_join(DATA, cyan, by = c("year", "doy"))

cyan$season <- factor(cyan$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

# CA and CI are a 1:1 correlation
ggplot(cyan) + geom_point(aes(x = MaxCI, y = MaxCA))
ggplot(cyan) + geom_point(aes(x = AvgCI, y = AvgCA))

#subset <- cyan %>% filter(NApercent <=50)
sorted <- cyan %>% dplyr::arrange(desc(MaxCA))

# Max CA acros doy
ggplot(cyan) +
  geom_point(aes(x = doy, y = MaxCA, color = season), size = 1.8) +
  #facet_wrap(~year, ncol = 2) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  xlab("Day of year") +
  ylab("Max Cyanobacterial Abundance (cells/mL)") +
  theme_bw(base_size = 16)

# Avg CA across doy
ggplot(cyan) +
  geom_point(aes(x = doy, y = AvgCA, color = season), size = 1.8) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  xlab("Day of year") +
  ylab("Avg Cyanobacterial Abundance (cells/mL)") +
  theme_bw(base_size = 16)

cyan$avg_phyco_rfu_corr[cyan$avg_phyco_rfu_corr > 6] <- NA

# Correlations with buoy phycocyanin - all seasons
ggplot(cyan) +
  geom_point(aes(x = AvgCA, y = avg_phyco_rfu_corr, color = season), size = 1.8) +
  scale_color_manual(values=c("#999999", "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  theme_bw(base_size = 16) +
  facet_wrap(~season)

# why are there no spring data points? - the CyAN dataset doesn't seem to have any spring readings

# Just focusing on early summer, late summer, and fall
cyan_subset <- cyan %>% subset(season %in% c("Early Summer", "Late Summer", "Fall"))

# Correlations with buoy phycocyanin - only early summer, late summer, and fall
ggplot(cyan_subset) +
  geom_point(aes(x = MaxCA, y = avg_phyco_rfu_corr, color = season), size = 1.8) +
  geom_smooth(aes(x = MaxCA, y = avg_phyco_rfu_corr, color = season)) +
  theme_bw(base_size = 16) +
  facet_wrap(~season, ncol = 1, scales = "free_x")