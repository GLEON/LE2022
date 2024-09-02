# Looking into HAB spectral libraries from USGS
# https://www.sciencebase.gov/catalog/item/64e372b9d34e5f6cd5549f37
# July 2024
# AGS

# Load packages
library(tidyverse)

# Load in the sample mastersheet
mastersheet <- read.csv("Data/spectral_libraries/Hyperspectralpr/HABs2022_SampleMastersheet.csv")

# Create list of all the individual spectral curve data files (314 total)
algal_files <- list.files(path = "Data/spectral_libraries/Hyperspectralpr/HABs2022_DataTables_Algal", 
                         pattern = ".csv", full.names = TRUE)

# Combine into one data frame
algal_data <- algal_files %>%
  setNames(nm = .) %>% # getting the file names from the list of files
  map_df(~read.csv(.x, sep = ","), .id = "file_name") %>%
  mutate(Spectrum_ID_Algal = paste0(ROI, "_Algal")) # add "_Algal" to the end of the ID to facilitate joining with the mastersheet

# Join with the mastersheet
algal_data_master <- left_join(algal_data, mastersheet, by = "Spectrum_ID_Algal")

# Overview of all spectral curves
ggplot(algal_data_master,
       aes(x = Wavelength..nm., y = Mean.reflectance, col = Morphological.ID)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~Morphological.ID)

# Seems like they are all pretty similar, not sure how accurately I'd be able to tell them apart in our data, especially because we only have in-situ microbial and phytoplankton data close to the AVIRIS scenes and not the DESIS or PRISMA scenes. Are the y-axis differences meaningful or just the differences in shape?

# What is the difference between Mean.reflectance and Normalized.mean.reflectance? There is also Maximum.reflectance and Minimum.reflectance, as well as Standard.deviation.of.reflectance. Look into the metadata to figure this out.

# Let's look at a single spectrum to try to assess which metric we should be using
algal_example <- algal_data_master %>% 
  filter(Spectrum_ID_Algal == "Spectrum_0003_Algal") %>%
  pivot_longer(cols = c(Mean.reflectance, Normalized.mean.reflectance, 
                        Maximum.reflectance, Minimum.reflectance,
                        Standard.deviation.of.reflectance),
               names_to = "metric",
               values_to = "reflectance")
ggplot(algal_example,
       aes(x = Wavelength..nm., y = reflectance, col = metric)) +
  geom_point(alpha = 0.4)

# Looks like Mean.reflectance is the best one to use

############################################################################################################
# SLS
# I think Normalized.mean.reflectance is easier to compare - you can see the peaks and troughs easiest

# Plot averages
algal_data_master %>% 
  filter(!is.na(Morphological.ID)) %>%
  group_by(Wavelength..nm., Morphological.ID) %>%
  mutate(average.ref = mean(Normalized.mean.reflectance)) %>%
  ggplot(aes(x = Wavelength..nm., y = average.ref, col = Morphological.ID)) +
  geom_line() +
  facet_wrap(~Morphological.ID)

# Group by Division
# **Anna let me know if there are better groupings that make more sense for these phytoplankton
algal_data_master <- algal_data_master %>%
  mutate(Division = case_when(
    Morphological.ID == "Cymbella" |
      Morphological.ID == "Epithemia" |
      Morphological.ID == "Gomphoneis herculeana" ~ "Bacillariophyta",
    Morphological.ID == "Closterium" |
      Morphological.ID == "Mougeotia" |
      Morphological.ID == "Spirogyra" |
      Morphological.ID == "Zygnema" ~ "Charophyta",
    Morphological.ID == "Botryococcus" |
      Morphological.ID == "Cladophora" |
      Morphological.ID == "Draparnaldia" |
      Morphological.ID == "Green algae" |
      Morphological.ID == "Microspora" |
      Morphological.ID == "Oedogonium" |
      Morphological.ID == "Prasiola" |
      Morphological.ID == "Ulothrix" ~ "Chlorophyta",
    Morphological.ID == "Tribonema" ~ "Chromophyta",
    Morphological.ID == "Aphanizomenon" |
      Morphological.ID == "Dolichospermum" |
      Morphological.ID == "Gloeotrichia" |
      Morphological.ID == "Microcystis" |
      Morphological.ID == "Nostoc" |
      Morphological.ID == "Nostoc Spongiforme" |
      Morphological.ID == "Oscillatoria" |
      Morphological.ID == "Phormidium" |
      Morphological.ID == "Pseudoanabanea" ~ "Cyanobacteria"))

colorBlindGrey8   <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                       "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#661100")

# Plot Divisions
algal_data_master %>% 
  filter(!is.na(Morphological.ID)) %>%
  filter(Division == "Cyanobacteria") %>%
  # filter(Division == "Bacillariophyta") %>%
  # filter(Division == "Chlorophyta") %>%
  group_by(Wavelength..nm., Morphological.ID) %>%
  mutate(average.ref = mean(Normalized.mean.reflectance)) %>%
  ggplot(aes(x = Wavelength..nm., y = average.ref, col = Morphological.ID)) +
  geom_line(size = 1) +
  scale_color_manual(values = colorBlindGrey8) +
  facet_wrap(~Division) +
  xlim(c(450,700)) +
  # ylim(c(0.05,0.8)) +
  theme_bw() +
  xlab("Wavelength (nm)") +
  ylab("Reflectance") 
  

# ALSO look at water spectra to see difference with algae spectra
# not sure what the difference between water and algal spectra from metadata but 
# spectra are different
# algal spectra seem more useful
water_files <- list.files(path = "Data/spectral_libraries/Hyperspectralpr/HABs2022_DataTables_Water", 
                          pattern = ".csv", full.names = TRUE)
water_data <- water_files %>%
  setNames(nm = .) %>% # getting the file names from the list of files
  map_df(~read.csv(.x, sep = ","), .id = "file_name") %>%
  mutate(Spectrum_ID_Water = ROI) # facilitate joining with the mastersheet
water_data_master <- left_join(water_data, mastersheet, by = "Spectrum_ID_Water")

ggplot(water_data_master,
       aes(x = Wavelength..nm., y = Mean.reflectance, col = Morphological.ID)) +
  geom_point(alpha = 0.4) +
  facet_wrap(~Morphological.ID)

water_data_master %>% 
  filter(!is.na(Morphological.ID)) %>%
  group_by(Wavelength..nm., Morphological.ID) %>%
  mutate(average.ref = mean(Mean.reflectance)) %>%
  ggplot(aes(x = Wavelength..nm., y = average.ref, col = Morphological.ID)) +
  geom_line() +
  facet_wrap(~Morphological.ID)

############################################################################################################
# AGS

# I agree that the Normalized.mean.reflectance is best for visualizing the peaks and valleys of the spectra, so let's proceed using that. 
# I agree that the Water spectra don't seem as informative as the Algal spectra.
# I think the algal groupings make sense!

# Quick visualization of how the spectra generally differ between and among the groupings we chose
algal_data_master %>% 
  filter(!is.na(Morphological.ID)) %>%
  group_by(Wavelength..nm., Morphological.ID) %>%
  mutate(average.ref = mean(Normalized.mean.reflectance)) %>%
  ggplot(aes(x = Wavelength..nm., y = average.ref, col = Morphological.ID)) +
  geom_line(size = 1) +
  facet_wrap(~Division) +
  xlim(c(450,700)) +
  # ylim(c(0.05,0.8)) +
  theme_bw() +
  xlab("Wavelength (nm)") +
  ylab("Reflectance") 

# Some spectral patterns I notice right away:
# Cyanobacteria and Bacillariophyta have that extra peak around 650 nm, others don't
# Chlorophyta, Charophyta, and Chromophyta have a steeper increase from 450-550 nm compared to Cyanobacteria and Baillariophyta
# All have a valley around 675 nm