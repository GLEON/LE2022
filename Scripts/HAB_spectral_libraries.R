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