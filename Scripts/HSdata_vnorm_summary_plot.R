# Plot what we think are the most updated vector-normalized spectra
# for comparison with the SMASH cyanobacteria spectral libraries
# August 2024
# AGS

# Load packages
library(tidyverse)
library(janitor)

# Load data and plot vector-normalized spectra

# DESIS
DESIS_buoy <- read.csv("Data/DESIS/DESIS_stats_buoy_vnorm.csv") %>%
  dplyr::select(c(date, contains("avg"))) %>%
  t() %>%
  row_to_names(row_number = 1)
DESIS_bands <- read.csv('Data/DESIS/DESIS_Bands.csv')
DESIS_buoy <- cbind(DESIS_bands,DESIS_buoy)
DESIS_buoy_long <- DESIS_buoy %>%
  pivot_longer(!Wavelength_nm,names_to="date",values_to="Rrs")
DESIS_buoy_long$Wavelength_nm <- as.integer(DESIS_buoy_long$Wavelength_nm)
DESIS_buoy_long$Rrs <- as.numeric(DESIS_buoy_long$Rrs)
DESIS_buoy_long_clean <- DESIS_buoy_long[!is.na(as.numeric(DESIS_buoy_long$Rrs)), ] 
DESIS_buoy_long_clean %>%
  ggplot(aes(x=Wavelength_nm,y=Rrs)) +
  geom_line() +
  facet_wrap(~date) +
  theme_bw() +
  xlim(450,800) +
  #ylim(0,0.025) +
  labs(title='DESIS buoy-clipped mean spectra - vector normalized')

ggsave("Data/DESIS/DESIS_buoy_avg_vnorm_spectra.png")

# PRISMA
PRISMA_buoy <- read.csv('Data/PRISMA/PRISMA_stats_buoy.csv')
PRISMA_buoy <- PRISMA_buoy[2:8,]  
PRISMA_buoy <- PRISMA_buoy %>%
  dplyr::select(c(date, contains("avg"))) %>%
  t() %>%
  row_to_names(row_number = 1)
PRISMA_bands <- read.csv('Data/PRISMA/PRISMA_Bands.csv')
PRISMA_buoy <- cbind(PRISMA_bands,PRISMA_buoy)
PRISMA_buoy_long <- PRISMA_buoy %>%
  pivot_longer(!Wavelength_nm,names_to="date",values_to="Rrs")
PRISMA_buoy_long$Wavelength_nm <- as.integer(PRISMA_buoy_long$Wavelength_nm)
PRISMA_buoy_long$Rrs <- as.numeric(PRISMA_buoy_long$Rrs)
PRISMA_buoy_long_clean <- PRISMA_buoy_long[!is.na(as.numeric(PRISMA_buoy_long$Rrs)), ] 
PRISMA_buoy_long_clean %>%
  ggplot(aes(x=Wavelength_nm,y=Rrs)) +
  geom_line() +
  facet_wrap(~date) +
  theme_bw() +
  xlim(450,800) +
  #ylim(0,0.025) +
  labs(title='PRISMA buoy-clipped mean spectra - vector normalized')
# Data from 2021_06_05 isn't in this file and it was visualized in the non-vector normalized spectra..where did that data go?

ggsave("Data/PRISMA/PRISMA_buoy_avg_vnorm_spectra.png")

# AVIRIS
# sounds like the vector-normalized AVIRIS data needs to be re-created because the files are too large to be stored on the GitHub