#figures for paper
#plot mean spectra
DESIS_lake <- read_csv('Data/DESIS/DESIS_stats_lake.csv')
DL = DESIS_lake %>% 
  pivot_longer(cols = starts_with("Rrs")) %>% 
  select(-1) %>% 
  na.omit() %>% 
  separate(name, into = c("product", "wavelength", "statistic")) %>% 
  select(-product) %>% 
  mutate(wavelength = as.numeric(wavelength)) %>% 
  pivot_wider(names_from = statistic, values_from = value)
DL %>% 
  filter(date %in% c("2020_06_02", "2021_06_15"),
         wavelength < 800, wavelength > 450) %>% 
  ggplot()+
  theme_bw()+
  geom_line(aes(x = wavelength, y = avg, color = date, lty = date))+
  scale_linetype_manual(values = c(1,5), name = "Image date")+
  scale_color_manual(values = c("blue", "green"), name = "Image date")+
  labs(x = "Wavelength (nm)", y = "Remote sensing reflectance")

ggsave("Outputs/spectra_comparison_for_poster.png")

#quicklook images
library(raster)
library(RStoolbox)

dat1 = read_csv("Data/DESIS/L2W_destriped/DESIS_HSI_002_2020_06_02_20_11_33_Mendota_clean.csv")%>%
  mutate(y=-y) %>% 
  sf::st_as_sf(coords = c("x", "y")) #convert to SpatialPointsDataFrame

#convert to raster
dat_jun = raster(dat1, resolution = c(1,1)) #each point becomes one raster pixel
#trim to reasonable spectral range. this is for 450:850nm
colnames = names(dat1)[which(names(dat1)=="Rrs_404"):which(names(dat1)=="Rrs_801")]
#fill the raster with data from each band
dat_jun = rasterize(dat1, dat_jun, field = colnames)


dat2 = read_csv("Data/DESIS/L2W_destriped/DESIS_HSI_002_2021_06_15_14_41_33_Mendota_clean.csv")%>%
  mutate(y = -y) %>% 
  sf::st_as_sf(coords = c("x", "y")) #convert to SpatialPointsDataFrame
#convert to raster
dat_aug = raster(dat2, resolution = c(1,1)) #each point becomes one raster pixel
#trim to reasonable spectral range. this is for 450:850nm
colnames = names(dat2)[which(names(dat2)=="Rrs_404"):which(names(dat2)=="Rrs_801")]
#fill the raster with data from each band
dat_aug = rasterize(dat2, dat_aug, field = colnames)

ggRGB(dat_jun, r = 136, g = 58, b = 19)

ggRGB(dat_aug, r = 136, g = 58, b = 19)

plot(dat_aug)
