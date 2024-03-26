# Need to run as administrator to use RSToolbox

# PCA Analysis of AVIRIS-NG scene

# Load libraries
library(sp)
library(raster)
library(RStoolbox)
library(ggplot2)
library(tidyverse)
library(janitor)

################################################################################
## First 8/22/2016 AVIRIS-ng image
# this image is a csv file after sunglint correction
# so follow Caroline's script

filename <- 'Data/AVIRIS/lake/AVIRIS_20160822_250mclipped_30mdownsampled_sunglintcorr01.csv'
dat_20160822 <- read.csv(filename) %>%
  sf::st_as_sf(coords = c("x","y")) #convert to SpatialPointsDataFrame

#convert to raster
dat_ras_20160822 = raster(dat_20160822, resolution = 30)

# define column names to identify fields to pull raster data values for in rasterize function
colnames <- names(dat_20160822[16:88]) #16:88 are the columns corresponding to the visible spectrum 
colnames <- colnames[1:73] #remove "geometry from list of column names

#fill the raster with data from each band
dat_ras_20160822 = rasterize(dat_20160822, dat_ras_20160822, field = colnames)

plotRGB(dat_ras_20160822,r=43,g=24,b=7,stretch="lin")

# run pca
rpc_20160822 = rasterPCA(dat_ras_20160822, spca = T #, nSamples = ncell(dat_ras)/10 #can uncomment this to subsample and run faster
)
summary(rpc_20160822$model) #prints loadings
screeplot(rpc_20160822$model) #shows variance represented by each component
ggRGB(rpc_20160822$map, 1,2,3, stretch = "lin") #maps the top 3 components
ggsave("Data/AVIRIS/AVIRISng-20160822_PCA.png")

img_date = "20160822"
# get variances represented by each PC
variances_20160822 = as_tibble(rpc_20160822$model$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value), #proportion of total variance represented
         img_date = img_date) #add image information in a separate column
# decide how many components to retain
variances_20160822 = variances_20160822 %>% 
  filter(proportion >= 0.01) #just keep those representing at least 1% of variance
#slice_head(n = (nrow(variances[variances$proportion >= 0.01,])+1)) #or keep those plus one

# get loadings for each variance
loadings_20160822 = rpc_20160822$model$loadings[,1:nrow(variances_20160822)] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>%
  mutate(band = str_remove(band,"X")) %>%
  mutate(band = str_remove(band,".Nanometers")) %>%
  mutate(band = as.numeric(band))
 # janitor::clean_names() %>%

loadings_20160822 = loadings_20160822 %>%
  pivot_longer(cols = starts_with("comp")) #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case
  
# plot loadings  
ggplot(loadings_20160822, aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")+
  labs(title = "AVIRIS-ng 20160822 PCA Loadings")

ggsave("Data/AVIRIS/AVIRISng-20160822_PCA_loadings.png")

################################################################################
# 8/31/2022 Raster
# not a csv so slightly different procedure at beginning...

# Load raster data
# https://erinbecker.github.io/r-raster-vector-geospatial/05-raster-multi-band-in-r/index.html

# Lake clipped, 1 m downsampled
setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/Data/AVIRIS/30mdownsampled')
filename <- 'ang20160831t201002_rfl_v1n2_img_250mclipped_30mdownsampled'
AVIRIS_0831 <- stack(filename)

# clip rasterstack to visible spectrum
# 450nm-810nm
# bands 15-87
dat_ras_20160831 <- subset(AVIRIS_0831,15:87)

# spectral downsampling
# resample spectral to consistent bandwidth...

# Plot to review bands
plotRGB(dat_ras_20160831,r=43,g=24,b=7,stretch="lin")

# run RasterPCA
# http://bleutner.github.io/RStoolbox/rstbx-docu/RStoolbox.html

rpc_20160831 = rasterPCA(dat_ras_20160831, spca = T)

summary(rpc_20160831$model) #prints loadings
screeplot(rpc_20160831$model) #shows variance represented by each component
ggRGB(rpc_20160831$map, 1,2,3, stretch = "lin") #maps the top 3 components
ggsave("Data/AVIRIS/AVIRISng-20160831_PCA.png")

img_date = "20160831"
# get variances represented by each PC
variances_20160831 = as_tibble(rpc_20160831$model$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value), #proportion of total variance represented
         img_date = img_date) #add image information in a separate column
# decide how many components to retain
variances_20160831 = variances_20160831 %>% 
  filter(proportion >= 0.01) #just keep those representing at least 1% of variance
#slice_head(n = (nrow(variances[variances$proportion >= 0.01,])+1)) #or keep those plus one

# get loadings for each variance
loadings_20160831 = rpc_20160831$model$loadings[,1:nrow(variances_20160831)] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>%
  mutate(band = str_remove(band,"X")) %>%
  mutate(band = str_remove(band,".Nanometers")) %>%
  mutate(band = as.numeric(band))
# janitor::clean_names() %>%

loadings_20160831 = loadings_20160831 %>%
  pivot_longer(cols = starts_with("comp")) #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case

# plot loadings  
ggplot(loadings_20160831, aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading") +
  labs(title = "AVIRIS-ng 20160831 PCA Loadings")

ggsave("Data/AVIRIS/AVIRISng-20160831_PCA_loadings.png")
