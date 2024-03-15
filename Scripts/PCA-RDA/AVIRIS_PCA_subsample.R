# AVIRIS PCA comparisons

## Load libraries
library(sp)
#library(raster)
library(tidyverse)
library(plyr)
library(corrplot)
library(caret)
library(rpart)
library(rattle)
library(ade4)
library(plyr)
library(ggfortify)

##### Subsample
# subsample 10% AVIRIS data because 
# Critical Scale of Variability ~100m
# AVIRIS pixels are 2.6m originally but have been downsampled to 30m, 
# so 3x3 grid of AVIRIS pixels is ~100x100m 
# Which means we subsample 1/9 = ~10%
# Note: no binning or vector normalization on AVIRIS data

# ## ONLY NEED TO RUN ONCE AND THEN CAN LOAD COMPILED CSV IN LINE BELOW (COMMENT THE REST OUT)
# # compile AVIRIS scenes to one common file
# # Load first file and add to AVIRIS_lake that will eventually contain all AVIRIS lake data
# flist = dir("Data/AVIRIS/Vnorm")
# aviris <- read.csv(file.path("Data/AVIRIS/Vnorm",flist[1]))
# date = flist[1]
# AVIRIS_lake <- cbind(date,aviris)
# for(i in 2:length(flist)){
#   aviris <- read.csv(file.path("Data/AVIRIS/Vnorm",flist[i]))
#   date = flist[i]
#   temp <- cbind(date,aviris)
#   AVIRIS_lake <- plyr::rbind.fill(AVIRIS_lake,temp)
# }
# AVIRIS_lake <- AVIRIS_lake %>% mutate(date = substr(date,8,15))
# # export summary file
# write.csv(AVIRIS_lake,'Data/AVIRIS/vnorm_compiled/AVIRIS_lake_compiled.csv')

AVIRIS_lake <- read.csv('Data/AVIRIS/vnorm_compiled/AVIRIS_lake_compiled.csv')

AVIRIS_lake_sub<-AVIRIS_lake %>% 
  group_by(date) %>%
  drop_na() %>%
  slice_sample(prop=0.10) %>%
  select(c(1,21:101)) # Select columns for visible spectrum (450-850nm)

# run pca
AVIRIS<-AVIRIS_lake_sub %>% ungroup(date) %>% select(-date)

AVIRIS_pca<-princomp(AVIRIS,cor=TRUE)

summary(AVIRIS_pca) #prints loadings
screeplot(AVIRIS_pca)#shows variance represented by each component

# get variances represented by each PC
variances <- as_tibble(AVIRIS_pca$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value) #proportion of total variance represented
  ) #add image information in a separate column ? date = prsma_sub$date
# decide how many components to retain
# try top ten for now
vari10<- variances %>% filter(component %in% c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6",
                                               "Comp.7","Comp.8","Comp.9","Comp.10"))

#variances = variances %>% 
 # filter(proportion >= 0.01) #just keep those representing at least 1% of variance
#slice_head(n = (nrow(variances[variances$proportion >= 0.01,])+1)) #or keep those plus one

# get loadings for top 10 components
loadings = AVIRIS_pca$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>% 
  mutate(band = str_remove(band,"X")) %>%
  mutate(band = str_remove(band,".Nanometers")) %>%
  mutate(band = as.numeric(band)) %>%
  pivot_longer(cols = starts_with("comp")) #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case
#img_date = img_date) #tag with image date

# plot loadings -try top 5  
loadings %>% filter(name %in%c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5")) %>%
ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")

##########################################################################################################################

# Complete PCA on scenes individually (no subset of data)

aviris_20160822 <- read.csv('Data/AVIRIS/vnorm/AVIRIS_20160822_250mclipped_30mdownsampled_vnormalized.csv')

AVIRIS_20160822_sub<-aviris_20160822 %>% 
  drop_na() %>%
#  slice_sample(prop=0.10) %>% # don't subset data for pca on individual scenes
  select(c(20:100)) # Select columns for visible spectrum (450-850nm)

# run pca
AVIRIS_20160822_pca<-princomp(AVIRIS_20160822_sub,cor=TRUE)

summary(AVIRIS_20160822_pca) #prints loadings
screeplot(AVIRIS_20160822_pca)#shows variance represented by each component

# get variances represented by each PC
variances <- as_tibble(AVIRIS_20160822_pca$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value)) #proportion of total variance represented
# keep top 10 components
vari10<- variances %>% filter(component %in% c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6",
                                               "Comp.7","Comp.8","Comp.9","Comp.10"))
# get loadings for top 10 components
loadings = AVIRIS_20160822_pca$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>% 
  mutate(band = str_remove(band,"X")) %>%
  mutate(band = str_remove(band,".Nanometers")) %>%
  mutate(band = as.numeric(band)) %>%
  pivot_longer(cols = starts_with("comp")) #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case

# plot loadings -try top 5  
loadings %>% filter(name %in%c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5")) %>%
  ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")

########################################################################################################################

aviris_20160831 <- read.csv('Data/AVIRIS/vnorm/AVIRIS_20160831_250mclipped_30mdownsampled_vnormalized.csv')

AVIRIS_20160831_sub<-aviris_20160831 %>% 
  drop_na() %>%
  #  slice_sample(prop=0.10) %>% # don't subset data for pca on individual scenes
  select(c(20:100)) # Select columns for visible spectrum (450-850nm)

# run pca
AVIRIS_20160831_pca<-princomp(AVIRIS_20160831_sub,cor=TRUE)

summary(AVIRIS_20160831_pca) #prints loadings
screeplot(AVIRIS_20160831_pca)#shows variance represented by each component

# get variances represented by each PC
variances <- as_tibble(AVIRIS_20160831_pca$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value)) #proportion of total variance represented
# keep top 10 components
vari10<- variances %>% filter(component %in% c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6",
                                               "Comp.7","Comp.8","Comp.9","Comp.10"))
# get loadings for top 10 components
loadings = AVIRIS_20160831_pca$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>% 
  mutate(band = str_remove(band,"X")) %>%
  mutate(band = str_remove(band,".Nanometers")) %>%
  mutate(band = as.numeric(band)) %>%
  pivot_longer(cols = starts_with("comp")) #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case

# plot loadings -try top 5  
loadings %>% filter(name %in%c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5")) %>%
  ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")
