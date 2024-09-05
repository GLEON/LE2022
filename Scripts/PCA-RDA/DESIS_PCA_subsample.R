# DESIS PCA comparisons

## Load libraries
library(sp)
#library(raster)
library(plyr)
library(corrplot)
library(caret)
library(rpart)
library(rattle)
library(ade4)
library(plyr)
library(tidyverse)

# Load data
# Data from: https://drive.google.com/drive/u/1/folders/1R44K6R9mDm4yEkIY2JKdZVFDJZdhxRXG
desis <- read.csv("Data/DESIS/L2W_vnorm/vnorm_desis_lake.csv")
desis_buoy <- read.csv("Data/DESIS/L2W_vnorm/vnorm_desis_buoy.csv")

# Check data first - plot
desis_mean <- desis %>%
  group_by(date,wavelength) %>%
  summarise(mean = mean(norm, na.rm=T),
            sd = sd(norm, na.rm=T))
desis_mean %>%
  ggplot(aes(x=as.numeric(wavelength),y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha =0.2) +
  facet_wrap(~date,scales='free')

desis_buoy_mean <- desis_buoy %>%
  group_by(date,wavelength) %>%
  summarise(mean = mean(norm, na.rm=T),
            sd = sd(norm, na.rm=T))
desis_buoy_mean %>%
  ggplot(aes(x=as.numeric(wavelength),y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha =0.2) +
  facet_wrap(~date,scales='free')

# Smooth with Savitzy-Golay
# library(prospectr)
# # https://rdrr.io/cran/prospectr/man/savitzkyGolay.html
# # Savitzy-Golay Smoothing Filter with SMASH parameters for DESIS:
# # m	 = an integer indicating the differentiation order      = 2
# # p	 = an integer indicating the polynomial order           = 3
# # w	 = an integer indicating the window size (must be odd)  = 7
# index <- desis_wide[,1:4]
# desis_smooth <- savitzkyGolay(X = desis_wide[,-c(1:4)], m=2 , p=3 , w=7 )
# not working...

# Try moving average smoothing...
library(zoo)
desis_smooth <- desis %>%
  group_by(index,date) %>%
  arrange(wavelength) %>%
  mutate(mov_avg = rollmean(norm,
                            k=2,
                            fill=NA,
                            align="left"))

desis_buoy_smooth <- desis_buoy %>%
  group_by(index,date) %>%
  arrange(wavelength) %>%
  mutate(mov_avg = rollmean(norm,
                            k=2,
                            fill=NA,
                            align="left"))

# Check plot
desis_smooth_mean <- desis_smooth %>%
  group_by(date,wavelength) %>%
  summarise(mean = mean(mov_avg, na.rm=T),
            sd = sd(mov_avg, na.rm=T))
desis_smooth_mean %>%
  ggplot(aes(x=as.numeric(wavelength),y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha =0.2) +
  facet_wrap(~date,scales='free')

desis_buoy_smooth_mean <- desis_buoy_smooth %>%
  group_by(date,wavelength) %>%
  summarise(mean = mean(mov_avg, na.rm=T),
            sd = sd(mov_avg, na.rm=T))
desis_buoy_smooth_mean %>%
  ggplot(aes(x=as.numeric(wavelength),y=mean)) +
  geom_line() +
  geom_ribbon(aes(ymin=mean-sd,ymax=mean+sd),alpha =0.2) +
  facet_wrap(~date,scales='free')
  
desis_smooth_wide <- pivot_wider(desis_smooth,
                                 id_cols = c(index,date,x,y),
                                 names_from = wavelength,
                                 values_from = norm)
desis_buoy_smooth_wide <- pivot_wider(desis_buoy_smooth,
                                 id_cols = c(index,date,x,y),
                                 names_from = wavelength,
                                 values_from = norm)
  
##### PCA
##### Subsample
# subsample 10% DESIS? data because 
# Critical Scale of Variability ~100m (Sharp et al. 2021)
# DESIS pixels are 30m, so 3x3 grid of DESIS pixels is ~100x100m 
# Note: binned to 10nm 
# Which means we subsample 1/9 = 11.1% so 10% is fine
set.seed(1234) #for replicability

desis_sub = desis_smooth_wide %>% 
  group_by(date) %>% 
  drop_na() %>% 
  slice_sample(prop=0.10) 

desis_pca_input <- desis_sub %>% ungroup() %>% select(-c(index,date,x,y))

pca_dss <- princomp(desis_pca_input,cor=TRUE)


desis_buoy_pca_input <- desis_buoy_smooth_wide %>% ungroup() %>% 
  dplyr::select(-c(index,date,x,y))

pca_buoy_dss <- princomp(desis_buoy_pca_input,cor=TRUE)
pca_buoy_dss_scores = pca_buoy_dss$scores
pca_buoy_scores_mean <- cbind(desis_buoy_smooth_wide[,2],pca_buoy_dss_scores) %>%
  group_by(date) %>%
  summarize_all(.funs=mean)

saveRDS(pca_buoy_scores_mean, "Outputs/PCA_outputs/DESIS/desis_buoy_pca_means.rds")


summary(pca_dss) #prints loadings
biplot(pca_dss)

#save PCA output
pca_dss_scores = pca_dss$scores
pca_dss_ldgs = pca_dss$loadings
saveRDS(pca_dss_ldgs, "Outputs/PCA_outputs/DESIS/pca_dss_ldgs.rds")
saveRDS(pca_dss_scores, "Outputs/PCA_outputs/DESIS/pca_dss_scores.rds")

#write_csv(desis_corrected[,c(1,56,62:63)],"Outputs/PCA_outputs/DESIS/desis_geo.csv" )


# screeplot(pca_dss)#shows variance represented by each component
# #improve biplot
# library(ggfortify)
# peu<-autoplot(pca_dss,loadings=TRUE,loadings.label=T,loadings.label.repel=T, loadings.colour="black",loadings.label.colour="black")+
#   theme_bw()
# peu
# # or to include date
# #install.packages('ggord')
# # require(ggord)
# # 
# # dss_gg<-ggord(pca_dss,desis_subW$date,cols=c("firebrick3","orange","yellowgreen","cyan3","cornflowerblue","purple3","pink3","deeppink3"),ellipse=FALSE,vec_ext=4.5,vectyp="solid",xlims=c(-4,3),ylims=c(-3,4),repel=TRUE,grp_title="Bout",size=3,max.overlaps=50)
# 
# # get variances represented by each PC
# variances <- as_tibble(pca_dss$sdev^2, rownames = "component") %>% #convert SD to variance
#   mutate(proportion = value/sum(value) #proportion of total variance represented
#   ) #add image information in a separate column ? date = prsma_sub$date
# # decide how many components to retain
# #try top ten for now
# vari10<- variances %>% filter(component %in% c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6",
#                                                "Comp.7","Comp.8","Comp.9","Comp.10"))
# 
# get loadings for top 10 components
loadings = pca_dss$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>%
  janitor::clean_names() %>%
  pivot_longer(cols = starts_with("comp")) %>% #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case
  mutate(band = as.numeric(str_remove(band, "X"))) #format wavelengths for plotting
#img_date = img_date) #tag with image date

loadings_buoy = pca_buoy_dss$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>%
  janitor::clean_names() %>%
  pivot_longer(cols = starts_with("comp")) %>% #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case
  mutate(band = as.numeric(str_remove(band, "X")))

# plot loadings -try top 5  
loadings %>% filter(name %in%c("comp_1","comp_2","comp_3","comp_4")) %>%
ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")+
  facet_wrap(~name)

loadings %>% filter(name %in%c("comp_1","comp_2","comp_3","comp_4")) %>%
  ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")+
  facet_wrap(~name)

# save information to files
# write_csv(loadings, "Data/DESIS/desis_PCAtop10_loadings.csv")
# write_csv(vari10, "Data/DESIS/desis_PCAtop10_variances.csv")

