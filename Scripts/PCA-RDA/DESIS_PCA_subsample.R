# DESIS PCA comparisons

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

##### Subsample
# subsample 10% DESIS? data because 
# Critical Scale of Variability ~100m
# DESIS pixels are 30m, so 3x3 grid of DESIS pixels is ~100x100m 
#Note: binned to 10nm 
# Which means we subsample 1/9 = 11.1% so 10% is fine
desis_binned<-read.csv('Data/DESIS/DESIS_bin10_lake.csv') #CO updated with Lara's new file 2 sep 2024: https://drive.google.com/drive/u/1/folders/14Q1EKsK6G4q8bq-kR5hJs5mvk2iG-VTG
set.seed(1234) #for replicability
# desis_sub<-desis_binned %>% select(-c('405','446','457','823','813','723','692')) %>% #drop because of NAs: 405,457, 823, 813,723,692
#   group_by(date) %>% select(X,c('467':'875')) %>%
#   drop_na() %>%
#   slice_sample(prop=0.10)

## NEED to fix x,y values
desis_mins = desis_binned %>% 
  group_by(date) %>% 
  dplyr::summarize(min_x = min(x),
            min_y = min(y))

desis_offsets = desis_mins %>% 
  mutate(offset_x = min_x - desis_mins$min_x[desis_mins$date=='2020_06_02'],
         offset_y = min_y - desis_mins$min_y[desis_mins$date=='2020_06_02']) %>% 
  select(-c(min_x, min_y))

desis_corrected = desis_binned %>% 
  left_join(desis_offsets) %>% 
  mutate(x_corr = x - offset_x,
         y_corr = y - offset_y)

ggplot(desis_corrected)+ geom_point(aes(x=x_corr, y=-y_corr, color=date))+
  geom_point(d_buoy, mapping=aes(x=x, y=-y))

#redo with new column names
desis_sub = desis_binned %>% 
  select(X, date, c('X467':'X875')) %>% 
  select(-c('X823', 'X692', 'X723')) %>% 
  group_by(date) %>% 
  drop_na() %>% 
  slice_sample(prop=0.10) %>% 
  column_to_rownames('X')

#convert to rhow from rrs
desis_subW = desis_sub %>%
  mutate(across(where(is.numeric), function(x) x * pi))
# run pca
dss_sw<-desis_subW %>% ungroup() %>% select(-date)

pca_dss<-princomp(dss_sw,cor=TRUE)

summary(pca_dss) #prints loadings
biplot(pca_dss)

#save PCA output
pca_dss_scores = pca_dss$scores
#rownames(pca_dss_scores) = desis_subW$X
pca_dss_ldgs = pca_dss$loadings
saveRDS(pca_dss_ldgs, "Outputs/PCA_outputs/DESIS/pca_dss_ldgs.rds")
saveRDS(pca_dss_scores, "Outputs/PCA_outputs/DESIS/pca_dss_scores.rds")

write_csv(desis_corrected[,c(1,56,62:63)],"Outputs/PCA_outputs/DESIS/desis_geo.csv" )


screeplot(pca_dss)#shows variance represented by each component
#improve biplot
library(ggfortify)
peu<-autoplot(pca_dss,loadings=TRUE,loadings.label=T,loadings.label.repel=T, loadings.colour="black",loadings.label.colour="black")+
  theme_bw()
peu
# or to include date
#install.packages('ggord')
require(ggord)

dss_gg<-ggord(pca_dss,desis_subW$date,cols=c("firebrick3","orange","yellowgreen","cyan3","cornflowerblue","purple3","pink3","deeppink3"),ellipse=FALSE,vec_ext=4.5,vectyp="solid",xlims=c(-4,3),ylims=c(-3,4),repel=TRUE,grp_title="Bout",size=3,max.overlaps=50)

# get variances represented by each PC
variances <- as_tibble(pca_dss$sdev^2, rownames = "component") %>% #convert SD to variance
  mutate(proportion = value/sum(value) #proportion of total variance represented
  ) #add image information in a separate column ? date = prsma_sub$date
# decide how many components to retain
#try top ten for now
vari10<- variances %>% filter(component %in% c("Comp.1","Comp.2","Comp.3","Comp.4","Comp.5","Comp.6",
                                               "Comp.7","Comp.8","Comp.9","Comp.10"))

#variances = variances %>% 
 # filter(proportion >= 0.01) #just keep those representing at least 1% of variance
#slice_head(n = (nrow(variances[variances$proportion >= 0.01,])+1)) #or keep those plus one

# get loadings for top 10 components
loadings = pca_dss$loadings[,1:10] %>% #only keep the ones that have been retained
  as_tibble(rownames = "band") %>% 
  janitor::clean_names() %>%
  pivot_longer(cols = starts_with("comp")) %>% #this will cause an error if only retaining one PC - just comment out this line and fix it manually for that case
  mutate(band = as.numeric(str_remove(band, "X"))) #format wavelengths for plotting
#img_date = img_date) #tag with image date

# plot loadings -try top 5  
loadings %>% filter(name %in%c("comp_1","comp_2","comp_3","comp_4","comp_5")) %>%
ggplot(aes(x = band, y = value))+
  theme_bw()+
  geom_line(aes(group = name, color = name))+
  scale_color_discrete(name = "Component")+
  labs(x = "Wavelength", y = "Loading")

# save information from this image -NOT WORKING
# vars = bind_rows(vars, variances)
# lds = bind_rows(lds, loadings)


# save information to files
write_csv(loadings, "Data/DESIS/desis_PCAtop10_loadings.csv")
write_csv(vari10, "Data/DESIS/desis_PCAtop10_variances.csv")
