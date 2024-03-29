library(raster)
library(vegan)
library(RStoolbox)
library(tidyverse)
# http://www.statistics4u.com/fundstat_eng/cc_pca_loadscore.html
# Data = Scores * Loading^T
##
# so
# NewData = NewScores * Loading^T
# NewData * Loading = NewScores

# AVIRIS ####

# Read in PCA data
group_scores <- readRDS("Outputs/PCA_outputs/AVIRIS/pca_AVNG_scores.rds")[,1:10]
AVNG_ldgs <- readRDS("Outputs/PCA_outputs/AVIRIS/pca_AVNG_ldgs.rds") # dim 81*81
AVNG_geo = read_csv("Outputs/PCA_outputs/AVIRIS/AVNG_geo_info.csv")

# Plot original (averaged) scores
group_df = AVNG_geo %>% 
  filter(date =="20160822") %>% 
  right_join(as_tibble(group_scores, rownames= "X") %>% mutate(X = as.numeric(X))) %>% 
  select(-c(date, X))

group_ras = rasterFromXYZ(group_df)
png("Outputs/PCA_outputs/AVIRIS/top_3_PCs_average.png")
plotRGB(group_ras, stretch = "lin", main = "Top 3 PCA components for averaged images")
dev.off()

# Read in individual images
## first image
# Select columns for visible spectrum (450-850nm)
img_0822 = read_csv("Data/AVIRIS/Vnorm/AVIRIS_20160822_250mclipped_30mdownsampled_vnormalized.csv") %>% 
  select(x,y,20:100)

img_matrix = as.matrix(img_0822[,-c(1,2)]) #dim 4663 * 81
NewScores = img_matrix %*% AVNG_ldgs

new_df = AVNG_geo %>% 
  filter(date == "20160822") %>%  
  right_join(as_tibble(NewScores, rownames="X") %>% mutate(X = as.numeric(X))) %>% 
  select(-c(date, X))

new_ras = rasterFromXYZ(new_df)  
png("Outputs/PCA_outputs/AVIRIS/top_3_PCs_20160822.png")
plotRGB(new_ras, stretch = "lin", main = "Top 3 PCA components for 2016-08-22")
dev.off()
# save final scores
write_csv(new_df, "Outputs/PCA_outputs/AVIRIS/scores_20160822.csv")
# second image
img_0831 = read_csv("Data/AVIRIS/Vnorm/AVIRIS_20160831_250mclipped_30mdownsampled_vnormalized.csv") %>% 
  dplyr::select(x,y,20:100)

img_matrix = as.matrix(img_0831[,-c(1,2)]) #dim 4663 * 81
NewScores = img_matrix %*% AVNG_ldgs

new_df = AVNG_geo %>% 
  filter(date == "20160831") %>%  
  right_join(as_tibble(NewScores, rownames="X") %>% mutate(X = as.numeric(X))) %>% 
  select(-c(date, X))

new_ras = rasterFromXYZ(new_df)  
png("Outputs/PCA_outputs/AVIRIS/top_3_PCs_20160831.png")
plotRGB(new_ras, stretch = "lin", main = "Top 3 PCA components for 2016-08-31")
dev.off()
# save final scores
write_csv(new_df, "Outputs/PCA_outputs/AVIRIS/scores_20160831.csv")

# PRISMA ####
flist = dir("Data/PRISMA/Vnorm/lake")
group_scores = readRDS("Outputs/PCA_outputs/PRISMA/pca_prsm_scores.rds")
PRS_ldgs <- readRDS("Outputs/PCA_outputs/PRISMA/pca_prsm_ldgs.rds") # dim 44*44
PRS_geo = read_csv("Outputs/PCA_outputs/PRISMA/prisma_geo.csv")


# Plot original (averaged) scores
group_df = PRS_geo %>% 
  right_join(as_tibble(group_scores[,1:10], rownames= "X") %>% mutate(X = as.numeric(X))) %>% 
  dplyr::select(-c(date, X, X.1, X.2))

group_ras = rasterFromXYZ(group_df[,c(1:2,5:14)])
# png("Outputs/PCA_outputs/PRISMA/top_3_PCs_average.png")
# plotRGB(group_ras, stretch = "lin", main = "Top 3 PCA components for averaged images")
# dev.off()

for(file in flist){
  datestr = substr(file, 8,17)
  # Read in individual images
  img = read_csv(paste0("Data/PRISMA/Vnorm/lake/",file)) %>% 
    dplyr::select("X"="...1", Rrs_453:Rrs_849)
  if("Rrs_765" %in% names(img)){img = dplyr::select(img, -Rrs_765)}
  img_matrix = as.matrix(img[,-1]) #dim 34441 * 44
  NewScores = img_matrix %*% PRS_ldgs
  rownames(NewScores) = img$X
  
  new_df = PRS_geo %>% 
    filter(date == datestr) %>%  
    right_join(as_tibble(NewScores, rownames="X.1") %>% mutate(X.1 = as.numeric(X.1))) %>% 
    dplyr::select(5,6,9:18)

  new_ras = rasterFromXYZ(new_df)  
  png(paste0("Outputs/PCA_outputs/PRISMA/top_3_PCs_",datestr,".png"))
  plotRGB(new_ras, stretch = "lin", main = paste("Top 3 PCA components for",datestr))
  dev.off()
  # save final scores
  write_csv(new_df, paste0("Outputs/PCA_outputs/PRISMA/scores_",datestr,".csv"))

}

# DESIS ####
flist = dir("Data/DESIS/Vnorm_output/Vnorm (lake)")
group_scores = readRDS("Outputs/PCA_outputs/DESIS/pca_dss_scores.rds")
DSS_ldgs <- readRDS("Outputs/PCA_outputs/DESIS/pca_dss_ldgs.rds") # dim 36*36
DSS_geo = read_csv("Outputs/PCA_outputs/DESIS/desis_geo.csv")


# Plot original (averaged) scores
group_df = DSS_geo %>% 
  right_join(as_tibble(group_scores[,1:10], rownames= "X") %>% mutate(X = as.numeric(X))) %>% 
  select(-c(X, pixel_ct)) #%>% 
  #dplyr::mutate(x = rank(lon, ties.method= "random"), y = rank(lat, ties.method = "random"), .before = everything())
# group_ras = rasterFromXYZ(group_df[1:10,-c(3:5)])

coordinates(group_df)=~lon+lat
group_ras = raster(group_df)
crs(group_ras) = CRS("+init=epsg:4326")
group_ras = projectRaster(group_ras, to = "")
values(group_ras) = as.matrix(as.data.frame(group_df))

png("Outputs/PCA_outputs/DESIS/top_3_PCs_average.png")
plotRGB(group_ras, stretch = "lin", main = "Top 3 PCA components for averaged images")
dev.off()

desis_all = read_csv("Data/DESIS/Vnorm_output/DESIS_bin10_lake.csv") %>% 
  select(-c('405','446','457','823','813','723','692')) %>%
  #drop because of NAs: 405,457, 823, 813,723,692
  select('X' = `...1`, date,c('467':'875')) %>%
  mutate(across('467':'875', function(x) x * pi))

for(file in flist){
  datestr = substr(file, 15,24)
  # Read in individual images
  img = desis_all %>% 
    filter(date == datestr)
  img_matrix = as.matrix(img[,-c(1,2)]) #dim 20189 * 36
  NewScores = img_matrix %*% DSS_ldgs
  rownames(NewScores) = img$X
  
  new_df = DSS_geo %>% 
    filter(date == datestr) %>%  
    right_join(as_tibble(NewScores, rownames="X") %>% mutate(X = as.numeric(X))) %>% 
    dplyr::select(-c("X", "pixel_ct"))
  
  # new_ras = rasterFromXYZ(new_df)
  # png(paste0("Outputs/PCA_outputs/PRISMA/top_3_PCs_",datestr,".png"))
  # plotRGB(new_ras, stretch = "lin", main = paste("Top 3 PCA components for",datestr))
  # dev.off()
  # save final scores
  write_csv(new_df, paste0("Outputs/PCA_outputs/DESIS/scores_",datestr,".csv"))
  
}

## Try getting x-y for DESIS
DSS_geo2 = DSS_geo 

# Map plotting ####

## AVIRIS ####
alist = dir("Outputs/PCA_outputs/AVIRIS/", pattern = regex("scores.*csv"))
av_scores = tibble()
for(fn in alist){
  datestr = substr(fn, 8,15)
  temp = read_csv(paste0("Outputs/PCA_outputs/AVIRIS/",fn)) %>% 
    mutate('date' = datestr, .before = everything())
  av_scores= bind_rows(av_scores, temp)
}
rm(temp)

# Plot component 1
png("Outputs/PCA_outputs/AVIRIS/plots_of_PC1.png", height = 550, width = 550)
ggplot(av_scores)+
  geom_tile(aes(x=x, y=y, fill= Comp.1))+
  scale_fill_continuous(name="Score")+
  theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
  facet_wrap(~date, nrow = 2)+
  labs(x="", y="", title = "PC 1 for AVIRIS images")
dev.off()

#plot remaining components
for(i in 2:10){
  tmp = av_scores %>% 
    dplyr::select(date,y,x,comp=i+3)
  p = ggplot(tmp, aes(x=x, y=y, fill= comp))+
    geom_raster()+
    scale_fill_continuous(name = "Score")+
    theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
    facet_wrap(~date, nrow = 2)+
    labs(x="", y="", title = paste("PC", i, "for AVIRIS images"))
  png(paste0("Outputs/PCA_outputs/AVIRIS/plots_of_PC",i,".png"), height = 550, width = 550)
  print(p)
  dev.off()
  rm(tmp, p)
}


## PRISMA #### 
# Organize data
plist = dir("Outputs/PCA_outputs/PRISMA/", pattern = regex("scores.*csv"))
pris_scores = tibble()
for(fn in plist){
  datestr = substr(fn, 8,17)
  temp = read_csv(paste0("Outputs/PCA_outputs/PRISMA/",fn)) %>% 
    dplyr::mutate('date' = datestr, .before = everything())
  pris_scores = bind_rows(pris_scores, temp)
}
rm(temp)

# Plot component 1
png("Outputs/PCA_outputs/PRISMA/plots_of_PC1.png", height = 550, width = 550)
ggplot(pris_scores)+
  geom_tile(aes(x=x, y=-y, fill= Comp.1))+
  scale_fill_continuous(name="Score")+
  theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
  facet_wrap(~date)+
  labs(x="", y="", title = "PC 1 for PRISMA images")
dev.off()

for(i in 2:10){
   tmp = pris_scores %>% 
    dplyr::select(date,y,x,comp=i+3)
  p = ggplot(tmp, aes(x=x, y=-y, fill= comp))+
    geom_raster()+
    scale_fill_continuous(name = "Score")+
    theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
    facet_wrap(~date)+
    labs(x="", y="", title = paste("PC", i, "for PRISMA images"))
  png(paste0("Outputs/PCA_outputs/PRISMA/plots_of_PC",i,".png"), height = 550, width = 550)
  print(p)
  dev.off()
  rm(tmp, p)
}

## DESIS ####

# Organize data
dlist = dir("Outputs/PCA_outputs/DESIS/", pattern = regex("scores_20.*csv"))
dss_scores = tibble()
for(fn in dlist){
  datestr = substr(fn, 8,17)
  temp = read_csv(paste0("Outputs/PCA_outputs/DESIS/",fn)) %>% 
    dplyr::mutate('date' = datestr, .before = everything())
  dss_scores = bind_rows(dss_scores, temp)
}
rm(temp)

# Plot component 1
png("Outputs/PCA_outputs/DESIS/plots_of_PC1.png", height = 550, width = 550)
ggplot(dss_scores)+
  geom_point(aes(x=lon, y=lat, color= Comp.1), pch = ".")+
  scale_color_continuous(name="Score")+
  theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
  facet_wrap(~date)+
  labs(x="", y="", title = "PC 1 for DESIS images")
dev.off()

for(i in 2:10){
  tmp = dss_scores %>% 
    dplyr::select(date,lat,lon,comp=i+3)
  p = ggplot(tmp, aes(x=lon, y=lat, color= comp))+
    geom_point(pch = ".")+
    scale_color_continuous(name = "Score")+
    theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
    facet_wrap(~date)+
    labs(x="", y="", title = paste("PC", i, "for DESIS images"))
  png(paste0("Outputs/PCA_outputs/DESIS/plots_of_PC",i,".png"), height = 550, width = 550)
  print(p)
  dev.off()
  rm(tmp, p)
}


# Time Series plots ####

## DESIS ####

dss_scores %>% 
  select(1,4:10) %>% 
  pivot_longer(2:8) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "DESIS - whole lake")

ggsave("Outputs/PCA_outputs/DESIS/timeseries_boxplots_lake.png", width = 6, height = 8, units = "in")

dss_scores %>% 
  select(1,4:10) %>% 
  pivot_longer(2:8) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_violin(aes(y = value, x = factor(date)))+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "DESIS - whole lake")

ggsave("Outputs/PCA_outputs/DESIS/timeseries_violin_lake.png", width = 6, height = 8, units = "in")


## PRISMA ####

pris_scores %>% 
  select(1,4:7) %>% 
  pivot_longer(2:5) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "PRISMA - whole lake")

ggsave("Outputs/PCA_outputs/PRISMA/timeseries_boxplots_lake.png", width = 6, height = 5, units = "in")

pris_scores %>% 
  select(1,4:7) %>% 
  pivot_longer(2:5) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_violin(aes(y = value, x = factor(date)))+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "PRISMA - whole lake")

ggsave("Outputs/PCA_outputs/PRISMA/timeseries_violin_lake.png", width = 6, height = 5, units = "in")


## AVIRIS ####

av_scores %>% 
  select(1,4:13) %>% 
  pivot_longer(2:11) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", nrow = 5)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "AVIRIS - whole lake")

ggsave("Outputs/PCA_outputs/AVIRIS/timeseries_boxplots_lake.png", width = 6, height = 5, units = "in")

av_scores %>% 
  select(1,4:13) %>% 
  pivot_longer(2:11) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_violin(aes(y = value, x = factor(date)))+
  facet_wrap(~name, scales = "free_y", ncol = 2)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "AVIRIS - whole lake")

ggsave("Outputs/PCA_outputs/AVIRIS/timeseries_violin_lake.png", width = 6, height = 5, units = "in")

# Plot buoy region only ####

## PRISMA ####

p_buoy = read_csv("Data/PRISMA/buoy/PRISMA_2021_06_05_buoy.csv") %>% 
  select(-c(aot_550:glint_std)) %>% 
  mutate(xy = paste(x, y))

pris_scores_buoy = pris_scores %>% 
  mutate(xy = paste(x, y)) %>% 
  filter(xy %in% p_buoy$xy)

#check 
# ggplot(pris_scores_buoy)+
#   geom_tile(aes(x=x, y=-y, fill= Comp.1))+
#   scale_fill_continuous(name="Score")+
#   theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
#   facet_wrap(~date)+
#   labs(x="", y="", title = "PC 1 for PRISMA images")

pris_scores_buoy %>% 
  select(1,4:7) %>% 
  pivot_longer(2:5) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "PRISMA - buoy")
ggsave("Outputs/PCA_outputs/PRISMA/timeseries_boxplots_buoy.png", width = 6, height = 5, units = "in")

## DESIS ####

d_buoy = read_csv("Data/DESIS/Vnorm_output/Vnorm (buoy clipped)/DESIS_HSI_002_2020_06_02_20_11_33_buoy_vnormalized.csv") %>% 
  mutate(ll = paste(round(lon, 3), round(lat,3)))

dss_scores_buoy = dss_scores %>% 
  mutate(ll = paste(round(lon,3), round(lat,3))) %>% 
  filter(ll %in% d_buoy$ll)

#check 
# ggplot(dss_scores_buoy)+
#   geom_point(aes(x=lon, y=lat, fill= Comp.1))+
#   scale_fill_continuous(name="Score")+
#   theme_bw()+theme(panel.grid = element_blank(), strip.text = element_text(face= "bold", size = "14"))+
#   facet_wrap(~date)

dss_scores_buoy %>% 
  select(1,4:10) %>% 
  pivot_longer(2:8) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", ncol = 1)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "DESIS - buoy")

ggsave("Outputs/PCA_outputs/DESIS/timeseries_boxplots_buoy.png", width = 6, height = 8, units = "in")

## AVIRIS ####

a_buoy = read_csv("Data/AVIRIS/AVIRIS_20160822_buoyclipped_30mdownsampled.csv") %>% 
  mutate(x = round(x,-1), 
         y = round(y,-1),
         xy = paste(x,y))

av_scores_buoy = av_scores %>% 
  mutate(x = round(x,-1)+10, 
         y = round(y,-1)+10,
         xy = paste(x,y)) %>% 
  filter(xy %in% a_buoy$xy)
# 
# ggplot(av_scores_buoy)+
#   geom_tile(aes(x=x,y=y, fill=Comp.1))+
#   facet_wrap(~date)

av_scores_buoy %>% 
  select(1,4:13) %>% 
  pivot_longer(2:11) %>% 
  mutate(date = as_date(date),
         name = str_replace(name, fixed("Comp."), "PC ")) %>% 
  ggplot()+
  geom_boxplot(aes(x = factor(date), y = value), outlier.alpha = 0)+
  geom_jitter(aes(x = factor(date), y = value), alpha = 0.2)+
  facet_wrap(~name, scales = "free_y", nrow = 5)+
  theme(panel.grid = element_blank(), panel.background = element_rect(fill = "white", color = "black"))+
  labs(x = "Image date", y = "Component score",
       title = "AVIRIS - buoy")

ggsave("Outputs/PCA_outputs/AVIRIS/timeseries_boxplots_buoy.png", width = 6, height = 5, units = "in")

# WRITE OUTPUTS ####

all_buoy_scores = av_scores_buoy %>% 
  select(-c(xy, x, y)) %>% 
  mutate(sensor = "AVNG", .before = everything()) %>% 
  select(sensor:Comp.10) %>% 
  bind_rows(pris_scores_buoy %>% 
              mutate(sensor = "PRISMA") %>% 
              select(sensor, date, Comp.1:Comp.10))%>% 
  bind_rows(dss_scores_buoy %>% 
              mutate(sensor = "DESIS") %>% 
              select(sensor, date, Comp.1:Comp.10)) %>% 
  filter(!is.na(Comp.1)) %>% 
  group_by(sensor, date) %>% 
  summarise(n=n(),
            across(Comp.1:Comp.10, \(x) mean(x, na.rm = T)))

write_csv(all_buoy_scores, "Outputs/PCA_outputs/mean_PCA_scores_at_buoy.csv")
