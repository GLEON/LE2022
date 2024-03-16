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
write_csv(new_df, "Outputs/PCA_outputs/AVIRIS/scores_20160822.csv")

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
png("Outputs/PCA_outputs/PRISMA/top_3_PCs_average.png")
plotRGB(group_ras, stretch = "lin", main = "Top 3 PCA components for averaged images")
dev.off()

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
#in progress