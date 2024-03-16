## Summarizing HS data ###
# GLEON LEXP 22 ###
#start date: 3/8/2023 ###

# updated: 7/7/2023 by Samantha for other sensors

#load packages
library(tidyverse)
library(plyr)

################################################################################
# DESIS buoy clipped data
#load file-DESIS for now
setwd()
desis100<-read.csv('Data/DESIS/DESIS_all_clip2buoy_destriped.csv')

#Data summarize -mean, median, std dev
desis_stats<-desis100 %>%
  select(-c(pixel_ct,lon,lat)) %>% #drop pixel count, Long, Lat
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,
                     sd=sd)) #overall mean & median are relatively similar
#export desis summary file
write.csv(desis_stats,'Data/DESIS/DESIS_stats_buoy.csv')

################################################################################
# DESIS lake clipped data

#Load first file and add to DESIS_lake that will eventually contain all DESIS lake data
flist = dir("Data/DESIS/L2W_destriped")
desis <- read.csv(file.path("Data/DESIS/L2W_destriped",flist[1]))
date = flist[1]
DESIS_lake <- cbind(date,desis)
for(i in 2:length(flist)){
  desis <- read.csv(file.path("Data/DESIS/L2W_destriped",flist[i]))
  date = flist[i]
  temp <- cbind(date,desis)
  DESIS_lake <- rbind.fill(DESIS_lake,temp)
}

DESIS_lake <- DESIS_lake %>%
  mutate(date = substr(date,15,24))

#Data summarize -mean, median, std dev
desis_stats <- DESIS_lake %>%
  select(-c(id,y,x,lon,lat,l2_flags,SPM_Nechad2010_645,geometry)) %>% #drop unnecessary columns
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,sd=sd)) 

#export summary file
write.csv(desis_stats,'Data/DESIS/DESIS_stats_lake.csv')

################################################################################
# PRISMA buoy clipped data

#Load first file and add to PRISMA_buoy that will eventually contain all PRISMA buoy data
flist = dir("Data/PRISMA/buoy")
prisma <- read.csv(file.path("Data/PRISMA/buoy",flist[1]))
date = flist[1]
PRISMA_buoy <- cbind(date,prisma)
for(i in 2:length(flist)){
  prisma <- read.csv(file.path("Data/PRISMA/buoy",flist[i]))
  date = flist[i]
  temp <- cbind(date,prisma)
  PRISMA_buoy <- rbind.fill(PRISMA_buoy,temp)
}

PRISMA_buoy <- PRISMA_buoy %>%
  mutate(date = substr(date,8,17))

#Data summarize -mean, median, std dev
prisma_stats <- PRISMA_buoy %>%
  select(-c("...1",X,y,x,aot_550,glint_mean,glint_std,lon,lat,BUFF_DIST,ORIG_FID,geometry)) %>% #drop unnecessary columns
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,sd=sd)) 

#export summary file
write.csv(prisma_stats,'Data/PRISMA/PRISMA_stats_buoy.csv')

################################################################################
# PRISMA lake clipped data

#Load first file and add to PRISMA_lake that will eventually contain all PRISMA lake data
flist = dir("Data/PRISMA/lake")
prisma <- read.csv(file.path("Data/PRISMA/lake",flist[1]))
date = flist[1]
PRISMA_lake <- cbind(date,prisma)
for(i in 2:length(flist)){
  prisma <- read.csv(file.path("Data/PRISMA/lake",flist[i]))
  date = flist[i]
  temp <- cbind(date,prisma)
  PRISMA_lake <- rbind.fill(PRISMA_lake,temp)
}

PRISMA_lake <- PRISMA_lake %>%
  mutate(date = substr(date,11,20))

#Data summarize -mean, median, std dev
prisma_stats <- PRISMA_lake %>%
  select(-c("X.1",X,y,x,aot_550,glint_mean,glint_std,lon,lat,geometry)) %>% #drop unnecessary columns
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,sd=sd)) 

#export summary file
write.csv(prisma_stats,'Data/PRISMA/PRISMA_stats_lake.csv')

################################################################################
# AVIRIS buoy clipped data

#Load first file and add to AVIRIS_buoy that will eventually contain all AVIRIS buoy data
temp <- read.csv("Data/AVIRIS/AVIRIS_20160822_buoyclipped_30mdownsampled.csv")
date = "20160822"
AVIRIS_20160822 <- cbind(date,temp)
temp <- read.csv("Data/AVIRIS/AVIRIS_20160831_buoyclipped_30mdownsampled.csv")
date = "20160831"
AVIRIS_20160831 <- cbind(date,temp)
AVIRIS_buoy <- rbind.fill(AVIRIS_20160822,AVIRIS_20160831)

#Data summarize -mean, median, std dev
aviris_stats <- AVIRIS_buoy %>%
  select(-c(X,y,x)) %>% #drop unnecessary columns
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,sd=sd)) 

#export summary file
write.csv(aviris_stats,'Data/AVIRIS/AVIRIS_stats_buoy.csv')

################################################################################
# AVIRIS lake clipped data

#Load first file and add to AVIRIS_lake that will eventually contain all AVIRIS lake data
temp <- read.csv("Data/AVIRIS/lake/AVIRIS_20160822_250mclipped_30mdownsampled_sunglintcorr01.csv")
date = "20160822"
AVIRIS_20160822 <- cbind(date,temp)
temp <- read.csv("Data/AVIRIS/lake/AVIRIS_20160831_250mclipped_30mdownsampled.csv")
date = "20160831"
AVIRIS_20160831 <- cbind(date,temp)
AVIRIS_lake <- rbind.fill(AVIRIS_20160822,AVIRIS_20160831)

#Data summarize -mean, median, std dev
aviris_stats <- AVIRIS_lake %>%
  select(-c(id,y,x)) %>% #drop unnecessary columns
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,sd=sd)) 

#export summary file
write.csv(aviris_stats,'Data/AVIRIS/AVIRIS_stats_lake.csv')





