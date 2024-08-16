#### DESIS binning ######
#Author: Lara Jansen
#start date: 03/04/24

# set up ------------------------------------------------------------------

library(tidyverse)

#load data

flist = dir("Data/DESIS/Vnorm_nonclipped")
desis <- read.csv(file.path("Data/DESIS/Vnorm_nonclipped",flist[1]))
date = flist[1]
dess <- cbind(date,desis)
for(i in 2:length(flist)){
  desis <- read.csv(file.path("Data/DESIS/Vnorm_nonclipped",flist[i]))
  date = flist[i]
  temp <- cbind(date,desis)
  dess <- plyr::rbind.fill(dess,temp)
}

dess <- dess %>%
  mutate(date = substr(date,15,24))

t = dess %>% 
  left_join(dess %>%
              group_by(date) %>% 
              dplyr::summarise(pixel_ct = n())
  ) %>% 
  #relocate(Rrs_909, .before = Rrs_912) %>% 
  relocate(pixel_ct, lon, lat, .after = date) %>% 
  select(-c(id, y, x, geometry, l2_flags, SPM_Nechad2010_645)) #for clip: ORIG_FID, BUFF_DIST


# Spectral binning to 10 nm -----------------------------------------------------------------------
###  rolling mean fxn
byapply <- function(x, by, fun, ...)
{
  # Create index list
  if (length(by) == 1)
  {
    nc <- ncol(x)
    split.index <- rep(1:ceiling(nc / by), each = by, length.out = nc)
  } else # 'by' is a vector of groups
  {
    nc <- length(by)
    split.index <- by
  }
  index.list <- split(seq(from = 1, to = nc), split.index)
  
  # Pass index list to fun using sapply() and return object
  sapply(index.list, function(i)
  {
    do.call(fun, list(x[, i], ...))
  })
}

t2<-t %>% select(-X) # not averaged

desis_binned <- cbind(byapply(t2[-(1:4)], 4, rowMeans), t2[1:4])

#buoy
dbin<-read.csv('Data/DESIS/Vnorm_output/DESIS_bin_names.csv')
#lake
lbin<-read.csv('Data/DESIS/Vnorm_output/desis_lake_bin_names.csv')

#buoy
dbin<-read.csv('Data/DESIS/Vnorm_output/DESIS_bin_names.csv')
#lake
lbin<-read.csv('Data/DESIS/Vnorm_output/desis_lake_bin_names.csv')

d<-as.character(dbin$Avg_norm_rrs)

names(desis_binned)[1:54]<-d

write.csv(desis_binned,'Data/DESIS/Vnorm_output/DESIS_bin10_lake.csv')

# Data summarize ----------------------------------------------------------
# mean, median, std dev
dbin_stats<-desis_binned %>%
  select(-c(pixel_ct,lon,lat)) %>% #drop pixel count, Long, Lat
  drop_na(date)%>%
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,
                     sd=sd)) #overall mean & median are relatively similar
#export desis summary file
write.csv(dbin_stats,'Data/DESIS/Vnorm_buoy_clipped/DESIS_stats_binn.csv')