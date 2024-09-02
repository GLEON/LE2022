# DESIS binning ---------------------------------------------------------------
#Author: Lara Jansen
#start date: 03/04/24 - updated 8/30/24

# set up ------------------------------------------------------------------

library(tidyverse)

# LOAD DATA
# lake data ---------------------------------------------------------------
flist = dir("Data/DESIS/L2W_vnorm/lake")

desis <- read.csv(file.path("Data/DESIS/L2W_vnorm/lake",flist[1])) # lake

date = flist[1]
dess <- cbind(date, desis)

for(i in 2:length(flist)){
  desis <- read.csv(file.path("Data/DESIS/L2W_vnorm/lake",flist[i]))
  date = flist[i]
  temp <- cbind(date,desis)
  dess <- plyr::rbind.fill(dess,temp)
}

dess <- dess %>%
  mutate(date = substr(date,15,24))

t = dess %>% 
  left_join(dess %>%
              group_by(date) %>% 
              dplyr::summarise(pixel_ct = n())) %>% 
  #relocate(Rrs_909, .before = Rrs_912) %>% 
   relocate(pixel_ct, x, y, .after = date) %>% 
  select(-c(geometry, l2_flags, SPM_Nechad2010_645)) #for clip: ORIG_FID, BUFF_DIST

# buoy data ---------------------------------------------------------------

flist2 = dir("Data/DESIS/L2W_vnorm/buoy")

desis_b <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[1])) # buoy
date = flist2[1]
des_b <- cbind(date, desis_b)

# 2
desis_b2 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[2])) 
date = flist2[2]
des_b2 <- cbind(date, desis_b2)

# 3
desis_b3 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[3])) 
date = flist2[3]
des_b3 <- cbind(date, desis_b3)

# 4
desis_b4 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[4])) 
date = flist2[4]
des_b4 <- cbind(date, desis_b4)

# 5
desis_b5 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[5])) 
date = flist2[5]
des_b5 <- cbind(date, desis_b5)

# 6
desis_b6 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[6])) 
date = flist2[6]
des_b6 <- cbind(date, desis_b6)

# 7
desis_b7 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[7])) 
date = flist2[7]
des_b7 <- cbind(date, desis_b7)

# 8
desis_b8 <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[8])) 
date = flist2[8]
des_b8 <- cbind(date, desis_b8)

dss_1b8 <- plyr::rbind.fill(des_b, des_b2, des_b3, des_b4,
                            des_b5, des_b6, des_b7, des_b8)

# loop - not working for the buoy data
#for(i in 2:length(flist2)){
 # desis_b <- read.csv(file.path("Data/DESIS/L2W_vnorm/buoy", flist2[i]))
  #date = flist2[i]
  #temp <- cbind(date, desis_b)
  #dss_b <- plyr::rbind.fill(des_b, temp)
  #}

dss_1b8 <- dss_1b8 %>%
  mutate(date = substr(date,15,24))

t_b = dss_1b8 %>% 
  left_join(dss_b %>%
              group_by(date) %>% 
              dplyr::summarise(pixel_ct = n())
  ) %>% 
  #relocate(Rrs_909, .before = Rrs_912) %>% 
  relocate(pixel_ct, x, y, .after = date) %>% 
  select(-c(geometry, l2_flags, SPM_Nechad2010_645)) #for clip: ORIG_FID, BUFF_DIST

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

## lake --------------- 

desis_binned <- cbind(byapply(t[-(1:4)], 4, rowMeans), t[1:4])

# names
dbin <- read.csv('Data/DESIS/L2W_binned/DESIS_bin_names.csv')

l <- as.character(dbin$Avg_norm_rrs)

names(desis_binned)[1:54] <- l

write.csv(desis_binned,'Data/DESIS/L2W_binned/DESIS_bin10_lake.csv')

## buoy ------------------

des_b_binned <- cbind(byapply(t_b[-(1:4)], 4, rowMeans), t_b[1:4])

db <- as.character(dbin$Avg_norm_rrs)

names(des_b_binned)[1:54] <- db
write.csv(des_b_binned,'Data/DESIS/L2W_binned/DESIS_bin10_buoy.csv') #for all users: Data/DESIS/..csv
# Data summarize ----------------------------------------------------------
# mean, median, std dev
dbin_stats <- desis_binned %>%
  select(-c(pixel_ct,lon,lat)) %>% #drop pixel count, Long, Lat
  drop_na(date)%>%
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,
                     sd=sd)) #overall mean & median are relatively similar
#export desis summary file
write.csv(dbin_stats,'Data/DESIS/L2W_binned/DESIS_stats_binn.csv')