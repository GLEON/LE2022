#### Sentinel 3 clip & compile######
#Author: Lara Jansen
#start date: 11/20/23

#####Reference info:
#Sentinel 3 -Project coordinate system: UTM Zone 16
#WGS84 +units=m +no_defs

library(tidyverse)
library(sf)

#check
s381622 <- read.csv('Data/Sentinel3/S3B_OLCI_2022_08_22_16_21_43_L2R_projected.csv')

summary(s381622$lon)
# set up files
source_dir = "Data/Sentinel3/"
flist = dir(source_dir, pattern = ".csv", full.names = T)
# read shapefile
shape = sf::read_sf("Data/Buoy_100m_shp/Buoy_100m.shp")
#plot(shape) 

for(i in 1:length(flist)){
  # read each SPDF
  data = read_csv(flist[i]) 
    #st_as_sf(coords = c("lon", "lat"))
  # set CRS
  #st_crs(data) = st_crs(shape) #EPSG:4326
  
  #extract filename and date
  img_name = (str_remove(flist[i], source_dir)) %>% str_remove("_L2R_projected.csv")
  t = (img_name %>% str_split("_"))[[1]][3:5]
  tt = paste(t[1], t[2], t[3]) %>% 
    lubridate::as_date(format = "%Y %m %d")
  
  # clip SPDF to shapefile
  #cutout = st_intersection(data,shape)<- does not work for S3 due to coarse res
  cut=(data %>%filter(lon==-89.40209 & lat==43.092613))
  #plot(cutout)
  
  # add sample date as column in results
  result = cut %>% 
    mutate(date = tt, .before = everything())
  # write file
  write_csv(result, paste0("Data/Sentinel3/L2R_clipped_to_buoy/", img_name, "_buoy.csv"))
}

#############################
# takes all the individual files as inputs and returns one CSV

library(tidyverse)

flist = dir("Data/Sentinel3/L2R_clipped_to_buoy", full.names = T)

dataset = tibble()
for (data in flist){
  tempory <-read_csv(data)
  dataset <-bind_rows(dataset, tempory)
  rm(tempory)
}

t = dataset %>% 
  left_join(dataset) %>% 
  relocate(lon, lat, .after = date) %>% 
  dplyr::select(-c(id, y, x, geometry, transverse_mercator,sza,vza,saa,vaa,raa))

write_csv(t, "Data/Sentinel3/s3_all_clip2buoy.csv")

################################################################################
# DESIS buoy clipped data
#load file-DESIS for now
setwd()
s3<-t
s3<-read.csv('Data/Sentinel3/s3_all_clip2buoy.csv')

#Data summarize -mean, median, std dev
s3_stats<-s3%>%
  dplyr::select(-c(lon,lat)) %>% #drop pixel count, Long, Lat
  group_by(date) %>%
  summarise_all(list(avg=mean,med=median,
                     sd=sd)) #overall mean & median are relatively similar
#export desis summary file
write.csv(s3_stats,'Data/Sentinel3/s3_stats_buoy.csv')
