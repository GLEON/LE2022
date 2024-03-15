library(tidyverse)
library(sf)

#clip new PRISMA files to buoy

# set up files
source_dir = "Data/PRISMA/lake"
flist = dir(source_dir, pattern = "cloudless", full.names = T)

# read shapefile
shape = sf::read_sf("Data/Buoy_100m_shp/Buoy_100m.shp")
#plot(shape) 

for(i in 1:8){
  # read each SPDF
  data = read_csv(flist[i]) %>% 
    st_as_sf(coords = c("lon", "lat"))
  # set CRS
  st_crs(data) = st_crs(shape) #EPSG:4326
  
  #extract filename and date
  img_name = (str_remove(flist[i], source_dir)) %>% str_remove(".csv") %>% str_remove("/cloudless_")
  tt = lubridate::as_date(img_name)
  # clip SPDF to shapefile
  cutout = st_intersection(data, shape)
  #plot(cutout)
  
  # add sample date as column in results
  result = cutout %>% 
    mutate(date = tt, .before = everything())
  # write file
  write_csv(result, paste0("Data/PRISMA/buoy/PRISMA_", img_name, "_buoy.csv"))
}


