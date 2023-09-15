library(tidyverse)
#buoy: 43.09885, -89.40545
lat = 43.09885
long = -89.40545
d_lat  = (50 / 6378000) * (180 / pi);
d_lon = (50/6378000) * (180 / pi) / cos(43.09885* pi/180)

flist = dir("Data/DESIS/L2W_destriped/", full.names = T)
data = read_csv(flist[8])

ggplot(data)+
  geom_point(aes(y = lat, x = lon, color = Rrs_640))+
  lims(x= c(-89.43,-89.40))+
  geom_rect(ymax = lat + d_lat, ymin = lat - d_lat,
                xmax = long + d_lon, xmin = long - d_lon)

#DESIS checks - all good for clouds, check images 1,3,4 for sunglint
#06_02_20 - no cloud cover at buoy, lots around lake, possible surface scums or glint
#2020_12_25 - no cloud issue
#2021_06_15 - no issue at buoy, potential sunglint or surface scums
#2021_08_15- no issue at buoy, image has some cloud shadows
#2022_06_23 - no issue at buoy
#2022_06_02 - no issue at buoy
#2022_10_10 - no issue at buoy
#2022_08_31 - no issue at buoy


#PRISMA checks - all good

#2021-06-05: no cloud
#2021-06-11: no cloud issue, surface scums
#2021-09-05: no cloud issue
#2021_11_02: no cloud issue in buoy area
#2021_12_13: no cloud issue
#2022_05_13: no cloud issue
#2022_06_16: no cloud issue
#2022_07_27: no cloud issue in buoy area



