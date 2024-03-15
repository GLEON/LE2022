## Extract hyperspectral raster pixel values as csv

library(sp)
library(raster)
library(sf)

setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/Data/AVIRIS/Buoy_clipped')
r <- stack('ang20160822t193312_rfl_v1n2_img_clipped_downsampled') # read in raster
x <- rasterToPoints(r) # cell values and coordinates
write.csv(x,"AVIRIS_20160822_buoyclipped_30mdownsampled.csv") # Save as csv

r <- stack('ang20160831t201002_rfl_v1n2_img_clipped_downsampled') # read in raster
x <- rasterToPoints(r) # cell values and coordinates
write.csv(x,"AVIRIS_20160831_buoyclipped_30mdownsampled.csv") # Save as csv


setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/Data/AVIRIS/30mdownsampled')
r <- stack('ang20160822t193312_rfl_v1n2_img_250mclipped_30mdownsampled') # read in raster
x <- rasterToPoints(r) # cell values and coordinates
write.csv(x,"AVIRIS_20160822_250mclipped_30mdownsampled.csv") # Save as csv

r <- stack('ang20160831t201002_rfl_v1n2_img_250mclipped_30mdownsampled') # read in raster
x <- rasterToPoints(r) # cell values and coordinates
write.csv(x,"AVIRIS_20160831_250mclipped_30mdownsampled.csv") # Save as csv
