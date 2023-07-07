# Preliminary analysis of CyAN data for Lake Mendota - and creating loop
# January 6, 2023 and updated May 30, 2023
# AGS

# Load packages
pacman::p_load(raster, sf, rdgal, tidyverse)

# Before the for loop
outline = shapefile("/Users/Anna/Desktop/LakeExpedition/cyan/mendota_outline/mendota_outline.shp")
dataset = data.frame(DN = rep(NA, 365),
                     date = rep(NA, 365))

# everntually put in the for loop
crs(data) = "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0
+y_0=0 +datum=NAD83 +units=m +no_defs"
extent(data) = c(xmin = 434403, xmax = 1034403, ymin = 2114826, ymax = 2714826)
aea_outline = spTransform(outline, crs(data))
mendota_cyan = mask(data, aea_outline) %>% crop(aea_outline)
new_extent <- extent(530590, 531000, 2255000, 2255005) # wrong extent, change later
cropped <- crop(x = mendota_cyan, y = new_extent)



file_path <- "/Users/Anna/Desktop/LakeExpedition/cyan/files.rtf"
links <- readLines(file_path) # ignore the warning message
links <- links[10:341]
links <- substr(links, 1, nchar(links) - 1)

# Step 3: Iterate over the links
for (i in seq_along(links)) {
  dataset$date[i] <- paste(links[i])

  # Download the file locally
  dest_file <- paste("/Users/Anna/Desktop/LakeExpedition/cyan/2002/", basename(links[i]), sep = "")
  download.file(links[i], destfile = dest_file, mode = "wb")
  
  # Extract the file name without the path and use it to read the raster
  file_name <- basename(dest_file)
  raster_data <- raster(file_name)
  
  # Compute statistics on the raster data
  # raster_stats <- calc(raster_data, fun = mean, na.rm = TRUE)
  # Access the computed statistics and assign to the dataset
  # dataset$DN[i] <- raster_stats
  
  #dataset$DN[i] <- paste(cropped@data@values)
  #dataset$DN[i] <- mean(raster_data[], na.rm = TRUE)
  
  # Remove the temporary file
  file.remove(dest_file)
}

  
  
 
  
 

# Looks like the .tif files it creates aren't even openable or good, they're corrupted some how. stopping for the day. 
# Maybe this website holds the key: https://oceancolor.gsfc.nasa.gov/data/download_methods/ 









# Step 3: Load the downloaded file using raster()
data <- raster("path/to/your/local/file.tif")



