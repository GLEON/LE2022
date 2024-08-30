# PRISMA Vector Normalization ----------------------------------------------
# author: L. Jansen
# start date: 08/30/2024

# set up ------------------------------------------------------------------

require(tidyverse)
require(devtools)

# load data - lake example 
prsm0621 <- read.csv(file = "Data/PRISMA/L2W_clipped_to_lake250m/PRISMA_2021_06_11_17_05_50_lake250m.csv")

# vector normalize test ------------------------------------------------

# test another fxn: dividing each y-value by the sum of the y-values in a given spectrum
nrm2 <- function(x) {x/ sum(x)}

prsm0621_n2m <- t(apply(prsm0621[5:159], 1, function(x) nrm2(x)))
prsm0621_n2m <- as.data.frame(prsm0621_n2m) # <- row mean v close to 0 & sum of 1
##  w/ppls

# sum & mean check 
rowMeans(prsm0621_n2m[19:22,], na.rm = TRUE)
rowSums(prsm0621_n2m[19:22,], na.rm = TRUE)

# lake vnorm  all scenes --------------------------------------------------------------
nrm2 <- function(x) {x/ sum(x)}

# set up files
source_dir = "Data/PRISMA/L2W_clipped_to_lake250m/"
flist = dir(source_dir, pattern = "_lake250m.csv", full.names = T)

for(i in 1:length(flist)){
  # read each file
  data = read_csv(flist[i]) 
  
  #extract filename and date
  img_name = (str_remove(flist[i], source_dir)) %>% 
    str_remove("_lake250m.csv")
  
  # vector normalization
  spec <- data %>% select(contains("Rrs_")) 
  meta <- data %>% select(c( y, x, date, l2_flags, SPM_Nechad2010_646, geometry))
  vnorm <- t(apply(spec, 1, function(x) nrm2(x)))
  vnorm <- as.data.frame(vnorm) 
  vnorm <- cbind(vnorm, meta)
  
  # add sample date as column in results
  result = vnorm %>% 
    dplyr::mutate(date = date, .before = everything())
  # write file
  write_csv(result, paste0("Data/PRISMA/L2W_vnorm/lake/", img_name, "_lk250m_vnorm.csv"))
}

# buoy v_norm -------------------------------------------------------------

# set up files
source_dir2 = "Data/PRISMA/L2W_clipped_to_buoy/"
flist2 = dir(source_dir2, pattern = "_buoy.csv", full.names = T)

for(i in 1:length(flist2)){
  
  # read each file
  data = read_csv(flist2[i]) 
  
  #extract filename and date
  img_name = (str_remove(flist2[i], source_dir2)) %>% 
    str_remove("_buoy.csv")
  
  # vector normalization
  spec <- data %>% select(contains("Rrs_")) 
  meta <- data %>% select(c( y, x, date, l2_flags, SPM_Nechad2010_646, geometry))
  vnorm <- t(apply(spec, 1, function(x) nrm2(x)))
  vnorm <- as.data.frame(vnorm) 
  vnorm <- cbind(vnorm, meta)
  
  # add sample date as column in results
  result = vnorm %>% 
    dplyr::mutate(date = date, .before = everything())
  # write file
  write_csv(result, paste0("Data/PRISMA/L2W_vnorm/buoy/", img_name, "_buoy_vnorm.csv"))
}
