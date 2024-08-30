# DESIS Vector Normalization -------------------
# author: L. Jansen
# start date: 08/16/2024

# set up ------------------------------------------------------------------

require(tidyverse)
require(devtools)
#remotes::install_github("meireles/spectrolab") # not working
install.packages("https://cran.r-project.org/src/contrib/Archive/spectrolab/spectrolab_0.0.18.tar.gz", repos = NULL)
require(spectrolab)
#require(prospectr)

install.packages("https://cran.r-project.org/src/contrib/Archive/ppls/ppls_1.6-1.1.tar.gz", repos = NULL)
require(ppls)

require(ChemoSpec)

# load data - lake example 
des0622 <- read.csv(file = "Data/DESIS/L2W_clipped_to_lake250m/DESIS_HSI_002_2020_06_02_20_11_33_lake250m.csv")

# check 8/15/20 file w/minimal data
des0821 <- read.csv(file = "Data/DESIS/L2W_clipped_to_lake250m/DESIS_HSI_002_2021_08_15_20_30_19_lake250m.csv")

# check a buoy file
ds0620 <- read.csv(file = "Data/DESIS/L2W_clipped_to_buoy/DESIS_HSI_002_2020_06_02_20_11_33_buoy.csv")

# load already vnorm data
ds0622vm <- read.csv(file = "Data/DESIS/old_data/Vnorm_nonclipped/DESIS_HSI_002_2020_06_02_20_11_33_Mendota_clean_vnormalized.csv")

# vector normalize test ---------------------------------------------------

# test a fxn ~ as normalize.vector
norm <- function(x) {x / sqrt(sum(x^2))}
spec <- des0821 %>% select(contains("Rrs_")) 
meta <- des0821 %>% select(c( y, x, l2_flags, SPM_Nechad2010_645, geometry))
vnorm <- t(apply(spec, 1, function(x) nrm2(x)))
vnorm <- as.data.frame(vnorm) 
vnorm <- cbind(vnorm, meta)

des0821_nom <- t(apply(des0821[5:217], 1, function(x) norm(x)))
des0821_nom <- as.data.frame(des0821_nom)
 
# test another fxn: dividing each y-value by the sum of the y-values in a given spectrum
nrm2 <- function(x) {x/ sum(x)}

des0821_n2m <- t(apply(des0821[5:217], 1, function(x) nrm2(x)))
des0821_n2m <- as.data.frame(des0821_n2m) # <- row mean v close to 0 & sum of 1

# sum & mean check ------------------------
rowMeans(des0821_n2m[3925:3930,], na.rm = TRUE)
rowSums(des0821_n2m[3925:3930,], na.rm = TRUE)

# lake v_norm all scenes ----------------------------------------------------
nrm2 <- function(x) {x/ sum(x)}

#DESIS 
#des0622_n2m <- t(apply(des0622[7:218], 1, function(x) nrm2(x)))

# set up files
source_dir = "Data/DESIS/L2W_clipped_to_lake250m/"
flist = dir(source_dir, pattern = "csv", full.names = T)

for(i in 1:length(flist)){
  # read each SPDF
  data = read_csv(flist[i]) 
  #extract filename and date
  img_name = (str_remove(flist[i], source_dir)) %>% 
    str_remove("_lake250m.csv")
  #t = (img_name %>% str_split("_"))[[1]][4:6]
  #tt = paste(t[1], t[2], t[3]) %>% 
   # lubridate::as_date(format = "%Y %m %d")
  
  # vector normalization
  spec <- data %>% select(contains("Rrs_")) 
  meta <- data %>% select(c( x, y, date, l2_flags, SPM_Nechad2010_645, geometry))
  vnorm <- t(apply(spec, 1, function(x) nrm2(x)))
  vnorm <- as.data.frame(vnorm) 
  vnorm <- cbind(vnorm, meta)
  
  # add sample date as column in results
  #result = vnorm %>% 
   # dplyr::mutate(date = tt, .before = everything())
  
  # write file
  write_csv(vnorm, paste0("Data/DESIS/L2W_vnorm/lake", img_name, "_lk250m_vnorm.csv"))
}

# buoy v_norm --------------------------------------------------------------------

# set up files
source_dir2 = "Data/DESIS/L2W_clipped_to_buoy/"
flist2 = dir(source_dir2, pattern = "_buoy.csv", full.names = T)

for(i in 1:length(flist2)){
  # read in file
  data = read_csv(flist2[i]) 
 
  #extract filename and date
  img_name = (str_remove(flist2[i], source_dir2)) %>% 
    str_remove("_buoy.csv")
  
  # vector normalization
  spec <- data %>% select(contains("Rrs_"))  
  meta <- data %>% select(c(y, x, date, lon, lat, 
                            l2_flags, SPM_Nechad2010_645, geometry))
  vnorm <- t(apply(spec, 1, function(x) nrm2(x)))
  vnorm <- as.data.frame(vnorm) 
  vnorm <- cbind(vnorm, meta)
  
  # add sample date as column in results
  result = vnorm %>% 
    dplyr::mutate(date = date, .before = everything())
  # write file
  write_csv(result, paste0("Data/DESIS/L2W_vnorm/buoy/", img_name, "_buoy_vnorm.csv"))
}
# Alternative options ---------------------------------------------------------------
# spectrolab
des0622sp <- des0622 %>% 
  select(!c(Rrs_401, y, x, lon, lat, l2_flags, SPM_Nechad2010_645, geometry)) %>% 
  rename_with(~ str_remove(., "Rrs_"), everything()) %>% 
  as_spectra(name_idx = 1)

d0622_vn_sp <- normalize(des0622sp)
# compare w/prospectr
snv_d0622 <- as.data.frame(standardNormalVariate(X = des0622[7:218])) # different - not working


