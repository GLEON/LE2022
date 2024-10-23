library(tidyverse)
library(lubridate)
library(prospectr)


prisma <- read.csv('Data/PRISMA/L2W_vnorm/vnorm_prisma_lake_500clip_noBin.csv')
prisma_wavs <- unique(prisma$wavelength)

desis <- read.csv('Data/DESIS/L2W_vnorm/vnorm_desis_lake_500clip_noBin.csv')

desis_wav <- unique(desis$wavelength)

data_small = desis %>% 
  select(-X,-index,-values,-summed) %>% 
  pivot_wider(names_from=wavelength,values_from=norm)

data_index <- data_small[,1:3]

desis_binned <- as.data.frame(resample(data_small[,-c(1:3)],wav=desis_wav,new.wav=prisma_wavs))

write.csv(desis_binned,file='Data/DESIS/L2W_vnorm/vnorm_desis_lake_500clip_binnedAfterNormalize.csv')

# Format long and add back in dates etc
desis_long <- cbind(data_small[,c(1:3)],desis_binned) %>%
  pivot_longer(!c(date,x,y),names_to="wavelength",values_to="norm_binned")

write.csv(desis_long,file='Data/DESIS/L2W_vnorm/vnorm_desis_lake_500clip_binnedAfterNormalize_long.csv')
