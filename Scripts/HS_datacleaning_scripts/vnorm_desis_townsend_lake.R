library(tidyverse)
library(lubridate)


source_dir <- "Data/DESIS/L2W_clipped_to_lake250m/"
flist <- dir(source_dir, pattern = "csv", full.names = T)

norm_list <- list()
for(i in 1:length(flist)){
  
  data_small <- read.csv(flist[i]) %>%
    mutate(date = substr(flist[i],50,59)) %>% 
    mutate(date = ymd(date)) %>% 
    select(date,x,y,contains("Rrs"))
  
  data_long = data_small %>%
    rownames_to_column("index") %>%
    pivot_longer(cols=contains('Rrs'),
                 names_to="wavelength",
                 values_to="values",
                 names_prefix="Rrs_") %>%
    mutate(wavelength = as.numeric(wavelength)) %>% 
    filter(wavelength >= 500 & wavelength <= 800) %>% 
    filter(!is.na(wavelength)&!is.na(values))
  #this section does the binning------------
  desis_wav = unique(data_long$wavelength)
  prisma_wavs <- unique(data_long$wavelength)#run this on the data_long from the prisma script first
  
  data_small_small = data_long %>% 
    pivot_wider(names_from=wavelength,values_from=values)
  
  data_index <- data_small_small[,1:4]
  
  desis_binned <- as.data.frame(resample(data_small_small[,-c(1:4)],wav=desis_wav,new.wav=prisma_wavs))

  data_small2 = cbind(data_index,desis_binned)
  #----------------------------
  
  data_long2 = data_small2 %>%
    pivot_longer(cols=5:ncol(data_small2),
                 names_to="wavelength",
                 values_to="values") %>%
    mutate(wavelength = as.numeric(wavelength)) %>% 
    filter(wavelength >= 500 & wavelength <= 800)
  
  normalized = data_long %>% 
    group_by(index) %>% 
    mutate(summed = sum(values^2,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(norm = values/sqrt(summed))
  
  norm_list[[i]] = normalized
  
}

normalized_df <- do.call(rbind,norm_list)

write.csv(normalized_df,file='Data/DESIS/vnorm_desis_lake_500clip_noBin.csv')
normalized_df <- read.csv('Data/DESIS/vnorm_desis_lake.csv')

ggplot(normalized_df,aes(wavelength,norm,group=index))+
  geom_line()+
  facet_wrap(~date)

ggplot(normalized_df,aes(wavelength,values,group=index))+
  geom_line()+
  facet_wrap(~date)

normalized_means <- normalized_df %>% 
  group_by(date,wavelength) %>%  
  summarise(normal = mean(norm,na.rm=T),
            normal_sd = sd(norm,na.rm=T),
            orig = mean(values,na.rm=T),
            orig_sd = sd(values,na.rm=T))

ggplot(normalized_means,aes(wavelength,normal))+
  geom_line()+
  geom_ribbon(aes(ymin=normal-normal_sd,ymax=normal+normal_sd),alpha=0.2)+
  facet_wrap(~date,scales='free')

empty <- data.frame(date = seq(as.Date("01-01-2000", format = "%m-%d-%Y"), as.Date("12-31-2022", format = "%m-%d-%Y"), by = "day")) %>%
  mutate(doy = yday(date),
         year = year(date))
seasons <- read.csv('MetabolismModel/seasons_from_Robins_paper_long_withplaceholders.csv') %>%
  select(-placeholder) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"))
DATA <- left_join(empty, seasons) %>%
  fill(season, .direction="down")

DATA$season <- factor(DATA$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

new <- merge(x=normalized_means,y=DATA,
             by.x='date',by.y='date')

ggplot(new,aes(wavelength,normal,color=season,fill=season,group=date))+
  geom_line(size=1)+
  scale_color_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_fill_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  # scale_x_log10()+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')


