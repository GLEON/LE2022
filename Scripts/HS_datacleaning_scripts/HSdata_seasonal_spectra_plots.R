## Vector normalized & binned spectra plotted seasonally

library(tidyverse)

## DESIS

normalized_df_desis <- read.csv('Data/DESIS/L2W_vnorm/vnorm_desis_lake_500clip_binnedAfterNormalize_long.csv')

normalized_means_desis <- normalized_df_desis %>% 
  group_by(date,wavelength) %>%  
  summarise(normal = mean(norm_binned,na.rm=T),
            normal_sd = sd(norm_binned,na.rm=T))

empty <- data.frame(date = seq(as.Date("01-01-2000", format = "%m-%d-%Y"), 
                               as.Date("12-31-2022", format = "%m-%d-%Y"), by = "day")) %>%
  mutate(doy = yday(date),
         year = year(date))

seasons <- read.csv('MetabolismModel/seasons_from_Robins_paper_long_withplaceholders.csv') %>%
  select(-placeholder) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"))

DATA <- left_join(empty, seasons) %>%
  fill(season, .direction="down")

DATA$season <- factor(DATA$season, levels = 
                        c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))

new_desis <- merge(x=normalized_means_desis,y=DATA,
             by.x='date',by.y='date')

ggplot(new_desis,aes(wavelength,normal,color=season,fill=season,group=date))+
  geom_line(size=1)+
  scale_color_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_fill_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  # scale_x_log10()+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')

## PRISMA

normalized_df <- read.csv('Data/PRISMA/L2W_vnorm/vnorm_prisma_lake_500clip_noBin.csv')

normalized_means <- normalized_df %>% 
  group_by(date,wavelength) %>%  
  summarise(normal = mean(norm,na.rm=T),
            normal_sd = sd(norm,na.rm=T),
            orig = mean(values,na.rm=T),
            orig_sd = sd(values,na.rm=T))

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
             by.x='date',by.y='date') %>% 
  mutate(img = case_when(season == 'Spring' ~ 1,
                         season == 'Clearwater' ~ 1,
                         season == 'Early Summer' ~ 1,
                         season == 'Late Summer' & date == '2021-09-05' ~ 1,
                         season == 'Late Summer' & date == '2022-07-27' ~ 2,
                         season == 'Fall' & date == '2021-11-02' ~ 1,
                         season == 'Fall' & date == '2021-12-13' ~ 2))

ggplot(new,aes(wavelength,normal,color=season,fill=season,group=date))+
  geom_line(size=1)+
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_fill_manual(values=c( "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  # scale_x_log10()+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')
