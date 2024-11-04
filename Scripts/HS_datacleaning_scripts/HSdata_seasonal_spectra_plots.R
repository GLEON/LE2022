## Vector normalized & binned spectra plotted seasonally

library(tidyverse)
library(egg)

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

## Combined plot...

ggplot()+
  geom_line(data=new,aes(wavelength,normal,color=season,group=date),size=1)+
  geom_line(data=new_desis,aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')

# With Clear Lake example spectra...
# Read in rrs files (processed radiometer data)
Example_RRS <- read_csv('Data/Reference_field_spectra/Sharp et al 2019_example Clear Lake spectra.txt',skip=31,col_names=FALSE) %>%
  rename(nm=X1,rrs=X2)

p_Spring <-
  ggplot()+
  geom_line(data=filter(new,season=="Spring"),aes(wavelength,normal,color=season,group=date),size=1)+
  geom_ribbon(data=filter(new,season=="Spring"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                    fill=season),alpha=0.2)+
  geom_line(data=filter(new_desis,season=="Spring"),aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
  geom_ribbon(data=filter(new_desis,season=="Spring"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                      fill=season),alpha=0.2)+
  scale_color_manual(values="#73456D") +
  scale_fill_manual(values= "#73456D") +
  labs(y='Normalized Rrs',
       x='Wavelength (nm)',
       title='Spring')+
  theme_bw()+
  theme(legend.position = 'none')+
  xlim(c(500,800))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank())
  
p_Clearwater <-
  ggplot()+
    geom_line(data=filter(new,season=="Clearwater"),aes(wavelength,normal,color=season,group=date),size=1)+
    geom_ribbon(data=filter(new,season=="Clearwater"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                      fill=season),alpha=0.2)+
    geom_line(data=filter(new_desis,season=="Clearwater"),aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
    geom_ribbon(data=filter(new_desis,season=="Clearwater"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                            fill=season),alpha=0.2)+
    scale_color_manual(values="#B2CCF1") +
    scale_fill_manual(values= "#B2CCF1") +
    labs(y='Normalized Rrs',
         x='Wavelength (nm)',
         title='Clearwater')+
    theme_bw()+
    theme(legend.position = 'none')+
    xlim(c(500,800))+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())

p_EarlySummer <-
  ggplot()+
    geom_line(data=filter(new,season=="Early Summer"),aes(wavelength,normal,color=season,group=date),size=1)+
    geom_ribbon(data=filter(new,season=="Early Summer"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                          fill=season),alpha=0.2)+
    geom_line(data=filter(new_desis,season=="Early Summer"),aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
    geom_ribbon(data=filter(new_desis,season=="Early Summer"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                                fill=season),alpha=0.2)+
    scale_color_manual(values="#EE914A") +
    scale_fill_manual(values= "#EE914A") +
    labs(y='Normalized Rrs',
         x='Wavelength (nm)',
         title='Early Summer')+
    theme_bw()+
    theme(legend.position = 'none')+
    xlim(c(500,800))+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())

p_LateSummer <-
  ggplot()+
    geom_line(data=filter(new,season=="Late Summer"),aes(wavelength,normal,color=season,group=date),size=1)+
    geom_ribbon(data=filter(new,season=="Late Summer"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                            fill=season),alpha=0.2)+
    geom_line(data=filter(new_desis,season=="Late Summer"),aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
    geom_ribbon(data=filter(new_desis,season=="Late Summer"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                                  fill=season),alpha=0.2)+
    scale_color_manual(values="#9AD67A") +
    scale_fill_manual(values= "#9AD67A") +
    labs(y='Normalized Rrs',
         x='Wavelength (nm)',
         title='Late Summer')+
    theme_bw()+
    theme(legend.position = 'none')+
    xlim(c(500,800))+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())

p_Fall <-
  ggplot()+
    geom_line(data=filter(new,season=="Fall"),aes(wavelength,normal,color=season,group=date),size=1)+
    geom_ribbon(data=filter(new,season=="Fall"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                           fill=season),alpha=0.2)+
    geom_line(data=filter(new_desis,season=="Fall"),aes(wavelength,normal,color=season,group=date),size=1,linetype="dashed")+
    geom_ribbon(data=filter(new_desis,season=="Fall"),aes(x=wavelength,y=normal,ymin=normal-normal_sd,ymax=normal+normal_sd,
                                                                 fill=season),alpha=0.2)+
    scale_color_manual(values="#138E90") +
    scale_fill_manual(values= "#138E90") +
    labs(y='Normalized Rrs',
         x='Wavelength (nm)',
         title='Fall')+
    theme_bw()+
    theme(legend.position = 'none') +
    xlim(c(500,800)) 

p_RefSpectra <-
  ggplot() +
    geom_line(data = as.data.frame(Example_RRS), aes(x=nm, y=rrs)) +
    theme_bw() +
    labs(x = "Wavelength (nm)", 
         y = "Rrs",
         title='Reference field spectra') +
    xlim(c(500,800)) +
    ylim(c(0,0.05))

#Seasonal_Spectra_figure <- 
  ggarrange(p_Spring,
            p_Clearwater,
            p_EarlySummer,
            p_LateSummer,
            p_Fall,
            p_RefSpectra,
            ncol = 2)
  