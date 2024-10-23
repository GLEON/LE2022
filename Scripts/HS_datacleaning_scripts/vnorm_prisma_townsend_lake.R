library(tidyverse)


source_dir <- "Data/PRISMA/L2W_clipped_to_lake250m/"
flist <- dir(source_dir, pattern = "csv", full.names = T)

norm_list <- list()
for(i in 1:length(flist)){
  
  data_small <- read.csv(flist[i]) %>%
    mutate(date = substr(flist[i],44,53)) %>% 
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
  

  normalized = data_long %>% 
    group_by(index) %>% 
    mutate(summed = sum(values^2,na.rm=T)) %>% 
    ungroup() %>% 
    mutate(norm = values/sqrt(summed))
  
  norm_list[[i]] = normalized
  
}

normalized_df <- do.call(rbind,norm_list)

write.csv(normalized_df,file='Data/PRISMA/vnorm_prisma_lake_500clip_noBin.csv')
normalized_df <- read.csv('Data/PRISMA/vnorm_prisma_lake.csv')

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
             by.x='date',by.y='date') %>% 
  mutate(img = case_when(season == 'Spring' ~ 1,
                         season == 'Clearwater' ~ 1,
                         season == 'Early Summer' ~ 1,
                         season == 'Late Summer' & date == '2021-09-05' ~ 1,
                         season == 'Late Summer' & date == '2022-07-27' ~ 2,
                         season == 'Fall' & date == '2021-11-02' ~ 1,
                         season == 'Fall' & date == '2021-12-13' ~ 2))

ggplot(new,aes(wavelength,normal,color=season,fill=season,group=date))+
  geom_line(aes(linetype=factor(img)),size=1)+
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_fill_manual(values=c( "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  # scale_x_log10()+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')

s3_files <- list.files('Data/Sentinel3/S3_L2_acolite/All_S3_PRISMA',full.names = TRUE)
s3_list <- list()

for(i in 1:length(s3_files)){
  
  df = read.csv(s3_files[i]) %>% 
    mutate(date = substr(s3_files[i],53,62)) %>% 
    pivot_longer(cols=c(7:23),names_to='wavelength',values_to='reflectance') %>% 
    mutate(wavelength = parse_number(wavelength)) %>% 
    filter(!is.na(reflectance)) %>% 
    select(date,wavelength,reflectance) %>% 
    mutate(sensor = 'S3') %>% 
    group_by(date,wavelength,sensor) %>% 
    summarise(reflectance = mean(reflectance,na.rm=T))
  
  s3_list[[i]] = df
  
}
s3_df <- do.call(rbind,s3_list) %>% 
  mutate(date = ymd(date))

s3_season <- merge(x=s3_df,y=DATA,
                   by.x='date',by.y='date')


s3_comp <- merge(x=new,y=s3_season,
                 by=c('date','wavelength','season'),
                 all=T) %>% 
  mutate(date=as.character(date)) %>% 
  filter(wavelength <= 800) %>% 
  mutate(img = case_when(season == 'Spring' ~ 1,
                         season == 'Clearwater' ~ 1,
                         season == 'Early Summer' ~ 1,
                         season == 'Late Summer' & date == '2021-09-05' ~ 1,
                         season == 'Late Summer' & date == '2022-07-27' ~ 2,
                         season == 'Fall' & date == '2021-11-02' ~ 1,
                         season == 'Fall' & date == '2021-12-13' ~ 2))

s3_df2 <- s3_df %>% select(date,wavelength,reflectance,sensor) %>% 
  mutate(date = as.character(date))
new2 <- new %>% select(date,wavelength,normal) %>% 
  mutate(sensor='PRISMA') %>% 
  rename('reflectance'=normal)

s3_comp2 <- rbind(s3_df2,new2)

s3_comp3 <- merge(x=s3_comp2,y=DATA,
                  by.x='date',by.y='date') %>% 
  mutate(img = case_when(season == 'Spring' ~ 1,
                         season == 'Clearwater' ~ 1,
                         season == 'Early Summer' ~ 1,
                         season == 'Late Summer' & date == '2021-09-05' ~ 1,
                         season == 'Late Summer' & date == '2022-07-27' ~ 2,
                         season == 'Fall' & date == '2021-11-02' ~ 1,
                         season == 'Fall' & date == '2021-12-13' ~ 2)) %>% 
  filter(wavelength <= 800)


ggplot(s3_comp3 %>% filter(sensor=='PRISMA'),aes(wavelength,reflectance,color=season,fill=season,group=date))+
  geom_line(aes(linetype=factor(img)),size=1)+
  geom_line(data=s3_comp3 %>% filter(sensor=='S3'),
            aes(wavelength,reflectance*8,linetype=factor(img)),color='grey',size=1)+
  geom_point(data=s3_comp3 %>% filter(sensor=='S3'),
             aes(wavelength,reflectance*8),color='grey',size=1.5)+
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  scale_fill_manual(values=c( "#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  facet_wrap(~season)+
  # scale_x_log10()+
  labs(y='Normalized Rrs',
       x='Wavelength (nm)')+
  theme(legend.position = 'none')

