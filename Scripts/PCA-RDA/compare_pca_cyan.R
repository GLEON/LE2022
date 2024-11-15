source('Scripts/CyAN_vs_buoyphyco.R')
# Statistical comparison across seasons
cyan_subset %>% 
  # filter(MaxCA < 4000000) %>% 
  filter(avg_phyco_rfu_corr < 3) %>% 
  ggplot(mapping=aes(x = MaxCA, y = avg_phyco_rfu_corr, color = season)) +
  geom_point(size = 1.8) +
  geom_smooth(method = 'lm') +
  theme_bw(base_size = 16) +
  facet_wrap(~season, ncol = 1, scales = "free_x", strip.position = "left")+
  scale_x_log10()

mod1 = lm(avg_phyco_rfu_corr ~ MaxCI*season, data= cyan_subset)
# filter(cyan_subset, 
#        avg_phyco_rfu_corr < 3,
#        MaxCA < 4000000))
summary(mod1)
plot(mod1)

ggplot(cyan_subset)+
  geom_boxplot(aes(x= season, y = avg_phyco_rfu_corr))

ggplot(cyan_subset)+
  geom_boxplot(aes(x=season, y= MaxCI))

# There is no seasonal difference in the relationship of CI and phyco 

## Look at CI vs. PCA scores

prsm_pc = readRDS('Outputs/PCA_outputs/PRISMA/prsm_buoy_pca_means_1024.rds') %>% 
  mutate(date = as.Date(date))

dat = cyan %>% 
  right_join(prsm_pc) %>% 
  select(date:season,avg_phyco_rfu_corr:avg_turbidity, AvgCI, MaxCI,Comp.1:Comp.4) %>% 
  mutate(sensor = 'PRISMA')

desis_pc = readRDS('Outputs/PCA_outputs/DESIS/desis_buoy_pca_means_1024.rds') %>% 
  mutate(date = as.Date(date))

datd = cyan %>% 
  right_join(desis_pc) %>% 
  select(date:season,avg_phyco_rfu_corr:avg_turbidity, AvgCI, MaxCI,Comp.1:Comp.4) %>% 
  mutate(sensor = 'DESIS')

dat_all = dat %>% 
  bind_rows(datd) %>% 
  pivot_longer(Comp.1:Comp.4, names_to = 'PC')

ggplot(dat_all, aes(x = AvgCI, y = value))+
  geom_smooth(#data = filter(dat_all, sensor=="DESIS"), 
              method='lm', color = 'black')+
  geom_point(aes(shape = sensor, color = season), cex = 2)+
  scale_shape_manual(values= c(19,1))+
  scale_color_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ggpmisc::stat_poly_eq(label.x = .95, label.y = .05)+
  facet_wrap(~PC, scales = 'free_y')+
  theme_bw()+
  labs(y = "Mean Principal Component value", x = "Mean CyAN CI")


ggplot(dat_all, aes(x = MaxCI, y = value))+
  geom_smooth(#data = filter(dat_all, sensor=="DESIS"), 
    method='lm', color = 'black')+
  geom_point(aes(shape = sensor, color = season), cex = 2)+
  scale_shape_manual(values= c(19,1))+
  scale_color_manual(values=c("#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  ggpmisc::stat_poly_eq(label.x = .95, label.y = .05)+
  facet_wrap(~PC, scales = 'free_y')+
  theme_bw()+
  labs(y = "Mean Principal Component value", x = "Maximum CyAN CI")