source('Scripts/TimeSeries_scripts/time_series_plot.R')
#rm(list =ls()[which(ls()!="DATA_long")])

data = DATA %>% 
  select(date, doy, year, season, 
         EpiR_mgC_L, EpiNPP_mgC_L, 
         Cyanobacteria_rel_abund_16S,
         avg_phyco_rfu, avg_chlor_rfu, scaled_chlor_rfu, scaled_phyco_rfu,
         avg_turbidity, avg_fdom,
         Red.RedEdge1.r, ST_B10, MaxCA)
data$season <- factor(data$season, levels = c("Ice-on", "Spring", "Clearwater", "Early Summer", "Late Summer", "Fall"))
# 
# data = DATA_long %>% 
#   filter(!is.na(value)) %>% 
#   pivot_wider(names_from = variable, values_from = value)

ggplot(data)+
  geom_point(aes(x = avg_turbidity, y = avg_fdom))

data %>% 
  select(avg_turbidity, avg_fdom, date, doy, year, season) %>% 
  pivot_longer(cols = c(avg_turbidity, avg_fdom)) %>% 
  na.omit(value) %>% 
  filter(value < 20) %>% 
  ggplot()+
  theme_bw()+
  scale_color_manual(values=c("#73456D",  "#B2CCF1","#EE914A", "#9AD67A","#138E90")) +
  geom_point(aes(x = date, y = value, color = season))+
  facet_wrap(~name, ncol = 1)

data %>% 
  select(avg_turbidity, avg_fdom, date, doy, year, season) %>% 
  filter(avg_turbidity < 20) %>% 
  ggplot()+
  theme_bw()+
  geom_point(aes(x = avg_fdom, y = avg_turbidity, color = year))+
  facet_wrap(~season, scales = "free")

####


####
# pca comparisons
loadings = read_csv("Data/DESIS/desis_PCAtop10_loadings.csv")
  
desis_ldgs = loadings %>% 
  mutate(band = as.numeric(str_remove(band, "X")),
         Sensor = "DESIS") %>% 
  filter(name %in% c("comp_1","comp_2","comp_3","comp_4","comp_5"))

prs_lds = read_csv("Data/PRISMA/prisma_PCAtop10_loadings.csv") %>% 
  mutate(Sensor = "PRISMA")%>% 
  filter(name %in% c("comp_1","comp_2","comp_3","comp_4","comp_5"))

loadings_all = desis_ldgs %>% 
  na.omit(band) %>% 
  bind_rows(prs_lds) %>% 
  filter(name != "comp_5",
         band <= 800) %>% 
  mutate(name = str_replace_all(name, "comp_", "Component "))

ggplot(loadings_all)+
  geom_line(aes(x = band, y = value, color = name, lty = Sensor), lwd = 1.5)+
  theme_bw()+
  labs(x = "Wavelength (nm)", y = "PC Loading")+
  facet_wrap(~name)+
  scale_color_discrete(guide = "none")+
  geom_abline(aes(slope = 0, intercept = 0), lty = 3)+
  theme(text = element_text(size = 14),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.key.width = unit(1, "cm"))
