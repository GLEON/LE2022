#########################
# Correlation testing of PCA outputs vs in-situ
# L, Jansen
# start date: 4/22/2024
###########################


# set up ------------------------------------------------------------------

library(tidyverse)
library(corrplot)
library(GGally)

# files

hs_pca <- read_csv("Data/Buoy/mean_PCA_scores_at_buoy.csv")

buoy <- read_csv("Data/Buoy/hyperspectral_dates_conditions_v2.csv")

# join

hs_pca_b <- 
  hs_pca %>% 
  left_join(buoy,
            by = join_by(date))

# corr testing ------------------------------------------------------------

# pull out PRISMA n DESIS for now
# then select component 1-3 & NPP, R, secchi
# corr_plot

## metabolism model data 1st
corr_test<-
  hs_pca_b %>% 
  filter(sensorid != "AVIRIS") %>% 
  select(EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  cor(method = "spearman")

corrplot(corr_test, type = "upper")

## rfu data
corr_rfu <-
  hs_pca_b %>% 
  filter(sensorid != "AVIRIS") %>% 
  select(scaled_phyco_rfu, scaled_chlor_rfu, avg_chlor_rfu,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")

corrplot(corr_rfu, type = "upper")
# no notable correlations

## Turbidity
corr_turb <-
  hs_pca_b %>% 
  filter(sensorid != "AVIRIS") %>% 
  select(avg_fdom, avg_turbidity, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")  

corrplot(corr_turb, type = "upper")

# try a single sensor -------------------------------------------------------------
#- PRISMA

corr_tpsm <-
  hs_pca_b %>% 
  filter(sensorid == "PRISMA") %>% 
  select(avg_fdom, avg_turbidity, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")  

corrplot(corr_tpsm, type = "upper")

# comp. 3 n turb highly - corr: -0.94
# comp. 3 n fdom high corr: 0.77

#- DESIS 
corr_tdss <-
  hs_pca_b %>% 
  filter(sensorid == "DESIS") %>% 
  select(avg_fdom, avg_turbidity, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")  

corrplot(corr_tdss, type = "upper")

# comp. 2 n turb highly corr: 0.89

###  Chla/phyco rfu
# PRISMA

corr_cpsm <-
  hs_pca_b %>% 
  filter(sensorid == "PRISMA") %>% 
  select(scaled_chlor_rfu, scaled_phyco_rfu, avg_chlor_rfu,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")  

corrplot(corr_cpsm, type = "upper")

# comp. 3 n chloro corr: -0.77
# comp. 2 n phyco corr: 0.65

## desis

corr_cdss <-
  hs_pca_b %>% 
  filter(sensorid == "DESIS") %>% 
  select(scaled_chlor_rfu, scaled_phyco_rfu, avg_chlor_rfu,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")  

corrplot(corr_cdss, type = "upper")

# comp. 2 n chloro highly corr: 0.90
# comp. 2 n phyco corr: 0.68

### metabolism
# prisma
corr_mpsm<-
  hs_pca_b %>% 
  filter(sensorid == "PRISMA") %>% 
  select(EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  cor(method = "spearman")

corrplot(corr_mpsm, type = "upper")

# desis

corr_mdss<-
  hs_pca_b %>% 
  filter(sensorid == "DESIS") %>% 
  select(EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  cor(method = "spearman")

corrplot(corr_mdss, type = "upper")

### visualize the final things --------------------------------------------------------------------

# restricted corrplots to notable correlations
# DESIS
corr_dss2<-
  hs_pca_b %>% 
  filter(sensorid == "DESIS") %>% 
  select(avg_turbidity, avg_fdom,
         scaled_phyco_rfu, scaled_chlor_rfu,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")

corrplot(corr_dss2, type = "upper")

#PRISMA

corr_psm2<-
  hs_pca_b %>% 
  filter(sensorid == "PRISMA") %>% 
  select(avg_turbidity, avg_fdom,
         scaled_phyco_rfu, scaled_chlor_rfu,
         Comp.1, Comp.2, Comp.3, Comp.4) %>% 
  drop_na() %>% 
  cor(method = "spearman")

corrplot(corr_psm2, type = "upper")

## matrix of all variables ---------------------

#  ggplot2::aes(colour=season)

# DESIS
hs_pca_b$season<-as.factor(hs_pca_b$season)

short <-c(PC1 = "Comp.1", PC2 = "Comp.2", PC3="Comp.3",
          PC4 = "Comp.4", NPP = "EpiNPP_mgC_L", Resp = "EpiR_mgC_L",
          secchi = "Secchi_m", phyco = "scaled_phyco_rfu",
          chla = "scaled_chlor_rfu", fdom = "avg_fdom",
          turb = "avg_turbidity")

hs_pca_b %>% 
  filter(sensorid == "DESIS") %>% 
  select(Comp.1, Comp.2, Comp.3,
         Comp.4, EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m,
         scaled_phyco_rfu, scaled_chlor_rfu, avg_fdom,
         avg_turbidity, season) %>% 
  rename(any_of(short)) %>% 
  drop_na() %>% 
  ggpairs(columns = 1:11,
          lower = list(continuous = "smooth"),
          upper = list(continuous = wrap("cor", method = "spearman"))) +
  theme_bw()

# PRISMA

hs_pca_b %>% 
  filter(sensorid == "PRISMA") %>% 
  select(Comp.1, Comp.2, Comp.3,
         Comp.4, EpiNPP_mgC_L, EpiR_mgC_L, Secchi_m,
         scaled_phyco_rfu, scaled_chlor_rfu, avg_fdom,
         avg_turbidity, season) %>% 
  rename(any_of(short)) %>% 
  drop_na() %>% 
  ggpairs(columns = 1:11,
          lower = list(continuous = "smooth"),
          upper = list(continuous = wrap("cor", method = "spearman")))+
  theme_bw()

  