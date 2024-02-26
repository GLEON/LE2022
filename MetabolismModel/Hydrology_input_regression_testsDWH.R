#### Updating Hydrology input data for metabolism model

library(tidyverse)
library(dataRetrieval)
library(ggpmisc)

dates <- data.frame( "Date" = seq(ymd("1979-01-01"), ymd("2022-12-31"), by = "day"))

#### Modeled hydrology from Paul
hydro_modeled <- read_csv("./MetabolismModel/MetabolismMendotaLE2022/DriverData/hydro_inputs_1979_2015.csv")
hydro_modeled <- hydro_modeled %>% 
  rename("Date" = "time")

#### Get daily Yahara River and Pheasant Branch USGS data 
#set site numbers 
sitenos <- c("05427850", "05427948") # PB: "05427948"

#get site info
siteinfo <- readNWISsite(sitenos)
data_available <- whatNWISdata(siteNumber = sitenos, service = "dv")

#get daily Q
startDate <- "1979-01-01"
endDate <- "2023-05-30"
parameter <- "00060"

Qdat <- readNWISdv(sitenos, parameter, startDate, endDate) %>% 
  renameNWISColumns()

Qdata <- left_join(Qdat, siteinfo, by = c("site_no"))

Qdat_wide <- Qdat %>% 
  select(Date, site_no, Flow) %>% 
  pivot_wider(names_from = site_no, values_from = Flow) %>% 
  rename(Yah = "05427850",
         Pb = "05427948")


#### Compare USGS to modeled hydro 
hydro_comp <- full_join(hydro_modeled, Qdat_wide, by = c("Date"))

hydro_comp %>% 
  ggplot(aes(x = Yah, y = yah_flow_vol))+
  geom_point()+
  stat_poly_line() +
  stat_poly_eq() 


hydro_comp %>% 
  ggplot(aes(x = Pb, y = pb_flow_vol))+
  geom_point()+
  stat_poly_line() +
  stat_poly_eq() 

hydro_comp %>% 
  ggplot(aes(x = log10(Yah), y = log10(total_inflow_volume)))+
  geom_point()+
  stat_poly_line() +
  stat_poly_eq() 

hydro_comp %>% 
  ggplot(aes(x = Pb, y = total_inflow_volume))+
  geom_point()+
  stat_poly_line() +
  stat_poly_eq() 

summary(lm(data = hydro_comp, total_inflow_volume ~ Yah))

yahLM <- lm(data = hydro_comp, total_inflow_volume ~ Yah)
hist(yahLM$residuals)
# summary(lm(data = hydro_comp, total_inflow_volume ~ Yah + Pb))
summary(yahLM)

yahLMlog <- lm(data = hydro_comp, log10(total_inflow_volume) ~ log10(Yah))
hist(yahLMlog$residuals)
# summary(lm(data = hydro_comp, total_inflow_volume ~ Yah + Pb))
summary(yahLMlog)


hydro_comp$total_modeled_regress <- ((hydro_comp$Yah * 5202.92) + 174202.93)

hydro_comp %>% 
  ggplot()+
  geom_line(aes(x = Date, y = total_inflow_volume, col = "PennSt Model"))+
  geom_line(aes(x = Date, y = total_modeled_regress, col = "USGS"))+
  ylim(0, 2e+06)









