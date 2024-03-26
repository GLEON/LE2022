#### Clean up met input data 
#### First add 2017 - 2020 from Robert, then add 2021-2022 from Adrianna

#### 2017-2020 ####
#Need to line up 2017-2020 NDLAS that Robert sent to the Kludg file that has copied met 
#then get formated to read into CleanMetFile.R 

library(tidyverse)
library(lubridate)

#Look at Kludg thats curretnly being used
kludg <- read_csv("./MetabolismModel/MetabolismMendotaLE2022/bc/NLDAS2_Mendota_1979_2016_forKludg_CLEANED.csv")
view(kludg)
#this has 1995 through 2020 (but post 2016 is copied)

#Look at Roberts NDLAS he shared 
nldas_rl <- read_csv("./MetabolismModel/Paul_NLDAS2/Mendota_Final/Mendota_2017_2020_box_5_CT.csv")
view(nldas_rl)

nldas_for_join <- nldas_rl %>% 
  mutate(Precip_mm_perday = Rain.m_day * 1000) %>% 
  rename(datetime = dateTime,
         Shortwave_Radiation_Downwelling_wattPerMeterSquared = ShortWave.W_m2,
         Longwave_Radiation_Downwelling_wattPerMeterSquared = LongWave.W_m2,
         Air_Temperature_celsius = AirTemp.C,
         Relative_Humidity_percent = RelHum,
         Ten_Meter_Elevation_Wind_Speed_meterPerSecond = WindSpeed.m_s,
         Precipitation_millimeterPerDay = Precip_mm_perday,
         Surface_Level_Barometric_Pressure_pascal = SurfPressure.Pa
          ) %>% 
  mutate(Snowfall_millimeterPerDay = NA) %>% 
  select(datetime, Shortwave_Radiation_Downwelling_wattPerMeterSquared, Longwave_Radiation_Downwelling_wattPerMeterSquared, Air_Temperature_celsius,
         Relative_Humidity_percent, Ten_Meter_Elevation_Wind_Speed_meterPerSecond, Precipitation_millimeterPerDay, Snowfall_millimeterPerDay,
         Surface_Level_Barometric_Pressure_pascal) %>% 
  filter(datetime > ymd_hms("2017-01-01 00:00:00"))



#the initial kludg had through 2016, so just need to update with 2017 onwards 
kludg_for_join <- kludg %>% 
  filter(datetime <= ymd_hms("2017-01-01 00:00:00"))

#Join met data 
tail(kludg_for_join)
head(nldas_for_join)
joined_mets <- rbind(kludg_for_join, nldas_for_join)

# write.csv(joined_mets, "./bc/NLDAS2_Mendota_1995_2020_forKludg_DWH_12jan22.csv", row.names = F)
# test_newkludg <- read_csv("./bc/NLDAS2_Mendota_1995_2020_forKludg_DWH_12jan22.csv")

#### 2021-2022 ####
nldas_21_22 <- read_csv("./MetabolismModel/Mendota_2021_2022_box_5_CT.csv") %>% 
  mutate(datetime = mdy_hm(datetime)) %>% 
  filter(!is.na(datetime))

head(nldas_21_22)

head(joined_mets)
tail(joined_mets)


joined_mets_FIN <- rbind(joined_mets, nldas_21_22)
head(joined_mets_FIN)
tail(joined_mets_FIN)

write.csv(joined_mets_FIN, "./MetabolismModel/MetabolismMendotaLE2022/bc/NLDAS2_Mendota_1995_2022_forKludg_DWH_14jul23.csv", row.names = F)
test_newkludg <- read_csv("./MetabolismModel/MetabolismMendotaLE2022/bc/NLDAS2_Mendota_1995_2022_forKludg_DWH_14jul23.csv")
head(test_newkludg)
tail(test_newkludg)
