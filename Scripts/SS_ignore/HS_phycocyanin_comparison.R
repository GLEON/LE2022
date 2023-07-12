## Review Phycocyanin data

# Load Libraries
library(tidyverse)
library(ggplot2)

# Load data
setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/R/LE2022/Data/Buoy')
buoy <- read.csv('daily_buoy.csv')

setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/R/LE2022/Data/AVIRIS')
AVIRIS_20220822 <- read.csv('AVIRIS-NG_20160822_BuoyClipped_30m.csv')
AVIRIS_20220831 <- read.csv('AVIRIS-NG_20160831_BuoyClipped_30m.csv')
setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/Data/AVIRIS')
AVIRIS_bands <- read.csv('AVIRIS_Bands.csv')


setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/R/LE2022/Data/DESIS')
DESIS <- read.csv('DESIS_stats_use.csv')

setwd('C:/Users/ssharp/Box/UCD EDL/GLEON LE2022/R/LE2022/Data/PRISMA')




# Define hyperspectral scene dates
AVIRIS_dates <- c('2016-08-22',
                  '2016-08-31')

DESIS_dates <- DESIS$date

dates <- c(AVIRIS_dates,DESIS_dates)

Buoy_subset <- buoy %>%
  filter(sampledate == dates[1] |
           sampledate == dates[2] |
           sampledate == dates[3] |
           sampledate == dates[4] |
           sampledate == dates[5] |
           sampledate == dates[6] |
           sampledate == dates[7] |
           sampledate == dates[8] |
           sampledate == dates[9] |
           sampledate == dates[10])

AVIRIS_20220822_mean <- data_frame(c(AVIRIS_bands,colMeans(AVIRIS_20220822)))

       
       