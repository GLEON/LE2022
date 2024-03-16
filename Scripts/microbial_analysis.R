# Analysis of 16S microbial data in limony dataset
# November 2022, cleaned up March 2023
# AGS

pacman::p_load(tidyverse, devtools, lubridate, limony)

# Load the data file generated in the microbial_cleaning.R script


#----------------------------------------------------------------------
# Removing rare phyla
#----------------------------------------------------------------------

# Subset to only keep phyla that have a % relative abundance >2 at some point
species_filter <- data_summed %>%
  group_by(phylum) %>%
  summarize(max = max(abund)) %>%
  filter(max < 0.2) # change back to > 

data_summed_summarized <- data_summed %>%
  filter(phylum %in% species_filter$phylum)

# Correlation matrix between these 25 phyla

wide <- pivot_wider(data_summed_summarized, names_from = phylum, values_from = abund)

wide <- wide[,-c(1:6)]

cormatrix <- cor(wide)



#----------------------------------------------------------------------
# For random forest analysis
#----------------------------------------------------------------------






library(randomForest)
library(pdp)

data_summed2 <- data_summed %>%
  select(-c(date, day, month, year, doy)) 

summarized <- data_summed2 %>%
  pivot_wider(names_from = phylum, values_from = abund)

metabolism <- read.csv("Data/Metabolism_Model_Output/SimResultsMatrix_MetabData_run12jan23.csv") # load metab data

# try cutting off > 0.5 for NPP

metabolism <- metabolism %>% rename("sampledate" = "SimDate")

metabolism$sampledate <- as.Date(metabolism$sampledate)

rf_input <- merge(x = summarized, y = metabolism, by.x = "sampledate")

rf_input <- rf_input %>%
  select(1:73, 78) %>%
  #select(1:27, 32) %>%
  select(-contains("-")) # columns with dashes were causing us trouble

# Create training and test sets
rf_training <- sample_frac(tbl=rf_input,size=0.7,replace=FALSE)
rf_test <- rf_input[!(rf_input$sampledate %in% rf_training$sampledate),]

# Remove the date from the training set
rf_training <- rf_training %>% select(-c(sampledate))

# Run the random forest
rf <- randomForest(data=rf_training, EpiNPP_mgC_L~., localImp=TRUE, err.rate=TRUE)

rf
#plot(rf) # what it's supposed to look like

# Plot predicted vs. observed NPP
rf_test$predict <- predict(rf, rf_test)
rf_test %>%
  ggplot(aes(x= EpiNPP_mgC_L, y = predict)) +
  geom_point() +
  geom_abline()

# Variable importance plot
varImpPlot(rf)
# node purity - not many people use
# %IncMSE - % increase in MSE if that variable is not included

# Partial dependence plot - how the prediction responds in relation to one of the variables
pd <- partial(object = rf,
              pred.var = 'Spirochaetota',
              plot = TRUE)
# the yhat (metabolism) goes up steeply with any presence of Spiro
pd <- partial(object = rf,
              pred.var = 'Cyanobacteria',
              plot = TRUE)
pd <- partial(object = rf,
              pred.var = 'Bacteroidota',
              plot = TRUE)
pd <- partial(object = rf,
              pred.var = 'Planctomycetota',
              plot = TRUE)

# Run actual regression and see what the r-squared is
model <- lm(data = rf_test, formula = predict ~ EpiNPP_mgC_L)

summary(model)
# pretty good!



#--------------------------------------------------------------
# Plot trends in four desired phyla over time
#-------------------------------------------------------------

# Subset
fourphyla <- subset(data_summed, phylum %in% c("Cyanobacteria", "Spirochaetota", "Planctomycetota", "Bacteroidota"))

fourphyla$year <- as.character(fourphyla$year)
fourphyla$phylum <- factor(fourphyla$phylum,
                           levels = c("Spirochaetota",
                                      "Cyanobacteria",
                                      "Planctomycetota",
                                      "Bacteroidota"))

# Plot with a different line for each year, faceted by phylum
fourphyla %>%
  ggplot(aes(x = doy, y = abund, col = year)) + 
  geom_line(stat = "identity") +
  facet_wrap(~phylum, ncol = 1, scales = "free_y") +
  theme_classic() +
  labs(x = "Day of year", y = "Phylum relative abundance (%)", title = "")
