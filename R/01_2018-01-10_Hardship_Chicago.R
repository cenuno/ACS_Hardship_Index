########################
# Title : Hardship_Chicago
# Purpose : Re-calculate hardship index for 2008-2012 data
# Author: Patricia van Hissenhoven 
# Date : Jan. 10th, 2018 
########################

########################
# Import packages 
########################

library(ggplot2)
library(magrittr)
library(MASS)
library(car)
library(mosaic)
library(knitr)
library(tidyverse)
library(ggformula)
library(gridExtra)
library(broom)
library(GGally)
library(gdata)

########################
# Import and explore data 
########################
indicators.chicago <- read.csv("https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD")
dim(indicators.chicago) # 78 rows, 9 columns
colnames(indicators.chicago) # Community area number, Community area name, Percent of housing crowded, 
                             # Percent households below poverty, Percent aged 16 unemployed, Percent aged 25 without High School Diploma, 
                             # Percent aged under 18 or over 64, Per capita income, Hardship Index

#################################
# TRIAL 1 
#################################

# In this trial I tried to recreate the formula above and weigh each indicator equally
# For each indicator I found the max and min values and standardized each observation with the following formula
# X = ((Observation-min.indicator)/(max.indicator-min.indicator)). Then I averaged the values for the six indicators.

# The first step seems to be to identify the min and max for each indicator (the means were used in the second trial)

  # Crowded housing
  cro.hou.max <- max(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                     , na.rm = TRUE)
  cro.hou.min <- min(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                     , na.rm = TRUE)
  # Poverty 
  bel.pov.max <- max(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                      , na.rm = TRUE)
  bel.pov.min <- min(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                     , na.rm = TRUE)
  # Unemployment
  une.max <- max(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                     , na.rm = TRUE)
  une.min <- min(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                     , na.rm = TRUE)
  # Education Attainment
  no.hsd.max <- max(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                 , na.rm = TRUE)
  no.hsd.min <- min(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                 , na.rm = TRUE)
  # Dependency 
  dep.max <- max(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                 , na.rm = TRUE)
  dep.min <- min(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                 , na.rm = TRUE)
  # Per capita income
  inc.max <- max(indicators.chicago$PER.CAPITA.INCOME
                 , na.rm = TRUE)
  inc.min <- min(indicators.chicago$PER.CAPITA.INCOME
                 , na.rm = TRUE)

# The next step is to standardize the values according to the formula
  # Crowded housing 
   indicators.chicago$std.cro.hou <- ((indicators.chicago$PERCENT.OF.HOUSING.CROWDED - cro.hou.min) / (cro.hou.max-cro.hou.min))*100
  # Poverty 
   indicators.chicago$std.pov <- ((indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.min) / (bel.pov.max-bel.pov.min))*100
  # Unemployment
   indicators.chicago$std.une <- ((indicators.chicago$PERCENT.AGED.16..UNEMPLOYED - une.min) / (une.max-une.min))*100
  # Education Attainment
   indicators.chicago$std.no.hsd <- ((indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.min) / (no.hsd.max-no.hsd.min))*100
  # Dependency
   indicators.chicago$std.dep <- ((indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.min) / (dep.max-dep.min))*100
 
  # Per capita income, inverting the denominator, as suggested in the cited part
   indicators.chicago$std.inc <- ((indicators.chicago$PER.CAPITA.INCOME - inc.min) / (inc.min-inc.max))*100
   
# Re-calculate the hardship index
  indicators.chicago$recalc.trial1 <- (1/6)*(indicators.chicago$std.cro.hou + indicators.chicago$std.une + indicators.chicago$std.dep + indicators.chicago$std.inc + indicators.chicago$std.no.hsd + indicators.chicago$std.pov)
  
# Compare the values of the recalculation to the actual index. The difference should be zero.  
  indicators.chicago$difference <- indicators.chicago$HARDSHIP.INDEX - indicators.chicago$recalc.trial1
  
# Since the ranking of the values was different, I tried again. 
  
#############################
# Trial 2 
#############################
  
# Standardize the values (second trial with Z-scores)
# I first found the mean and standard deviation for all of the indicators 
  # Crowded housing 
  cro.hou.mean <- mean(indicators.chicago$PERCENT.OF.HOUSING.CROWDED 
                       , na.rm = TRUE)
  cro.hou.s <- sd(indicators.chicago$PERCENT.OF.HOUSING.CROWDED
                  , na.rm = TRUE)
  # Poverty 
  bel.pov.mean <- mean(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                       , na.rm = TRUE)
  bel.pov.s <- sd(indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY
                  , na.rm = TRUE)
  # Unemployment
  une.mean <- mean(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
                   , na.rm = TRUE)
  une.s <- sd(indicators.chicago$PERCENT.AGED.16..UNEMPLOYED
              , na.rm = TRUE)
  # Education Attainment
  no.hsd.mean <- mean(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                      , na.rm = TRUE)
  no.hsd.s <- sd(indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
                 , na.rm = TRUE)
  # Dependency 
  dep.mean <- mean(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
                   , na.rm = TRUE)
  dep.s <- sd(indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64
              , na.rm = TRUE)
  # Income
  inc.mean <- mean(indicators.chicago$PER.CAPITA.INCOME
                   , na.rm = TRUE)
  inc.s <- sd(indicators.chicago$PER.CAPITA.INCOME
              , na.rm = TRUE)
  
# I then standardized the indicators using z-score
  # Crowded housing 
  std2.cro.hou <- ((indicators.chicago$PERCENT.OF.HOUSING.CROWDED - cro.hou.mean) / (cro.hou.s))
  
  # Poverty 
  std2.pov <- ((indicators.chicago$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.mean) / (bel.pov.s))
 
  # Unemployment
  std2.une <- ((indicators.chicago$PERCENT.AGED.16..UNEMPLOYED - une.mean) / (une.s))

  # Education Attainment
  std2.no.hsd <- ((indicators.chicago$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.mean) / (no.hsd.s))
   
  # Dependency
  std2.dep <- ((indicators.chicago$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.mean) / (dep.s))

  # Per capita income, inverting the denominator
  std2.inc <- ((indicators.chicago$PER.CAPITA.INCOME - inc.mean) / (inc.s))


# Re-calculate the hardship index
  indicators.chicago$recalc.trial2 <- (1/6)*(std2.cro.hou + std2.une + std2.dep + std2.inc + std2.no.hsd + std2.pov)


# The ranking is very different. So I tried again. 
 
  
#################################
# Trial 3
#################################
# Import data again (Start off with clean data)  
  trial3 <- read.csv("https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD")

# Exclude Chicago observation  
  trial3  <- trial3[1:77,]

# I am going to do each step of the formula independently  
  
  # I add values to the column for each indicator that shows the value (Observation-indicator.min) 
  trial3$crowded.housing.diff <- trial3$PERCENT.OF.HOUSING.CROWDED-cro.hou.min
  trial3$education.attainment.diff <- trial3$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA - no.hsd.min
  trial3$poverty.diff <- trial3$PERCENT.HOUSEHOLDS.BELOW.POVERTY - bel.pov.min
  trial3$dependency.diff <- trial3$PERCENT.AGED.UNDER.18.OR.OVER.64 - dep.min
  trial3$unemployment.diff <- trial3$PERCENT.AGED.16..UNEMPLOYED - une.min
  trial3$income.diff <-   trial3$PER.CAPITA.INCOME - inc.min
  
  # I add values to the column for each indicator that shows the difference divided by the range
  trial3$crowded.housing.rat <- (trial3$crowded.housing.diff/(cro.hou.max-cro.hou.min))*100
  trial3$education.attainment.rat <- (trial3$education.attainment.diff/(no.hsd.max-no.hsd.min))*100
  trial3$poverty.rat <- (trial3$poverty.diff/ (bel.pov.max - bel.pov.min))*100
  trial3$dependency.rat <- (trial3$dependency.diff / (dep.max-dep.min))*100
  trial3$unemployment.rat <- (trial3$unemployment.diff/ (une.max- une.min))*100
  trial3$income.rat <- (trial3$income.diff/(inc.min-inc.max))*100
  
  
  # Hardship index
  trial3$recalc.trial3 <- (1/6)*(trial3$crowded.housing.rat + trial3$education.attainment.rat + trial3$poverty.rat + trial3$dependency.rat + trial3$unemployment.rat+ trial3$income.rat)
 
  # Since the literature says that they are equally weighed, I tried to divide the hardship index by 
  # The documentation says that each of the values are equally weighed,
  # so I am going to see if there is a coefficient that is applied to the sum 
  # of the values. 
  
  trial3$coefficients <-  trial3$HARDSHIP.INDEX/(trial3$crowded.housing.rat + trial3$education.attainment.rat + trial3$poverty.rat + trial3$dependency.rat + trial3$unemployment.rat+ trial3$income.rat)
  
  # I am doubting whether the hardship index is correct, considering that the lowest is 1 and the highest is 98. 
  # The hardship index seems to be a ranking of sorts over 100. I will take them to be the percentile, creating a 
  # predicted percentile of each index. Since the higher the score, the higher the hardship; we take it that a higher
  # percentile implies a higher index. 
  
 predicted.percentile <- quantile( x = sort(trial3$HARDSHIP.INDEX) , prob = seq(0,1, length = 77)) 
  
  # We can see that the predicted percentiles are the same values as the hardship index. This means that the "HARDSHIP.INDEX" is actually the percentile of the hardship index.
  # This means that we know the order of the scores, so our model should produce a list of the community areas' scores in that same order. 
  
 

###########################
# TRIAL 4
###########################
  
  # Import data again (Start off with clean data)  
  trial5 <- read.csv("https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD")
  
  # Exclude Chicago observation  
   trial6  <- trial6[1:77,]
  
  
  # Create coefficients
  trial5$PERCENT.OF.HOUSING.CROWDED.7 <- trial5$PERCENT.OF.HOUSING.CROWDED/trial5$PERCENT.OF.HOUSING.CROWDED[78]
  trial5$PERCENT.AGED.16..UNEMPLOYED.7 <-  trial5$PERCENT.AGED.16..UNEMPLOYED/ trial5$PERCENT.AGED.16..UNEMPLOYED[78]
  trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY.7 <- trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY / trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY[78]
  trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA.7 <- trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA / trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA[78]
  trial5$PERCENT.AGED.UNDER.18.OR.OVER.64.7 <- trial5$PERCENT.AGED.UNDER.18.OR.OVER.64/ trial5$PERCENT.AGED.UNDER.18.OR.OVER.64[78]

  trial5$PER.CAPITA.INCOME.7 <-   trial5$PER.CAPITA.INCOME /trial5$PER.CAPITA.INCOME[78]

  # Calculate the hardship index

  min.housing <- min(trial5$PERCENT.OF.HOUSING.CROWDED , na.rm = TRUE) 
  max.housing <- max(trial5$PERCENT.OF.HOUSING.CROWDED , na.rm = TRUE) 
  min.unemployment <- min(trial5$PERCENT.AGED.16..UNEMPLOYED , na.rm = TRUE)
  max.unemployment <- max(trial5$PERCENT.AGED.16..UNEMPLOYED , na.rm = TRUE) 
  min.poverty <- min(trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY , na.rm = TRUE)
  max.poverty <- max(trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY, na.rm = TRUE)
  min.education <- min(trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA , na.rm = TRUE)
  max.education <- max(trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA , na.rm = TRUE)
  min.dependency <- min(trial5$PERCENT.AGED.UNDER.18.OR.OVER.64,na.rm = TRUE)
  max.dependency <- max(trial5$PERCENT.AGED.UNDER.18.OR.OVER.64, na.rm = TRUE)
  min.income <- min(trial5$PER.CAPITA.INCOME, na.rm = TRUE)
  max.income <- max(trial5$PER.CAPITA.INCOME, na.rm = TRUE)
  
  
  
  trial5$housing <- ((trial5$PERCENT.OF.HOUSING.CROWDED - min.housing)/(max.housing-min.housing))*100
  trial5$unemployment <- ((trial5$PERCENT.AGED.16..UNEMPLOYED - min.unemployment)/(max.unemployment-min.unemployment))*100
  trial5$poverty <- ((trial5$PERCENT.HOUSEHOLDS.BELOW.POVERTY - min.poverty)/(max.poverty-min.poverty))*100
  trial5$education <- ((trial5$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA -min.education)/(max.education-min.education))*100
  trial5$dependency <- ((trial5$PERCENT.AGED.UNDER.18.OR.OVER.64- min.dependency)/( max.dependency-min.dependency))*100
  
  trial5$income <- ((max.income- trial5$PER.CAPITA.INCOME )/(min.income-max.income))*100
  
  trial5$trial5 <- (trial5$housing + trial5$unemployment + trial5$education + trial5$dependency + trial5$income + trial5$poverty)/6

  ########################
  # TRIAL 5
  ########################
  # Tired of my unsuccessful attempts, I looked for data that actually contained the hardship index scores. I came 
  # upon it for 2012-2014 data in https://greatcities.uic.edu/wp-content/uploads/2016/07/GCI-Hardship-Index-Fact-SheetV2.pdf
  
  # Import dataset
  great.cities <- read.csv( file = "/Users/patricia/Desktop/hardship.greatcities.csv")
  
  # Find mins and max
  min.housing.7 <- min( great.cities$PERCENT.OF.CROWDED.HOUSING , na.rm = TRUE) 
  max.housing.7 <- max( great.cities$PERCENT.OF.CROWDED.HOUSING, na.rm = TRUE) 
  min.unemployment.7 <- min( great.cities$UNEMPLOYMENT.RATE.FOR.POPULATION.AGE.16.and.OVER , na.rm = TRUE)
  max.unemployment.7 <- max( great.cities$UNEMPLOYMENT.RATE.FOR.POPULATION.AGE.16.and.OVER , na.rm = TRUE) 
  min.poverty.7 <- min( great.cities$PERCENT.OF.HOUSEHOLDS.WITH.INCOME.BELOW.POVERTY.LEVEL , na.rm = TRUE)
  max.poverty.7 <- max( great.cities$PERCENT.OF.HOUSEHOLDS.WITH.INCOME.BELOW.POVERTY.LEVEL, na.rm = TRUE)
  min.education.7 <- min( great.cities$PERCENT.AGED.25..AND.OVER.WITH.NO.HIGH.SCHOOL.DIPLOMA , na.rm = TRUE)
  max.education.7 <- max( great.cities$PERCENT.AGED.25..AND.OVER.WITH.NO.HIGH.SCHOOL.DIPLOMA , na.rm = TRUE)
  min.dependency.7 <- min( great.cities$PERCENT.POPULATION.UNDER.AGE.18.AND.OVER.AGE.64 ,na.rm = TRUE)
  max.dependency.7 <- max( great.cities$PERCENT.POPULATION.UNDER.AGE.18.AND.OVER.AGE.64 , na.rm = TRUE)
  min.income.7 <- min( great.cities$PER.CAPITA.INCOME , na.rm = TRUE)
  max.income.7 <- max( great.cities$PER.CAPITA.INCOME , na.rm = TRUE)
  
  # Compute the standardized value for each observation  
  great.cities$housing <- ((great.cities$PERCENT.OF.CROWDED.HOUSING - min.housing.7)/(max.housing.7-min.housing.7))*100
  great.cities$unemployment <- ((great.cities$UNEMPLOYMENT.RATE.FOR.POPULATION.AGE.16.and.OVER - min.unemployment.7)/(max.unemployment.7-min.unemployment.7))*100
  great.cities$poverty <- ((great.cities$PERCENT.OF.HOUSEHOLDS.WITH.INCOME.BELOW.POVERTY.LEVEL - min.poverty.7)/(max.poverty.7-min.poverty.7))*100
  great.cities$education <- ((great.cities$PERCENT.AGED.25..AND.OVER.WITH.NO.HIGH.SCHOOL.DIPLOMA -min.education.7)/(max.education.7-min.education.7))*100
  great.cities$dependency <- ((great.cities$PERCENT.POPULATION.UNDER.AGE.18.AND.OVER.AGE.64- min.dependency.7)/( max.dependency.7-min.dependency.7))*100

  # I need to flip the values in such a way that the numerator and the denominator are both negative (to get a positive
  # value for greater hardship). So I subtract the max income from the income to make sure the community area with the 
  # highest income has a standardized value of 0 and the one with the smallest income has a standardized value of 100. 
 
  great.cities$income <- (( great.cities$PER.CAPITA.INCOME - max.income.7 )/(min.income.7-max.income.7))*100
  great.cities$trial7 <- (great.cities$housing + great.cities$unemployment + great.cities$education + great.cities$dependency + great.cities$income + great.cities$poverty)/6
  
  # Upon comparison, the values seem to be the same. This means that trial 5 is the preferred equation. 