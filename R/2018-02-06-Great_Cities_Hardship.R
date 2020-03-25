#################
# Title: UIC Great Cities Institute Calculated Hardship Index
# Purpose: Discover the formula used to calculate the Hardship Index based on the data provided
# Author: Patricia van Hissenhoven 
# Date: February 6th, 2018
############
# This is the same procedure cited as Trial 5 in the Hardship_Chicago trials. This produces an RDS 
# file with the dataset and shows the calculations used to infer the equation

# Import dataset
great.cities <- read.csv( file = "/Users/patricia/Desktop/hardship.greatcities.csv")


# Find minimum and maximum
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

# I need to flip the values in such a way that the numerator and the denominator are both negative (to get a positive value
# that varies in proportion to Hardship) 


great.cities$income <- (( great.cities$PER.CAPITA.INCOME - max.income.7 )/(min.income.7-max.income.7))*100
great.cities$trial7 <- (great.cities$housing + great.cities$unemployment + great.cities$education + great.cities$dependency + great.cities$income + great.cities$poverty)/6

# Write CSV

write.csv( x = great.cities
           , row.names = FALSE
           , file = paste0( getwd(), "/2018-02-06-ACS_2010to2014_CommunityArea_SE_Indicator_UIC_Great_Cities.csv" )
)
# Save RDS file 
saveRDS( object = great.cities
         , file = paste0( getwd(), "/2018-02-02-ACS_2010to2014_CommunityArea_SE_Indicator_UIC_Great_Cities.rds" )
)
