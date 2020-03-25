
########################
# Date:     February 2, 2018
# Purpose :  Using ACS 2012-2016 5-Year Estimate data
#            to update 'Census Data - Selected socioeconomic indicators in Chicago, 2008 - 2012'
#            a public data set made available on the Chicago Data Portal.
#            Available here: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data
#          
# Author: Patricia van Hissenhoven and Cristian E. Nuno
########################

# install necessary packages
install.packages( pkgs = c( "acs", "dplyr", "sp", "readxl", "rgdal" ) )

## load necessary packages
library( acs )
library( dplyr )
library( sp )
library( readxl )
library( rgdal )

## load necessary data

# assumption that a census api key is already installed on your system
# here, the key is stored as CENSUS_API_KEY=YOURKEY in the .Renviron file
  census_api_key( key = Sys.getenv( x = "CENSUS_API_KEY" ) )


# Table Shells for ACS 2012-2016 5-Year Estimate Data
# Note: used to ensure correct variables are being selected
download.file( url = "https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/user_tools/ACS2016_Table_Shells.xlsx" 
                , destfile = "ACS_2016_Table_Shells.xlsx" )
acs.2016.table.shells <- read_xlsx( path = paste0( getwd() , "/ACS_2016_Table_Shells.xlsx" )
                                    , na = c( "", "NA", "na", "N/A", "n/a", "missing" ) )


# Create chicago community area spatial polygon data frame
comarea606 <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
                       , layer = "OGRGeoJSON"
                       , stringsAsFactors = FALSE
                       , verbose = FALSE # to hide progress message after object is created
)



# Create chicago census tract spatial polygon data frame
chicago.census.tracts <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON"
                                  , layer = "OGRGeoJSON"
                                  , stringsAsFactors = FALSE
)

# NOTE: There a few judgements being made regarding which census tracts belong in which community areas
View( x = slot( object = chicago.census.tracts, name = "data")[ which(
  slot( object = chicago.census.tracts, name = "data")$note != ""
) , ])

# store transparent gray color
custom.color <- rgb( r = 204, g = 204, b = 204, alpha = 100, maxColorValue = 255 )

# overlay the two geographies 
# on top of each other to gain 
# a better sense of the judgements
# being made
par( mar = c(2, 0, 2, 0 ) )
plot( x = chicago.census.tracts
      , col = "#4d4d4d"
      , border = "#4d4d4d"
      , main = "Overlaying Geographies in Chicago"
)
plot( x = comarea606
      , col = custom.color
      , border = custom.color
      , add = TRUE
)
legend( x = "left"
        , legend = c( "Chicago Census Tracts", "Chicago Current Community Areas" )
        , pch = 15
        , pt.cex = 2
        , col = c( "#4d4d4d", "#CCCCCC" )
        , bty = "n"
        , title = "Legend"
)
mtext( side = 1
       , adj = 0.99
       , cex = 0.75
       , line = 1
       , text = "Source: Chicago Data Portal"
)

# Because the goal is to update this data set,
# it is an assumption we are making that the
# City used the crosswalk contain in the census tract spatial polygons data frame
# to determine which census tracts belong in which community areas

# create the census tract to community area number crosswalk
census.tract.community.area.crosswalk.df <-
  slot( object = chicago.census.tracts, name = "data" )[ c( "commarea_n", "tractce10" ) ]

# create the community area number to community area name crosswalk
community.area.number.name.crosswalk.df <- 
  slot( object = comarea606, name = "data" )[ c( "community", "area_numbe" ) ]

# create a master census tract to community area crosswalk
master.census.tract.community.area.crosswalk.df <-
  left_join( x = census.tract.community.area.crosswalk.df
             , y = community.area.number.name.crosswalk.df
             , by = c( "commarea_n" = "area_numbe" ) 
  )

# Identify census tracts within Cook County
cook.county.census.tracts <- geo.make(state = "IL"
                                      , county = "Cook"
                                      , tract = "*"
)

# Download ACS 2011-2015 5-Year Estimate Data
# https://census.gov/programs-surveys/acs/technical-documentation/table-shells.html
# Methodology
# 1. Avoiding the use of the `table.number` parameter
# 2. keep the 'geography' and 'estimate' data frames within each acs object
# 3. keep only the census tracts from the acs data frame
#    that appear in the master.census.tract.community.area.crosswalk.df
#   

# Stub:         SEX BY AGE
# Universe:     Total Population
# Table ID:     B01001
population.df <- acs.fetch( endyear = 2016
                            , span = 5
                            , geography = cook.county.census.tracts
                            , variable = c( paste( "B01001"
                                                   , 1:9
                                                   , sep = "_00"
                            )
                            , paste( "B01001"
                                     , 10:49
                                     , sep = "_0"
                            )
                            )
                            , key = my.key 
)

# filter the acs object
# to only include the two
# data frames of interest
population.df <- cbind.data.frame(
  slot( object = population.df, name = "geography" )
  , slot( object = population.df, name = "estimate" )
)

# Stub:         SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
# Universe:     Population 25 years and over
# Table ID:     B15002
education.df <- acs.fetch( endyear = 2016
                           , span = 5
                           , geography = cook.county.census.tracts
                           , variable = c( paste( "B15002"
                                                  , 1:9
                                                  , sep = "_00"
                           )
                           , paste( "B15002"
                                    , 10:35
                                    , sep = "_0"
                           )
                           )
                           , key = my.key 
)

# filter the acs object
# to only include the two
# data frames of interest
education.df <- cbind.data.frame(
  slot( object = education.df, name = "geography" )
  , slot( object = education.df, name = "estimate" )
)

# Stub:     POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER
# Universe: Households
# Table ID: B17017
poverty.df <- acs.fetch( endyear = 2016
                         , span = 5
                         , geography = cook.county.census.tracts
                         , variable = c(
                           paste( "B17017"
                                  , 1:9
                                  , sep = "_00"
                           )
                           , paste( "B17017"
                                    , 10:59
                                    , sep = "_0"
                           )
                         )
                         , key = my.key 
)

# filter the acs object
# to only include the two
# data frames of interest
poverty.df <- cbind.data.frame(
  slot( object = poverty.df, name = "geography" )
  , slot( object = poverty.df, name = "estimate" )
)

# Stub:     AGGREGATE INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)
# Universe: Population 15 years and over
# Table ID: B19313
agg.pci.df <- acs.fetch( endyear = 2016
                         , span = 5
                         , geography = cook.county.census.tracts
                         , variable = "B19313_001"
                         , key = my.key
                         )

# filter the acs object
# to only include the two
# data frames of interest
agg.pci.df <- cbind.data.frame(
  slot( object = agg.pci.df, name = "geography" )
  , slot( object = agg.pci.df, name = "estimate" )
)


# Stub:     SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER
# Universe: Population 16 years and over
# Table ID: B23001
employment.df <- acs.fetch( endyear = 2016
                            , span = 5
                            , geography = cook.county.census.tracts
                            , variable = c(
                              paste( "B23001"
                                     , 1:9
                                     , sep = "_00"
                              )
                              , paste( "B23001"
                                       , 10:99
                                       , sep = "_0"
                              )
                              , paste( "B23001"
                                       , 100:173
                                       , sep = "_"
                              )
                            )
                            , key = my.key
)

# filter the acs object
# to only include the two
# data frames of interest
employment.df <- cbind.data.frame(
  slot( object = employment.df, name = "geography" )
  , slot( object = employment.df, name = "estimate" )
)

# Stub:     TENURE BY OCCUPANTS PER ROOM
# Universe: Total
# Table ID: B25014
occupants.per.room.df <- acs.fetch( endyear = 2016
                                    , span = 5
                                    , geography = cook.county.census.tracts
                                    , variable = c(
                                      paste( "B25014"
                                             , 1:9
                                             , sep = "_00"
                                      )
                                      , paste( "B25014"
                                               , 10:13
                                               , sep = "_0"
                                      )
                                    )
                                    , key = my.key
)

# filter the acs object
# to only include the two
# data frames of interest
occupants.df <- cbind.data.frame(
  slot( object = occupants.per.room.df , name = "geography" )
  , slot( object = occupants.per.room.df , name = "estimate" )
)


## filter data to match the socioeconomic descriptors

# Name:           Percent Aged Under 18 or Over 64	
# Description:    Percent of the population under 18 or over 64 years of age 
#                 (i.e., dependency)

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
population.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                         , y = population.df
                                         , by = c("tractce10" = "tract")
)

# aggregate census tract data
# to the community area level
dependency.df <- 
  population.chicago.only.df %>%
  group_by( community )%>%
  summarize( total.population                      = sum( B01001_001 )
            , population.aged.under18.over64       = sum( B01001_003 , B01001_004 , B01001_005 , B01001_006
                                                          , B01001_027 , B01001_028 , B01001_029 , B01001_030
                                                          , B01001_020 , B01001_021 , B01001_022 , B01001_023
                                                          , B01001_024 , B01001_025 , B01001_044 , B01001_046
                                                          , B01001_047 , B01001_048 , B01001_049 ) 
            , population.aged.under18.over64.ratio = population.aged.under18.over64 / total.population * 100 ) 

# aggregate community area data
# to find statistic for the entire city
dependency.df <-
  rbind.data.frame( 
    dependency.df
    , data.frame( community = "CHICAGO"
                  , total.population                     = sum( dependency.df$total.population )
                  , population.aged.under18.over64       = sum( dependency.df$population.aged.under18.over64 )
                  , population.aged.under18.over64.ratio = sum( dependency.df$population.aged.under18.over64 ) / sum( dependency.df$total.population ) * 100 
    )
    , stringsAsFactors = FALSE
  )


# for the Per Capita Income calculation,
# store the total population for each cca
cca.total.population <- dependency.df[ c("community", "total.population" ) ]

# Name:           Percent Aged 25+ Without High School Diploma
# Description:    Percent of persons aged 25 years or older without a high school diploma

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
education.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                        , y = education.df
                                        , by = c("tractce10" = "tract")
)

# aggregate census tract data
# to the community area level
persons.aged25plus.without.hs.diploma <-
  education.chicago.only.df %>%
  group_by( community ) %>%
  summarise( total.population                                 = sum( B15002_001 )
             , population.aged25plus.without.hs.diploma       = sum( B15002_003 , B15002_004 , B15002_005 , B15002_006
                                                                   , B15002_007 , B15002_008 , B15002_009 , B15002_010
                                                                   , B15002_020 , B15002_021 , B15002_022 , B15002_023
                                                                   , B15002_024 , B15002_025 , B15002_026 , B15002_027 ) 
             , population.aged25plus.without.hs.diploma.ratio = population.aged25plus.without.hs.diploma / total.population * 100 )

# aggregate community area data
# to find statistic for the entire city
persons.aged25plus.without.hs.diploma <-
  rbind.data.frame( 
    persons.aged25plus.without.hs.diploma
    , data.frame( community = "CHICAGO"
                  , total.population                               = sum( persons.aged25plus.without.hs.diploma$total.population )
                  , population.aged25plus.without.hs.diploma       = sum( persons.aged25plus.without.hs.diploma$population.aged25plus.without.hs.diploma )
                  , population.aged25plus.without.hs.diploma.ratio = sum( persons.aged25plus.without.hs.diploma$population.aged25plus.without.hs.diploma ) / sum( persons.aged25plus.without.hs.diploma$total.population ) * 100 
    )
    , stringsAsFactors = FALSE
  )


# Name:           Percent Households Below Poverty
# Description:    Percent of households living below the federal poverty level

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
poverty.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                      , y = poverty.df
                                      , by = c("tractce10" = "tract")
)

# aggregate census tract data
# to the community area level
households.living.below.fpl <-
  poverty.chicago.only.df %>%
  group_by( community ) %>%
  summarise( total.population                    = sum( B17017_001 )
             , population.living.below.fpl       = sum( B17017_002 )
             , population.living.below.fpl.ratio = population.living.below.fpl / total.population * 100 )

# aggregate community area data
# to find statistic for the entire city
households.living.below.fpl <-
  rbind.data.frame( 
    households.living.below.fpl
    , data.frame( community = "CHICAGO"
                  , total.population                  = sum( households.living.below.fpl$total.population )
                  , population.living.below.fpl       = sum( households.living.below.fpl$population.living.below.fpl )
                  , population.living.below.fpl.ratio = sum( households.living.below.fpl$population.living.below.fpl ) / sum( households.living.below.fpl$total.population ) * 100 
    )
    , stringsAsFactors = FALSE
  )


# Name:           Per Capita Income
# Description:    A community area’s per capita income was estimated by dividing 
#                 the aggregate income of the census tracts within the community area 
#                 (from table B19313) by the number of residents (from table B01001).

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
agg.pci.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                      , y = agg.pci.df
                                      , by = c("tractce10" = "tract")
)
# NOTE THAT THERE ARE THREE CENSUS TRACTS 
# (980100, 980000, and 381700) 
# THAT REGISTER THE SAME
# NEGATIVE PER CAPITA INCOME VALUE, -666666666
# Remove them.
negative.pci.ct <- c( "980100", "980000", "381700" )

agg.pci.chicago.only.df <- 
  agg.pci.chicago.only.df[ which( !agg.pci.chicago.only.df$tractce10 %in% negative.pci.ct ) , ]

# aggregate census tract data
# to the community area level
estimated.pci <-
  agg.pci.chicago.only.df %>%
  group_by( community ) %>%
  summarise( agg.pci = sum( B19313_001 ) )

# add the total population to each cca
estimated.pci <- left_join( x = estimated.pci
                            , y = cca.total.population
                            , by = "community"
)

# calculate the estimated pci per cca
estimated.pci$estimated.pci <- 
  estimated.pci$agg.pci / estimated.pci$total.population

# aggregate community area data
# to find statistic for the entire city
estimated.pci <-
  rbind.data.frame( 
    estimated.pci
    , data.frame( community           = "CHICAGO"
                  , agg.pci           = sum( estimated.pci$agg.pci )
                  , total.population  = sum( estimated.pci$total.population )
                  , estimated.pci     = sum( estimated.pci$agg.pci ) / sum( estimated.pci$total.population ) 
    )
    , stringsAsFactors = FALSE
  )


# Name:           Percent Aged 16+ Unemployed
# Description:    Percent of persons aged16 years or older in the labor force that are unemployed

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
employment.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                         , y = employment.df
                                         , by = c("tractce10" = "tract")
)

# Aggregate by community area 
unemployed.over.16 <- 
  employment.chicago.only.df %>%
  group_by( community )%>%
  summarize( unemployed           = sum( B23001_008 ,  B23001_015 ,  B23001_022 ,  B23001_029 
                                        , B23001_036 ,  B23001_043 ,  B23001_050 ,  B23001_057   
                                        , B23001_064 ,  B23001_071 ,  B23001_076 ,  B23001_081  
                                        , B23001_086 ,  B23001_094 ,  B23001_101 ,  B23001_108  
                                        , B23001_115 ,  B23001_122 ,  B23001_129 ,  B23001_136  
                                        , B23001_143 ,  B23001_150 ,  B23001_157 ,  B23001_162  
                                        , B23001_167 ,  B23001_172 )
             , labor.force        = sum(  B23001_004  ,  B23001_011 ,  B23001_018  ,  B23001_025  
                                          , B23001_032  ,  B23001_039 ,  B23001_046  ,  B23001_053  
                                          , B23001_060  ,  B23001_067 ,  B23001_074  ,  B23001_079  
                                          , B23001_084  ,  B23001_090 ,  B23001_097  ,  B23001_104 
                                          , B23001_111  ,  B23001_118 ,  B23001_125  ,  B23001_132  
                                          , B23001_139  ,  B23001_146 ,  B23001_153  ,  B23001_160  
                                          , B23001_165 ,  B23001_170 )
             , unemployment.ratio = unemployed / labor.force * 100 )

# aggregate community area data
# to find statistic for the entire city
unemployed.over.16 <-
  rbind.data.frame( 
    unemployed.over.16
    , data.frame( community             = "CHICAGO"
                  , unemployed          = sum( unemployed.over.16$unemployed )
                  , labor.force         = sum( unemployed.over.16$labor.force )
                  , unemployment.ratio  = sum( unemployed.over.16$unemployed ) / sum( unemployed.over.16$labor.force ) * 100 
    )
    , stringsAsFactors = FALSE
  )

# Name:           Percent of Housing Crowded
# Description:    Percent of occupied housing units with more than one person per room 
#                 (i.e., crowded housing)

# Left join the Chicago Census Tract data
# onto only the Cook County census tracts
# that lay within the city of Chicago
crowded.housing.chicago.only.df <- left_join( x = master.census.tract.community.area.crosswalk.df
                                              , y = occupants.df
                                              , by = c("tractce10" = "tract")
)

# Aggregate by community area 
housing.crowded <- 
  crowded.housing.chicago.only.df %>%
  group_by( community ) %>%
  summarize( total.households     = sum( B25014_001 )
             , crowded.households = sum(  B25014_005   ,  B25014_006   ,  B25014_007  
                                          ,  B25014_011  ,  B25014_012   ,  B25014_013 )
             , crowded.ratio      = crowded.households / total.households * 100 ) 

# aggregate community area data
# to find statistic for the entire city
housing.crowded <-
  rbind.data.frame( 
    housing.crowded
    , data.frame( community             = "CHICAGO"
                  , total.households    = sum( housing.crowded$total.households )
                  , crowded.households  = sum( housing.crowded$crowded.households )
                  , crowded.ratio       = sum( housing.crowded$crowded.households ) / sum( housing.crowded$total.households ) * 100 
    )
    , stringsAsFactors = FALSE
  )

# Name:           Hardship Index
# Description:    The hardship index is a score that incorporates each of the six selected socioeconomic indicators according to the method described in An Update on Urban Hardship 
#                 (see footnote 2 on page 53 of http://www.rockinst.org/pdf/cities_and_neighborhoods/2004-08-an_update_on_urban_hardship.pdf). 
#                 Scores on the index can range from 1 to 100, with a higher index number representing a greater level of hardship. 
#                 The scores are standardized according to the data for the 77 community areas, 
#                 and therefore cannot be compared to scores generated for other jurisdictions.
# Recreation:     Please see the UIC Chicago Community Area report on the Hardship Index
#                 to learn more about how this function was recreated.
#                 https://greatcities.uic.edu/wp-content/uploads/2016/07/GCI-Hardship-Index-Fact-SheetV2.pdf

## Prior to calculating the Hardship Index
## I must create a data frame that contains 78 rows and 9 columns
# Row order: One for each community area (77) + one for the entire City
# Column order:
# 1. CCA number
# 2. CCA name
# 3. % housing crowded
# 4. % households living below the federal poverty level
# 5. % aged 16+ who are unemployed
# 6. % aged 25+ without a high school diploma
# 7. % aged under 18 or over 64
# 8. Per capita income
# 9. Hardship Index
## See here for an example of the schema: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data

# Join area number + area name onto 
# % housing crowded
acs.2016.cca.se.indicators <- 
  right_join( x = comarea606@data[ c( "area_numbe", "community" ) ]
             , y = housing.crowded[ c( "community", "crowded.ratio" ) ]
             , by = "community"
  )

# Join area name onto
# % households living below the federal poverty level
acs.2016.cca.se.indicators <-
  left_join( x = acs.2016.cca.se.indicators
             , y = households.living.below.fpl[ c( "community", "population.living.below.fpl.ratio" ) ] 
             , by = "community"
  )

# Join area name onto
# % aged 16+ who are unemployed
acs.2016.cca.se.indicators <-
  left_join( x = acs.2016.cca.se.indicators
             , y = unemployed.over.16[ c( "community", "unemployment.ratio" ) ] 
             , by = "community"
  )

# Join area name onto
# % aged 25+ without a high school diploma
acs.2016.cca.se.indicators <-
  left_join( x = acs.2016.cca.se.indicators
             , y = persons.aged25plus.without.hs.diploma[ c( "community", "population.aged25plus.without.hs.diploma.ratio" ) ] 
             , by = "community"
  )

# Join area name onto
# % aged under 18 or over 64
acs.2016.cca.se.indicators <-
  left_join( x = acs.2016.cca.se.indicators
             , y = dependency.df[ c( "community", "population.aged.under18.over64.ratio" ) ] 
             , by = "community"
  )

# Join area name onto
# Per capita income
acs.2016.cca.se.indicators <-
  left_join( x = acs.2016.cca.se.indicators
             , y = estimated.pci[ c( "community", "estimated.pci" ) ] 
             , by = "community"
  )

# check dim
dim( acs.2016.cca.se.indicators ) # [1] 78  8

# Rename the columns (make it comprehensible)  
colnames( acs.2016.cca.se.indicators ) <- 
  c( "COMMUNITY.AREA.NUMBER" , "COMMUNITY.AREA.NAME"
     , "PERCENT.OF.HOUSING.CROWDED" , "PERCENT.HOUSEHOLDS.BELOW.POVERTY" 
     , "PERCENT.AGED.16..UNEMPLOYED" , "PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA"
     , "PERCENT.AGED.UNDER.18.OR.OVER.64" ,  "PER.CAPITA.INCOME" 
  )

# Recast the cca number as a numeric vector
acs.2016.cca.se.indicators$COMMUNITY.AREA.NUMBER <- 
  as.numeric( x = acs.2016.cca.se.indicators$COMMUNITY.AREA.NUMBER )

# Create a condition that avoids
# the 78th row
# because the entire city does not go into the hardship index calculations
avoid.using.the.entire.city <- which( !is.na( acs.2016.cca.se.indicators$COMMUNITY.AREA.NUMBER ) )

# create a condition that avoids
# community area number and 
# community area name
se.indicator.columns <- 
  colnames( acs.2016.cca.se.indicators )[
    which( !colnames( acs.2016.cca.se.indicators ) %in%
             c( "COMMUNITY.AREA.NUMBER", "COMMUNITY.AREA.NAME" )
    )
    ]

# create a condition that avoids
# community area number,
# community area name, and
# per capita income
non.pci.se.indicator.columns <- 
  colnames( acs.2016.cca.se.indicators )[
    which( !colnames( acs.2016.cca.se.indicators ) %in%
             c( "COMMUNITY.AREA.NUMBER", "COMMUNITY.AREA.NAME", "PER.CAPITA.INCOME" )
    )
    ]

# create a condition that
# only retains per capita income
pci.se.indicator.column <- "PER.CAPITA.INCOME"

# Identify the min and max values 
# for each socioeconomic indicator
# being sure to use the conditions
# Note: this object is a list.
#       The first element in each vector is the minimum value
#       The second element in each vector is the maximum value
list.of.min.max.se.indicator.values <-
  lapply( X = acs.2016.cca.se.indicators[ se.indicator.columns ][ avoid.using.the.entire.city, ]
          , FUN = function( i ) append( x = min( i ), values = max( i ) )
  )

# Create a condition that avoids
# the Per Capita Income min and max values
avoid.pci.values <- which( names( list.of.min.max.se.indicator.values ) != "PER.CAPITA.INCOME" )

# Standardize each
# socioeconomic indicator value
# EXCEPT Per Capita Income
list.of.standardized.se.indicator.values <-
  mapply( FUN = function( value, min.value, max.value )
    ( value - min.value ) / ( max.value - min.value ) * 100
    , acs.2016.cca.se.indicators[ non.pci.se.indicator.columns ][ avoid.using.the.entire.city, ]
    , lapply( X = list.of.min.max.se.indicator.values[ avoid.pci.values ], FUN = "[[", FUN.VALUE = 1 )
    , lapply( X = list.of.min.max.se.indicator.values[ avoid.pci.values ], FUN = "[[", FUN.VALUE = 2 )
    , SIMPLIFY = FALSE
    , USE.NAMES = TRUE
  )

# To standardize Per Capita Income
# I need to flip the values in such a way that the numerator and the denominator 
# are both negative (to get a positive value for greater hardship). 
# So I subtract the max income from the income 
# to make sure the community area with the highest income has a standardized value of 0 
# and the one with the smallest income has a standardized value of 100. 
list.of.standardized.se.indicator.values[[ pci.se.indicator.column ]] <-
  mapply( FUN = function( value, min.value, max.value )
    ( value - max.value ) / ( min.value - max.value ) * 100
    , acs.2016.cca.se.indicators[ pci.se.indicator.column ][ avoid.using.the.entire.city, ]
    , lapply( X = list.of.min.max.se.indicator.values[ pci.se.indicator.column ], FUN = "[[", FUN.VALUE = 1 )
    , lapply( X = list.of.min.max.se.indicator.values[ pci.se.indicator.column ], FUN = "[[", FUN.VALUE = 2 )
    , SIMPLIFY = TRUE
    , USE.NAMES = TRUE
  )


# Calculate the hardship index
# for each community area
list.of.hardship.indices <-
  sapply( X = 1:nrow( acs.2016.cca.se.indicators[ avoid.using.the.entire.city, ] )
          , FUN = function( i ) 
            Reduce( f = sum
                    , lapply( X = list.of.standardized.se.indicator.values
                              , FUN = "[["
                              , FUN.VALUE = i
                    )
            ) /
            length( list.of.standardized.se.indicator.values )
  )

# add hardship index
# back to the output data frame
acs.2016.cca.se.indicators$HARDSHIP.INDEX <-
  append( x = list.of.hardship.indices
          , values = NA
  )

# Lastly, 
# sort the data frame by community area number
acs.2016.cca.se.indicators <-
  acs.2016.cca.se.indicators[ order( acs.2016.cca.se.indicators$COMMUNITY.AREA.NUMBER ) , ]


# Export data as both a CSV and an RDS file
write.csv( x = acs.2016.cca.se.indicators
           , row.names = FALSE
           , file = paste0( getwd(), "/2018-02-02-ACS_2012to2016_CommunityArea_SE_Indicator_Estimates.csv" )
)
saveRDS( object = acs.2016.cca.se.indicators
         , file = paste0( getwd(), "/2018-02-02-ACS_2012to2016_CommunityArea_SE_Indicator_Estimates.rds" )
)

# end of script #
