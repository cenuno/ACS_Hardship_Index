# 
# Author: Grace Hwang
# Purpose: Draft map
#

#############################
## Load necessary packages ##
#############################
library( leaflet )
library( htmltools )
library( shiny )
library( sp )
library( rgdal )

###########################
## Load necessary data ##
###########################
census.data <- readRDS( gzcon( url( description = "https://github.com/pvanhissenhovenf/ACS_Hardship_Index/blob/master/Data/hardship.rds?raw=true" ) ) )

# Transform URL into spatial dataframe
comarea <- readRDS( gzcon( url( description = "https://github.com/pvanhissenhovenf/ACS_Hardship_Index/blob/master/Data/comarea.rds?raw=true" ) ) )

# Merge the non-spatial data frame onto spatial polygon data frame
comarea <- sp::merge( x = comarea
                      , y = census.data
                      , by.x = "area_numbe"
                      , by.y = "Community.Area.Number"
)

# Make colors
color.function <- colorRampPalette( c( "#CCCCCC", "#660066" ) )
color.ramp <- color.function( n = 5 )

# Assign colors to groups
# Crowded Housing
comarea$color_crowded <- as.character(
  cut(
    x = rank( comarea$PERCENT.OF.HOUSING.CROWDED )
    , breaks = 5
    , labels = color.ramp
  )
)

# Poverty
comarea$color_poverty <- as.character(
  cut(
    x = rank( comarea$PERCENT.HOUSEHOLDS.BELOW.POVERTY )
    , breaks = 5
    , labels = color.ramp
  )
)

# Unemployed
comarea$color_unemployed <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.16..UNEMPLOYED )
    , breaks = 5
    , labels = color.ramp
  )
)

# Diploma
comarea$color_diploma <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA )
    , breaks = 5
    , labels = color.ramp
  )
)

# Age
comarea$color_age <- as.character(
  cut(
    x = rank( comarea$PERCENT.AGED.UNDER.18.OR.OVER.64 )
    , breaks = 5
    , labels = color.ramp
  )
)

# Income
comarea$color_income <- as.character(
  cut(
    x = rank( comarea$PER.CAPITA.INCOME )
    , breaks = 5
    , labels = color.ramp
  )
)

# Hardship
comarea$color_hardship <- as.character(
  cut(
    x = rank( comarea$HARDSHIP.INDEX )
    , breaks = 5
    , labels = color.ramp
  )
) 



# Make hover labels
# Courtesy of https://github.com/USAspendingexplorer/USAspending-explorer/blob/master/Build%20App/leaflet_ny.r#L36
# Crowded Housing
label_crowded <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.OF.HOUSING.CROWDED
  )
  , HTML)

# Poverty
label_poverty <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.HOUSEHOLDS.BELOW.POVERTY
  )
  , HTML)

# Unemployed
label_unemployed <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.16..UNEMPLOYED
  )
  , HTML)

# Diploma
label_diploma <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.25..WITHOUT.HIGH.SCHOOL.DIPLOMA
  )
  , HTML)

# Age
label_age <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s percent"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PERCENT.AGED.UNDER.18.OR.OVER.64
  )
  , HTML)

# Income
label_income <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s dollars"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$PER.CAPITA.INCOME
  )
  , HTML)

# Hardship
label_hardship <- lapply(
  sprintf(
    "<strong>%s:</strong></br/>%s"
    , comarea$COMMUNITY.AREA.NAME
    , comarea$HARDSHIP.INDEX
  )
  , HTML)

# north arrow icon url
northArrowIcon <- "<img src='http://ian.umces.edu/imagelibrary/albums/userpics/10002/normal_ian-symbol-north-arrow-2.png' style='width:40px;height:60px;'>"


# make custom map title
mapTitle <- paste0(
  "<p style='color:#660066; font-size:20px;'>"
  , "Census Data - Selected socioeconomic indicators in Chicago, 2008 - 2012"
  , "</p>"
)

# Make map
census_map <- 
  leaflet( data = comarea
           , options = leafletOptions( zoomControl = FALSE
                                       , minZoom = 11
                                       , maxZoom = 11
                                       , dragging = FALSE
           ) ) %>%
  
  # add background to map
  addTiles( urlTemplate = "https://{s}.tile.openstreetmap.se/hydda/full/{z}/{x}/{y}.png" ) %>%
  
  # set zoom level
  setView( lng = -87.707988
           , lat = 41.832249
           , zoom = 11 ) %>%
  
  
  # add Crowded Housing polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_crowded
               , color = "white"
               , label = label_crowded
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Crowded Housing" ) %>%
  
  # Add Poverty polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_poverty
               , color = "white"
               , label = label_poverty
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Households Below Poverty" ) %>%
  
  # Add Unemployed polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_unemployed
               , color = "white"
               , label = label_unemployed
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged 16+ Unemployed" ) %>%
  
  # Add Diploma polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_diploma
               , color = "white"
               , label = label_diploma
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged 25+ Without High School Diploma" ) %>%
  
  # Add Age polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_age
               , color = "white"
               , label = label_age
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Percent of Aged Under 18 or Over 64" ) %>%
  
  # Add Income polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_income
               , color = "white"
               , label = label_income
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Per Capita Income" ) %>%
  
  # Add Hardship polygons
  addPolygons( smoothFactor = 0.3
               , fillOpacity = 1
               , fillColor = comarea$color_hardship
               , color = "white"
               , label = label_hardship
               , labelOptions = labelOptions( style = list( "font-weight" = "normal" )
                                              , textsize = "15px"
               )
               , highlightOptions = highlightOptions( color = "black"
                                                      , weight = 6
                                                      , bringToFront = TRUE
               )
               , group = "Hardship Index" ) %>%
  
  # add Layers control
  addLayersControl( baseGroups = c( "Percent of Crowded Housing"
                                    , "Percent of Households Below Poverty"
                                    , "Percent of Aged 16+ Unemployed"
                                    , "Percent of Aged 25+ Without High School Diploma"
                                    , "Percent of Aged Under 18 or Over 64"
                                    , "Per Capita Income"
                                    , "Hardship Index" )
                    , position = "topright"
                    , options = layersControlOptions( collapsed = FALSE ) ) 