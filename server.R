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

######################
## Build the server ##
######################
server <- function( input, output ) {
  
  # render leaflet output
  output$mymap <- leaflet::renderLeaflet({
    census_map 
  }) # end of renderleaflet
  
  # Dynamic Legend
  observeEvent( input$mymap_groups,{
    
    mymap <- leafletProxy( "mymap" ) %>% clearControls()
    
    if( input$mymap_groups == "Percent of Crowded Housing" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "0.46% - 3.2%"
                                 , "3.3% - 6%"
                                 , "6.1% - 8.8%"
                                 , "8.9% - 11.5%"
                                 , "11.6% - 14.29%" )
                   , opacity = 1
                   , group = "Percent of Crowded Housing" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Households Below Poverty" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "3.27% - 14.5%"
                                 , "14.6% - 25.8%"
                                 , "25.9% - 37%"
                                 , "37.1% - 48.3%"
                                 , "48.4% - 59.57%" )
                   , opacity = 1
                   , group = "Percent of Households Below Poverty" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged 16+ Unemployed" ) {
      mymap <- mymap %>%
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "3.22%- 10%"
                                 , "10.1% - 16.7%"
                                 , "16.8% - 23.4%"
                                 , "23.5% - 30.2%"
                                 , "30.3% - 36.93%" )
                   , opacity = 1
                   , group = "Percent of Aged 16+ Unemployed" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged 25+ Without High School Diploma" ) {
      mymap <- mymap %>%
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "1.61% - 11.2%"
                                 , "11.3% - 20.8%"
                                 , "20.9% - 30.4%"
                                 , "30.5% - 40%"
                                 , "40.1% - 49.64%" )
                   , opacity = 1
                   , group = "Percent of Aged 25+ Without High School Diploma" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Percent of Aged Under 18 or Over 64" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "14.24% - 20.9%"
                                 , "21% - 27.6%"
                                 , "27.7% - 34.3%"
                                 , "34.4% - 41%"
                                 , "41.1% - 47.7%" )
                   , opacity = 1
                   , group = "Percent of Aged Under 18 or Over 64" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Per Capita Income" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "$8,425 - $24,400"
                                 , "$24,401 - $40,500"
                                 , "$40,501 - $56,500"
                                 , "$56,501 - $72,500"
                                 , "$72,501 - $88,507" )
                   , opacity = 1
                   , group = "Per Capita Income" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
      
    } else if( input$mymap_groups == "Hardship Index" ) {
      mymap <- mymap %>% 
        addLegend( "bottomright"
                   , colors = color.ramp
                   , title = "Legend"
                   , labels = c( "8.58 - 24.2"
                                 , "24.3 - 39.8"
                                 , "39.9 - 55.4"
                                 , "55.5 - 71"
                                 , "71.1 - 86.6" )
                   , opacity = 1
                   , group = "Hardship Index" ) %>%
        addControl( html = mapTitle
                    , position = "topleft" ) %>%
        addControl( html = northArrowIcon
                    , position = "bottomleft" )
    } # end of else if statements
    
  }) # end of observe event
  
} # closing out server