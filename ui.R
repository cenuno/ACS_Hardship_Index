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

##################
## Build the UI ##
##################
ui <- fillPage( leaflet::leafletOutput( outputId = "mymap", height = "100%" ) )