# 
# Author: Grace Hwang
# Purpose: Save Community Area into RDS format
#

# transform vector into spatial dataframe
comarea <- readOGR( dsn = "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GEOJSON"
                    , layer = "OGRGeoJSON"
                    , stringsAsFactors = FALSE
)

saveRDS( comarea, file = "/Users/gracehwang/Desktop/Census Data/Hardship_Index/Data/comarea.rds" )