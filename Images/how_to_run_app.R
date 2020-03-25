# Install necessary packages
install.packages( c( "shiny", "htmltools", "sp", "devtools", "rgdal" ) )

# install 'leaflet' package from source
# for more info, click here: https://rstudio.github.io/leaflet/
devtools::install_github( "rstudio/leaflet" )

# Load necessary packages
library( shiny )

# Run shiny app from your Rstudio console
shiny::runUrl( "https://github.com/gracehwang9584/ACS_Hardship_Index/archive/master.zip" )




