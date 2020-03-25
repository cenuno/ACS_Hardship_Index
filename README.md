# ACS_Hardship_Index

This project is an open source interactive map that visualizes census data of selected socioeconomic indicators for the City of Chicago across its 77 community areas. Based on the 2008-2012 public data made available through the City of Chicago data portal (https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data), this project is an updated 2012-2016 version with data from the ACS 2012-2016 5-Year Estimate data set.

With the map, you can visually analyze the socioeconomic indicators across different community areas. These indicators include:

1. Percent of Crowded Housing
2. Percent of Households below Poverty
3. Percent of Unemployed Aged 16+
4. Percent of Aged 25+ without High School Diploma
5. Percent of Aged Under 18 or Over 64
6. Per Capita Income
7. Hardship Index

# Launch this application in your R console
```R
# Install necessary packages
install.packages( c( "shiny", "htmltools", "sp", "devtools", "rgdal" ) )

# install 'leaflet' package from source
# for more info, click here: https://rstudio.github.io/leaflet/
devtools::install_github( "rstudio/leaflet" )

# Load necessary packages
library( shiny )

# Run shiny app from your Rstudio console
shiny::runUrl( "https://github.com/pvanhissenhovenf/ACS_Hardship_Index/archive/master.zip" )
```

# Screenshots of ACS_Hardship_Index app
![per capita income](https://github.com/gracehwang9584/ACS_Hardship_Index/blob/master/Images/per_capita.png)

![hardship index](https://github.com/gracehwang9584/ACS_Hardship_Index/blob/master/Images/hardshipindex.png)

# Collaborators
- Grace Hwang
- Patricia van Hissenhoven

# Supervisor
- Cristian Nuno

**Last updated on February 2, 2018**
