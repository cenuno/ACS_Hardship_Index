#
# Author:   Cristian E. Nuno
# Date:     May 28, 2020
# Purpose:  Calculate a hardship index for each community area in Chicago
#           using ACS 2012-2016 5-Year Estimate using 6 SES variables to update
#           'Census Data - Selected socioeconomic indicators in Chicago, 2008 - 2012'
#            a public data set made available on the Chicago Data Portal.
#            Available here: https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2/data
# Note:     For more on the index metholodgy, see the following:
#           https://github.com/cenuno/ACS_Hardship_Index/blob/master/articles/2004-08-an_update_on_urban_hardship.pdf
#         
#           The formula standardizes each of the 
#           component variables so that they are all given equal weight 
#           in the composite Intercity Hardship Index. 
#           The Index represents the average of the standardized 
#           ratios of all six component variables. 
#           The Intercity Hardship Index ranges from 0 to 100 with 
#           a higher number indicating greater hardship.
#
# API Key:  This script requires the user to obtain a US Census API key.
#           Obtain one here: https://api.census.gov/data/key_signup.html
#
# Wiki:     For more Chicago community areas, see:
#           http://www.encyclopedia.chicagohistory.org/pages/319.html
#

# load necessary packages ----
library(acs)
library(tidyverse)
library(readxl)
library(sf)

# load necessary data ----

# store constants
NA_STRINGS <- c("", "NA", "na", "N/A", "n/a", "missing")
TABLE_SHELL_URL <- "https://www2.census.gov/programs-surveys/acs/summary_file/2016/documentation/user_tools/ACS2016_Table_Shells.xlsx"
CHICAGO_CCA_URL <- "https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON"
CHICAGO_CT_URL <- "https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON"
ACS_VARIABLE_NAMES <-
       # Stub:         SEX BY AGE
       # Universe:     Total Population
       # Table ID:     B01001
  list("population" = c(paste("B01001", 1:9, sep = "_00"),
                        paste("B01001", 10:49, sep = "_0")),
       # Stub:         SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
       # Universe:     Population 25 years and over
       # Table ID:     B15002
       "edu_attain" = c(paste("B15002", 1:9, sep = "_00"),
                        paste("B15002", 10:35, sep = "_0")),
       # Stub:     POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER
       # Universe: Households
       # Table ID: B17017
       "poverty"    = c(paste("B17017", 1:9, sep = "_00"), 
                        paste("B17017", 10:59, sep = "_0")),
       # Stub:     AGGREGATE INCOME IN THE PAST 12 MONTHS (IN 2016 INFLATION-ADJUSTED DOLLARS)
       # Universe: Population 15 years and over
       # Table ID: B19313
       "agg_inc" = "B19313_001",
       # Stub:     SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER
       # Universe: Population 16 years and over
       # Table ID: B23001
       "employment" = c(paste("B23001", 1:9, sep = "_00"), 
                        paste("B23001", 10:99, sep = "_0"), 
                        paste("B23001", 100:173, sep = "_")),
       # Stub:     TENURE BY OCCUPANTS PER ROOM
       # Universe: Total
       # Table ID: B25014
       "crowded_hs" = c(paste("B25014", 1:9, sep = "_00"), 
                        paste("B25014", 10:13, sep = "_0"))
  )

# assumption that a census api key is already installed on your system
# here, the key is stored as CENSUS_API_KEY=YOURKEY in the .Renviron file.
acs::api.key.install(key = Sys.getenv(x = "CENSUS_API_KEY"))

# download the table shells to identify appropriate columns per stub
download.file(url = TABLE_SHELL_URL,
              destfile = "ACS_2016_Table_Shells.xlsx")

acs_2016_table_shells <- readxl::read_xlsx(path = "ACS_2016_Table_Shells.xlsx",
                                           na = NA_STRINGS)

# create chicago community area spatial polygon data frame
chicago_cca <- sf::read_sf(CHICAGO_CCA_URL)

# create chicago census tract spatial polygon data frame
chicago_ct <- sf::read_sf(CHICAGO_CT_URL)

# identify census tracts within Cook County
cook_county_ct <- acs::geo.make(state = "IL", county = "Cook", tract = "*")

# download Cook County ACS 2012-2016 5-Year Estimate data and store in a list
# https://census.gov/programs-surveys/acs/technical-documentation/table-shells.html
# note: avoid the use of the `table.number` parameter
# note: keep the 'geography' and 'estimate' data frames within each acs object
acs_ses_data <- 
  purrr::map(.x = ACS_VARIABLE_NAMES, 
             # for each set of SES indicators,
             # retrieve the relevant ACS data from the API
             .f = ~ acs::acs.fetch(endyear = 2016,
                                   span = 5,
                                   geography = cook_county_ct,
                                   variable = .x)) %>%
  # for each SES acs object,
  # return a data frame that contains one row
  # per census tract and its ACS estimate value.
  purrr::map(.x = .,
             .f = ~ dplyr::bind_cols(slot(.x, name = "geography"),
                                     dplyr::as_tibble(
                                       slot(.x, name = "estimate")
                                       )) %>%
               dplyr::as_tibble()) %>%
  # name each SES data frame with its corresponding SES short name
  purrr::set_names(names(ACS_VARIABLE_NAMES))
  
# identify Chicago census tracts (CT) to community area (CCA) crosswalk ----
# this is not a perfect 1:1 aggregation of one CT per CCA
chicago_ct %>%
  filter(notes != "") %>%
  select(name10, commarea, notes)

# ASSUMPTION NOTE:
# Assuming the City of Chicago used the CT-CCA crosswalk 
# within the CT spatial polygons data frame
# to determine which CT belong in which CCA
ct_cca_xwalk <- 
  chicago_ct %>%
  dplyr::select(commarea_n, tractce10) %>%
  # note: explict casting to tibble is needed since these are 
  #       sf geometry objects
  dplyr::as_tibble() %>%
  dplyr::left_join(y = chicago_cca %>%
                       dplyr::select(community, area_numbe) %>%
                       dplyr::as_tibble(),
            by = c("commarea_n" = "area_numbe")) %>%
  dplyr::select(-c(geometry.x, geometry.y))

# Limit acs SES data from Chicago CT that are found in the CT-CCA xwalk ----
chicago_acs_ses_data <-
  purrr::map(.x = acs_ses_data,
             .f = ~ .x %>%
                    dplyr::inner_join(y = ct_cca_xwalk,
                                      by = c("tract" = "tractce10")))

# calculuate each ses indicator, matching the description from the data portal -----
hardship_indicators <-
  # Name:           Percent Aged Under 18 or Over 64	
  # Description:    Percent of the population under 18 or over 64 years of age 
  #                 (i.e., dependency)
  list("dependency" = chicago_acs_ses_data$population %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(total_population = sum(B01001_001),
                   population_aged_under18_over64 = 
                     sum(B01001_003, B01001_004, B01001_005, B01001_006,
                         B01001_027, B01001_028, B01001_029, B01001_030, 
                         B01001_020, B01001_021, B01001_022, B01001_023, 
                         B01001_024, B01001_025, B01001_044, B01001_046,
                         B01001_047, B01001_048, B01001_049)
                    , population_aged_under18_over64_ratio = 
                     100 * (population_aged_under18_over64 / total_population)
         ) %>%
         dplyr::mutate(indicator = "Percent Aged Under 18 or Over 64") %>%
         dplyr::select(indicator, everything()),
       # Name:           Percent Aged 25+ Without High School Diploma
       # Description:    Percent of persons aged 25 years or older without a high school diploma
       "without_hs" = chicago_acs_ses_data$edu_attain %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(total_population = sum(B15002_001), 
                   population_aged25plus_without_hs_diploma = 
                     sum(B15002_003, B15002_004, B15002_005, B15002_006, 
                         B15002_007, B15002_008, B15002_009, B15002_010, 
                         B15002_020, B15002_021, B15002_022, B15002_023, 
                         B15002_024, B15002_025, B15002_026, B15002_027), 
                   population_aged25plus_without_hs_diploma_ratio = 
                     100 * (population_aged25plus_without_hs_diploma / 
                        total_population)
         ) %>%
         dplyr::mutate(indicator = 
                         "Percent Aged 25+ Without High School Diploma") %>%
         dplyr::select(indicator, everything()),
       # Name:           Percent Households Below Poverty
       # Description:    Percent of households living below the federal poverty level
       "below_fpl" = chicago_acs_ses_data$poverty %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(total_population = sum(B17017_001), 
                          population_living_below_fpl = sum(B17017_002), 
                          population_living_below_fpl_ratio = 
                            100 * (population_living_below_fpl / 
                                     total_population)
         ) %>%
         dplyr::mutate(indicator = 
                          "Percent Households Below Poverty") %>%
         dplyr::select(indicator, everything()),
       # Name:           Per Capita Income
       # Description:    A community area’s per capita income was estimated by dividing 
       #                 the aggregate income of the census tracts within the community area 
       #                 (from table B19313) by the number of residents (from table B01001).
       # Note:           Logic happens after this list is created down
       "pci" = chicago_acs_ses_data$agg_inc %>%
         # remove any CT where the PCI is less than zero
         dplyr::filter(B19313_001 >= 0) %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(agg_inc = sum(B19313_001)) %>%
         dplyr::mutate(indicator = 
                         "Aggregate per capita income") %>%
         dplyr::select(indicator, everything()),
       # Name:           Percent Aged 16+ Unemployed
       # Description:    Percent of persons aged16 years or older in the labor force that are unemployed
       "unemployed" = chicago_acs_ses_data$employment %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(
           unemployed = sum(B23001_008, B23001_015, B23001_022, B23001_029, 
                            B23001_036, B23001_043, B23001_050, B23001_057,  
                            B23001_064, B23001_071, B23001_076, B23001_081, 
                            B23001_086, B23001_094, B23001_101, B23001_108, 
                            B23001_115, B23001_122, B23001_129, B23001_136, 
                            B23001_143, B23001_150, B23001_157, B23001_162, 
                            B23001_167, B23001_172),
           labor_force = sum(B23001_004, B23001_011, B23001_018, B23001_025, 
                             B23001_032, B23001_039, B23001_046, B23001_053, 
                             B23001_060, B23001_067, B23001_074, B23001_079, 
                             B23001_084, B23001_090, B23001_097, B23001_104, 
                             B23001_111, B23001_118, B23001_125, B23001_132, 
                             B23001_139, B23001_146, B23001_153, B23001_160,
                             B23001_165, B23001_170),
           unemployment_ratio = 100 * (unemployed / labor_force)
       ) %>%
         dplyr::mutate(indicator = "Percent Aged 16+ Unemployed") %>%
         dplyr::select(indicator, everything()),
       # Name:           Percent of Housing Crowded
       # Description:    Percent of occupied housing units with more than one person per room 
       #                 (i.e., crowded housing)
       "crowded_housing" = chicago_acs_ses_data$crowded_hs %>%
         dplyr::group_by(community) %>%
         dplyr::summarize(total_households = sum(B25014_001), 
                          crowded_households = sum(B25014_005, B25014_006, 
                                                   B25014_007, B25014_011, 
                                                   B25014_012,  B25014_013),
                          crowded_ratio = 100 * (crowded_households / 
                                                   total_households) 
         ) %>%
         dplyr::mutate(indicator = "Percent of Housing Crowded") %>%
         dplyr::select(indicator, everything())
  )

# calculate the per capita income by obtain the total population
hardship_indicators$pci <-
  hardship_indicators$pci %>%
  dplyr::left_join(hardship_indicators$dependency %>%
              dplyr::select(community, total_population),
              by = "community") %>%
  dplyr::mutate(pci = agg_inc / total_population)


# create df with one row per community and one column per SES indicator ----
# that will determine the hardship index.
hardship_indicator_df <-
  hardship_indicators %>%
  purrr::reduce(.f = left_join, by = "community") %>%
  dplyr::select(community, 
                crowded_ratio, 
                population_living_below_fpl_ratio,
                unemployment_ratio,
                population_aged25plus_without_hs_diploma_ratio,
                population_aged_under18_over64_ratio,
                pci)

# calculate the hardship index for each CCA ----
# Description:    The hardship index is a score that incorporates each of the six selected socioeconomic indicators according to the method described in An Update on Urban Hardship 
#                 (see footnote 2 on page 53 of https://github.com/cenuno/ACS_Hardship_Index/blob/master/articles/2004-08-an_update_on_urban_hardship.pdf). 
#                 Scores on the index can range from 1 to 100, with a higher index number representing a greater level of hardship. 
#                 The scores are standardized according to the data for the 77 community areas, 
#                 and therefore cannot be compared to scores generated for other jurisdictions.

# Identify the min and max values 
# for each socioeconomic indicator
min_max_values_df <-
  hardship_indicator_df %>%
  # for each indicator, find the min and max value
  summarise_if(is.numeric, list(~min(.), ~max(.))) %>%
  # reshape to one record per indicator per min/max value 
  tidyr::gather(key = "indicator", 
                value = "value") %>%
  dplyr::mutate(metric = if_else(str_detect(indicator, "_min"), 
                                 "min", "max"),
                indicator = stringr::str_remove_all(indicator, 
                                                    "_(min|max)")) %>%
  dplyr::select(metric, everything())

# Standardize each
# socioeconomic indicator value
# EXCEPT Per Capita Income
standardized_indicator_values_df <- 
  purrr::pmap(.l = list(min_value = min_max_values_df %>%
                          filter(metric == "min" & indicator != "pci") %>% 
                          pull(value),
                        max_value = min_max_values_df %>%
                          filter(metric == "max" & indicator != "pci") %>% 
                          pull(value),
                        raw_values = hardship_indicator_df %>%
                          select(-community, -pci)),
              .f = function(min_value, max_value, raw_values)
                (raw_values - min_value) / (max_value - min_value) * 100) %>%
  purrr::set_names(hardship_indicator_df %>% select(-community, -pci) %>% names()) %>%
  dplyr::bind_cols(., hardship_indicator_df %>% select(community)) %>%
  dplyr::select(community, everything())

# To standardize Per Capita Income
# I need to flip the values in such a way that the numerator and the denominator 
# are both negative (to get a positive value for greater hardship). 
# So I subtract the max income from the income 
# to make sure the community area with the highest income has a standardized value of 0 
# and the one with the smallest income has a standardized value of 100. 
standardized_indicator_values_df <-
  standardized_indicator_values_df %>%
  mutate(pci = 
           (
             (hardship_indicator_df$pci - 
              min_max_values_df %>% 
              dplyr::filter(metric == "max" & indicator == "pci") %>%
              dplyr::pull()) /
           (min_max_values_df %>% 
              dplyr::filter(metric == "min" & indicator == "pci") %>%
              dplyr::pull() - 
           min_max_values_df %>% 
             dplyr::filter(metric == "max" & indicator == "pci") %>%
             dplyr::pull())
           ) * 100
  )

# now find the average of the standardized ratios to compute the index
hardship_index_df <-
  standardized_indicator_values_df %>%
  # reshape from wide to long
  # note: one row per CCA per standardized indicator value
  tidyr::gather(key = "indicator", 
                value = "standardized_value", 
                -community) %>%
  dplyr::group_by(community) %>%
  dplyr::summarize(hardship_index = mean(standardized_value))

# visualize results -----
chicago_cca %>%
  dplyr::left_join(y = hardship_index_df, 
                   by = "community") %>%
  ggplot2::ggplot(aes(fill = hardship_index)) +
  ggplot2::geom_sf() +
  ggplot2::scale_fill_continuous(type = "viridis",
                                 name = "Hardship Index") +
  ggplot2::labs(title = "Residents of the South and West sides of Chicago\nface more hardship than other communities",
                subtitle = "The Intercity Hardship Index ranges from 0 to 100\nwitha higher number indicating greater hardship.",
                caption = "Data source: ACS 2012-2016 5-Year Estimates | Made by @cenuno_") +
  ggplot2::theme_minimal() +
  ggplot2::ggsave("chicago_community_area_hardship_2012_2016.png",
                  width = 9.45,
                  height = 7.95,
                  dpi = 450)

# export findings ----
readr::write_csv(hardship_indicator_df %>%
                   dplyr::left_join(hardship_index_df,
                                    by = "community"),
                 "chicago_community_area_hardship_2012_2016.csv")
