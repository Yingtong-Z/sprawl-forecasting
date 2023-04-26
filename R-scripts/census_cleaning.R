library(tidyverse)
library(sf)
library(raster)
library(knitr)
library(kableExtra)
library(tidycensus)
library(tigris)
library(FNN)
#library(QuantPsyc) # JE Note: in R 4.1, QuantPsyc package not available.
library(caret)
library(yardstick)
library(pscl)
library(plotROC) 
library(ggrepel)
library(pROC)
library(grid)
library(gridExtra)
library(viridis)
library(igraph)



census_api_key("20ad38c6d9e241be49ae3a1a6f74125448db674e", install = TRUE, overwrite = TRUE)

#acs_variable_list.2000 <- load_variables(2000, #year
                                         "sf1", #five year ACS estimates
                                         #cache = TRUE)
#acs_variable_list.20002 <- load_variables(2000, #year
                                         #"sf3", #five year ACS estimates
                                        # cache = TRUE)


# Define the variables you want to retrieve
variables1 <- c("P001001", "H001001", "H003003", "P003002")
names(variables1) <- c("total_population", "total_housing_units", "total_vacant_households", "total_white")

variables2 <- c("P053001","P036021","P036044")
names(variables2) <- c("median_household_income","Male Total graduate degree holders", "Female Total graduate degree holders")


# Retrieve the data
data_2000_cv1 <- get_decennial(
  geography = "block group",
  year = 2000,
  variables = variables1,
  state = "VA",
  county = "Charlottesville",
  output = "wide",
  geometry = TRUE
)

data_2000_cv2 <- get_decennial(
  geography = "block group",
  sumfile = 'sf3',
  year = 2000,
  variables = variables2,
  state = "VA",
  county = "Charlottesville",
  output = "wide",
  geometry = TRUE
)


data_2000_am1 <- get_decennial(
  geography = "block group",
  year = 2000,
  variables = variables1,
  state = "VA",
  county = "Albemarle",
  output = "wide",
  geometry = TRUE
)

data_2000_am2 <- get_decennial(
  geography = "block group",
  sumfile = 'sf3',
  year = 2000,
  variables = variables2,
  state = "VA",
  county = "Albemarle",
  output = "wide",
  geometry = TRUE
)

# join the data
cvam_2000_census1 <- rbind(data_2000_cv1, data_2000_am1)
cvam_2000_census2 <- rbind(data_2000_cv2, data_2000_am2)

cvam_2000_census <- st_join(cvam_2000_census1, 
                            cvam_2000_census2[, c("median_household_income", "Male Total graduate degree holders", "Female Total graduate degree holders")], 
                            join = st_equals)  %>%
                    mutate(`total_graduate_degree_holders` = `Male Total graduate degree holders` + `Female Total graduate degree holders`) %>%
                    select(-`Male Total graduate degree holders`, -`Female Total graduate degree holders`)






ggplot()+
  geom_sf(data = cvam_2000_census_new, 
          aes(fill = total_population))
