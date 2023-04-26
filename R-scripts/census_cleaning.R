library(tidyverse)
library(sf)
library(raster)
library(knitr)
library(kableExtra)
library(tidycensus)
library(tigris)



# load census variables list
#acs_variable_list.2020 <- load_variables(2020, #year
#                                        "acs5", #five year ACS estimates
#                                        cache = TRUE)

#acs_variable_list.2000A <- load_variables(2000, #year
#                                          "sf1", #five year ACS estimates
#                                         cache = TRUE)

#acs_variable_list.2000B <- load_variables(2000, #year
#                                          "sf3", #five year ACS estimates
#                                          cache = TRUE)


##################### copy codes below ############################

census_api_key("20ad38c6d9e241be49ae3a1a6f74125448db674e", install = TRUE, overwrite = TRUE)

#funtion to get and clean 2000 census data (decennial)
clean_2000_census_data <- function() {
  
  #varibles to use for 2000 decenital variables
  variables2000A <- c("P001001", "H001001", "H003003", "P003003")
  names(variables2000A) <- c("total_population", "total_housing_units", "total_vacant_households", "total_white")
  
  variables2000B <- c("P053001","P036021","P036044")
  names(variables2000B) <- c("median_household_income","Male Total graduate degree holders", "Female Total graduate degree holders")
  
  #funtion to get 2000 decenital variables
  get_decenial_variables <- function(year, sumfile, variables, county) {
    data <- get_decennial(
      geography = "block group",
      year = year,
      sumfile = sumfile,
      variables = variables,
      state = "VA",
      county = county,
      output = "wide",
      geometry = TRUE
    )
    
    return(data)
  }
  
  # get variables from different decennial files for CV and AM
  data_cv1 <- get_decennial_variables(2000,'sf1', variables2000A, "Charlottesville")
  data_cv2 <- get_decennial_variables(2000,'sf3', variables2000B, "Charlottesville")
  data_am1 <- get_decennial_variables(2000,'sf1', variables2000A, "Albemarle")
  data_am2 <- get_decennial_variables(2000,'sf3', variables2000B, "Albemarle")
  
  # join the data
  cvam_census1 <- rbind(data_cv1, data_am1)
  cvam_census2 <- rbind(data_cv2, data_am2)
  
  cvam_census <- st_join(cvam_census1, 
                         cvam_census2[, c("median_household_income", "Male Total graduate degree holders", "Female Total graduate degree holders")], 
                         join = st_equals)  %>%
    mutate(`total_graduate_degree_holders` = `Male Total graduate degree holders` + `Female Total graduate degree holders`) %>%
    dplyr::select(-`Male Total graduate degree holders`, -`Female Total graduate degree holders`)
  
  return(cvam_census)
}



#funtion to get and clean 2020 census data (acs5)
clean_2020_census_data <- function() {
  
  #varibles to use for 2020 decenital variables
  variables2020A <- c("B01003_001E", "B25001_001E", "B25002_003E", "B02001_002E")
  names(variables2020A) <- c("total_population", "total_housing_units", "total_vacant_households", "total_white")
  
  variables2020B <- c("B19049_001E","B14002_022E","B14002_046E")
  names(variables2020B) <- c("median_household_income","Male Total graduate degree holders", "Female Total graduate degree holders")
  
  #funtion to get 2020 decenital variables
  get_acs_variables <- function(year, variables, county) {
    data <- get_acs(
      geography = "block group",
      survey = "acs5",
      year = year,
      variables = variables,
      state = "VA",
      county = county,
      output = "wide",
      geometry = TRUE
    )
    
    return(data)
  }
  
  # get variables from different acs files for CV and AM
  data_cv1 <- get_acs_variables(2020, variables2020A, "Charlottesville") %>% dplyr::select(!ends_with("M"))
  data_cv2 <- get_acs_variables(2020, variables2020B, "Charlottesville") %>% dplyr::select(!ends_with("M"))
  data_am1 <- get_acs_variables(2020, variables2020A, "Albemarle") %>% dplyr::select(!ends_with("M"))
  data_am2 <- get_acs_variables(2020, variables2020B, "Albemarle") %>% dplyr::select(!ends_with("M"))
  
  # join the data
  cvam_census1 <- rbind(data_cv1, data_am1)
  cvam_census2 <- rbind(data_cv2, data_am2)
  
  cvam_census <- st_join(cvam_census1, 
                         cvam_census2[, c("median_household_income", "Male Total graduate degree holders", "Female Total graduate degree holders")], 
                         join = st_equals)  %>%
    mutate(`total_graduate_degree_holders` = `Male Total graduate degree holders` + `Female Total graduate degree holders`) %>%
    dplyr::select(-`Male Total graduate degree holders`, -`Female Total graduate degree holders`)
  
  return(cvam_census)
}


# run the funtions 
cvam_2000_census <- clean_2000_census_data() # 2000 census data
cvam_2020_census <- clean_2020_census_data() # 2020 census data



# plot the census data
ggplot()+
  geom_sf(data = cvam_2020_census, 
          aes(fill = median_household_income))
