library(tidyverse)
library(tidycensus)
library(magrittr)

data_folder <- "C:/Users/sec3343/Box/R01 Urban Planning/Aim 1 AP Sources and Conc Across Tx/Data/"

#Load census geography (edit to specify year)
census_geography <- readRDS(paste0(data_folder,"Spatial/Tract geometry/statewide_tracts_2020.RDS"))
census_geography_CACES <- readRDS(paste0(data_folder,"Spatial/Tract joined CACES/CACES_2020.RDS"))


add_ACS_data_state <- function(census_tracts,
                         state_pick = "Texas",
                         year_pick){
  # Create list of names to query from ACS for race/ethnicity  
  ACS_names_list <- load_variables(year_pick, "acs5", cache = TRUE)
  
  pop_groups <- ACS_names_list %>% 
    filter(str_detect(ACS_names_list$name, "B01001B|B01001H|B01001I")) %>% 
    mutate(Ecodes = paste0(name,"E"))
  
  # Retrieve race/ethnicity data from ACS using tidycensus 
  CT_race_eth_pop <- get_acs(geography = "tract",
                             variables = c(pop_groups$name,"B01001_001",
                                           "B03002_012","B03002_013","B03002_014"),
                             state = state_pick,
                             year = year_pick,
                             survey = "acs5",
                             output = "wide") %>% 
    mutate(Total = B01001_001E,
           White_ped = rowSums(across(pop_groups$Ecodes[c(34:37,49:52)])),
           Black_ped = rowSums(across(pop_groups$Ecodes[c(3:6,18:21)])),
           Latinx_ped = rowSums(across(pop_groups$Ecodes[c(65:68,80:83)])),
           White_adu = rowSums(across(pop_groups$Ecodes[c(38:47,53:62)])),
           Black_adu = rowSums(across(pop_groups$Ecodes[c(7:16,22:31)])),
           Latinx_adu = rowSums(across(pop_groups$Ecodes[c(69:78,84:93)])),
           L_total = B03002_012E,
           WL_total = B03002_013E,
           BL_total = B03002_014E,
           WL_share = B03002_013E/B03002_012E,
           BL_share = B03002_014E/B03002_012E,
           WBL_ped = White_ped + Black_ped + Latinx_ped,
           WBL_adu = White_adu + Black_adu + Latinx_adu,
           White_tot = White_ped + White_adu,
           Black_tot = Black_ped + Black_adu,
           Latinx_tot = Latinx_ped + Latinx_adu,) %>%
    dplyr::select(c("GEOID","Total","White_ped","Black_ped",
                    "Latinx_ped","White_adu","Black_adu",
                    "Latinx_adu","L_total","WL_total","BL_total",
                    "BL_share","WL_share","WBL_ped","WBL_adu",
                    "White_tot","Black_tot","Latinx_tot")
    )
  
  census_tracts %<>% left_join(., CT_race_eth_pop)
  
  # Create list of names to query from ACS for SES
  ses_groups = ACS_names_list %>% 
    filter(str_detect(ACS_names_list$name, 
                      paste(c("B01001_001", #Total pop
                              "B17001_001","B17001_002", #Population under poverty
                              #"B08014_001","B08014_002", #Pop of workers >16 w/o vehicle
                              "B25044_001","B25044_003","B25044_010", #Occupied Housing Units, no vehicles available
                              "B15003", #Educational attainment, pop >25
                              "B28010_001","B28010_007", #No computer, households
                              "B28006_", #No computer, pop >25
                              "B27001", #Health insurance for civilian population
                              "B25040_001","B25040_002", #Occupied households, utility gas
                              "B25014_", #Occupants per room
                              "B25004", # Unoccupied housing units
                              "B25024", #Number of housing units
                              "B25041", #Number of bedrooms
                              "B16001_001","B16001_005", #Linguistic isolation, spanish speaking
                              "B25037_001","B25037_002","B25037_003" #Median year structure built
                      ), collapse = "|"))) %>% 
    mutate(Ecodes = paste0(name,"E"))
  
  # Query ACS with tidycensus for SES data
  CT_ses <- get_acs(geography = "tract",
                    variables = ses_groups$name,
                    state = state_pick,
                    year = year_pick,
                    survey = "acs5",
                    output = "wide") %>% 
    dplyr::select(c("GEOID","NAME",ses_groups$Ecodes)) %>% 
    mutate(Total = B01001_001E,
           Total_pov = B17001_001E,
           Under_pov = B17001_002E,
           Veh_units = B25044_001E,
           Unit_no_veh = rowSums(across(c(B25044_003E,B25044_010E))),
           Over25 = B15003_001E,
           Under_hs = rowSums(across(B15003_002E:B15003_016E)),
           Vacant_units = B25004_001E,
           Occupied_units = B25014_001E,
           Total_units = B25024_001E,
           Crowded_units = rowSums(across(c(B25014_005E:B25014_007E,
                                            B25014_011E:B25014_013E))),
           Attached_housing = rowSums(across(B25024_007E:B25024_009E)),
           Less2_bedrooms = rowSums(across(B25041_002E:B25041_003E)),
           Utility_gas = B25040_002E,
           Civilian = B27001_001E,
           Uninsur = rowSums(across(c(B27001_005E,B27001_008E,B27001_011E,
                                      B27001_014E,B27001_017E,B27001_020E,
                                      B27001_023E,B27001_026E,B27001_029E,
                                      B27001_033E,B27001_036E,B27001_039E,
                                      B27001_042E,B27001_045E,B27001_048E,
                                      B27001_051E,B27001_054E,B27001_057E)))
           
    ) %>% 
    dplyr::select(!(ses_groups$Ecodes))
  
  # Calculate proportions from SES data
  SES_CT_shares <- CT_ses %>%  
    mutate(poverty = Under_pov/Total_pov,
           no_vehicle = Unit_no_veh/Veh_units,
           less_than_HS = Under_hs/Over25,
           no_insur = Uninsur/Civilian,
           gas_heating = Utility_gas/Occupied_units,
           crowding = Crowded_units/Occupied_units,
           vacant = Vacant_units/Total_units,
           attached_10 = Attached_housing/Total_units,
           less2_bed = Less2_bedrooms/Total_units
    ) %>% 
    dplyr::select(!c(NAME:Uninsur))
  
  # Join SES shares with exposure data and calculate race/ethnicity shares
  census_tracts %<>% left_join(., SES_CT_shares, by = "GEOID") %>% 
    mutate(share_black = Black_tot/Total,
           share_latinx = Latinx_tot/Total,
           share_bl = (Black_tot - BL_total + Latinx_tot)/Total)
  
  return(census_tracts)
}


geography_plus_demographics <- add_ACS_data_state(census_geography, year_pick = 2020)
geography_CACES_demographics <- add_ACS_data_state(census_geography_CACES, year_pick = 2020)

glimpse(geography_CACES_demographics)
names(geography_CACES_demographics)

saveRDS(geography_CACES_demographics, paste0(data_folder,"Spatial/Tract with demographics and pollution/CACES/statewide_2020.RDS"))
