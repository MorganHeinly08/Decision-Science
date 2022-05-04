
# US CITIES EXTRACTION FOR OPTIMAL ROUTE ANALYSIS / TRAVELING SALESMAN PROBLEM

library(datasets)
library(tidyverse)

# get state names and regions

states <- datasets::state.name
abb <- datasets::state.abb
regions <- datasets::state.region

# create data frames
state_region_df <- tibble(state = states,
                          region = regions,
                          abbs = abb )

# filter by south region
south_region_df <- state_region_df %>%
  filter(region == "South")

# now let's get some cities
cities_df <- tibble(maps::us.cities) %>%
  rename(abbs = country.etc,
         city = name)

# let' see if there are any missing from each since these are different packages

south_final <- cities_df %>% semi_join(south_region) %>% # no missing cities
  separate(city,c("city","state"),sep = "[A-Z]{2}") %>% # separate city / state in name column
  select(-state) %>% #remove old state column
  mutate(city_state = str_c(city,abbs,sep = " ")) %>% # prepare column for google API format
  filter(abbs %in% c("GA","NC","SC")) # filter only states covered by sales rep

# load gmapsdistance library

#install.packages("gmapsdistance") - a package that uses google maps API to obtain distances for US cities / states, etc.

library(gmapsdistance)

set.seed(123)

origins <- south_final %>% pull(city_state) # extract list of city/states
destinations <- south_final %>% pull(city_state) #extract list of city/states. origin same as destinations

# set API key

api_key <- set.api.key("enter google cloud API Key") # api key via your google maps account (free $300 credit)

# get distances in matrix format for selected cities

results <- gmapsdistance(
  origins,
  destinations,
  combinations = "all"
  #mode = "driving",
  # key = key = get.api.key(api_key),
  # shape = "wide",
  # avoid = "",
  # departure = "",
  # dep_date = "",
  # dep_time = "",
  # traffic_model = "None",
  # arrival = "",
  # arr_date = "",
  # arr_time = ""
)

# get the matrix out of list and save the results

final_results <- results[[1]] %>% pluck()

write.table(as.matrix(final_results),"final_results.txt")






