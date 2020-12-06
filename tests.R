library(tidyverse)
library(readr)

file_url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if(!file.exists("./data/reproducible_research_project_2.bz2")){
  download.file(file_url, destfile = "./data/reproducible_research_project_2.csv.bz2", method = "curl")
}

# 2. Read data into global environment
if(!exists("storm_data")) {
  storm_data <- read.table("data/reproducible_research_project_2.csv.bz2", header = TRUE, sep = ",", dec = ".")
}

# 3. Check the dimensions of the data
dim(storm_data)

# 4. Check "event type" to get a sense for what types of events are in the dataset
# Ensure that all the event types are upper case & extra spaces are removed
storm_data$EVTYPE <- toupper(storm_data$EVTYPE)
storm_data$EVTYPE <- trimws(storm_data$EVTYPE, which = c("both"))
unique_states <- unique(storm_data$STATE)
unique_events <- unique(storm_data$EVTYPE)
NROW(unique_events)

# 5. Add a 'year' variable 
storm_data$begin_date <- as.Date(storm_data$BGN_DATE,"%m/%d/%Y")
storm_data$year <- format(storm_data$begin_date,"%Y")

# 6. Add a 'region' variable to look at regional differences (regions from NOAA website)
regions <- data.frame(region = "Central", state = c("IL", "IN",	"KY",	"MO",	"OH",	"TN",	"WV", "LE"))
east_n_central <- (data.frame(region = "East North Central", state = c("IA",	"MI",	"MN",	"WI", "LC", "LH", "LM", "LS")))
northeast <- (data.frame(region = "Northeast", state = c("CT",	"DE",	"ME",	"MD",	"MA",	"NH",	"NJ",	"NY",	"PA",	"RI",	"VT", "AN", "SL", "LO", "XX")))
northwest <- (data.frame(region = "Northwest", state =c("ID",	"OR",	"WA", "AK", "PK")))
south <- (data.frame(region = "South", state =c("AR",	"KS",	"LA",	"MS",	"OK",	"TX")))
southeast <- (data.frame(region = "Southeast", state =c("AL",	"FL",	"GA",	"NC",	"SC",	"VA", "DC", "AM", "GM")))
southwest <- (data.frame(region = "Southwest", state =c("AZ",	"CO",	"NM",	"UT")))
west <- (data.frame(region = "West", state =c("CA", "NV")))
west_n_central <- (data.frame(region = "West North Central", state =c("MT",	"NE",	"ND",	"SD",	"WY")))
islands <- (data.frame(region = "Islands", state =c("HI", "PR", "AS", "GU", "MH", "VI", "PH", "PZ", "PM")))

regions <- rbind(regions, east_n_central, northeast, northwest, south, southeast, southwest, west, west_n_central, islands)

states_regions <- regions$state  
unique_states <- unique(storm_data$STATE)
common_states <-  intersect(unique_states, states_regions)
unmatched_states <- unique_states [! unique_states %in% common_states]

library(glue)

##make region table to include the numbers of the states with a merge on state 

state_numbers <- data.frame(storm_data[, c("STATE", "STATE__")])

library(dplyr)
state_numbers <- distinct(state_numbers, STATE, .keep_all = TRUE)

regions <- regions %>% 
  rename(
    STATE = state
  )

regions <- merge(regions, state_numbers)

## Set duplicate state number 20 to NA 
bad_state_index = match("ST", storm_data$STATE)
storm_data$STATE__[bad_state_index] <- NA

## Build vector of regions at index 
build_region_lookup <- function(regions) {
  max_state_id <- max(regions$STATE__)
  my_lookup_vector <- vector(mode="character", length=max_state_id)
  for (i in 1:nrow(regions)) {
    state_id <- regions$STATE__[i]
    if (!is.na(state_id)) {
      my_lookup_vector[state_id] <- regions$region[i]
    }
  }
  return (my_lookup_vector)
} 

state_id_to_region <- build_region_lookup(regions)
storm_data$region <-state_id_to_region[storm_data$STATE__]


# # Welp, didn't need any of that, just call `format(difftime)` instead
# nicely_format_elapsed_time <- function(elapsed_time) {
#   seconds_per_minute = 60
#   minutes_per_hour = 60
#   
#   elapsed_time <- as.integer(round(elapsed_time))
#   just_seconds = elapsed_time %% seconds_per_minute
#   just_minutes = as.integer(elapsed_time / seconds_per_minute) %% minutes_per_hour
#   just_hours = as.integer(elapsed_time / (seconds_per_minute*minutes_per_hour))
#   
#   return_string = glue("{just_seconds}s")
#   if (just_minutes > 0 || just_hours > 0) {
#     return_string = glue("{just_minutes}m{return_string")
#     if (just_hours > 0) {
#       return_string = glue("{just_hours}h{return_string")
#     }
#   }
#   return(return_string)
# }

# Format Percent
fpercent <- function(p, digits = 0) {
  return(paste(format(p*100, digits=digits),"%", sep=""))
}

assign_regions <- function(df) {
  execution_start_time <- Sys.time()
  status_increment = 10000
  nrows_in_df <- nrow(df)
  
  # build the look-up table (it's the key to making our later loop run quickly)
  state_id_to_region <- build_region_lookup(regions)

  df$region <- "NULL" 
  for (n in 1:nrows_in_df) {
    if (n==1 | n%%status_increment == 0) {
      percent_done = n/nrows_in_df
      elapsed_time = Sys.time() - execution_start_time
      estimated_overall_time = elapsed_time * nrows_in_df / n
      message(glue("{fpercent(percent_done)}.  Working on row #{n} of {nrows_in_df}. Estimated {format(estimated_overall_time, digits=2)} for all."))
    }
    
    state_id <- df$STATE__[n]
    if(!is.na(state_id)) {
      region <- state_id_to_region[state_id]
      df$region[n] <- region 
    }
  }
  
  execution_end_time <- Sys.time()
  elapsed_time <- execution_end_time - execution_start_time
  message(glue("assign_regions() completed for {nrows_in_df} rows in {format(elapsed_time)}."))
  
  return(df)
}

storm_data_small <- head(storm_data, n=10000)

#storm_data_small$region <- "NULL" 


storm_data_with_regions <- assign_regions(storm_data)

