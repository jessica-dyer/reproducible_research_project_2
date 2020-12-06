storm_data_small <- head(storm_data, n=10000)

storm_data_small$region <- "NULL" 

##make region table to include the numbers of the states with a merge on state 

state_numbers <- data.frame(storm_data[, c("STATE", "STATE__")])

library(dplyr)
state_numbers <- distinct(state_numbers, STATE, .keep_all = TRUE)

regions <- regions %>% 
  rename(
    STATE = state
  )

regions <- merge(regions, state_numbers)

bad_state_index = match("ST", storm_data$STATE)
storm_data$STATE__[bad_state_index] <- NA