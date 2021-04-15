####### DECISION PROCESS ANALYSIS #########
### Jennifer L Beaudry, Jamal K Mansour, Roy Groncki, & Mai-Tram Nguyen

##### LOAD PACKAGES #####
library(here)
library(tidyverse)

##### LOAD DATA #####
read.csv(here::here("experiment", "data", "decision_process_data.csv")) %>%
  mutate(id = 1:n()) %>% # add a unique id number
  relocate(id, .before = ParticipantID) %>% # move it to the first column
  select(-ParticipantID) # delete the random number id



