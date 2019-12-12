library(tidyverse)

dat <- readxl::read_excel("data/Taylor-Joker-Study-T123-ds2-cleaned.xlsx") %>%
  rename_all(tolower) %>% # Make lower case
  filter(!(is.na(number))) %>% # Remove NA cellnumbers
  group_by(number) %>%
  mutate(movie = max(movie, na.rm=TRUE), # Add movie condition to T1
         time = ifelse(time == 3, time + day - 1, time) # Correct T3 variable if they responded a day late or more
         ) %>% 
  ungroup()