library(tidyverse)

dat <- readxl::read_xlsx("data/Taylor-Joker-Study.xlsx") %>%
  rename_all(tolower) %>% # Make lower case
  filter(!(is.na(number))) %>% # Remove NA cellnumbers
  group_by(number) %>%
  filter(n() == 2) %>% # Remove anyone who didnt complete both surveys
  mutate(movie = max(movie, na.rm=TRUE)) # Add movie condition to T1