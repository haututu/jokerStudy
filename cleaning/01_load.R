library(tidyverse)

dat <- readxl::read_excel("data/Taylor-Joker-Study-T123-ds2-cleaned.xlsx") %>%
  rename_all(tolower) %>% # Make lower case
  filter(!(is.na(number))) %>% # Remove NA cellnumbers
  mutate(number = number / 18) %>%
  group_by(number) %>%
  mutate(movie = max(movie, na.rm=TRUE), # Add movie condition to T1
         time = ifelse(time == 3, time + day - 1, time) # Correct T3 variable if they responded a day late or more
         ) %>% 
  #ungroup() %>%
  filter(n() > 1) %>%
  mutate(movie = max(movie, na.rm=TRUE),
         age = max(age, na.rm=TRUE),
         gender = max(gender, na.rm=TRUE), # Add movie condition to T1
         q19_1 = max(q19_1, na.rm=TRUE),
         time = case_when(
           (time == 3 & day == 1) ~ 3,
           (time == 3 & day != 1) ~ 4,
           TRUE ~ time) # Correct T3 variable if they responded a day late or more
         ) %>%
  ungroup() %>% 
  filter(gender %in% c(1, 2)) %>%
  mutate(movie = ifelse(movie == 1, 2, ifelse(movie == 2, 1, NA)), # Flipping the movie coding so Terminator is the baseline/reference in regression.
    movie = as.factor(movie),
         gender = as.factor(gender),
         number = as.factor(number),
         q19_1 = ifelse(q19_1 == 1, 1, 0),
         q19_1 = as.factor(q19_1)
         )
