library(tidyverse)

# Load
dat <- readxl::read_xlsx("data/Media-and-Mental-Ilness-Joker-Terminator-FINAL-ds2.xlsx", "FINAL") %>%
  rename_all(tolower) %>%
  filter(!(is.na(number))) # filter out anyone who didnt give a cellnumber

# Who submitted more than two surveys
dat %>%
  group_by(number) %>%
  filter(n() > 2)