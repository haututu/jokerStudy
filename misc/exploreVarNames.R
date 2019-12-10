library(tidyverse)

dat <- readxl::read_xlsx("data/Media+and+Mental+Illness_+Questionnaire+One_November+29,+2019_11.30.xlsx")

data.frame(
  heading = colnames(dat),
  question = as.character(dat[1,])
  ) %>%
  mutate(question = sub("^[^-]*", "", question)) %>%
  View()
