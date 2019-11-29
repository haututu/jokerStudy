library(tidyverse)

dat <- readxl::read_xlsx("Media+and+Mental+Illness_+Questionnaire+One_November+29,+2019_11.30.xlsx")

dat %>%
  select(Gender,
         DOB) %>%
  filter(row_number() != 1) %>%
  mutate(DOB_date = as.Date(DOB, "%d/%m/%Y"),
         Gender = as.factor(Gender),
         Age = as.numeric(difftime(as.Date("2019-11-29"), DOB_date)) / 365
  ) %>%
  filter(Age < 100) %>%
  ggplot(aes(x=Age)) +
  geom_density() +
  theme_classic()

dat %>%
  select(Gender,
         DOB) %>%
  filter(row_number() != 1) %>%
  mutate(DOB_date = as.Date(DOB, "%d/%m/%Y"),
         Gender = as.factor(Gender),
         Age = as.numeric(difftime(as.Date("2019-11-29"), DOB_date)) / 365
  ) %>%
  filter(Age < 100 & Gender %in% c(1, 2)) %>%
  ggplot(aes(x=Age, group=Gender, fill=Gender)) +
  geom_histogram(position="dodge") +
  theme_classic()
