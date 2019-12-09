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

dat %>%
  select(Gender,
         DOB,
         `Cell Number`) %>%
  filter(row_number() != 1) %>%
  mutate(DOB_date = as.Date(DOB, "%d/%m/%Y"),
         Gender = as.factor(Gender),
         Age = as.numeric(difftime(as.Date("2019-11-29"), DOB_date)) / 365
  ) %>%
  group_by(`Cell Number`) %>%
  summarise(n = n()) %>%
  filter(n > 1)

dat %>%
  select(Gender,
         DOB,
         `Cell Number`,
         `Duration (in seconds)`) %>%
  filter(row_number() != 1) %>%
  mutate(DOB_date = as.Date(DOB, "%d/%m/%Y"),
         Gender = as.factor(Gender),
         Age = as.numeric(difftime(as.Date("2019-11-29"), DOB_date)) / 365,
         Duration = as.numeric(`Duration (in seconds)`) / 60
  ) %>%
  group_by(`Cell Number`) %>%
  summarise(n = n()) %>%
  filter(n > 1)

dat %>%
  select(Gender,
         DOB,
         `Cell Number`,
         `Duration (in seconds)`) %>%
  filter(row_number() != 1) %>%
  mutate(DOB_date = as.Date(DOB, "%d/%m/%Y"),
         Gender = as.factor(Gender),
         Age = as.numeric(difftime(as.Date("2019-11-29"), DOB_date)) / 365,
         Duration = as.numeric(`Duration (in seconds)`) / 60
  ) %>%
  #filter(Duration < 8 & !(is.na(`Cell Number`))) %>%
  filter(Duration < 60 & !(is.na(`Cell Number`))) %>%
  ggplot(aes(x=Duration)) +
  geom_density() +
  theme_classic()
