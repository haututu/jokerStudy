library(tidyverse)
library(lme4)

joker_data <- read_csv("data/joker_data.csv")

head(joker_data)

## Regression
lm_fit <- lm(prejudice ~ age + gender + movie, data=)

## Multilevel regression