source("cleaning/01_load.R")

####### Recode questions

recodeInt <- function(x, upper) {
  upper - x
}

dat <- dat %>%
  mutate_at(vars(num_range("prejudice_", c(5:8, 13:16, 20:22, 26:28))), funs(recodeInt(., 7)))

####### Prejudice

dat <- dat %>%
  mutate(prejudice = rowSums(select(., contains("prejudice_")))
         ) %>%
  group_by(number) %>%
  mutate(prejudiceLag = lag(prejudice)) %>%
  ungroup()

# Reverse coding