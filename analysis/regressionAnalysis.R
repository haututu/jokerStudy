source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

####### Prejudice overall
brm.fit <- brm(
  prejudice ~ prejudice_lag + age + gender + q19_1 + movie,
  family = gaussian(),
  prior = 
    set_prior("normal(0, 1)", class="b"),
  cores = 4,
  chains = 4,
  data = filter(dat_clean, time == 2)
)

brm.fit <- brm(
  prejudice ~ age + gender + q19_1 + movie*time + (1|number),
  family = gaussian(),
  prior = set_prior("normal(0, 1)", class="b"),
  cores = 4,
  chains = 4,
  data = dat_clean
)
