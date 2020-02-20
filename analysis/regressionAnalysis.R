source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

####### Prejudice overall
brm.fit <- brm(
  prejudice ~ prejudice_lag + age + gender + q19_1 + movie * empathy + movie * vsas,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 1)", class="b"),
    set_prior("normal(0.15, 0.2)", coef="movie2")
  ),
  cores = 4,
  chains = 4,
  data = dat_clean
)

####### Multivariate model of four prejudice factors
brm.mv <- brm(
  bf(fearAvoid ~ fearAvoid_lag + age + gender + q19_1 + movie * empathy + movie * vsas) +
    bf(malevolence ~ malevolence_lag + age + gender + q19_1 + movie * empathy + movie * vsas) +
    bf(authority ~ authority_lag + age + gender + q19_1 + movie * empathy + movie * vsas) +
    bf(unpredict ~ unpredict_lag + age + gender + q19_1 + movie * empathy + movie * vsas),
  family = gaussian(),
  cores = 4,
  chains = 4,
  data = dat_clean
)

####### Prejudice overall with 48hr followup
brm.multipoint <- brm(
  prejudice ~ prejudice_lag + age + gender + q19_1 + movie * time + movie * empathy + movie * vsas + (1|number),
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class="b")
  ),
  cores = 4,
  chains = 4,
  data = dat_clean_multipoint
)