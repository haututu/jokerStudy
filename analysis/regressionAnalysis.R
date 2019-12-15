source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

####### Prejudice overall
brm.fit <- brm(
  prejudice ~ prejudice_lag + age + gender + q19_1 + movie,
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
  bf(fearAvoid ~ fearAvoid_lag + age + gender + q19_1 + movie) +
    bf(malevolence ~ malevolence_lag + age + gender + q19_1 + movie) +
    bf(authority ~ authority_lag + age + gender + q19_1 + movie) +
    bf(unpredict ~ unpredict_lag + age + gender + q19_1 + movie),
  family = gaussian(),
  prior = set_prior("normal(0, 1)", class="b"),
  cores = 4,
  chains = 4,
  data = dat_clean
)

####### Prejudice overall with 48hr followup
brm.multipoint <- brm(
  prejudice ~ age + gender + q19_1 + movie * time + (1|number),
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class="b")
  ),
  cores = 4,
  chains = 4,
  data = dat_clean_multipoint
)

brm.multipoint <- brm(
  prejudice ~ prejudiceLag + age + gender + q19_1 + movie * mo(time) + (1|number),
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class="b")
  ),
  cores = 4,
  chains = 4,
  data = dat_clean_multipoint
)
