source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

####### Prejudice overall
brm.fit <- brm(
  prejudice ~ prejudice_lag + age + gender + q19_1 + movie * empathy + movie * vsas,
  family = gaussian(),
  prior = c(
    set_prior("normal(0, 2)", class="b")
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
<<<<<<< HEAD
  data = dat_clean
)
=======
  data = testDat
)

dat %>%
  filter(time < 3) %>%
ggplot(aes(x=time, y=prejudice, group=number, color=movie)) +
  geom_line()

>>>>>>> 36b7e258dde76ca7e6f20327a03da4fef0e0bc78
