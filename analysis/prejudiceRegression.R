source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

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

brm.fit <- brm(
  mvbind(fearAvoid, malevolence, authority, unpredict) ~ fearAvoid + malevolence + authority + unpredict + age + gender + movie,
  family = gaussian(),
  cores = 4,
  chains = 4,
  data = testDat
)

dat %>%
  filter(time < 3) %>%
ggplot(aes(x=time, y=prejudice, group=number, color=movie)) +
  geom_line()

