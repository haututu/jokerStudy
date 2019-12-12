source("cleaning/02_makeScales.R")
theme_set(theme_classic())

library(brms)

brm.fit <- brm(
  prejudice ~ prejudiceLag + age + gender + movie,
  family = gaussian(),
  cores = 4,
  chains = 4,
  data = testDat
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
