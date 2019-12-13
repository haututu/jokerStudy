library(knitr)
library(kableExtra)

####### Descriptives
dat %>%
  filter(time == 2)

dat_prej %>%
  left_join(dat_emp, by=c("number", "time")) %>%
  left_join(dat_vsas, by=c("number", "time")) %>%
  filter(time == 2) %>%
  group_by(movie) %>%
  summarise(n = n(),
            females = sum(gender==2),
            age = mean(age)
            )

####### Basic analysis

# Figure 1
marginal_effects(brm.fit, "vsas:movie")$`vsas:movie` %>%
  mutate(effect2__ = ifelse(effect2__ == 2, "Joker", "Terminator: Dark Fate")) %>%
  ggplot(aes(x=effect1__, y=estimate__, ymax=upper__, ymin=lower__, group=effect2__, color=effect2__, linetype=effect2__)) +
  geom_line(size = 1) +
  geom_ribbon(aes(fill=effect2__, color=NULL), alpha=0.4) +
  labs(
    y = "Prejudice",
    x = "Authoritarianism",
    fill = "Movie",
    color = "Movie",
    linetype= "Movie"
  ) +
  theme(text = element_text(size=11))

# Table 1
summary(brm.fit)$fixed %>%
  data.frame() %>%
  mutate(Effect = rownames(.)) %>%
  select(Effect, everything(), -Eff.Sample, -Rhat) %>%
  cbind(
    pp = sjstats::rope(brm.fit, c(-1000, 0))[,5]
  ) %>%
  mutate(pp = ifelse(pp < 0.5, 1-pp, pp)
         )

####### Multivariate analysis

# Multivariate pps'
posterior_samples(brm.mv) %>% 
  gather(parameter, sample) %>% 
  group_by(parameter) %>% 
  summarise(pp = mean(sample > 0),
            pp = round(ifelse(pp < 0.5, 1-pp, pp), 2)
            ) %>%
  ungroup() %>%
  mutate(outcome = sapply(str_split(parameter, "_",), `[`, 2),
         effect = sapply(str_split(parameter, "_",), `[`, 3),
         pp = ifelse(pp >= 0.95,
                     cell_spec(pp, bold=TRUE),
                     cell_spec(pp, bold=FALSE))
         ) %>%
  select(outcome, effect, pp) %>%
  kable(align = "c", escape=F) %>%
  kable_styling(full_width = FALSE, bootstrap_options = "condensed") %>%
  collapse_rows(columns = 1, valign = "top")
