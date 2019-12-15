library(knitr)
library(kableExtra)

####### Descriptives
dat %>%
  filter(time == 2)

dat_prej %>%
  left_join(dat_emp, by=c("number", "time")) %>%
  left_join(dat_vsas, by=c("number", "time")) %>%
  filter(time == 2) %>%
  mutate(ethnicity = paste(ethnicity1, ethnicity2, ethnicity3, sep=",")) %>%
  group_by(movie) %>%
  summarise(n = n(),
            females = sum(gender==2),
            age_mean = mean(age),
            age_std = sd(age),
            eth_euro = mean(grepl("1", ethnicity)),
            eth_asian = mean(grepl("5", ethnicity)),
            eth_maoriPacific = mean(grepl("2|3", ethnicity)),
            eth_other = mean(!(grepl("1|2|3|5", ethnicity)))
            )

####### Basic analysis

# Figure 1
marginal_effects(brm.multipoint, effects = "time:movie")$`time:movie` %>%
  mutate(effect2__ = ifelse(effect2__ == 2, "Joker", "Terminator: Dark Fate")) %>%
  ggplot(aes(x=effect1__, y=estimate__, ymax=upper__, ymin=lower__, group=effect2__, color=effect2__, linetype=effect2__)) +
  geom_point(size = 1, position = position_dodge(width=0.2)) +
  geom_line(size = 1, position = position_dodge(width=0.2)) +
  geom_errorbar(width = 0.2, position = position_dodge(width=0.2)) +
  labs(
    y = "Prejudice",
    x = "Time",
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

# Figure 1
marginal_effects(brm.mv, effects = "movie")$`movie`

test <- marginal_effects(brm.mv)

bind_rows(test$fearAvoid.fearAvoid_movie %>% mutate(resp = "Fear Avoidance"),
          test$malevolence.malevolence_movie %>% mutate(resp = "Malevolence"),
          test$authority.authority_movie %>% mutate(resp = "Authoritarianism"),
          test$unpredict.unpredict_movie %>% mutate(resp = "Unpredictability")
          ) %>%
  mutate(effect1__ = ifelse(effect1__ == 2, "Joker", "Terminator: Dark Fate")) %>%
  ggplot(aes(x=resp, y=estimate__, ymin=lower__, ymax=upper__, group=effect1__, color=effect1__, shape=effect1__)) +
  geom_point(position = position_dodge(width=0.2), size=3) +
  geom_errorbar(position = position_dodge(width=0.2), width=0.5) +
  labs(
    y = "Prejudice",
    x = "Time",
    shape = "Movie",
    color = "Movie"
  ) +
  theme(text = element_text(size=11))
  
