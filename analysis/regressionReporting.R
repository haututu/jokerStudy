library(knitr)
library(kableExtra)

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
