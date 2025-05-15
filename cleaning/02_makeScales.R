source("cleaning/01_load.R")


library(lavaan)

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
  ungroup() %>%

# Reverse coding
  mutate(prejudice_na = rowSums(select(., contains("prejudice_")), na.rm=TRUE),
         empathy_na = rowSums(select(., contains("empathy_")), na.rm=TRUE),
         vsas_na = rowSums(select(., contains("vsas_")), na.rm=TRUE)
  ) %>%
  mutate_at(vars(num_range("prejudice_", c(1:4, 9:12, 17:19, 23:25))), funs(recodeInt(., 7))) %>%
  mutate_at(vars(num_range("empathy_", c(2, 4, 5))), funs(recodeInt(., 4))) %>%
  mutate_at(vars(num_range("vsas_", c(2, 3, 6))), funs(recodeInt(., 7)))

####### Establish base dataset then add latent vars

dat_clean <- dat %>%
  select(number,
         time,
         gender,
         age,
         movie,
         prejudice_na,
         empathy_na,
         vsas_na,
         q19_1)

####### Prejudice

fearAvoid <- paste("fearAvoid =~", paste0("prejudice_", 1:8, collapse=" + "))
malevolence <- paste("malevolence =~", paste0("prejudice_", 9:16, collapse=" + "))
authority <- paste("authority =~", paste0("prejudice_", 17:22, collapse=" + "))
unpredict <- paste("unpredict =~", paste0("prejudice_", 23:28, collapse=" + "))

cfa.prejudice <- cfa(
  paste(fearAvoid,
        malevolence,
        authority,
        unpredict,
        "prejudice =~ fearAvoid + malevolence + authority + unpredict",
        sep=" \n "
  ), 
  cluster="number",
  missing = "ML",
  data=dat %>% select(-prejudice))

dat_prej <- dat_clean %>%
  cbind(predict(cfa.prejudice))

####### Empathy

cfa.empathy <- cfa(
  paste("empathy =~", paste0("empathy_", 1:7, collapse=" + ")), 
  cluster="number",
  missing = "ML",
  data=dat)

dat_emp <- dat_clean %>%
  select(number, time) %>%
  cbind(predict(cfa.empathy))

####### Authoritarianism

cfa.vsas <- cfa(
  paste("vsas =~", paste0("vsas_", 1:6, collapse=" + ")), 
  cluster="number",
  missing = "ML",
  data=dat)

dat_vsas <- dat_clean %>%
  select(number, time) %>%
  cbind(predict(cfa.vsas))

####### Add latent measures to clean data

# Data for single timepoint analysis
dat_clean <- dat_prej %>%
  left_join(dat_emp, by=c("number", "time")) %>%
  left_join(dat_vsas, by=c("number", "time")) %>%
  mutate(
    prejudice = as.vector(scale(prejudice)),
    fearAvoid = as.vector(scale(fearAvoid)),
    malevolence = as.vector(scale(malevolence)),
    authority = as.vector(scale(authority)),
    unpredict = as.vector(scale(unpredict)),
    empathy = as.vector(scale(empathy)),
    vsas = as.vector(scale(vsas)),
    age = as.vector(scale(age))
  ) %>%
  group_by(number) %>%
  mutate(prejudice_lag = lag(prejudice),
         fearAvoid_lag = lag(fearAvoid),
         malevolence_lag = lag(malevolence),
         authority_lag = lag(authority),
         unpredict_lag = lag(unpredict),
         ) %>%
  ungroup() %>%
  filter(time == 2)

# Data for multi-timepoint analysis
dat_clean_multipoint <- dat_prej %>%
  left_join(dat_emp, by=c("number", "time")) %>%
  left_join(dat_vsas, by=c("number", "time")) %>%
  mutate(
    prejudice = as.vector(scale(prejudice)),
    fearAvoid = as.vector(scale(fearAvoid)),
    malevolence = as.vector(scale(malevolence)),
    authority = as.vector(scale(authority)),
    unpredict = as.vector(scale(unpredict)),
    empathy = as.vector(scale(empathy)),
    vsas = as.vector(scale(vsas)),
    age = as.vector(scale(age))
  ) %>%
  group_by(number) %>%
  mutate(prejudice_lag = lag(prejudice),
         fearAvoid_lag = lag(fearAvoid),
         malevolence_lag = lag(malevolence),
         authority_lag = lag(authority),
         unpredict_lag = lag(unpredict)
  ) %>%
  ungroup() %>%
  filter(time > 1) %>%
  group_by(number) %>%
  mutate(empathy = max(empathy, na.rm=TRUE),
         vsas = max(vsas, na.rm=TRUE)
         ) %>%
  ungroup() %>%
  mutate(time = as.ordered(time))


dat_stat <- dat_prej %>%
  select(number, time, gender, age, movie, prejudice) %>%
  filter(time <= 2) %>%
  mutate(age = plyr::round_any(age, 5, ceiling))

dat_stat %>%
  ggplot(aes(x=time, y=prejudice, color=movie)) +
  geom_smooth()

write_csv(dat_stat, "data/joker_data.csv")
