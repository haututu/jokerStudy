source("cleaning/01_load.R")

####### Recode questions

recodeInt <- function(x, upper) {
  upper - x
}

dat <- dat %>%
  mutate(prejudice_na = rowSums(select(., contains("prejudice_"))),
         empathy_na = rowSums(select(., contains("empathy_"))),
         vsas_na = rowSums(select(., contains("vsas_")))
  ) %>%
  mutate_at(vars(num_range("prejudice_", c(1:4, 9:12, 17:19, 23:25))), funs(recodeInt(., 7))) %>%
  mutate_at(vars(num_range("empathy_", c(2, 4, 5))), funs(recodeInt(., 4))) %>%
  mutate_at(vars(num_range("vsas_", c(1, 4, 5))), funs(recodeInt(., 7)))

####### Establish base dataset then add latent vars

dat_clean <- dat %>%
  select(number,
         time,
         gender,
         age,
         movie,
         prejudice_na,
         empathy_na,
         vsas_na)

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
  data=dat)

####### Empathy

cfa.empathy <- cfa(
  paste("empathy =~", paste0("prejudice_", 1:7, collapse=" + ")), 
  cluster="number",
  data=dat)

####### Authoritarianism

cfa.vsas <- cfa(
  paste("vsas =~", paste0("vsas_", 1:6, collapse=" + ")), 
  cluster="number",
  data=dat)

####### Add latent measures to clean data

dat_clean