source("cleaning/01_load.R")

####### Recode questions

recodeInt <- function(x, upper) {
  upper - x
}

dat <- dat %>%
  mutate(prejudice_na = rowSums(select(., contains("prejudice_")))
  ) %>%
  mutate_at(vars(num_range("prejudice_", c(1:4, 9:12, 17:19, 23:25))), funs(recodeInt(., 7)))

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

testDat <- cbind(
  filter(dat, !is.na(prejudice_na)),
  data.frame(predict(cfa.prejudice))
) %>%
  group_by(number) %>%
  mutate(prejudiceLag = lag(prejudice)) %>%
  ungroup() %>%
  filter(time == 2)

####### Empathy

####### Authoritarianism