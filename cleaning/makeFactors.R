## A list of functions to take in a dataset and give back an aggregated factor
library(tidyverse)
library(lavaan)

readxl::read_xlsx("data/Media-and-Mental-Ilness-Joker-Terminator-FINAL-ds2.xlsx", "FINAL") %>%
  rename_all(tolower) %>%
  filter(!(is.na(number))) %>%
  group_by(number) %>%
  filter(n() > 2)

######## Prejudice

fearAvoid <- paste("fearAvoid =~", paste0("prejudice_", 1:8, collapse=" + "))
malevolence <- paste("malevolence =~", paste0("prejudice_", 9:16, collapse=" + "))
authority <- paste("authority =~", paste0("prejudice_", 17:22, collapse=" + "))
unpredict <- paste("unpredict =~", paste0("prejudice_", 23:28, collapse=" + "))

prejudice <- cfa(
  paste(fearAvoid,
        malevolence,
        authority,
        unpredict,
        sep=" \n "
        ), 
  data=dat)

summary(prejudice)

semPlot::semPaths(prejudice, "std", residuals=FALSE, rotation=4, structural=TRUE, edge.label.cex = 1.2, curvature = 5)

