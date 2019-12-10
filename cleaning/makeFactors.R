## A list of functions to take in a dataset and give back an aggregated factor
library(lavaan)

dat <- haven::read_sav("data/JokerJustTimeCCprejudicescale1ds5.sav")

dat_old <- readxl::read_xlsx("data/Media+and+Mental+Illness_+Questionnaire+One_November+29,+2019_11.30.xlsx") %>%
  

######## Prejudice

fearAvoid <- paste("fearAvoid =~", paste0("Prejudice_", 1:8, collapse=" + "))
malevolence <- paste("malevolence =~", paste0("Prejudice_", 9:16, collapse=" + "))
authority <- paste("authority =~", paste0("Prejudice_", 17:22, collapse=" + "))
unpredict <- paste("unpredict =~", paste0("Prejudice_", 23:28, collapse=" + "))

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
