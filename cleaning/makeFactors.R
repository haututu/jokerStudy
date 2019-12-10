## A list of functions to take in a dataset and give back an aggregated factor
library(lavaan)

dat <- readxl::read_xlsx("data/Media+and+Mental+Illness_+Questionnaire+One_November+29,+2019_11.30.xlsx")
colnames(dat) <- gsub(" ", "", colnames(dat))

######## Prejudice

fearAvoid <- paste("fearAvoid =~", paste0("Prejudice1_", 1:8, collapse=" + "))
malevolence <- paste("malevolence =~", paste0("Prejudice1_", 9:14, collapse=" + "), "+ Prejudice2_1", "+Prejudice2_2")
authority <- paste("authority =~", paste0("Prejudice2_", 3:8, collapse=" + "))
unpredict <- paste("unpredict =~", paste0("Prejudice2_", 9:14, collapse=" + "))

prejudice <- cfa(
  paste(fearAvoid,
        malevolence,
        authority,
        unpredict,
        sep=" \n "
        ), 
  data=dat)

summary(prejudice)

semPlot::semPaths(prejudice, "est", residuals=FALSE, rotation=4, structural=TRUE, edge.label.cex = 1.2, curvature = 5)
