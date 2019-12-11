## A list of functions to take in a dataset and give back an aggregated factor
source("cleaning/01_load.R")
library(lavaan)

######## Prejudice

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
  data=dat)

summary(cfa.prejudice)

semPlot::semPaths(cfa.prejudice, "std", residuals=FALSE, rotation=4, structural=TRUE, edge.label.cex = 1.2, curvature = 5)
