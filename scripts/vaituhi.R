library(readr)
library(knitr)

vaituhi <- read_delim("~/cutting_block/R/data_analysis/data/vaituhi.tsv",
"\t", escape_double = FALSE, trim_ws = TRUE)
vaituhi$dens.vai=vaituhi$`Number of 'vai'`/vaituhi$`Total number of words`

windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))


nrow(vaituhi[which(vaituhi$`Number of 'vai'`==0),])/nrow(vaituhi)
te=vaituhi[which(vaituhi$`Number of 'vai'`!=0),]
boxplot(te$dens.vai, horizontal = T, col = "lightblue", pch=16, family="f2", main="density of particle vai in text")
