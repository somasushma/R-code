library("readr")
library("dplyr")

GDP = read_delim("~/cutting_block/R/data_analysis/GDP.csv", 
                        ";", escape_double = FALSE, col_types = cols(`Value Footnotes` = col_skip()), 
                        trim_ws = TRUE)
GDP=head(GDP, -2)
colnames(GDP)=c("code","country", "year", "GDP")

Gini = read_delim("~/cutting_block/R/data_analysis/Gini.csv", 
                 ";", escape_double = FALSE, col_types = cols(`Value Footnotes` = col_skip()), 
                 trim_ws = TRUE)
Gini=head(Gini, -11)
colnames(Gini)=c("code","country", "year", "Gini")

wk=GDP %>% distinct(code,country)
te=GDP %>% group_by(code) %>% summarize(mean(GDP))
wk=left_join(wk,te, by="code")
te=Gini %>% group_by(code) %>% summarize(mean(Gini))
wk=left_join(wk,te, by="code")
colnames(wk)=c("code", 'country', 'GDP', 'Gini')

hist(wk$Gini, breaks = 20, col="skyblue3")
box()
abline(v=median(wk$Gini, na.rm = T), col="orange", lwd=2)
abline(v=mean(wk$Gini, na.rm = T), col="darkgreen", lwd=2)
boxplot(wk$Gini, horizontal = T)

