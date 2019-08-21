library(readr)
scirank <- read_csv("~/cutting_block/R/data_analysis/data/Ioannidis_citation_2017.csv", 
                                    col_types = cols(`self%` = col_number()), 
                                    comment = "#")
scirank=data.frame(scirank)

sci5tot=6880389
tops=length(scirank$authfull)/6880389
worldpop=7.53*10^9

1/(sci5tot/(7.53*10^9)) #1 in how many is a scientist 

#Ioannidis rank
hist(scirank$c, breaks = 1000)
abline(v=scirank[960,"c"], col="red")
abline(v=scirank[2239,"c"], col="green")
curve(500*(100000*e^-(.5+3.11*x)), from = 3.2, to = 6, add = T, col="red")
curve(500*(100000*e^-(4.8+1.2*x^1.37)), from = 3.2, to = 6, col="blue", add=T)

# #of papers
hist(scirank$np6017, breaks = 1000, xlim = c(5,1000), col="gray")
abline(v=scirank[960,"np6017"], col="red")

# h-index
layout(mat = matrix(c(1,2),nrow=2, ncol=1), heights = c(3,1))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(scirank$h17, breaks = 200, xlim=range(scirank$h17), col="gray", main=bquote("h-index of top" ~.(round(tops*100,2))~"% researchers"), xlab = "h-index")
abline(v=c(median(scirank$h17), mean(scirank$h17)), col=c("red","green"), lwd=2)
abline(v=scirank[960,"h17"], col="blue")
box()
boxplot(scirank$h17, horizontal = T, ylim=range(scirank$h17), pch=16, col="lightblue", main="")

#h-index biomedical/biology
bio= scirank[grep("Bio", scirank$name22),]
layout(mat = matrix(c(1,2),nrow=2, ncol=1), heights = c(3,1))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(bio$h17, breaks = 200, xlim=range(bio$h17), col="gray", main=bquote("h-index of biomedical researchers in top" ~.(round(tops*100,2))~"% researchers"), xlab = "h-index")
abline(v=c(median(bio$h17), mean(bio$h17)), col=c("red","green"), lwd=2)
box()
boxplot(bio$h17, horizontal = T, ylim=range(bio$h17), pch=16, col="lightblue", main="")

#h-index astro
phys= scirank[grep("Math|Astro", scirank$name22),]
layout(mat = matrix(c(1,2),nrow=2, ncol=1), heights = c(3,1))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(phys$h17, breaks = 200, xlim=range(phys$h17), col="gray", main=bquote("h-index of mathematicians, physicists, astronomers in top" ~.(round(tops*100,2))~"% researchers"), xlab = "h-index")
abline(v=c(median(phys$h17), mean(phys$h17)), col=c("red","green"), lwd=2)
box()
boxplot(phys$h17, horizontal = T, ylim=range(phys$h17), pch=16, col="lightblue", main="")

#h-index Eng/comp science
engi= scirank[grep("Engineer|Infor", scirank$name22),]
layout(mat = matrix(c(1,2),nrow=2, ncol=1), heights = c(3,1))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(engi$h17, breaks = 100, xlim=range(engi$h17), col="gray", main=bquote("h-index of Engineers/computer scientists in top" ~.(round(tops*100,2))~"% researchers"), xlab = "h-index")
abline(v=c(median(engi$h17), mean(engi$h17)), col=c("red","green"), lwd=2)
box()
boxplot(engi$h17, horizontal = T, ylim=range(engi$h17), pch=16, col="lightblue", main="")

#h-index Physicians
medic= scirank[grep("Engineer|Infor", scirank$name22),]
layout(mat = matrix(c(1,2),nrow=2, ncol=1), heights = c(3,1))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(medic$h17, breaks = 100, xlim=range(medic$h17), col="gray", main=bquote("h-index of physicians in top" ~.(round(tops*100,2))~"% researchers"), xlab = "h-index")
abline(v=c(median(medic$h17), mean(medic$h17)), col=c("red","green"), lwd=2)
box()
boxplot(medic$h17, horizontal = T, ylim=range(medic$h17), pch=16, col="lightblue", main="")

#h versus c
plot(scirank$h17, scirank$c, pch=16, cex=.2,  main = bquote("h-index vs c for top"~.(round(tops*100,2))~"% of researchers"))

#h versus # of citations
plot(scirank$h17,scirank$nc9617, pch=16, cex=.2,  main = bquote("h-index vs # citations for top"~.(round(tops*100,2))~"% of researchers"))

# #publications versus # of citations
plot(x=scirank$np6017, y=scirank$nc9617, pch=16, log="xy", cex=.2, main = bquote("# citations vs # papers for top"~.(round(tops*100,2))~"% of researchers"))