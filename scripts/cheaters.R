te=c(27, 20, 7, 4, 1,1)
ye=c("European", "Indian", "Chinese", "West Asian", "Korean", "Japanese" )
df=data.frame(cbind(ye,te))
colnames(df)=c("origin", "count")
df$count=as.numeric(as.character(df$count))
df$percentage= round(df$count/sum(te)*100,1)
df$pop=c(197.3, 3.18, 3.79, 10.5, 1.7, 1.3 )
totalpop=sum(df$pop[c(2,3,5,6)])
df$biomed.pop=c(.58,.34*1.8/totalpop,.34*5.2/totalpop,  .025, .34*1.67/totalpop, .34*1.3/totalpop)*69000
df$cheaters.ppm=df$count/df$pop
df$cheaters.p1000=df$count/df$biomed.pop*1000

#tabulate
knitr::kable(df[,c(1,2,3,5,7)],format = "pandoc")
xtable::xtable(df[,c(1,2,3,5,7)])

#probability of Indian fraud
pbinom(q=20,size = 4236, prob = 60/69000,lower.tail = F)

#how many times more likely are Indians to commit fraud
4.7219854/((60-20)/(69000-4236)*1000)
