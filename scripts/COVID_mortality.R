setwd("~/cutting_block/R/data_analysis/data/")
library("dplyr")
library("knitr")
library("tidyr")

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

#mortality
covid=read.csv(file = "covid", header = F)
covid=data.frame(covid)
colnames(covid)=c("country", "infections", "deaths", "ipm")
covid=covid[order(covid$infections, decreasing = T),]

covid.d=covid[!is.na(covid$deaths),]
covid.d=covid.d[which(covid.d$deaths > 1),]#get countries with > 2 deaths

covid.d$mortality=round(covid.d$deaths/covid.d$infections*100, 2) 
covid.d=covid.d[order(covid.d$infections, decreasing = T),]

#mortality scatterplot
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
plot(x=covid.d$infections, y=covid.d$deaths, pch=16, col=c("brown",  "darkblue"), main=bquote("COVID-19 mortality:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "infections", ylab = "mortality", log = "xy", family= "f1")
text(x=covid.d$infections, y=covid.d$deaths, labels = substr(covid.d$country, 1,4), col=c("brown", "darkblue"),  pos = rep(c(1,3,4),100)[1:nrow(covid)], offset = .3, family = "f3", cex = .75)
curve(1/100*x, add = T, col="green",lty=2)
curve(2/100*x, add = T, col="darkviolet",lty=2)
curve(3/100*x, add = T, col="blue",lty=2)
curve(4/100*x, add = T, col="red",lty=2)
curve(5/100*x, add = T, col="gray",lty=2)

tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/R/Dataexperiments/Figs/covid19.mort.",tst,".png"), width=9, height=7, res=300, units="in")
dev.off()


#infections per million and death
te=covid[which(covid$infections>100 & !is.na(covid$ipm)),]

#mortality boxplot
covid.d=covid.d[order(covid.d$mortality),]

par(mar=c(2,2,2,1), mfrow=c(2,1))
te=boxplot(covid.d$mortality, horizontal = T, pch=16, col = "lightblue", main=bquote("COVID-19 mortality:"~ .(format(Sys.time(), "%a %b %d %Y"))), family="f1")
labels = substr(as.character(covid.d$country[which(covid.d$mortality %in% sort(te$out))]), 1,4)
text(x=te$out, y=rep(1.03, length(te$out)), labels = labels, srt=90, adj = 0,  family="f3")
text( x=mean(range(covid.d$mortality)), y=1.5, labels = bquote("mean="~.(mean(covid.d$mortality))~"; median="~.(median(covid.d$mortality))), cex = 1.2, family="f1")


#inf/per million boxplot
par(mar=c(2,2,2,1))
covid=covid[order(covid$ipm),]
te=boxplot(covid$ipm, horizontal = T, log = "x", pch=16, col = "lightblue", main=bquote("COVID-19 infections per million:"~ .(format(Sys.time(), "%a %b %d %Y"))), family="f1")

labels=substr(as.character(covid$country[which(covid$ipm %in% sort(te$out))]), 1,4)
sapply(1:length(te$out), function(x) text(x=te$out[x], y=rep(c(1.03, .97), length(te$out))[x], labels = labels[x],  srt=rep(c(90,-90), length(te$out))[x], adj = 0, family="f3"))
text( x=median(covid$ipm,na.rm = T), y=1.5, labels = bquote("mean="~.(mean(covid$ipm, na.rm = T))~"; median="~.(median(covid$ipm, na.rm = T))), cex = 1.2, family="f1")

tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/R/Dataexperiments/Figs/covid19.box.",tst,".png"), width=8, height=6, res=300, units="in")
dev.off()

#mortality histogram
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(covid$mortality, col = "lightblue", main="COVID-19 mortality")
abline(v=c(mean(covid$mortality), median(covid$mortality)), col=c("brown","darkgreen"))
box()


#tabulation
knitr::kable(covid[order(covid$deaths, decreasing = T),], row.names = F)

sum(covid$deaths)/sum(covid$infections) * 100 #total mortality
mean(covid$mortality)

#US undertesting
te=cbind(covid, round(19*covid$infections/covid$deaths, 0))
colnames(te)[5]="US.pred"
te=te[te$US.pred>395,]
te=te[order(te$US.pred),]

#symptoms
covid=read.csv(file = "covid19.symptoms", header = F)
par(mar=c(2,10,2,2))
barplot(rev(covid$V2), horiz = T, names.arg = rev(covid$V1), col = "darkcyan", las=2, main= "Covid-19 symptoms; n=55924")
box()

dev.copy(png, file="~/R/Dataexperiments/Figs/covid19.symptoms.png", height=8, width=11, units="in", res=300)
dev.off()

#age
covid=read.csv(file = "covid.age", header = F)
par(mar=c(2,4,2,2))
barplot(rev(covid$V2), horiz = T, names.arg = rev(covid$V1), col = "darkcyan", las=2, main= "Covid-19: by age mortality")
box()
dev.copy(png, file="~/R/Dataexperiments/Figs/covid19.age.png", height=8, width=11, units="in", res=300)
dev.off()

#time series cases-----------
cov.c.ts=read.csv(file = "covid19_cases.csv", header = T)
cov.c.ts=data.frame(cov.c.ts)
cov.c.ts=cov.c.ts[-1,]
cov.c.ts[,1]  = as.character(cov.c.ts[,1])
cov.c.ts[,2]  = as.character(cov.c.ts[,2])
cov.c.ts[,3]  = as.numeric(as.character(cov.c.ts[,3]))
cov.c.ts[,4]  = as.numeric(as.character(cov.c.ts[,4]))
cov.c.ts[,5]  = as.Date(as.character(cov.c.ts[,5]))
cov.c.ts[,6]  = as.numeric(as.character(cov.c.ts[,6]))

#china
chin=cov.c.ts[which(cov.c.ts$Country.Region=="China"),]
chin=chin[-which(chin$Province.State=="Hong Kong"),]
chin=data.frame(group_by(chin,Date) %>% summarize(cases=sum(Value)))

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
plot(chin, pch=16,type="o", log="y", col="darkred", main=bquote("COVID-19 infections till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "infections", family= "f1")
points(chin)

cnt=c("US","Korea, South", "Italy","Spain", "France", "United Kingdom", "Germany")
col=rainbow(length(cnt))
for (j in 1:length(cnt)) {
  infts=cov.c.ts[which(cov.c.ts$Country.Region==cnt[j]),]
  infts=data.frame(group_by(infts,Date) %>% summarize(cases=sum(Value)))
  
  points(infts, pch=16,type="o", col=col[j])  
  points(infts)
  
}

legend(x = "topleft", legend = c("China", cnt), col = c("darkred", col[1:length(cnt)]), pch = 16, lty = 1)


tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/R/Dataexperiments/Figs/covid19.ts.",tst,".png"), width=8, height=5, res=300, units="in")
dev.off()

#derivative of above
y=sapply(1:(length(chin$cases)-1), function(x) chin$cases[x+1]-chin$cases[x])

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
plot(x=tail(chin$Date, -1),y=y, pch=16,type="o", col="darkred", main=bquote("COVID-19 infection rate till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "infections", family= "f1")
points(x=tail(chin$Date, -1),y=y)

cnt=c("US","Italy","Spain", "France", "United Kingdom", "Germany")
col=rainbow(length(cnt))
for (j in 1:length(cnt)) {
  infts=cov.c.ts[which(cov.c.ts$Country.Region==cnt[j]),]
  infts=data.frame(group_by(infts,Date) %>% summarize(cases=sum(Value)))
  y=sapply(1:(length(infts$cases)-1), function(x) infts$cases[x+1]-infts$cases[x])
  points(x=tail(infts$Date, -1),y=y, pch=16,type="o", col=col[j])  
  points(x=tail(infts$Date, -1),y=y)
}


#time series deaths-----------
cov.d.ts=read.csv(file = "covid19_deaths.csv", header = T)
cov.d.ts=data.frame(cov.d.ts)
cov.d.ts=cov.d.ts[-1,]
cov.d.ts[,1]  = as.character(cov.d.ts[,1])
cov.d.ts[,2]  = as.character(cov.d.ts[,2])
cov.d.ts[,3]  = as.numeric(as.character(cov.d.ts[,3]))
cov.d.ts[,4]  = as.numeric(as.character(cov.d.ts[,4]))
cov.d.ts[,5]  = as.Date(as.character(cov.d.ts[,5]))
cov.d.ts[,6]  = as.numeric(as.character(cov.d.ts[,6]))

#china
chin=cov.d.ts[which(cov.d.ts$Country.Region=="China"),]
chin=chin[-which(chin$Province.State=="Hong Kong"),]
chin=data.frame(group_by(chin,Date) %>% summarize(cases=sum(Value)))

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
plot(chin, pch=16,type="o", log="y", col="darkred", main=bquote("COVID-19 deaths till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "infections", family= "f1")
points(chin)

cnt=c("US","Korea, South", "Italy","Spain", "France", "United Kingdom", "Germany")
col=rainbow(length(cnt))
for (j in 1:length(cnt)) {
  infts=cov.d.ts[which(cov.d.ts$Country.Region==cnt[j]),]
  infts=data.frame(group_by(infts,Date) %>% summarize(cases=sum(Value)))
  
  points(infts, pch=16,type="o", col=col[j])  
  points(infts)
  
}

legend(x = "topleft", legend = c("China", cnt), col = c("darkred", col[1:length(cnt)]), pch = 16, lty = 1)


tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/R/Dataexperiments/Figs/covid19.ts_d.",tst,".png"), width=8, height=5, res=300, units="in")
dev.off()

#death rate: time series------
#china
te=cov.c.ts[which(cov.c.ts$Country.Region=="China"),]
te=te[-which(te$Province.State=="Hong Kong"),]
te=data.frame(group_by(te,Date) %>% summarize(cases=sum(Value)))

ye=cov.d.ts[which(cov.d.ts$Country.Region=="China"),]
ye=ye[-which(ye$Province.State=="Hong Kong"),]
ye=data.frame(group_by(ye,Date) %>% summarize(cases=sum(Value)))

chin=merge(x = te, y = ye,by = "Date")
chin$mortality=chin$cases.y/chin$cases.x*100
plot(chin[,1], chin[,4], pch=16,type="o", col="darkred", main=bquote("COVID-19 mortality till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "mortality (%)", family= "f1", ylim = c(0,12))
points(chin[,1], chin[,4])

#others
cnt=c("US","Korea, South", "Italy","Spain", "France", "United Kingdom", "Germany")
col=rainbow(length(cnt))
for (j in 1:length(cnt)) {
  
  te=cov.c.ts[which(cov.c.ts$Country.Region==cnt[j]),]
  te=data.frame(group_by(te,Date) %>% summarize(cases=sum(Value)))
  
  ye=cov.d.ts[which(cov.d.ts$Country.Region==cnt[j]),]
  ye=data.frame(group_by(ye,Date) %>% summarize(cases=sum(Value)))
  infde=merge(x = te, y = ye,by = "Date")
  infde$mortality=infde[,3]/infde[,2]*100
  infde[which(is.nan(infde[,4])),4]=NA
  
  points(infde$Date, infde$mortality, pch=16,type="o", col=col[j])  
  points(infde$Date, infde$mortality)
  
}

legend(x = "topleft", legend = c("China", cnt), col = c("darkred", col[1:length(cnt)]), pch = 16, lty = 1)

tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/R/Dataexperiments/Figs/covid19.ts_mort.",tst,".png"), width=8, height=6, res=300, units="in")
dev.off()

#matrix------------
cnts=unique(cov.c.ts$Country.Region)
te=sapply(1:ncol(mat.cov.c), function(x) length(which(mat.cov.c[,x] >0)))
names(te)=cnts
cnts=names(sort(te,decreasing = T))

mat.cov.c=sapply(cnts, function(x) (cov.c.ts[which(cov.c.ts$Country.Region==x),] %>% group_by(Date)  %>% summarize(cases=sum(Value)))$cases)
rownames(mat.cov.c)=format(rev(unique(cov.c.ts$Date)), "%a %b %d %Y")

# install.packages("plot.matrix")
library('plot.matrix')
par(mar=c(6,6,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1))
plot(asinh(mat.cov.c), key=NULL, col=topo.colors(50), las=2, main=bquote("infection counts in asinh scale till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab="", ylab="", family="f1", cex.axis=0.5, breaks=50)

tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/cutting_block/R/data_analysis/Figures/covid19.matrix.",tst,".png"), width=15, height=10, res=300, units="in")
dev.off()

#Italy analysis--------
italy.cov=read.csv(file = "italy_data.csv", header = T)
italy.cov=data.frame(italy.cov)
italy.cov=italy.cov[,-c(2, 12)]
colnames(italy.cov)[c(1,4, 9,10)]=c("date", "total.hospitalized", "dead", "infected")
italy.cov$date=as.Date(italy.cov$date)

#mortality
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(2,2))
plot(italy.cov$date, italy.cov$dead/italy.cov$infected*100, type="o", pch=16, col="darkred", main="Italy: mortality% by day", xlab = "date", ylab = "Mortality %", family="f1")
points(italy.cov$date, italy.cov$dead/italy.cov$infected*100)
abline(v=as.Date("2020-03-09"), col="darkgreen", lwd=2, lty=2)

#infections, deaths
plot(italy.cov$date, italy.cov$infected, type="o", pch=16, col="darkblue", main="Italy: infections | deaths by day", xlab = "date", ylab = "infections | deaths", family="f1")
points(italy.cov$date, italy.cov$infected)

points(italy.cov$date, italy.cov$dead, type="o", pch=16, col="darkred")
points(italy.cov$date, italy.cov$dead)
abline(v=as.Date("2020-03-09"), col="darkgreen", lwd=2, lty=2)

#infection rate
y=sapply(1:(length(italy.cov$infected)-1), function(x) italy.cov$infected[x+1]-italy.cov$infected[x])

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(x=tail(italy.cov$date, -1),y=y, pch=16,type="o", col="darkblue", log="y", main=bquote("Italy COVID-19 infection | death rate till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "log infection | death rate", family= "f1")
points(tail(italy.cov$date, -1),y=y)

#death rate
y=sapply(1:(length(italy.cov$dead)-1), function(x) italy.cov$dead[x+1]-italy.cov$dead[x])

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
points(x=tail(italy.cov$date, -1),y=y, pch=16,type="o", col="darkred")
points(tail(italy.cov$date, -1), y=y)
abline(v=as.Date("2020-03-09"), col="darkgreen", lwd=2, lty=2)

#hospitalized
y=sapply(1:(length(italy.cov$total.hospitalized)-1), function(x) italy.cov$total.hospitalized[x+1]-italy.cov$total.hospitalized[x])

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(x=tail(italy.cov$date, -1),y=y, pch=16,type="o", col="red", main=bquote("Italy COVID-19 total hospitalization rate/day till:"~.(format(Sys.time(), "%a %b %d %Y"))), xlab = "date", ylab = "total hospitalization rate/day", family= "f1")
points(tail(italy.cov$date, -1),y=y)
abline(v=as.Date("2020-03-09"), col="darkgreen", lwd=2, lty=2)

tst=gsub(x=format(Sys.time(), "%a %b %d %Y"),pattern = " ", replacement = ".")
dev.copy(png, file=paste0("~/cutting_block/R/data_analysis/Figures/italy_mort.",tst,".png"), width=11, height=8, res=300, units="in")
dev.off()
