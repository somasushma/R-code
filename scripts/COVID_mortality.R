setwd("~/R/Dataexperiments/data/")
#mortality
covid=read.csv(file = "covid", header = F)
covid=data.frame(covid)
colnames(covid)=c("country", "infections", "deaths")
covid=covid[!is.na(covid$deaths),]
covid=covid[which(covid$deaths > 1),]

covid$mortality=round(covid$deaths/covid$infections*100, 2) #get countries with > 2 deaths
covid=covid[order(covid$infections),]

#mortality scatterplot
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(x=covid$infections, y=covid$deaths, pch=16, col=c("brown",  "darkblue"), main="COVID-19 mortality", xlab = "infections", ylab = "mortality", log = "xy", family= "f1")
text(x=covid$infections, y=covid$deaths, labels = substr(covid$country, 1,4), col=c("brown", "darkblue"),  pos = rep(c(1,3,4),100)[1:nrow(covid)], offset = .3, family = "f3", cex = .75)
curve(2.8/100*x, add = T, col="blue",lty=2)
curve(1/100*x, add = T, col="green",lty=2)
curve(2/100*x, add = T, col="darkviolet",lty=2)
curve(4/100*x, add = T, col="red",lty=2)
curve(5/100*x, add = T, col="gray",lty=2)

dev.copy(png, file="~/R/Dataexperiments/Figs/covid19.mort.scatter.png", width=10, height=9, res=300, units="in")
dev.off()

#mortality boxplot
par(mar=c(2,2,2,1))
boxplot(covid$mortality, horizontal = T, pch=16, col = "lightblue", main="COVID-19 mortality")

#mortality histogram
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(covid$mortality, col = "lightblue", main="COVID-19 mortality")
abline(v=c(mean(covid$mortality), median(covid$mortality)), col=c("brown","darkgreen"))
box()


#tabulation
knitr::kable(covid[order(covid$deaths, decreasing = T),], row.names = F)

sum(covid$deaths)/sum(covid$infections) * 100 #total mortality
mean(covid$mortality)
