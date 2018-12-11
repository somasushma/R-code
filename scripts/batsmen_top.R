library(readr)
batsmen <- read_csv("~/R/data_experiments/data/batsmen2.txt", 
                    col_names = FALSE)

bats=array(data = NA, dim = c(73,12))
for (j in 1:12) {
bats[,j]=unlist(sapply(seq(0+j,(864+j),12), function(x) batsmen[x,1]))
}
bats=data.frame(bats)
bats$X8=gsub("\\*", "", bats$X8)

bats[,c(1,4:12)]=sapply(c(1,4:12), function(x) as.numeric(as.character(bats[,x])))
bats[,c(2,3)]=sapply(c(2,3), function(x) as.character(bats[,x]))
colnames(bats)=c("#", "Player", "Country", "Mat", "Inns", "NO", "Runs", "HS", "100s", "50s", "Avg", "crate")

#plot 100s vs 50s
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
plot(bats$`50s`,bats$`100s`,  pch=16, col="blue", xlab = "50s", ylab = "100s", main="100s vs 50s for 73 top batsmen")
points(bats[which(bats$Player=="Bradman"),'50s'], bats[which(bats$Player=="Bradman"),'100s'], pch=16,col="red")
text(x=bats[which(bats$Player=="Bradman"),'50s'], y=bats[which(bats$Player=="Bradman"),'100s'],labels = bats[which(bats$Player=="Bradman"),'Player'], pos = 3, col="red", cex=1.5)
reg1=lm(bats$`100s` ~ bats$`50s`)
abline(reg1, col="gray40", lwd=2.5,lty=2)

text(x=50, y=20,labels = paste("r-sq=", round(summary(reg1)$r.squared,2)), col="blue", cex=1.5)
# text(x=50, y=15,labels = paste("100s/50s slope=", round(summary(reg1)$coefficients[2],2)), col="blue", cex=1.5)
#100s/50s histogram
hist(bats$`100s`/bats$`50s`, breaks=15, xlab="100s/50s", main = "histogram of 100s/50s ratio", col = topo.colors(10))
axis(1, at=seq(0,max(bats$`100s`/bats$`50s`),.25))
abline(v=median(bats$`100s`/bats$`50s`), col="red", lwd=2.5, lty=3)
text(x=.8, y=18,labels = paste("median=", round(median(bats$`100s`/bats$`50s`),2)), pos=4, col="red", cex=1.5)
box()

#plot innings vs 100s
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
plot(bats$`100s`, bats$Inns,  pch=16, col="blue", xlab = "100s", ylab = "# of innings", main="# of innings vs 100s for 73 top batsmen")
points(bats[which(bats$Player=="Bradman"),"100s"], bats[which(bats$Player=="Bradman"),'Inns'], pch=16,col="red")
text(x=bats[which(bats$Player=="Bradman"),'100s'], y=bats[which(bats$Player=="Bradman"),'Inns'],labels = bats[which(bats$Player=="Bradman"),'Player'], pos = 3, col="red", cex=1.5)
reg2=lm(bats$Inns ~ bats$`100s`)
abline(reg2, col="gray40", lwd=2.5,lty=2)
text(x=40, y=150,labels = paste("r-sq=", round(summary(reg2)$r.squared,2)), col="blue", cex=1.5)

#Inns/100 histogram
hist(bats$Inns/bats$`100s`, breaks=12, xlab="Inns/100s", main = "histogram of Inns/100s ratio", col = topo.colors(12))
axis(1, at=seq(1,max(bats$Inns/bats$`100s`),1))
abline(v=median(bats$Inns/bats$`100s`), col="red", lwd=2.5, lty=3)
text(x=9, y=25,labels = paste("median=", round(median(bats$Inns/bats$`100s`),2)), pos=4, col="red", cex=1.5)
box()

#hist averages
hist(bats$Avg, breaks=15, xlab="Average", main = "histogram of Average for top 73 batsmen", col = topo.colors(10), xaxt="n")
axis(1, at=seq(0,100,5))
abline(v=median(bats$Avg), col="gray30", lwd=2.5, lty=3)
text(x=60, y=10,labels = paste("median=", round(median(bats$Avg),2)), pos=4, col="red", cex=1.5)
text(x=60, y=7,labels = paste("p(Bradman)=", formatC(pnorm(q = bats[which(bats$Player=="Bradman"),"Avg"], mean = mean(bats$Avg), sd = sd(bats$Avg), lower.tail = F))), pos=4, col="red", cex=1.5)
box()

#hist #NO/innings
hist(bats$NO/bats$Inns, breaks=15, xlab="fraction of NO innings", main = "histogram of fraction of not-out innings for top 73 batsmen", col = topo.colors(15), xaxt="n")
axis(1, at=seq(0,.5,.05))
abline(v=median(bats$NO/bats$Inns), col="gray30", lwd=2.5, lty=3)
text(x=.08, y=8,labels = paste("median=", round(median(bats$NO/bats$Inns),2)), pos=4, col="red", cex=1.5)
abline(v=bats[bats$Player=="Kallis","NO"]/bats[bats$Player=="Kallis","Inns"], col="darkgreen", lwd=2.5, lty=3)
text(x=bats[bats$Player=="Kallis","NO"]/bats[bats$Player=="Kallis","Inns"], y=10,labels = "Kallis", pos=4, col="darkgreen", cex=1.5)
box()

#hist Highest score
hist(bats$HS, breaks=15, xlab="Highest score", main = "histogram of highest score for top 73 batsmen", col = topo.colors(15), xaxt="n")
axis(1, at=seq(1,400,50))
abline(v=median(bats$HS), col="red", lwd=3.5, lty=3)
text(x=220, y=10,labels = paste("median=", median(bats$HS)), pos=4, col="red", cex=1.5)
abline(v=bats[bats$Player=="Lara","HS"], col="darkgreen", lwd=2.5, lty=3)
text(x=bats[bats$Player=="Lara","HS"]-10, y=10,labels = "Lara", pos=2, col="darkgreen", cex=1.5)
box()

#-------
# read and process 301 batsmen dataset
batsmen <- read_csv("~/R/data_experiments/data/batsmen4.txt", 
                    col_names = FALSE)

bats1=array(data = NA, dim = c(302,13))
for (j in 1:13) {
  bats1[,j]=unlist(sapply(seq(0+j,(3913+j),13), function(x) batsmen[x,1]))
}
bats1=data.frame(bats1)
bats1[,c(1:13)]=sapply(c(1:13), function(x) as.character(bats1[,x]))
bats1$X7=gsub("\\*", "", bats1$X7)
bats1$X10=gsub("\\*", "", bats1$X10)
bats1$X9=gsub("\\+", "", bats1$X9)

te=unlist(bats1[1,])
bats1=bats1[-1,]
bats1[,c(3:13)]=sapply(3:13, function(x) as.numeric(as.character(bats1[,x])))

colnames(bats1)=te
bats1$Player=gsub(bats1$Player,pattern = "ICC\\/",replacement = "")

#PCA
bats1.pca=prcomp(bats1[,c(5:8, 10:13)], center = T, scale. = T)
bats1.pc.pred=predict(bats1.pca)

bats1[which(bats1.pc.pred[,1]> (3) & bats1.pc.pred[,2] > (-5)),"Player"]

paste(sort(bats1[which(bats1.pc.pred[,1]> (3) & bats1.pc.pred[,2] > (-2)),"Player"]), collapse=", ") #alphabetical list
#plotting PCA
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
j=1; k=5
plot(bats1.pc.pred[,j], bats1.pc.pred[,k], xlab=paste("PCA",j), ylab=paste("PCA",k), pch=16, col="gray35", main = "301 best Test batsmen")
abline(v=c(-3,0), h=c(-1), col="gray25", lwd=2, lty=2)

points(bats1.pc.pred[which(bats1.pc.pred[,j]< (-3) & bats1.pc.pred[,k] > (-1)),j], bats1.pc.pred[which(bats1.pc.pred[,j]< (-3) & bats1.pc.pred[,k] > (-1)),k], pch=16, col="red")

points(bats1.pc.pred[which(bats1.pc.pred[,j]< (-3) & bats1.pc.pred[,k] < (-1)),j], bats1.pc.pred[which(bats1.pc.pred[,j]< (-3) & bats1.pc.pred[,k] < (-1)),k], pch=16, col="darkgreen")

#-----------
library(readr)
batsmen <- read_csv("~/R/data_experiments/data/batsmen.csv")
batsmen=batsmen[,-1]
batsmen=subset(x = batsmen,subset = !is.na(batsmen$Runs))
bats2=subset(x = batsmen,subset = batsmen$Runs >= (100))
bats.test=batsmen[batsmen$type=="Test",]

#all centuries decay law
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
te=hist(bats2$Runs, breaks=100, xlab="100s", main = "histogram of all scores >=100", col = topo.colors(101), xaxt="n")
axis(1, at=seq(0,400,50))
box()

ye=cbind(te$counts, te$mids)
ye=ye[ye[,1] != 0,]
reg3=lm(log10(ye[,1]) ~ log10(ye[,2]))
curve(10^reg3$coefficients[1]*x^(reg3$coefficients[2]), from = 100,to=400, col="darkred", lwd=2, lty=2, add = T)
text(x=250,y=600, labels = expression(paste("y= k",x^a)), cex=3, col="darkred")
text(x=250,y=400, labels = paste("k= ",formatC(10^reg3$coefficients[1]), "; a= ", round(reg3$coefficients[2], 2)), cex=2, col="darkred")

#strikerate over career
bname=c("Sehwag", "Tendulkar", "Dravid", "Laxman", "Kohli", "Ganguly")

j=6
batter=subset(batsmen, grepl(batsmen$Player,pattern = bname[j]))

hist(batter$SR, breaks =30, col=terrain.colors(31), xlab="strike rate", main=bname[j])
box()
text(90,30, pos=4, labels = paste("median=", median(batter$SR, na.rm = T)), col="darkblue", cex=1.5)

#scores over career
par(mfrow=c(2,3))
for(j in 1:6){
batter=subset(bats.test, grepl(bats.test$Player,pattern = bname[j]))
den=density(batter$Runs)
plot(den, type="n", xlim=c(0, max(den$x)), ylim=c(0,.018), xlab="Runs", main=paste(bname[j], "Per/Inn=", round(sum(batter$Runs)/(length(batter$Runs)-length(which(batter$Notout==T))),2)))
abline(v=seq(0,400,50), h=seq(0,.02, .005), col="gray", lty=2)
points(den, type="l", col="red", lwd=2) 
}

#runs vs balls (strike rate angle) faced as angle
par(mfrow=c(2,3))
for(j in 1:6){
batter=subset(bats.test, grepl(bats.test$Player,pattern = bname[j]))
te=batter[batter$BF>= 75,]
b=c(max(te$Runs/te$BF, na.rm = T), min(te$Runs/te$BF, na.rm = T))
m=median(batter$Runs/batter$BF, na.rm = T)
ang=(atan(b[1])-atan(b[2]))*180/pi
m.ang=atan(m)*180/pi
plot(batter$BF, batter$Runs, type="n", xlab="balls", ylab = "runs", main=paste(bname[j], ", ang=",round(ang,2), ", m.ang=", round(m.ang,2)))
abline(v=seq(0,400,50), h=seq(0,600,50), col="gray", lty=2)
abline(a=0, b=b[1], col="darkgreen", lwd=2, lty=2)
abline(a=0, b=b[2], col="darkgreen", lwd=2, lty=2)
abline(a=0, b=m, col="blueviolet", lwd=2, lty=2)
points(batter$BF, batter$Runs, pch=16,col="darkred")
}

#distribution 6s and 4s in centuries/50s
par(mfrow=c(2,2))
for(j in c(50, 100, 150, 200)){
te=bats.test[bats.test$Runs >= j,]
par(mar=c(3,3,2,1), mgp=c(1.2,.5, 0))
a=hist((te$X4s*4+te$X6s*6)/te$Runs, breaks=60, plot = F)
plot(x=range(a$mids), y=range(a$counts), type ="n", main = paste(" fraction in 4s+6s in scores >=", j), xlab="# of 4s+6s", ylab="frequency")
abline(v=seq(0,1,.2), h=seq(0,ceiling(max(a$counts)/10^floor(log10(max(a$counts))))*10^floor(log10(max(a$counts))),10^floor(log10(max(a$counts)))/2), col="gray", lwd=2, lty=3)
hist((te$X4s*4+te$X6s*6)/te$Runs, breaks=60, col=heat.colors(70), add=T)
}

#century distribution average for innings tests
te=aggregate(Inns ~ Start.Date+Opposition, data = bats.test, unique) # gets all innings
#total number of innings
te=lengths(te$Inns)
inns=sum(te) 

#count of 100s and probability 100 in an innings
c100s=length(bats.test$Runs[bats.test$Runs>=100])
p100s=c100s/inns #probability 100 in an innings

#100s per innings
bats2=subset(x = bats.test,subset = bats.test$Runs >= (100))
te=aggregate(Runs ~ Start.Date+Opposition+Inns, data = bats2, length)
d100s=c((inns-length(te$Runs))/inns, table(te$Runs)/inns)

par(mfrow=c(1,1))
barplot(d100s, col="red")
barplot(dpois(0:6, p100s), col = adjustcolor("blue", .3), add = T)