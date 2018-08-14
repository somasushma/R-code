#birth rate----
n=20
m=75
inc=.1
# br=seq(.5,n,inc)
br=runif(n=n/inc, min = 1, max = n)
medage=rep(NA,length(br))
maxage=100
dec=60
pop0=c(seq(from=55, to=dec, length.out = (maxage-70)), rep(dec, 35), seq(from=dec, to=1, length.out = 35))
for (j in 1:length(br)) {
  pop=pop0
  pop[1:m]=pop[1:m]+c(rep(br[j],m)*(m:1))
  pop=as.integer(round(pop,0))
  te=unlist(sapply(1:maxage, function(x) rep(x,pop[x])))
  medage[j]=median(te)
}
par(mar=c(2,2,2,1), mgp=c(1.1,.3,0))
barplot(pop0, axes = F)
axis(2, at=seq(0,max(pop0),10), labels = round(seq(0,max(pop0),10)/sum(pop0)*100, 1), las=2)
plot(br, medage, pch=16, col="gray50", xlab="birth rate", ylab="median age")
reg=lm(medage ~ I(1/br))
curve(reg$coefficients[2]/x+reg$coefficients[1], add = T, col="skyblue", lwd=2)
abline(h=seq(0,100,2), v=seq(from=0,to=n, by=2), col="gray80", lty=3)
summary(reg)$r.squared

#TFR----
n=9
m=75
inc=.1
# br=seq(.5,n,inc)
br=runif(n=n/inc, min = 1, max = n)
medage=rep(NA,length(br))
maxage=100
dec=60
pop0=c(seq(from=58, to=dec, length.out = (maxage-80)), rep(dec, 45), seq(from=dec, to=1, length.out = 35))
for (j in 1:length(br)) {
  pop=pop0
  pop[1:m]=pop[1:m]+c(rep(br[j]^2,m)*(m:1))
  pop=as.integer(round(pop,0))
  te=unlist(sapply(1:maxage, function(x) rep(x,pop[x])))
  medage[j]=median(te)
}
par(mar=c(2,2,2,1), mgp=c(1.1,.3,0))
barplot(pop0, axes = F)
axis(2, at=seq(0,max(pop0),10), labels = round(seq(0,max(pop0),10)/sum(pop0)*100, 1), las=2)
plot(br, medage, pch=16, col="gray50", xlab="TFR/w", ylab="median age")
reg=lm(medage ~ I(1/br^2))
curve(reg$coefficients[2]/x^2+reg$coefficients[1], add = T, col="skyblue", lwd=2)
abline(h=seq(0,100,2), v=seq(from=0,to=n, by=2), col="gray80", lty=3)
summary(reg)$r.squared
