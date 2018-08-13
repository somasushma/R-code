n=30
m=30
inc=.1
# br=seq(.5,n,inc)
br=runif(n=n/inc,min = .5, max = n)
medage=rep(NA,length(br))
for (j in 1:length(br)) {
maxage=80
pop=c(seq(1, (maxage-60), 1), rep(20, 30), rep(6, 30))
pop[1:m]=pop[1:m]+c(rep(br[j],m)*(m:1))
te=unlist(sapply(1:maxage, function(x) rep(x,pop[x])))
medage[j]=median(te)
}
plot(br, medage, pch=16, col="gray50")
reg=lm(medage ~ I(1/br))
curve(reg$coefficients[2]/x+reg$coefficients[1], add = T, col="skyblue", lwd=2)
abline(h=0, v=0)
summary(reg)$r.squared