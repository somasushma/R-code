library("pracma")
library("numbers")
library("MASS")
library("xtable")
library("fractional")

n=20000
mi=1000
f=rep(0,n)
for (j in 1:n) {
  m=j
  k=0
    repeat {
    nu=numerators(m)
    de=denominators(m)
    m=sum(divisors(nu+de))/length(divisors(nu+de))
    if(k>mi){
      f[j]=0
      break
    }
    if(m==floor(m)){
      f[j]=m
      break
    }
    k=k+1
  }
}

par(mfrow=c(1,1))
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f, type="l", col=c("dodgerblue4","darkred"), xlab="n",ylab= substitute(paste(f,"[n]")), main=expression(paste("Wild numbers: sum(D(p+q))/N(D(p+q) ", n), list(n=n)))

abline(a=1/2, b=1/2, col="brown", lty=2)
abline(a=3/8, b=3/8, col="brown", lty=2)

te=f-floor(.5*(1:n))
points(x=which(te>5), y=f[which(te>5)],col="red", pch=16)
points(x=(2:floor(sqrt(n)))^2-1, y=f[(2:floor(sqrt(n)))^2-1],col="green", pch=1)

fr=table(f)

points(x=which(f==630), y=f[which(f==630)], col="blueviolet", pch=1)
points(x=which(f==504), y=f[which(f==504)], col="orange", pch=1)