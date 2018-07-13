library(numbers)
library(pracma)
library(MASS)

np=200000
p=primes(np)
n=length(p)
f=rep(NA, n)
f[1]=2
f[1]=1
for (j in 2:n) {
f[j]=p[j]-f[j-1]  
}

fo=f[sapply(1:floor(n/2), function(x) 2*x)]
fe=f[sapply(1:floor(n/2), function(x) 2*x-1)]

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(y=fo, x=sapply(1:floor(n/2), function(x) 2*x), type="l", col="darkred", lwd=2, xlab = "n", ylab="f", main="sums to prime sequence")
points(y=fe, x=sapply(1:floor(n/2), function(x) 2*x-1), type="l", col="blue", lwd=2, xlab = "n", ylab="f", main="sums to prime sequence")

curve(.5*x*log(x)+.5*x*log(log(x))-.5*x, from = 1, to=n, add = T, col="green")

curve(x*log(x), from = 1, to=n, add = T, col="green")