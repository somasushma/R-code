library(Rmpfr)
e=exp(1)
prec=3000
n=mpfr(100, prec)
ter=c()
for (k in 1:as.numeric(n)) {
  y=prod(sapplyMpfr(0:(k-1), function(x) n-mpfr(x, prec)))/n^mpfr(k,prec)
  if(as.numeric(y)<.01) break
ter[k]=as.numeric(y)
}
x=c(-(length(ter)-1):(length(ter)-1))
y=c(rev(tail(ter, -1)), ter)
plot(x,y, pch=16)
curve(e^(-x^2/175), from = min(x), to=max(x), col="darkred", add=T)

#normal approximation
n=100
te=choose(n,1:n)
par(pty="m", mar=c(2.2,2.3,2,1), mgp=c(1,.4,0))
plot(te,type="h", xlab="k", ylab=bquote(C[j]), main = bquote(y== (phantom(0)^n*C[n/2])*e^{-2(x-n/2)^2/n}))
a=choose(n,n/2)
curve(a*e^(-(x-n/2)^2/(n/2)), from = 0, to=1000, n=1000, col="blue", add=T, lwd=2)

n=100
a=choose(n,n/2)
te=choose(n,1:n)/a
par(pty="m", mar=c(2.2,2.3,2,1), mgp=c(1,.4,0))
plot(te,type="h", xlab="k", ylab=bquote(C[j]/C[max]), main = bquote(y== e^{-(x-n/2)^2/(n/2)}))
curve(e^(-(x-n/2)^2/(n/2)), from = 0, to=1000, n=10000, col="blue", add=T, lwd=2)

#nArAyaNa problem-3
s=c(1:9)
n=length(s)
factorial(n) #number of permutations
gamma(n) #number of permutations begining or ending in a number
sum(s)*gamma(n) #sum of digits in a particular place
sum(s)*gamma(n)*as.numeric(paste(rep(1,n),collapse = "")) # sum of all numbers
n^2*gamma(n) #total number of digits

aprp=function(x) x^2*gamma(x)

#Pi navagR^iha
n=50
p=3+6*sum(sapply(1:n, function(x) 1/((2*(2*x)^2-1)^2-(2*x)^2)))
p=3+4*sum(sapply(1:n, function(x) (-1)^(x-1)/((2*x+1)^3-(2*x+1))))