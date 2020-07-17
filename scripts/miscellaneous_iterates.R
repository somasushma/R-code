library(Rmpfr)
n=100
xn=.29
xbox=rep(NA, n)
for (j in 1:n) {
  if(xn >=0 && xn < 1/3) { xn1=3*xn } else if(xn >= 1/3 && xn < 2/3){
xn1=2-3*xn    
  } else{
    xn1=3*xn-2
  }
  xbox[j]=xn1
  xn=xn1
}

n=5000
prec=2000
xn=mpfr(3,prec)/mpfr(4,prec)
pee=Const('pi', prec)
xbox=rep(NA, n)
for (j in 1:n) {
  xn1=-mpfr(2,prec)/pee*atan(mpfr(2,prec)/tan(pee*xn))
  xbox[j]=as.numeric(xn1)
  xn=xn1
}

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(xbox, type="o", pch=16, cex=.5, col="darkblue", xlab = "n", ylab = bquote(x[n]), main = bquote(x[n+1]==-2/pi*arctan(2/tan(pi*x[n]))) )

#----------
n=10
f=rep(NA,n)
f[1]=0
f[2]=1
for (j in 3:n) {
  f[j]=j*(j-1)*f[j-1]+1/2*j*(j-1)^2*f[j-2]
}
