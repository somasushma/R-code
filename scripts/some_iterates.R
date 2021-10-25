library(Rmpfr)
n=20
xn=1/sqrt(2)
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

n=20
prec=2000
xn=mpfr(1,prec)/sqrt(mpfr(2,prec))
xbox=rep(NA, n)
for (j in 1:n) {
  te=as.numeric(xn)
  if(te >=0 && te < 1/3) { xn1=3*xn } else if(te >= 1/3 && te < 2/3){
    xn1=2-3*xn    
  } else{
    xn1=3*xn-2
  }
  xbox[j]=as.numeric(xn1)
  xn=xn1
}

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(xbox, type="o", pch=16, cex=.5, col="darkblue", xlab = "n", ylab = bquote(x[n]), main = "triple system")

#tangent map
n=500
prec=5000
xn=mpfr(1,prec)/mpfr(7,prec)
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