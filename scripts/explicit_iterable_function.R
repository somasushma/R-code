library(gmp)
library(Rmpfr)

prec=2000
f1=function(x) mpfr(16,prec)*x*(mpfr(1, prec)-mpfr(2, prec)*sqrt(x)+x)
f2=function(x)(mpfr(2,prec)*x^(mpfr(2,prec)/mpfr(3,prec))-mpfr(1,prec))^mpfr(3,prec)
  
n=1000
f=rep(NA,n)
x0=mpfr(1,prec)/mpfr(2,prec)
x=x0
for (j in 1:n) {
  x=f2(abs(x))*sign(x)
  f[j]=as.numeric(x)
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(f, type = "l", main=bquote(x[0]==~.(as.numeric(x0))), col="darkblue", lwd=1.5)

#for f1
k=mpfr(900,prec)
xk=sin(mpfr(2,prec)^mpfr(k, prec)*asin(x0^(mpfr(1,prec)/mpfr(4, prec))))^mpfr(4, prec)

#for f2
k=mpfr(900,prec)
xk=cos(mpfr(2,prec)^mpfr(k, prec)*acos(abs(x0)^(mpfr(1,prec)/mpfr(3, prec))))^mpfr(3, prec)

