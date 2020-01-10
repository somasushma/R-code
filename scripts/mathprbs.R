#deltoid fractal
fn1= function(x) z^2 - s* Conj(z)
phi= sqrt(5)/2+ 1/2
sbox=c(phi,  1.64312)
s=2

n=100000
zbox=rep(0, n)

z=0.1+.1i
for(j in 1:n){
  z=fn1(z)
  zbox[j]=z
}

par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
plot(zbox, pch=16, cex=.25, main=paste0("s = ", s), xlab="x", ylab="y", asp=1, col=c("darkblue"))

#logical fractal
n=100
dx=dy=.01
lims=1
xbox=seq(-lims,lims,dx)
ybox=seq(-lims,lims, dy)
pm=array(data = 0, dim = c(length(xbox)*length(ybox), 3))
l=1
for (x in xbox) {
  xn=x
  for (y in ybox) {
    yn=y
    for (j in 1:n) {
      xn1=1-abs((1-yn)-xn)
      yn1=1-abs(xn-yn) 
      xn=xn1
      yn=yn1
      d=sqrt(xn^2+yn^2)
    }
  
  }
  pm[l,]=c(x,y,d)
  l=l+1
}


par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
plot(xbox, ybox, pch=16, cex=.25, main=paste0("s = ", s), xlab="x", ylab="y", asp=1, col=c("darkblue"))

library(Rmpfr)
n=100
prec=1000
xbox=rep(0,n)
ybox=rep(0,n)
d=rep(0,n)
xn=mpfr(1,prec)/mpfr(5, prec)
yn=mpfr(1,prec)/mpfr(10,prec)
for (j in 1:n) {
  xbox[j]=as.numeric(formatMpfr(xn))
  ybox[j]=as.numeric(formatMpfr(yn))
  xn1=1-abs((1-xn)-xn)
  yn1=1-abs(xn-yn) 
  xn=xn1
  yn=yn1
  d[j]=sqrt(xn^2+yn^2)
}

plot(xbox, type="l")
