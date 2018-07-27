fx=function(x) a*x+(2*(1-a)*x^2)/(1+x^2)
a=cos(2*2*pi/9)-.01
b=.9998
n=20000
x=12
y=7.7
xbox=rep(NA, n)
ybox=rep(NA, n)
j=1
while (j <= n) {
  xbox[j]=x
  ybox[j]=y
  j=j+1
  x=b*y+fx(x)
  y=-xbox[j-1]+fx(x)
}

par(pty="s", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(x=xbox, y=ybox, pch=16, cex=.25, col=sapply(1:(n/1000), function(x) rep(rainbow(n/1000)[x],1000)), xlab="x",ylab= "y", main=paste("Mira", " a=", a, ", b=" , b, ", x0=" , xbox[1], ", y0=", ybox[1]), asp=1)
