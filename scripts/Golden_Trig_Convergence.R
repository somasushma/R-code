phi=1/2+sqrt(5)/2

n=10000
xbox=rep(NA, n)
ybox=rep(NA, n)
x=1
y=1.3
for (j in 1:n) {
  x=cos(atan(x^2))
  y=sin(atan(x^2))
  xbox[j]=x
  ybox[j]=y
  
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(xbox,ybox,type = "l", asp=1)