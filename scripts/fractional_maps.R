# some convergences------------------
kbox=c(1,2,3, 1/2, 1/3)  #k=1 mAtrA-meru-shredhi; k=2 f defined by Jacobsthal numbers; k=1/2 f converges of (sqrt(13)-1)/6; k= 1/2 f converges to sqrt(3)-1; k=1/3 f converges to (sqrt(21) - 3)/2

k=kbox[5]
n=100
f=rep(NA, n)
f[1]=0

for (j in 2:n) {
  f[j]=1/(1+k*f[j-1])
}

#convergent
(sqrt(4*k+1)-1)/(2*k)

#angle

#negative numerator
n=100
f=rep(NA, n)
f[1]=1

for (j in 2:n) {
  f[j]=(2*f[j-1]-1)/(2+4*f[j-1])
}


#negative term denominator 
k=kbox[2]
n=60000
f=rep(NA, n)
f[1]=0

for (j in 2:n) {
  f[j]=1/(1-k*f[j-1])
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f, type = "l", col="darkblue", lwd=1.5, xlab = "n", main = bquote("n= 1:"~.(n)))
abline(h=0, col="green", lty=2)

#minima of above: first 4 waves 
st=c(13, 226, 439, 652)
for (j in 1:length(st)) {
  te=f[sapply(0:floor((n-st[j])/13), function(x) 13*x+st[j])]
  ye=sapply(0:floor((n-st[j])/13), function(x) 13*x+st[j])
  points(ye, te, pch=16, col="darkred", cex=1/2.5)  
}

#maxima of above: first 4 waves 
st=c(213, 426, 639, 852)
for (j in 1:length(st)) {
  te=f[st[j]-(0:floor(213/13))*13]
  ye=st[j]-(0:floor(213/13))*13
  points(ye, te, pch=16, col="darkgreen", cex=1/1.5)  
}

#cycles of above
te=pi/acos(-sqrt(2)/4)
te=pi/atan(sqrt(7))
sapply(0:15, function(x) print(fractions(te, x)))

#brand transformation of above
k=2
n=100
g=rep(NA, n)
g[1]=polyroot(c(-1,-1,k))[1]

for (j in 2:n) {
  g[j]=1/k*(1+1/g[j-1])
}

#explicit
a=(1+1i*sqrt(7))/2
b=(1-1i*sqrt(7))/2
fx= function(x,n) {((-2*x+a)*a^(n-1)-(-2*x+b)*b^(n-1))/((-2*x+a)*a^n-(-2*x+b)*b^n)}

#negative term denominator with different numerator
k=kbox[2]
n=1000000
f=rep(NA, n)
f[1]=0

for (j in 2:n) {
  f[j]=(1+f[j-1])/(1-k*f[j-1])
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f, type = "l", col="darkblue", lwd=1.5)
abline(h=0, col="green", lty=2)

#cycles of above
te=acos(-1/sqrt(3))/pi
te=pi/atan(sqrt(2))
sapply(0:15, function(x) print(fractions(te, x)))


noquote(cbind(formatC(f[1:100], format = "f", digits = 5, width = 10, flag = " "))) #format values

#equivalent of above in standard form-----------
k=-3/4
n=500
f=rep(NA, n)
f[1]=0

for (j in 2:n) {
  f[j]=1/(1+k*f[j-1])
}


#intertwined process------------
k=1
n=1000
xbox=rep(NA, n)
ybox=rep(NA, n)
xbox[1]=-1
ybox[1]=0
for (j in 2:n) {
  xbox[j]=1/(1+k*ybox[j-1])
  ybox[j]=1/(1-k*xbox[j-1])
}

cx=(2*k+1-sqrt(4*k^2+1))/(2*k)
cy=(2*k-1+sqrt(4*k^2+1))/(2*k)

#intertwined Julia like (mostly uninteresting)
k=3
cx=(2*k+1-sqrt(4*k^2+1))/(2*k)
cy=(2*k-1+sqrt(4*k^2+1))/(2*k)
xrange=c(cx-3,cx+3)
yrange=c(cy-3,cy+3)
dx=.01
dy=.01
xbox=seq(from=xrange[1], to=xrange[2], by=dx)
ybox=seq(from=yrange[1], to=yrange[2], by=dy)
n=100
pm=array(0, dim = c(length(xbox)*length(ybox), 4))
m=1
for (x in xbox) {
  xi=x
  for (y in ybox) {
    yi=y
    j=1
    iterbox=array(0,c(n,2))
    while(j<n){
      iterbox[j,]=c(xi,yi)
      xi1=1/(1+k*yi)
      yi1=1/(1-k*xi)
      xi=xi1
      yi=yi1
      d=sqrt((cx-xi)^2+(cy-yi)^2)
      if(d<0.0000001) break
      j=j+1
    }
    pm[m,]=c(x, y, j, max(sqrt((iterbox[,1]-cx)^2+(iterbox[,2]-cy)^2)))
    m=m+1
  }
}


par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(pm[,1], pm[,2], col=match(round(pm[,4],1),unique(round(pm[,4],1))), pch=".", asp=1)
points(cx, cy, pch=1, cex=1)