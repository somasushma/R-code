par(mfrow=c(5,5))
n=5000
m=1000
l=0
amat=list()

for (k in 1:750) {
pm=array(data=NA, dim = c(n,2))
# a=rep(0,12)
# b=round(runif(n = 1, min = 4, max = 12),0)
a=runif(12, min=-1.2, max = 1.2)

xn=.1
yn=.1
ep=.0000001
xm=xn+ep
ym=xn+ep
dbox=rep(NA,m)
for(j in 1:m) {
  xn1=a[1]+ a[2]*xn + a[3]*xn^2 + a[4]*xn*yn + a[5]*yn+a[6]*yn^2
  yn1=a[7]+ a[8]*xn + a[9]*xn^2 + a[10]*xn*yn + a[11]*yn+a[12]*yn^2
  xm1=a[1]+ a[2]*xm + a[3]*xm^2 + a[4]*xm*ym + a[5]*ym+a[6]*ym^2
  ym1=a[7]+ a[8]*xm + a[9]*xm^2 + a[10]*xm*ym + a[11]*ym+a[12]*ym^2
  d=(xn1-xm1)^2+(yn1-ym1)^2
  dbox[j]=d
  xn=xn1
  yn=yn1
  xm=xm1
  ym=ym1
}
  if(dbox[m]>100 || is.nan(dbox[m])) { next 
    } else if(any(abs(dbox[(m-500):(m-1)]-dbox[m])<.0000001) ){ next
    } else {
      for (j in 1:n) {
        xn1=a[1]+ a[2]*xn + a[3]*xn^2 + a[4]*xn*yn + a[5]*yn+a[6]*yn^2
        yn1=a[7]+ a[8]*xn + a[9]*xn^2 + a[10]*xn*yn + a[11]*yn+a[12]*yn^2
        pm[j,]=c(xn1,yn1)
        xn=xn1
        yn=yn1
        
        
      }
      if(any(is.nan(pm))){
        break
      } else {
      l=l+1
      amat[[l]]=a
      par(pty="s", mar=c(2,2,2,1), mgp=c(1.1,.5,0))
      plot(pm[,1], pm[,2], pch=16, cex=.2, main=l, col=terrain.colors(10), xlab = "x", ylab = "y")
      }  
  }
}

#blow up selected------------
a=amat[[16]]
n=100000
pm=array(data=NA, dim = c(n,2))
xn=.1
yn=.1
for (j in 1:n) {
  xn1=a[1]+ a[2]*xn + a[3]*xn^2 + a[4]*xn*yn + a[5]*yn+a[6]*yn^2
  yn1=a[7]+ a[8]*xn + a[9]*xn^2 + a[10]*xn*yn + a[11]*yn+a[12]*yn^2
  pm[j,]=c(xn1,yn1)
  xn=xn1
  yn=yn1
  
  
}
par(mfrow=c(1,1), pty="m")
plot(x=pm[10:n,1], y=pm[10:n,2], pch=16, cex=.2, main="", xlab = "x", ylab = "y", col=terrain.colors(10))
