par(mfrow=c(5,5))
n=5000
m=1000
l=0
amat=list()

for (k in 1:100000) {
  pm=array(data=NA, dim = c(n,2))
  # a=rep(0,12)
  # b=round(runif(n = 1, min = 4, max = 12),0)
  a=runif(5, min=-1.2, max = 1.2)
  
  xn=0.1
  yn=0.1
  ep=.0000001
  xm=xn+ep
  ym=xn+ep
  dbox=rep(NA,m)
  for(j in 1:m) {
    xn1=a[1]+ a[2]*xn + a[3]*(xn)^2 + a[4]*xn^3 +yn
    yn1=a[5]- xn
    xm1=a[1]+ a[2]*xm + a[3]*(xm)^2 + a[4]*xm^3 +ym
    ym1=a[5]- xm
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
      xn1=a[1]+ a[2]*xn + a[3]*(xn)^2 + a[4]*xn^3 +yn
      yn1=a[5]- xn
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
a=amat[[12]]
n=5000
te=as.vector(outer(seq(-.1,.1,.01), seq(-.1,.1,.01)*1i, FUN = "+"))
xbox=Re(te)
ybox=Im(te)
m=length(xbox)
pm=array(data=NA, dim = c(n*m,3))

s=0
col=0
for (k in 1:m) {
  xn=xbox[k]
  yn=ybox[k]
  col=col+1
  for (j in 1:n) {
    xn1=a[1]+ a[2]*xn + a[3]*(xn)^2 + a[4]*xn^3 +yn
    yn1=a[5]- xn
    if(abs(xn1) > 4|| abs(yn1) > 4|| is.nan(xn1) || is.nan(yn1)){
      break
    } else {
      s=s+1  
      pm[s,]=c(xn1,yn1, col)
    }
    xn=xn1
    yn=yn1
    
  }
}
par(mfrow=c(1,1), pty="s")
plot(x=pm[1:s,1], y=pm[1:s,2], pch=16, cex=.2, main="", xlab = "x", ylab = "y", col=terrain.colors(10)[(pm[,3]) %% 10])
