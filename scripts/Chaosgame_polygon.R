phi=1/2+sqrt(5)/2
rlist=list(r3=1/2, r4=sqrt(2)-1, r5=1-1/phi, r6=1/3, r7=cos(2*pi/7)/2, r8=1-1/sqrt(2), r9=2-sqrt(3))

n=100000
m=7
zm=polyroot(c(1,rep(0,(m-1)),1))
z0=0+0i
r=.3077
zbox=rep(NA,n)
z=z0
for(j in 1:n) {
  rand=runif(1, min = 0, max=1)
  if(rand<1/m) {
    z=(zm[1]+z)*r
    zbox[j]=z
  } else if(rand>(m-1)/m){
    z=(zm[m]+z)*r
    zbox[j]=z
  } else{
    for(k in (1:(m-2))) {
      if(rand>k/m && rand < (k+1)/m){
        z=(zm[k+1]+z)*r
        zbox[j]=z
      }
    }
  }
}
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), pty="s")
plot(tail(zbox,-5), pch=16, cex=1/3, col="darkblue", xlab="x", ylab="y", main=paste("r=", r))
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "black")
points(tail(zbox,-5), pch=".",  col="gray88")
points(zm,pch=16, col="red")