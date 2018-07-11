#Recaman sequence
n=100
f=rep(0,n)
f[1]=12

for (j in 2:n) {
  te=f[j-1]-j
  if(te<0){
    f[j]=f[j-1]+j
    next
  }
  else if(te %in% f){
    f[j]=f[j-1]+j
    next
  }
  else f[j] = te
}

cen=sapply(1:(length(f)-1), function(x) (f[x+1]+f[x])/2)
rad=sapply(1:(length(f)-1), function(x) (f[x+1]-f[x])/2)

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(x=f, y=rep(0,length(f)), pch=16, cex=.5, col="darkgreen", asp=1, ylim=range(rad), axes = F,xlab="", ylab="", main=f[1])
axis(1, at=seq(0,(max(f)+20),20), pos = 0)


t=seq(0,pi,pi/48)
arcs=lapply(1:length(rad), function(x) abs(rad[x])*(cos(sign(rad[x])*t)+1i*sin(sign(rad[x])*t))+cen[x])
lapply(1:length(arcs), function(x) points(arcs[[x]], type = "l", col=rainbow(n-1)[x]))
