#Recaman sequence
n=100
f=rep(0,100)
f[1]=1
for (j in 2:100) {
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


plot(x=f, y=rep(0,length(f)), pch=16, cex=.25, asp=1)

cen=f1=sapply(1:(length(f)-1), function(x) (f[x+1]-f[x])/2)
t=seq(0,pi,pi/24)
r=3
cir=r*e^(cos(-t)+1i*sin(-t))
points(cir, type="l")
