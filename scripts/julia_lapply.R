f.list=list()
f.list$julpow2=function(z,c) z^2+c

f.list$cosjul=function(z,c) c*cos(z)

f.list$c3_2julia=function(z,c) z^3-(-z)^(2.00001)+c

f.list$julinvpow2=function(z,c) z^(-2)+c

col.list=list()
col.list$gray=gray.colors
col.list$CyBldrBl=colorRampPalette(c("cyan2", "black","darkblue"))

julia=function(z, n, c){
  for(j in 1:n){
    z=fz(z,c)
    if(Mod(z) >16 || Mod(z)==Inf){
      break
    }
  }
  return(j)
}



fz=f.list$julinvpow2
col=col.list$CyBldrBl
n=100
c=-1.2
b=1.5
dv=.005
pm= as.vector(outer(X = seq(-b,b,dv), Y = seq(-b,b,dv)*1i,FUN = "+"))
cm=sapply(pm, function(x) julia(x,n, c))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), pty="s")

plot(pm, pch=16, cex=.1, col=rev(col(100))[cm], xlab = "x", ylab = "iy", main=substitute(paste("f(z)=",f, "; c=", c), list(c=c, f=body(fz))))
