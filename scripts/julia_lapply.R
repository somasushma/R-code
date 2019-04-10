f.list=list()
f.list$julpow2=function(z,c) z^2+c

f.list$cosjul=function(z,c) c*cos(z)

f.list$c3_2julia=function(z,c) z^3-(-z)^(2.00001)+c

f.list$julinvpow2=function(z,c) z^(-2)+c

f.list$julpow2_inv1=function(z,c) z^2+c/z

f.list$julpow2_inv2=function(z,c) z^2+c/z^2

col.list=list()
col.list$gray=gray.colors
col.list$CyBldrBl=colorRampPalette(c("cyan2", "black","darkblue"))
col.list$TudrOldrBl=colorRampPalette(c("darkturquoise", "darkred","darkblue"))
col.list$WOdR=colorRampPalette(c("white","orange","darkred"))
col.list$dyad=c("black", "white")

julia=function(z, n, c){
  for(j in 1:n){
    z=fz(z,c)
    if(Mod(z) >16 || Mod(z)==Inf){
      break
    }
  }
  return(j)
}


fz=f.list$julpow2_inv2
col=col.list$TudrOldrBl
n=100
c=-.1
b=1.5
dv=.0025
pm= as.vector(outer(X = seq(-b,b,dv), Y = seq(-b,b,dv)*1i,FUN = "+"))
cm=sapply(pm, function(x) julia(x,n, c))

png(filename = "~/R/Figures/Figures1/Jul020.png", height = 1500, width = 1500 )
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), pty="s")
plot(pm, pch=16, cex=.1, col=col(100)[cm], xlab = "x", ylab = "iy", main=substitute(paste("f(z)=",f, "; c=", c), list(c=c, f=body(fz))), cex.main=2)
dev.off()