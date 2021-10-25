par(mfrow=c(3,3))
k=12
for (n in 0:8) {
dz=.02
zbox=as.vector(unlist(outer(X = seq(-5,5,dz), Y = seq(-5,5,dz)*1i, FUN = "+")))
val=sin(k*cos(Mod(zbox))-pi*Arg(zbox))
col=(val-min(val))/(max(val)-min(val))*100+1
par(pty="s", mar=c(2.2,2.2,2,1), mgp=c(1.2,.4,.0))
plot(zbox, pch=".", col=heat.colors(max(col))[col], xlab = "x", ylab="y", main=n)
}


#butterfly
a=.618
dz=.02
zbox=as.vector(unlist(outer(X = seq(-5,5,dz), Y = seq(-5,5,dz)*1i, FUN = "+")))
val=(Re(zbox)^2-Im(zbox)^2)*sin((Re(zbox)+Im(zbox))/a)/Mod(zbox)^2
val[which(is.nan(val))]=0
col=(val-min(val))/(max(val)-min(val))*100+1
par(pty="s", mar=c(2.2,2.2,2,1), mgp=c(1.2,.4,.0))
plot(zbox, pch=".", col=rainbow(max(col))[col], xlab = "x", ylab="y")