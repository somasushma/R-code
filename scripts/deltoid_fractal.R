#deltoid fractal
fn1= function(x) z^2 - s* Conj(z)
phi= sqrt(5)/2+ 1/2
sbox=c(phi,  1.64312)
s=1.8

n=100000
zbox=rep(0, n)

z=1/3+1i/2
for(j in 1:n){
  z=fn1(z)
  zbox[j]=z
}

par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
plot(zbox, pch=16, cex=.25, main=paste0("s = ", s), xlab="x", ylab="y", asp=1, col=c("darkblue"))
