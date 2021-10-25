n=50
par(mar=c(2,2,2,1), pty="s", mgp=c(1.2,.4,0))
plot(polyroot(c(1-2^2,2)), pch=16, cex=.25, xlim = c(-1.7,1.7), ylim = c(-1.7,1.7), asp=1, xlab="x", ylab="iy", cex.lab=2, cex.axis=2)
te=list()
l=1
for (j in 2:n) {
  col=8+j
 te[[l]]=polyroot(c((1-j^2),2:j))
 points(te[[l]], pch=16, cex=2, col=colors()[col])
 l=l+1
}

t=seq(0, 2*pi, .01)
points(cos(t), sin(t), type="l", col="blue")