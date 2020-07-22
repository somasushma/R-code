
#boundary
bdx=c(rep(180.0001,90), rep(180,90))
bdy=c(seq(-90,90,length.out = 90), seq(90,-90,length.out = 90))
te=ham.ait(bdx,bdy)
par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(te, type ="l", col="gray30", axes = F, xlab="l", ylab="b", family="f3", asp = 1000/1001)

#grid longitude
for (j in seq(0, 330, 30)) {
  bdx=rep(j, 180)
  bdy=c(seq(-90,90,length.out = 180))
  te=ham.ait(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}

#grid latitude
for (j in seq(-90, 90, 30)) {
  bdx=c(seq(180.0001, 360, length.out = 90),seq(0,180, length.out = 90))
  bdy=c(rep(j, 180))
  te=ham.ait(bdx, bdy)
  points(te, type = "l", lty=3, col="gray30")
}

l=seq(0,330,30)
b=rep(0, length(l))
te=ham.ait(l, b)
text(x = te[,1], y=te[,2], labels = seq(0, 330, 30), family="f3", col = "gray30")

b= seq(-90,90, 30)
l=rep(0, length(b))
te=ham.ait(l, b)
text(x = te[,1], y=te[,2], labels = seq(-90, 90, 30), family="f3", col = "gray30")

te=galc(Mira$X_RAJ2000, Mira$X_DEJ2000)
te=ham.ait(te[,1], te[,2])
points(te, pch=16, cex=.5, col="red")
