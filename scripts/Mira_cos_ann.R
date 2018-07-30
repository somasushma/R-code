library("MASS")

fx=function(x) (1+a*x^3+a*x^2+a*x)/(1+x^2)
angbox=c(
  (3), (4), (5), (6),
  (7), (7/2),(8), (9), 
  (9/2), (11/2), (11/3), (11/4),
  (13/2), (13/3), (13/4), (13/5)
)
abox=cos(2*pi/angbox)+.001
nct=1
par(mfrow=c(4,4))
for (a in abox) {
  b=.9985
  n=500
  m=96
  x0box=5*cos(c(0:m)*2*pi/m)
  y0box=5*sin(c(0:m)*2*pi/m)
  
  xbox=rep(NA, n*m)
  ybox=rep(NA, n*m)
  colbox=rep(NA, n*m)
  s=1
  for(k in 1:m){
    x=x0box[k]
    y=y0box[k]
    for(l in 1:n){
      colbox[s]=k 
      xbox[s]=x
      ybox[s]=y
      s=s+1
      x=b*y+fx(x)
      y=-xbox[s-1]+fx(x)
    }
  }
  
  par(pty="s", mar=c(2,2,1,1), mgp=c(1,.4,0))
  
  # plot(x=xbox, y=ybox, pch=16, cex=.25, col=rainbow(7)[(colbox %%7)+1], xlab="x",ylab= "y", main=paste("a=", a, ", b=" , b, ", x0=" , xbox[1], ", y0=", ybox[1]), asp=1)
  
#   plot(x=xbox, y=ybox, pch=16, cex=.25, col=rainbow(7)[(colbox %%7)+1], axes = F, xlab="",ylab= "", main=as.character(fractions(a)), asp=1)
# }

plot(x=xbox, y=ybox, pch=16, cex=.25, col=rainbow(7)[(colbox %%7)+1], axes = F, xlab="",ylab= "", main=substitute(paste("cos(2", pi, "/","(", ang, ")", ")"), list(ang=as.character(fractions(angbox[nct])))), asp=1)
nct=nct+1
}

#rainbow(7)[(colbox %%7)+1]

# abox=c(0,    1/2, 1/3, 2/3, 1/4, 3/4,
#        1/5,  2/5, 3/5, 4/5, 1/6, 5/6,
#        1/7,  2/7, 3/7, 4/7, 5/7, 6/7,
#        -1,  -1/2,-1/3,-2/3,-1/4,-3/4,
#        -1/5,-2/5,-3/5,-4/5,-1/6,-5/6,
#        -1/7,-2/7,-3/7,-4/7,-5/7,-6/7
# )

