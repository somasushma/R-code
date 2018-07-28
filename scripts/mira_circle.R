library("MASS")

fx=function(x) (1+a*x+a*x^2+a*x^3)/(1+x^2)
a=2/3
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

# plot(x=xbox, y=ybox, pch=16, cex=.25, col=rainbow(m)[colbox], xlab="x",ylab= "y", main=paste("a=", a, ", b=" , b, ", x0=" , xbox[1], ", y0=", ybox[1]), asp=1)

plot(x=xbox, y=ybox, pch=16, cex=.25, col=rainbow(7)[(colbox %%7)+1], axes = F, xlab="",ylab= "", main=as.character(fractions(a)), asp=1)
