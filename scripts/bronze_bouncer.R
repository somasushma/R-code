a=2
b=3
f=function(x,a,b) {(a*x-1)/(x^2+b*x-1)}
n=100
xmax=5
xlist=list()
l=1
for(k in seq(-xmax,xmax,.001)) {
  x=k
  xbox=rep(NA,n)
for(j in 1:n) {
  x=f(x,a,b)
  xbox[j]=x
}
xlist[[l]]=xbox
l=l+1
}

for(j in 1:length(xlist)) {
 if(j==1) plot(xlist[[j]], type="l", ylim = c(-10,10), col=rainbow(length(xlist))[j])
 else points(xlist[[j]], type="l", ylim = range(unlist(xlist)), col=rainbow(length(xlist))[j])
}


#plot evolution-----------
r1=polyroot(c(1,-3,0,1))
r2=polyroot(c(1,-2,-6,7,1))
abline(h=r2)

#second root vs first root---------
te=seq(-xmax,xmax,.001)
plot(x=te,y=rep(1, length(te)), col="red", pch=18, cex=.25)
points(x=te[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))],y=rep(1, length(te[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))])), col="black", pch=16, cex=1/2)
abline(v=c(r1,r2))

#individuals-----------------
which(unlist(lapply(xlist, function(x) any(abs(tail(x,10))>8))))

n=1000
x=te[6863]
for(j in 1:n) {
  x=f(x,a,b)
  xbox[j]=x
}
plot(xbox, type="l")

#-iterations to converge plot-------------
a=2
b=3
f=function(x,a,b) {(a*x-1)/(x^2+b*x-1)}
n=500
dx=.001
xmax=10
xlist=list()
tol=6
l=1
for(k in seq(-xmax,xmax,dx)) {
  x=k
  xbox=rep(NA,n)
  for(j in 1:n) {
    x=f(x,a,b)
    if( round(x,tol) %in% round(xbox, tol)) break
    xbox[j]=x
  }
  xlist[[l]]=c(na.exclude(xbox))
  l=l+1
}
te=seq(-xmax,xmax,dx)
plot(x=te, y=lengths(xlist), type="h")
abline(v=c(r1,r2), col="green")

which(unlist(lengths(xlist)==500))
k=12703
plot(xlist[[k]], type = "h", main=te[k])
xlist[[k]][which(abs(xlist[[k]])>20)-1]

