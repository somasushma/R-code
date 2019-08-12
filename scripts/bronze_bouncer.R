#with mpfr----------------
library("Rmpfr")
prec=1500
f1=function(x) x/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))
f2=function(x) (mpfr(2,prec)*x-mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))
f3=function(x) (mpfr(2,prec)*x+mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))

n=20000
xbox=rep(NA,n)
x0=mpfr(3, prec)/mpfr(2,prec)
x=x0
for(j in 1:n) {
  x=f3(x)
  xbox[j]=formatMpfr(x)
}

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(as.numeric(xbox), type="h")

plot(asinh(as.numeric(xbox)), type="h")


#plain------ (f1 is numerically unstable)
f2=function(x) {(2*x-1)/(x^2+3*x-1)}
f3=function(x){(2*x+1)/(x^2+3*x-1)}
f1=function(x){(x)/(x^2+3*x-1)}
n=600
dx=.1
xmax=5
xlist=list()
l=1
for(k in seq(-xmax,xmax,dx)) {
  x=k
  xbox=rep(NA,n)
for(j in 1:n) {
  x=f1(x)
  xbox[j]=x
}
xlist[[l]]=xbox
l=l+1
}
names(xlist)=seq(-xmax,xmax,dx)



#plot evolution-----------
for(j in 1:length(xlist)) {
  if(j==1) plot(xlist[[j]], type="l", ylim = c(-10,10), col=rainbow(length(xlist))[j])
  else points(xlist[[j]], type="l", ylim = range(unlist(xlist)), col=rainbow(length(xlist))[j])
}

r1=polyroot(c(1,-3,0,1))
r2=polyroot(c(1,-2,-6,7,1))
abline(h=r2)

#second root vs first root---------
te=seq(-xmax,xmax,dx)
plot(x=te,y=rep(1, length(te)), col="red", pch=18, cex=.25)
points(x=te[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))],y=rep(1, length(te[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))])), col="black", pch=16, cex=1/2)
abline(v=c(r1,r2))

#individuals-----------------
which(unlist(lapply(xlist, function(x) any(abs(tail(x,10))>8))))


#plain---------
n=20000
xbox=rep(NA,n)
x0=2/10
x=x0
for(j in 1:n) {
  #x=f(x,a,b)
  x=(x)/(x^2+3*x-1)
  xbox[j]=x
}
plot(xbox, type="h")

#-iterations to converge plot-------------

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

#-----
which(unlist(lengths(xlist)==500))
k=12703
plot(xlist[[k]], type = "h", main=te[k])
xlist[[k]][which(abs(xlist[[k]])>20)-1]

