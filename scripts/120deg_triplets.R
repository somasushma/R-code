library(numbers)
library(pracma)
#brute force
n=1000
cbox=c()
bbox=c()
abox=c()
for(j in 1:n){
  a=j
  for (k in 1:n) {
    b=k
    x=a^2+a*b+b^2
    if(sqrt(x)==floor(sqrt(x))){
    cbox=c(cbox,sqrt(x))
    abox=c(abox,a)
    bbox=c(bbox,b)
    }
    
  }
}

triples=cbind(abox, bbox, cbox)
triples=triples[which(sapply(1:length(triples[,1]), function(x) coprime(triples[x,1], triples[x,2]))),]
triples=as.array(triples)

#primitive triples
l=length(triples[,1])/2
ye=array(data = NA, dim=c(l,3))
k=1
for(j in 1: length(triples[,1])){
  te=triples[j,c(2,1,3)]
  ge= which((apply(triples, 1, function(x) all(te %in% x))))
  if(ge[1] < j) {
    next
  } else{
    ye[k,]= triples[j,]
    k=k+1
  }
}

par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples[,1], triples[,2], pch=16, main=bquote("120 degree triples,"~max(a)<.(n)), xlab="a", ylab="b", col="darkblue")

#direct method 120 degs------------------
n=400
j=1
triples.120=array(data=NA,dim = c(n*n,3))
for (u in 1:n) {
  for (v in 1:n) {
    if(coprime(u,v) && u > v){
      triples.120[j,]=c(u^2-v^2, 2*u*v+v^2, u^2+u*v+v^2)
      j=j+1
    }
  }
}

triples.120=triples.120[-which(is.na(triples.120[,1])),]
for (j in 1:length(triples.120[,1])) {
  te=gcd(triples.120[j,1], triples.120[j,2])
  if(te>1) triples.120[j,] = triples.120[j,]/te
}

triples.120=t(sapply(1:length(triples.120[,1]), function(x) sort(triples.120[x,])))

triples.120=triples.120[order(triples.120[,1], triples.120[,2]),]
triples.120=unique(triples.120, MARGIN = 1)


#triples.120 below certain number
te=triples.120[order(triples.120[,3]),]
m=16400
ye=sapply(seq(100,m, 100), function(x) length(which(te[,3]<x))/x)

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(ye, type="l", main=bquote(" fraction of 120 degree triple <"~.(m)), xlab="", ylab="", col="darkblue", xaxt="n", lwd=2, cex.main=2, cex.axis=1.5)
axis(1, at=seq(100,m,100), labels = seq(100,m,100)*100)
abline(h=sqrt(3)/(4*pi), col="gray35", lty=2, lwd=2)

#plot of sector of primitive triples sorted by c
par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples.120[which(triples.120[,3]<m),1], triples.120[which(triples.120[,3]<m),2], pch=16, cex=.25, main=bquote("120 degree triples max(c)<"~.(m)), xlab="a", ylab="b", col="darkgreen", asp=1)
curve(1/2*(sqrt(4*m^2-3*x^2)-x), from=0, to=m/sqrt(3), asp=1, add=T, col="darkred")
segments(0,0,0,m, col = "darkred")
segments(0,0,m/sqrt(3),m/sqrt(3), col = "darkred")

#normalized rational points on ellipse
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))

plot(triples.120[1:300,1]/triples.120[1:300,3], triples.120[1:300,2]/triples.120[1:300,3], pch=16, cex=.5, asp=1, col="gray25", xlab="a/c", ylab="b/c", main=bquote("primitive"~T[120]~"triples"))

#special points
col= c("red", "royalblue3", "orangered", "rosybrown4")
psize=c(4.5,3.5,2.5,1.5)

j=11
points(triples.120[j,1]/triples.120[j,3], triples.120[j,2]/triples.120[j,3], pch=16, cex=psize[4], asp=1, col=adjustcolor(col[2],alpha.f = .5)) 
text(labels = j, x=triples.120[j,1]/triples.120[j,3], y=triples.120[j,2]/triples.120[j,3],col=col[2], adj = c(-1,-1))

#matrix---
M120=matrix(c(4,3,4,3,4,4,6,6,7), byrow = T, nrow=3)

#perimeter, incircle.circumcircle product etc
triples.120=triples.120[order(triples.120[,3]),]
p120= sapply(1:nrow(triples.120), function(x) sum(triples.120[x,]))
in120=sapply(1:nrow(triples.120), function(x)  sqrt((p120[x]/2-triples.120[x,1])*(p120[x]/2-triples.120[x,2])*(p120[x]/2-triples.120[x,3])/(p120[x]/2)) )

incir_dia=sapply(1:nrow(triples.120), function(x) triples.120[x,1]*triples.120[x,2]*triples.120[x,3]/(triples.120[x,1]+triples.120[x,2]+triples.120[x,3]))
abline(a=0, b=pi^2/log((3+sqrt(3))/2), col="blue")

#divisibility
te=triples.120[order(triples.120[,3]),]
m=164000
te=te[which(triples.120[,3]<m),]

d=c(3,5,7,8, 6, 13)
te=te/d[4]
ye=which(sapply(1:nrow(te), function(x) any(te[x,] %in% round(te[x,],0))))
length(ye)/nrow(te)

# from dhenu saMkhya
f=sapply(1:20, function(x) fibonacci(x))
j=5
f[j]*f[j+3]; f[j+1]*f[j+4]; f[j]*f[j+2]+f[j+1]*f[j+4];


#60 degree case-------------
n=300
j=1
triples.60=array(data=NA,dim = c(n*n,3))
for (u in 1:n) {
  for (v in 1:n) {
    if(coprime(u,v) && u >= v){
      triples.60[j,]=c(u^2-v^2, 2*u*v-v^2, u^2-u*v+v^2)
      j=j+1
    }
  }
}
triples.60=triples.60[-which(is.na(triples.60[,1]) | triples.60[,1]==0 | triples.60[,2]==0),]

for (j in 1:length(triples.60[,1])) {
  te=gcd(triples.60[j,1], triples.60[j,2])
  if(te>1) triples.60[j,] = triples.60[j,]/te
}

triples.60=t(sapply(1:length(triples.60[,1]), function(x) sort(triples.60[x,])))

triples.60=triples.60[order(triples.60[,1], triples.60[,2]),]
triples.60=unique(triples.60, MARGIN = 1)

#triples.60 below certain number by longest side
te=triples.60[order(triples.60[,3]),]
m=22500
ye=sapply(seq(100,m, 100), function(x) length(which(te[,3]<x))/x)

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(ye, type="l", main=bquote("fraction of 60 degree triples max(c) <"~.(m)), xlab="", ylab="", col="darkred", xaxt="n", lwd=2, cex.main=2, cex.axis=2)
axis(1, at=seq(50,m,50), labels = seq(50,m,50)*100, cex.axis=2)
abline(h=9*log(3)/(4*pi^2), col="gray35", lty=2, lwd=2)

#plot of sector of primitive triples sorted by c
par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples.60[which(triples.60[,3]<m),1], triples.60[which(triples.60[,3]<m),2], pch=16, cex=.25, main=bquote("60 degree triples max(c)<"~.(m)), xlab="a", ylab="b", col="darkgreen", asp=1)
curve(sqrt(x^2-m*x+m^2), from=0, to=m, asp=1, add=T, col="darkred")
segments(0,0,0,m, col = "darkred")
segments(0,0,m,m, col = "darkred")

#triples.60 below certain number by middle side
te=triples.60[order(triples.60[,2]),]
m=22800
ye=sapply(seq(100,m, 100), function(x) length(which(te[,2]<x))/x)

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(ye, type="l", main=bquote(" fraction of 60 degree triples <"~.(m)), xlab="", ylab="", col="darkred", xaxt="n", lwd=2, cex.main=2, cex.axis=2)
axis(1, at=seq(50,m,50), labels = seq(50,m,50)*100)
abline(h=sqrt(3)/(2*pi), col="gray35", lty=2, lwd=2)


#plot of sector of primitive triples sorted by middle side b
par(pty="s", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(triples.60[which(triples.60[,2]<m),1], triples.60[which(triples.60[,2]<m),3], pch=16, cex=.25, main=bquote("60 degree triples max(b)<"~.(m)), xlab="a", ylab="b", col="darkgreen", asp=1)
curve(1/2*(sqrt(4*m^2-3*x^2)+x), from=0, to=m, asp=1, add=T, col="darkred")
segments(0,0,0,m, col = "darkred")
segments(0,0,m,m, col = "darkred")