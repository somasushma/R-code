library(pander)
library(pracma)
library(numbers)

setwd("~/R/Figures/Figures1/")

cbrt=function(x) x^(1/3)
isInt=function(x) round(x, 10)==round(x,1)
asInt=function(x) round(x, 0)
  
n=30000
pm=array(data = 0, dim = c(n, 3))
j=1
for (x in 1:n) {
  for(y in x:n){
    z=cbrt(x^2+y^2)
    if(isInt(z)){
      pm[j,]=c(x,y,z)
      j=j+1
    }
  }
}

pm=pm[which(pm[,1] !=0),]

#3D plot
library(rgl)
plot3d(pm, type = "s", size = 1, col = "dodgerblue3")

#x-y plot
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(pm[,1], pm[,2], col="dodgerblue2", pch=16, cex=.5, xlab="x", ylab = "y", main=parse(text = paste0('"Integer solutions of"', '~x^2+y^2==z^3')))

#family 1: x=n^2+1, y=n^3+n
m=1:100
x=m^2+1; y=m^3+m
points(x, y, pch=16, cex=.5, col="darkred")

#family 2: x=c(3*m^2-1, 3*m^2+27)
m=1:100
x=c(3*m^2-1, 3*m^2+27); y=c(m^3-3*m, m^3+9*m)
points(x, y, pch=16, cex=.5, col="blue")

#family 3: x=2*n^3; y=2*n^3
m=1:100
x=2*m^3; y=2*m^3
points(x, y, pch=16, cex=.5, col="red")

sink("squares_cube.txt")
pander(pm, justify="rrr", col.names=c("x", "y", "z"))
sink()

#number below N
N= max(pm[,3])
te= pm[order(pm[,3]),]
ye= unique(te[,3])
cnts=sapply(seq(5,1020,5), function(x) length(which(ye <= (x)))/x*sqrt(log(x)))

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(seq(5,1020,5), cnts, type="o", pch=16, cex=.25, lwd=2, xlab = "x", ylab = "K", main = parse(text = "N(z)*sqrt(log(x))/x") )
abline(h=mean(tail(cnts, 100)), col="blue", lty=2, lwd=2)

#creating 4n+1 and products with 2
l=c(5, 13, 17, 29, 37, 53)
p=l[1]
for (j in 2:length(l)) {
  p=sort(unique(c(l[j], p, as.vector(outer(X = c(p,l[j]), Y = c(p,l[j]), FUN = "*")))))
}
te=p[which(!p[1:25] %in% l)]
te=sort(unique(c(te, as.vector(outer(X = te, Y = sapply(1:5, function(x) 2^x))))))
