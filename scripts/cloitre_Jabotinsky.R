library(MASS)
library(numbers)
library(pracma)

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("Book Antiqua"),
             f3 = windowsFont("Cambria Math"))

e=exp(1)

#Cloitre Pi & e
n=100
ubox=rep(NA, 100)
vbox=rep(NA, 100)
ubox[1]=vbox[1] <-0
ubox[2]=vbox[2] <-1

for (j in 3:n) {
  ubox[j]=ubox[j-1]+ubox[j-2]/(j-2)
  vbox[j]=vbox[j-1]/(j-2)+vbox[j-2]
  
}

par(mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(1:n, ubox, pch=16, xlab="n", ylab=bquote(x[n]~"or"~y[n]), col="darkred", family="f3")
points(1:n, vbox, pch=16, xlab="n", ylab=bquote(x[n]|y[n]), col="darkgreen", family="f3")
curve(x/e, from=1, to=n, col="darkred", lwd=2, add = T) 
curve(sqrt(2*x/pi), from=0, to=n, col="darkgreen", lwd=2, add = T) 
grid(col="gray55")
text(x=40, y=7, labels = bquote(y==sqrt(2*n/pi)), col="darkgreen")
text(x=40, y=18, labels = bquote(x==n/e), col="darkred")

dev.copy(png, file="~/R/Figures/Figures1/jaber3.png", width=8, height=6, res=300, units="in")
dev.off()

#Erdos-Jabotinsky pi------------
jab=function(x){
  if(x==1) return(1) else{
  y=x-1
  w=ceiling(x/y)*y
  while(y>=2){
    y=y-1
    w=ceiling(w/y)*y
  }
  return(w)
  }
}

n=10000
jabbox=sapply(1:n, function(x) jab(x))

par(mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(jabbox, type="h", xlab="n", ylab=bquote(f[n]), col="darkgreen", family="f3")

curve(x^2/pi, from=1, to=n, col="darkred", lwd=2, add = T) 

dev.copy(png, file="~/R/Figures/Figures1/jaber2.png", width=8, height=6, res=300, units="in")
dev.off()

te=cbind(1:n, abs(c(1:n)^2/jabbox-pi))
head(te[order(te[,2]),])

#sieving-----
n=100
m=10000
jaber=vector(mode="list", length = n)
jaber[[1]]=1:m
jabbox=rep(NA,n)
for (j in 1:n) {
  jabbox[j]=head(jaber[[j]],1)
  jaber[[j+1]]=jaber[[j]][-(0:(ceiling(length(jaber[[j]])/(j+1))-1)*(j+1)+1)]
  
}

#prime sieve-----------
n=100
m=10000
nlist=vector(mode="list", length = n)
nlist[[1]]=2:m
pbox=rep(NA,n)
for (j in 1:n) {
  pbox[j]=head(nlist[[j]],1)
  nlist[[j+1]]=nlist[[j]][-(which(nlist[[j]] %% nlist[[j]][1]==0))]
}

#new sieve 1-----------
n=10000
m=200000
nlist=vector(mode="list", length = n)
nlist[[1]]=1:m
pbox=rep(NA,n)
for (j in 1:n) {
  pbox[j]=head(nlist[[j]],1) ->k
  
  nlist[[j+1]]=nlist[[j]][-(0:(ceiling(length(nlist[[j]])/(k+1))-1)*(k+1)+1)]
}

par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(pbox, type="h", xlab="n", ylab=bquote(a[n]), col="darkgreen", family="f3")
curve(x*log(x), from=1, to=n, col="gray55", lwd=2, add = T) #prime approximation

te=Primes(150000)
points(te[1:10000], type="l", col="black", lwd=2)

curve(x*log(x)+1/2*x*(log(log(x)))^2, from=1, to=n, col="darkred", lwd=2, lty=2, add = T)
curve(x*log(x)+1/2*x*(log(log(x)))^2+(2+digamma(1))*x*log(log(x)), from=1, to=n, col="darkred", lwd=2, add = T)

dev.copy(png, file="~/R/Figures/Figures1/jaber1.png", width=8, height=6, res=300, units="in")
dev.off()

#new sieve 2-----------
n=10000
m=200000
nlist=vector(mode="list", length = n)
nlist[[1]]=1:m
pbox=rep(NA,n)
for (j in 1:n) {
  pbox[j]=head(nlist[[j]],1) ->k
  k=2*k
  
  nlist[[j+1]]=nlist[[j]][-(0:(ceiling(length(nlist[[j]])/(k+1))-1)*(k+1)+1)]
}

par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(pbox, type="h", xlab="n", ylab=bquote(b[n]), col="darkgreen", family="f3")
curve(x*log(x), from=1, to=n, col="gray55", lwd=2, add = T) #prime approximation

curve(1/2*x*log(x)+1/4*x*(log(log(x)))^2, from=1, to=n, col="darkred", lwd=2, lty=2, add = T)
curve(1/2*x*log(x)+1/4*x*(log(log(x)))^2+1/2*(2+digamma(1))*x*log(log(x)), from=1, to=n, col="darkred", lwd=2, add = T)

dev.copy(png, file="~/R/Figures/Figures1/jaber2.png", width=8, height=6, res=300, units="in")
dev.off()

#new sieve 3 Ludic-----------
n=10000
m=200000
nlist=vector(mode="list", length = n)
nlist[[1]]=2:m
pbox=rep(NA,n)
for (j in 1:n) {
  pbox[j]=head(nlist[[j]],1) ->k
  k=k-1
  nlist[[j+1]]=nlist[[j]][-(0:(ceiling(length(nlist[[j]])/(k+1))-1)*(k+1)+1)]
}

par(mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(pbox, type="h", xlab="n", ylab=bquote(a[n]), col="darkgreen", family="f3")
curve(x*log(x)+1/2*x*(log(log(x)))^2+(2+digamma(1))*x*log(log(x)), from=1, to=n, col="darkred", lwd=2, add = T)
curve(x*log(x)+1/2*x*(log(log(x)))^2, from=1, to=n, col="darkred", lwd=2, add = T)

#2pi-------------
erd=function(x){
  if(x==1) return(1) else{
    y=x-2
    w=ceiling(x/y)*y
    while(y>=3){
      y=y-2
      w=ceiling(w/y)*y
    }
    return(w)
  }
}

