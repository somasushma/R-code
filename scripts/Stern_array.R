library("pracma")
library("numbers")
library("MASS")
library("xtable")

n=15
f1=vector("list", n)
f2=vector("list", n)
f1[[1]]=c(1,1)
f2[[1]]=c(0,1)

for(j in 2:n){
  l=2*length(f1[[j-1]])-1
  f1[[j]]=rep(0,l)
  f2[[j]]=rep(0,l)
  
  f1[[j]][seq(1,l,2)]=f1[[j-1]]
  f1[[j]][seq(2,(l-1),2)]=sapply(1:(length(f1[[j-1]])-1), function(x) f1[[j-1]][x]+f1[[j-1]][x+1])
  f2[[j]][seq(1,l,2)]=f2[[j-1]]
  f2[[j]][seq(2,(l-1),2)]=sapply(1:(length(f2[[j-1]])-1), function(x) f2[[j-1]][x]+f2[[j-1]][x+1])  
}

#plot as points
par(mfrow=c(1,1))
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(unlist(f1), pch=16, cex=.25, col=c("dodgerblue4","darkred"), xlab="n",ylab= expression(paste(f,"[n]")), main=expression(paste(f, " Stern denominators")))

#as continuous
plot(unlist(f1), type="l", cex=.25, col=c("dodgerblue4","darkred"), xlab="n",ylab= expression(paste(f,"[n]")), main=expression(paste(f, " Stern numerators")))

#last cycle of denominators
plot(unlist(f1[n]), type="l", cex=.25, col=c("dodgerblue4","darkred"), xlab="n",ylab= expression(paste(f,"[n]")), main=substitute(paste("Stern denominators, cycle ", n), list(n=n)))

#plot Stern fractions
plot(unlist(f2[n])/unlist(f1[n]), type="l", cex=.25, col=c("dodgerblue4","darkred"), lwd=2, xlab="n",ylab= expression(paste(f,"[n]")), main=expression(paste(f, " Stern fraction sequence")))
