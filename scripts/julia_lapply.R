julia=function(z, n, c){
  for(j in 1:n){
    z=z^2+c
    if(Mod(z) >16 || Mod(z)==Inf){
     break
    }
  }
  return(j)
}

cosjulia=function(z, n, c){
  for(j in 1:n){
    z=c*cos(z)
    if(Mod(z) >16 || Mod(z)==Inf){
      break
    }
  }
  return(j)
}

c3julia=function(z, n, c){
  for(j in 1:n){
    z=z^3-(-z)^(2.00001)+c
    if(Mod(z) >16 || Mod(z)==Inf){
      break
    }
  }
  return(j)
}

fracjulia=function(z, n, c){
  for(j in 1:n){
    z=z^(-2)+c
    if(Mod(z) >16 || Mod(z)==Inf){
      break
    }
  }
  return(j)
}

n=100
c=-2.95
b=pi
dv=.005
pm= as.vector(outer(X = seq(-b,b,dv), Y = seq(-b,b,dv)*1i,FUN = "+"))
cm=sapply(pm, function(x) cosjulia(x,n, c))
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), pty="s")
# plot(pm, pch=16, cex=.1, col=gray.colors(100)[cm], xlab = "x", ylab = "iy", main=paste("c=", c))

# col=colorRampPalette(c("cyan2", "black","darkblue"))
plot(pm, pch=16, cex=.1, col=col(100)[cm], xlab = "x", ylab = "iy", main=paste("c=", c))