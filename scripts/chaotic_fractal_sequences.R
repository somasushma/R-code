#f1 subtraction
n=1030

f1=rep(NA,n)
f1[1]=1
f1[2]=1
for (j in 1:(n/2-1)) {
  f1[2*j+1]=f1[j]
  if(j == 1) { f1[2*j+2]=2*f1[j]
  } else{
    f1[2*j+2]=2*f1[j]-f1[j-1]
  }
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f1, type = "l", main=bquote("chaotic sequence"~f[1]), col="darkblue", lwd=1.5)

#f2 addition
n=1030
f2=rep(NA,n)
f2[1]=1
f2[2]=1
for (j in 1:(n/2-1)) {
  f2[2*j+1]=f2[j]
  if(j == 1) { f2[2*j+2]=2*f2[j]
  } else{
    f2[2*j+2]=2*f2[j]+f2[j-1]
  }
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f2, type = "l", main=bquote("chaotic sequence f2"), col="darkblue", lwd=1.5)

#successive record values: record values are attained at 2^k-2; 2^k*3-2, 2^k*3-4; 2^k*5-2
te=unique(unlist(sapply(1:n, function(x) which(f2[1:x]==max(f2[1:x])))))
cbind(te, f2[te])

#new highs: these are attained at 2^k-2 and 2^k*3-2. The ratio successive highs converge to sqrt(2). The ratio of successive points they occur alternate between 4/3 and 3/2
ye=c(sapply(1:10, function(x) 2^x), sapply(1:10, function(x) 2^x*3))-2
ye=tail(sort(ye), -1)
ye=cbind(ye, f2[ye])
ye=ye[!is.na(ye[,2]),]

#1s(minima) occur at 2^k-1; 2*k*3-1
te=which(f2==1)

#plot with max, min etc
plot(f2, type = "l", main=bquote("chaotic sequence f2"), col="darkblue", lwd=1.5)
points(ye[,1],ye[,2], col="red", pch=16)
points(2^(0:10), f2[2^(0:10)], col="darkgreen", pch=16)
points(which(f2==1), f2[which(f2==1)], col="darkblue", pch=16)

#f3 
n=1000
f3=rep(NA,n)
f3[1]=1
f3[2]=1
for (j in 1:(n/2-1)) {
  f3[2*j+1]=f3[j+1]
  if(j == 1) { f3[2*j+2]=2*f3[j]
  } else{
    f3[2*j+2]=2*f3[j]-f3[j-1]
  }
}

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f3, type = "l", main=bquote("chaotic sequence"~f[3]), col="darkblue", lwd=1.5)

#successive record values
te=unique(unlist(sapply(1:n, function(x) which(f3[1:x]==max(f3[1:x])))))
cbind(te, f3[te])
