#binary function
base2=function(x){
  r=x %% 2
  d= x-r
  b=0
  repeat{
    k=floor(log2(d))
    d=d-2^k
    b=b+10^k
    if(d==0) break
  }
  b=b+r
  return(b)
}

#----------------------
#make binary sequence
m=15
n=2^m+1
f1=sapply(0:n, function(x) base2(x))

#count 1s
ones=nchar(as.character(format(f1, scientific = F))) -nchar( gsub("1", "", as.character(format(f1, scientific = F)), fixed = T))

#count 0s
zeros=nchar(as.character(f1)) -nchar( gsub("0", "", as.character(f1), fixed = T))

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(ones, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]")

#2 power summatory function of 1s
f2=sapply(1:n, function(x) sum(2^(ones[1:(x-1)])))
f2=tail(f2,-1)

#2 power summatory function of 0s
f3=sapply(1:n, function(x) sum(2^(zeros[1:(x-1)])))
f3=tail(f3,-1)

#their difference
dffr=f2-f3

#----------------  
#plot 1s
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f2, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)
points(3^(log2(1:(n-1))), type="l", col="hotpink", lty=2, lwd=1.5)

#rectified 1s
plot(3^(log2(1:(n-1)))-f2, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)

#maxima point of each cycle
te=3^(log2(1:(n-1)))-f2
sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))/(2^(x+1)-2^x))
ye=sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))+(2^x))
abline(v=ye, col="hotpink", lwd=1.5,lty=2)
abline(h=0, col="gray", lwd=1.5, lty=2)

#division Harborth/Stolarsky
plot(f2/3^(log2(1:(n-1))), pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)

#minima of above
te=f2/(3^(log2(1:(n-1))))
ye=sapply(1:(m-1), function(x) min(te[2^x:2^(x+1)]))
abline(h=tail(ye,1), lty=2, lwd=2, col="hotpink")
abline(h=1, lty=2, lwd=1.5, col="gray")

#location of minima
sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==min(te[2^x:2^(x+1)]))/(2^(x+1)-2^x))
ye=sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==min(te[2^x:2^(x+1)]))+2^x)
abline(v=ye, col="chocolate3", lwd=1.5,lty=2)

#--------------------
#plot 0s
plot(f3, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)
points(3^(log2(1:n))*0.4564521, type="l", col="blue", lty=2, lwd=1.5)

#rectified 0s
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f3-3^(log2(1:(n-1)))*0.4564521, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)

#maxima point of each cycle
te=f3-3^(log2(1:(n-1)))*0.4564
sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))/(2^(x+1)-2^x))
ye=sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))+(2^x))
abline(v=ye, col="hotpink", lwd=1.5,lty=2)
abline(h=0, col="gray", lwd=1.5, lty=2)

#division 0s
plot(f3/(3^(log2(1:(n-1)))*0.4564521), pch=16, type = "l", ylim=c(.99,1.4), col="darkblue", xlab="n", ylab="f[n]", lwd=2)

#maxima of above
te=f3/(3^(log2(1:(n-1)))*0.4564521)
ye=sapply(1:(m-1), function(x) max(te[2^x:2^(x+1)]))
abline(h=tail(ye,1), lty=2, lwd=2, col="hotpink")
abline(h=1, lty=2, lwd=1.5, col="gray")

#location of minima
sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))/(2^(x+1)-2^x))
ye=sapply(1:(m-1), function(x) which(te[2^x:2^(x+1)]==max(te[2^x:2^(x+1)]))+2^x)
abline(v=ye, col="chocolate3", lwd=1.5,lty=2)

#---------------
#plot of both 0 and 1 functions
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f2, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)
points(3^(log2(1:(n-1))), type="l", col="hotpink", lty=2, lwd=1.5)

points(f3, pch=16, type = "l", col="darkgreen", xlab="n", ylab="f[n]", lwd=2)
points(3^(log2(1:n))*0.4564, type="l", col="hotpink", lty=2, lwd=1.5)

abline(v=2^(1:15), lty=2, lwd=1.5, col="chocolate3")
legend(x="topleft", legend = c("f0","f1"), col =c("darkgreen", "darkblue"), lwd = 2)

#-------------
#f2 from recurrence
n=2^15
f2r=rep(NA,n)
f2r[1]=1
f2r[2]=3
for(j in 3:n){
  if(floor(j/2)==j/2){ f2r[j] = 3*f2r[j/2]
  } else{
    f2r[j]=2*f2r[(j-1)/2]+1*f2r[(j+1)/2]
  }
}

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f2r, type="l", col="darkblue", xlab="n", ylab="f[n]")
curve(3^(log2(x)),from = 2, to= 2^15, add=T, lwd=1.5, lty=2, col="hotpink")


f3r=rep(NA,n)
f3r[1]=1
f3r[2]=3
for(j in 3:n){
  if(floor(j/2)==j/2){ f3r[j] = 3*f3r[j/2]
  } else{
    f3r[j]=1*f3r[(j-1)/2]+2*f3r[(j+1)/2]
  }
}
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f3r, type="l", col="darkblue", xlab="n", ylab="f[n]")
curve(3^(log2(x)),from = 2, to= 2^15, add=T, lwd=1.5, lty=2, col="hotpink")

#-----------
#plot difference
plot(dffr, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)
points(3^(log2(1:n))*0.5449817, type="l", col="hotpink", lty=2, lwd=1.5)

#rectified difference:
plot(3^(log2(1:(n-1)))*0.5449817-dffr, pch=16, type = "l", col="darkblue", xlab="n", ylab="f[n]", lwd=2)

#----------
#meru
m=8
meru=vector(mode = "list", m)
meru[[1]]=c(1)
for(j in 2:m){
  meru[[j]][1]=1
  meru[[j]][j]=1
  k=2
  while(k < j){
  meru[[j]][k]=meru[[j-1]][k-1]+meru[[j-1]][k]
  k=k+1
  }
}
te=matrix(data=rep(NA, m^2), byrow = T, nrow = m)

for (j in 1:m) {
  te[j,1:j]=meru[[j]]
}
meru=te
library("xtable")
print(xtable(meru,digits = 0), include.rownames=FALSE, include.colnames=F)