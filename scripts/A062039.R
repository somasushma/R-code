#A062039
n=2000
f=rep(0,n)
f[1]=1
for (j in 1:(n-1)) {
  if(f[j]>j) {
    f[j+1]=f[j]-j
  } else {
    f[j+1]=f[j]+f[f[j]]
  }
}

par(pty="m", mar=c(2.2,2.2,2,1), mgp=c(1.2,.4,.0))
plot(f, type="h", col="darkgreen", xlab = "n", ylab="f[n]")

f1=unique(sapply(1:n, function(x) max(f[1:x])))
f2=match(x = f1,table = f)
points(f2,f1,pch=16, col="red", cex=.5)

abline(a=0, b=1, lty = 2)