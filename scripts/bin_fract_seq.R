n=2^15
f=rep(NA,n)
f[1]=1
f[2]=3
for(j in 3:n){
  if(floor(j/2)==j/2){ f[j] = 3*f[j/2]
  } else{
    f[j]=1*f[(j-1)/2]+2*f[(j+1)/2]
  }
}

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot(f, type="l", col="darkblue", xlab="n", ylab="f[n]")
curve(3^(log2(x)),from = 2, to= 2^15, add=T, lwd=1.5, lty=2, col="hotpink")
