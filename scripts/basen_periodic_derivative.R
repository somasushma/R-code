#convert from base 10 to base x
nnary=function(x,y){
  if(y==0) return(0)
  else {
    k=y
    b=c()
    while(k !=0){
      b=c(b, k %% x)
      k=floor(k/x)
    }
    return(rev(b))
  }
}

l=5 #base
pow=6
n=l^pow
f1=sapply(1:n, function(x) nnary(l,x)) #base n

#make periodic derivative of n
f2=vector(n,mode = "list")
for (j in 1:length(f1)) {
  if(length(f1[[j]])==1){
    f2[[j]]=(f1[[j]]+f1[[j]]) %% l
  } else {
    f2[[j]]=c(sapply(1:(length(f1[[j]])-1), function(x) (f1[[j]][x]+f1[[j]][x+1]) %% l), ((tail(f1[[j]],1)+head(f1[[j]],1)) %% l))
  }
}

#periodic derivative as matrix plot compared with n-base numbers
m=lengths(tail(f2,1))
pm=sapply(f2, function(x) c(rep(0, m - length(x)), x))
pm=t(pm)
bm=sapply(f1, function(x) c(rep(0, m - length(x)), x))
bm=t(bm)
pm=cbind(pm,bm)
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
image(pm, asp=(2*m)/n, col = topo.colors(l), axes=F)

#base b to base 10 converter
bto10=function(x,b){
  l=0
  while(x>2){
    k=b^(floor(log10(x)))
    y=as.numeric(paste(nnary(b,k), collapse = ""))
    x=x-y
    l=l+k
  }
  return(l+x)
}

f3=unlist(lapply(f2, function(x) as.numeric(paste(x, collapse = "")))) #convert to number
f3=sapply(f3, function(x) bto10(x,l))
#plot
par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f3, type="n", col=c("#e41a1c","#377eb8", "#4daf4a"), xlab="n",ylab= substitute(paste(f,"[n]")))
abline(v=sapply(1:pow, function(x) l^x), h=0, col="blue", lty=3)
abline(h=sapply(1:pow, function(x) l^x), col="blue", lty=3)
points(f3, type="l", col=c("#e41a1c","#377eb8", "#4daf4a"), xlab="n",ylab= substitute(paste(f,"[n]")))

#plot as square
par(pty="s", mar=c(2,2,1,1), mgp=c(1,.35,0))
plot(f3, type="n", col=c("#e41a1c","#377eb8", "#4daf4a"), xlab="n",ylab= substitute(paste(f,"[n]")))
abline(v=sapply(1:pow, function(x) l^x), h=0, col="blue", lty=3)
abline(h=sapply(1:pow, function(x) l^x), col="blue", lty=3)
points(f3, type="l", lwd=2, col=c("#e41a1c","#377eb8", "#4daf4a"), xlab="n",ylab= substitute(paste(f,"[n]")))

#minima
which(f3==0 | f3==1 | f3==2 |f3==3) #locating minima
l^(1:pow)

consmin=(l-2)/(l^2-1) #direct formula for first minimum constant

#k=2-(min_n1+1)/base^(n-1)
ceiling(l^(1:pow)*(2-consmin)-1) #recapitulating first min

#maxima
consmax=2/(l+1) #formula for first maximum constant
ceiling(l^(1:pow)*(2-consmax)-1) #recapitulating first max