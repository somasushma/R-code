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

l=4 #base
n=l^6
f1=sapply(1:n, function(x) nnary(l,x)) #base n

#make periodic derivative of n
f2=vector(n,mode = "list")
for (j in 1:length(f1)) {
  if(length(f1[[j]])==1){
    f2[[j]]=f1[[j]]
  } else {
    f2[[j]]=c(sapply(1:(length(f1[[j]])-1), function(x) (f1[[j]][x]+f1[[j]][x+1]) %% l), ((tail(f1[[j]],1)+head(f1[[j]],1)) %% l))
  }
}

#periodic derivative as matrix plot
m=lengths(tail(f2,1))
pm=sapply(f2, function(x) c(rep(0, m - length(x)), x))
pm=t(pm)

par(pty="m", mar=c(2,2,1,1), mgp=c(1,.35,0))
image(pm, asp=m/n, col = terrain.colors(5), axes=F)

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

plot(f3, type="l", col=c("#e41a1c","#377eb8", "#4daf4a"), xlab="n",ylab= substitute(paste(f,"[n]")))
abline(v=sapply(1:6, function(x) l^x), h=0, col="blue", lty=2)

