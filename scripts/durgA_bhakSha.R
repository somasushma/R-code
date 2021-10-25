#for (t in 30:(30+35)) {
  n=400
  m=7
  r=1
  k=0
  j=n
  while(j > 1){
    if(j >= m){
      j=j-round(j/m,0)
      k=k+1 } else {
        j=j-1
        k=k+round(m/j,0)
      }
     print(j)
  }
  k=k+1
 print(c(n,k))
  
  circ=rep(1,n*k)
  j=1
  k=1
  while(j < length(circ)){
    s=sum(circ[k:j])
    if(s==m){
      circ[j]=0
      k=j+1
    }
    if(j %% n == 0){
      circ[(j+1):(j+n)]=circ[((j-n)+1):j]
    }
    if(sum(circ[(j+1):(j+n)])==r) break
    j=j+1
  }
  pm=matrix(circ, ncol=n, byrow = T)
 
   if(length(which(apply(pm, 1, function(x) all(x==1))))> 0) {
     pm=pm[-which(apply(pm, 1, function(x) all(x==1))),]
}
 pm=head(pm,-1)
  
  # prmatrix(pm,rowlab = rep("", nrow(pm)), collab = rep("", ncol(pm)))
  srv=apply(pm, 2, sum )
  srv=which(srv==max(srv))
  par(mar=c(.1,.1,2,.1))
  image(t(pm), asp=nrow(pm)/ncol(pm), col=c("#00ad96", "#001c6e"), axes=F)
  title(main=bquote(n==.(n)~","~m==.(m)~","~s==.(srv)))
  
#  text(x=.5, y=1.2, labels = bquote(n==.(n)~","~m==.(m)~","~s==.(srv)), cex=2)
  
  # par(mar=c(2,2,2,1), mgp=c(1.2,.5,0))
  # plot(srv, type="l")
  
  
#}
