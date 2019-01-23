ini=c(1)
n=5
mlist1=list()
for (j in 1:n) {
mlist=as.list(ini)
mlist=lapply(mlist, function(x) if(x==1) matrix(data =c(0,1,0,1,0,1,0,1,0), nrow = 3, byrow = T) else matrix(data =c(1,1,1,1,1,1,1,1,1), nrow = 3, byrow = T))
ini=unlist(mlist)
}
n=sqrt(length(mlist))
for (j in 1:n) {
  mlist1[[j]]=do.call(cbind, mlist[((j-1)*n+1):(n*j)])
}
mlist2=do.call(rbind, mlist1)
image(mlist2, asp=1)