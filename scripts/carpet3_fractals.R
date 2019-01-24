rule.list=list()
rule.list$box1_0=c(1,0,1,0,2,0,1,0,1)
rule.list$box1_1=c(2,1,2,1,2,1,2,1,2)
rule.list$box1_2=c(0,0,0,0,0,0,0,0,0)

rule.list$box2_0=c(1,1,1,1,2,1,1,1,1)
rule.list$box2_1=c(2,1,2,1,2,1,2,1,2)
rule.list$box2_2=c(0,0,0,0,0,0,0,0,0)

rule.list$box3_0=c(1,1,1,1,2,1,1,1,1)
rule.list$box3_1=c(2,1,2,1,2,1,2,1,2)
rule.list$box3_2=c(0,0,0,0,1,0,0,0,0)

rule.list$box4_0=c(2,1,2,1,0,1,2,1,2)
rule.list$box4_1=c(0,0,0,0,1,0,0,0,0)
rule.list$box4_2=c(2,2,2,2,0,2,2,2,2)

rule.list$box5_0=c(1,1,1,1,1,1,1,1,1)
rule.list$box5_0=c(1,2,1,2,1,2,1,2,1)
rule.list$box5_0=c(0,1,0,1,0,1,0,1,0)

rule.list$box6_0=c(1,1,1,1,0,1,1,1,1)
rule.list$box6_1=c(2,2,2,2,1,2,2,2,2)
rule.list$box6_2=c(2,0,2,0,2,0,2,0,2)

rule.list$var0=c(1,1,1,1,0,1,1,1,1)
rule.list$var1=c(2,1,2,1,1,1,2,1,2)
rule.list$var2=c(0,0,0,0,2,0,0,0,0)

ini=c(2)
n=5
for (j in 1:n) {
  mlist=as.list(ini)
  mlist=lapply(mlist, function(x) if(x==0) matrix(data =rule.list$var0, nrow = 3, byrow = T) else if(x==1) matrix(data =rule.list$var1, nrow = 3, byrow = T) else matrix(data =rule.list$var2, nrow = 3, byrow = T))
  m=sqrt(length(mlist))
  mlist1=list()
  for (j in 1:m) {
    mlist1[[j]]=do.call(cbind, mlist[((j-1)*m+1):(m*j)])
  }
  mlist1=do.call(rbind, mlist1)
  ini=unlist(mlist1)
}
par(pty="s", mar=c(.5,.5,.5,.5))
image(mlist1, asp=1, axes=F, col=c("darkturquoise", "ghostwhite","red4"))
#image(mlist1, asp=1, axes=F, col=c("darkred","black",  "cyan3"))
