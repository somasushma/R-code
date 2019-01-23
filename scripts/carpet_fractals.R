rule.list=list()
rule.list$haferman0=c(1,1,1,1,1,1,1,1,1)
rule.list$haferman1=c(0,1,0,1,0,1,0,1,0)
rule.list$sierpinski0=c(0,0,0,0,0,0,0,0,0)
rule.list$sierpinski1=c(1,1,1,1,0,1,1,1,1)
rule.list$box0=c(0,1,0,1,0,1,0,1,0)
rule.list$box1=c(1,1,1,1,1,1,1,1,1)
rule.list$sierbox0=c(0,1,0,1,0,1,0,1,0)
rule.list$sierbox1=c(1,1,1,1,0,1,1,1,1)
rule.list$boxdot0=c(0,0,0,0,1,0,0,0,0)
rule.list$boxdot1=c(1,1,1,1,0,1,1,1,1)
rule.list$sq3_0=c(0,0,0,1,0,1,0,0,0)
rule.list$sq3_1=c(1,0,1,0,1,0,1,0,1)

rule.list$var0=c(0,1,0,0,1,0,0,1,0)
rule.list$var1=c(1,0,0,1,1,1,0,0,1)

ini=c(0)
n=5
for (j in 1:n) {
  mlist=as.list(ini)
  mlist=lapply(mlist, function(x) if(x==0) matrix(data =rule.list$var0, nrow = 3, byrow = T) else matrix(data =rule.list$var1, nrow = 3, byrow = T))
  m=sqrt(length(mlist))
  mlist1=list()
  for (j in 1:m) {
    mlist1[[j]]=do.call(cbind, mlist[((j-1)*m+1):(m*j)])
  }
  mlist1=do.call(rbind, mlist1)
  ini=unlist(mlist1)
}
par(pty="s", mar=c(.5,.5,.5,.5))
image(mlist1, asp=1, col=c("darkblue", "darkturquoise"), axes=F)
#image(mlist1, asp=1, col=c("white", "black"), axes=F)