library("numbers")
library("pracma")
library("mpfr")
library("gmp")
library("igraph")
#create maheshvara pearls
n=50
s=sapply(0:n, function(x) paste(c(1, rep(0,x),1),collapse = ""))
s=as.bigz(s)

sapply(s, function(x) format(pow.bigz(x,2))) #square x

#factorization of s: takes long
fs=lapply(1:n, function(x) factorize(s[x]))
fs=lapply(fs, format)

#examine factors------------
te=log(unlist(lapply(fs, function(x) max(as.numeric(x)))))
plot(te, pch=16 )
abline(a = log(7), b=2)

#reading big list of above
library(readr)
maheshvara_pearls <- read_csv("R/Figures/Figures1/maheshvara_pearls.csv", 
                              col_names = FALSE, col_types = cols(X1 = col_integer(), 
                                                                  X2 = col_character()))

te=as.bigz(maheshvara_pearls$X2)[2:313]                              
ye=pow.bigz(10,(1:312))+1
ge=log10.bigz(te)/log10.bigz(ye)

par(mar=c(2,2,2,1), mgp=c(1,.3,0))
plot(x=log10.bigz(ye), y=log10.bigz(te), pch=16, col="chartreuse4",  xlab = "log10(f[n])", ylab = "log10(pmax)", main="greatest factor of f[n] vs f[n]")
abline(a = 0, b=max(ge), col="coral3", lty=2, lwd=1.25)
abline(a = 0, b=min(ge), col="coral3", lty=2, lwd=1.25)
abline(a = 0, b=median(ge), col="darkblue", lty=2, lwd=1.25)

lines(x=log10.bigz(ye), y=log10.bigz(te), col="cornflowerblue")


#table of frequencies
te=sort(table(unlist(fs)), decreasing = T)
knitr::kable(te,format = "pandoc")

which(unlist(lapply(1:n, function(x) "1409" %in% fs[[x]])))

forlist=list()
forlist$`11`=function(x) 2*x+1
forlist$`101`=function(x) 4*x+2
forlist$`7`=function(x) 6*x+3
forlist$`13`=function(x) 6*x+3
forlist$`73`=function(x) 8*x+4
forlist$`137`=function(x) 8*x+4
forlist$`9091`=function(x) 10*x+5
forlist$`9901`=function(x) 12*x+6
forlist$`909091`=function(x) 14*x+7
forlist$`17`=function(x) 16*x+8
forlist$`5882353`=function(x) 16*x+8
forlist$`19`=function(x) 18*x+9
forlist$`52579`=function(x) 18*x+9
forlist$`3541`=function(x) 20*x+10
forlist$`27961`=function(x) 20*x+10
forlist$`23`=function(x) 22*x+11
forlist$`4093`=function(x) 22*x+11
forlist$`8779`=function(x) 22*x+11
forlist$`99990001`=function(x) 24*x+12
forlist$`859`=function(x) 26*x+13
forlist$`1058313049`=function(x) 26*x+13
forlist$`29`=function(x) 28*x+14
forlist$`281`=function(x) 28*x+14
forlist$`121499449`=function(x) 28*x+14
forlist$`211`=function(x) 30*x+15
forlist$`241`=function(x) 30*x+15
forlist$`2161`=function(x) 30*x+15
forlist$`353`=function(x) 32*x+16
forlist$`449`=function(x) 32*x+16
forlist$`641`=function(x) 32*x+16
forlist$`1409`=function(x) 32*x+16
forlist$`69857`=function(x) 32*x+16

#reconstruct factors
ind.list=vector(mode = "list", length = length(forlist))
for(j in 1:length(forlist)){
  te=paste(body(forlist[[j]]), collapse = "")
  te=unlist(strsplit(te, "\\*|\\+"))
  te=as.numeric(te[2])
  f=forlist[[j]]
  #[[j]]=f(0:floor(50/te)) # till 50
  ind.list[[j]]=f(0:3) #4 terms
  
}
names(ind.list)=names(forlist)
lapply(ind.list, function(x) print(paste(x, collapse = ", ")))

#factor network--------------
l=lengths(fs)
l=sum(sapply(l, function(x) sum(1:x)-x))

netmat=array(data=NA, dim=c(l,2))
k=1
for (j in 1:length(fs)) {
  if(length(fs[[j]])<2) next
  else{
    te=lapply(1:(length(fs[[j]])-1), function(y) t(sapply((y+1):length(fs[[j]]), function(x) c(fs[[j]][y],fs[[j]][x]))))
    te=do.call(what = rbind,args = te)
    netmat[k:(k+nrow(te)-1),]=te
    k=k+nrow(te)
  }
}

n.s=sort(table(netmat), decreasing = T)
netmat.ass=sapply(1:l, function(x) paste(netmat[x,],collapse = ","))
e.w = sort(table(netmat.ass), decreasing = T)
e.list=names(e.w)
e.list=sapply(e.list,  function(x) strsplit(x =x,split = ","))
e.list= do.call(what=rbind, args = e.list)
rownames(e.list)=c()

g=graph_from_data_frame(data.frame(e.list), vertices = names(n.s), directed = F)
V(g)$size=((n.s)-min(n.s))/(max(n.s)-min(n.s))*20+1
V(g)$color=rainbow(11)[((n.s)-min(n.s))/(max(n.s)-min(n.s))*10+1]
E(g)$width=((e.w)-min(e.w))/(max(e.w)-min(e.w))*15+1
col=colorRampPalette(colors = c("gray40","blue","green"))
E(g)$color=col(10)[e.w]
((e.w)-min(e.w))/(max(e.w)-min(e.w))*5+1
tkplot(g, layout=norm_coords(layout_components(g,layout = layout_with_kk), xmin=1, xmax=10, ymin=1, ymax=10), vertex.frame.color=NA, vertex.label.color="black", vertex.label.dist=1)

plot.igraph(g, layout=norm_coords(layout_components(g,layout = layout_with_fr),xmin = 1,xmax = 20, ymin = 1,ymax = 15))

#separating families
fams= components(g)
te=names(fams$membership[fams$membership==6])
ye=unique(unlist(fs))
ge=ye[ye %in% te]
