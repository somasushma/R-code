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

#examine factors
te=sort(table(unlist(fs)), decreasing = T)
knitr::kable(te,format = "pandoc")

which(unlist(lapply(1:n, function(x) 909091 %in% fs[[x]])))

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
