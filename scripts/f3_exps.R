library("readr")
library("dplyr")
library("cluster")
library("dendextend")

setwd("R/Dataexperiments/data/")
f3m = read_csv("f3_matrix_preliminary.csv")
te=f3m$X1
f3m=f3m[,-1]
f3m=data.frame(f3m)
rownames(f3m)=te

ye=-log(f3m)

ye= daisy(f3m, metric = "manhattan")
ye=diana(ye)
ye=as.dendrogram(ye)
ye=set(ye, "labels_cex", .5)
ye=set(ye, "branches_k_color", k = 12)
ye=set(ye, "branches_lwd", 2)
pdf(file= "~/cutting_block/R/data_analysis/Figures/poptree.pdf", width = 12, height = 40)
par(mar=c(1,1,1,5))
plot(ye, horiz=T, axes=F, xlab="", ylab = "", main="position cluster dendrogram", cex=.1) #horizontal
dev.off()

#checking for one population
te=f3m["Loebanr_IA",]
te=te[order(te,decreasing = T)]
head(t(te),20)


#distance tree from average PCA
g25a= read_csv("~/R/Dataexperiments/data/Global_25_PCA_pop_averages.csv")
te=g25a$Pop
g25a=g25a[,-1]
g25a=data.frame(g25a)
rownames(g25a)=te

ye= daisy(g25a, metric = "manhattan")
ye=diana(ye)
ye=as.dendrogram(ye)
ye=set(ye, "labels_cex", .5)
ye=set(ye, "branches_k_color", k = 16)
ye=set(ye, "branches_lwd", 2)
pdf(file= "poptree_PCA.pdf", width = 12, height = 60)
par(mar=c(1,1,1,5))
plot(ye, horiz=T, axes=F, xlab="", ylab = "", main="position cluster dendrogram", cex=.1) #horizontal
dev.off()

#as network
#distance matrix
ye= daisy(g25a, metric = "manhattan")
ye=as.numeric(ye)
dmat=data.frame(t(combn(row.names(g25a),2)), ye)
dmat$X1=as.character(dmat$X1)
dmat$X2=as.character(dmat$X2)
colnames(dmat)=c("pop1","pop2","dist")
pop=unique(dmat$pop1)

te=lapply(pop, function(x) dmat[dmat$pop1==x,])
te=lapply(te, function(x) head(x[order(x$dist),],2))
popg=do.call("rbind",te)

library("igraph")
g = graph_from_data_frame(popg[,c(1,2)], directed=F)
tkplot(g, vertex.color=rainbow(length(V(g))),vertex.size=4, vertex.label.color="black")


#ind pop 
popind= read_csv("popind.txt")
popind=as.vector(popind$Abdul_Hosein_N)
#network for Indian populations only
te=lapply(popind, function(x) dmat[dmat$pop1==x,])
te=do.call("rbind",te)
te=lapply(popind, function(x) te[te$pop2==x,])
te=lapply(te, function(x) head(x[order(x$dist),],5))
popg=do.call("rbind",te)
g = graph_from_data_frame(popg[,c(1,2)], directed=F)
tkplot(g, layout=layout_nicely, vertex.color=rainbow(length(V(g))),vertex.size=4, vertex.label.color="black")

te=tk_coords(5)
# pdf(file= "popnet.pdf", width = 30, height = 20)
png(file= "popnet.png", width = 1800, height = 1200)
par(mar=c(.5,.5,.5,.5))
plot(g, layout=te, vertex.color=rainbow(length(V(g))),vertex.size=2, vertex.label.color="black", vertex.label.dist=0, vertex.label.cex=1.5, asp=0,edge.curved=F)
dev.off()

#phylogenetic
library("phangorn")
ye= dist(g25a, method = "maximum")
te=NJ(ye)
pdf(file= "poptree_PCA_NJ_max.pdf", width = 12, height = 70)
par(mar=c(.1,.1,.1,.1))
plot(te,cex=.6, font=1, edge.width = 1.5, edge.color = "darkblue", tip.color = rainbow(length(pops))) 
dev.off()

te=neighborNet(ye)
pdf(file= "poptree_PCA_NNET.pdf", width = 40, height = 40)
par(mar=c(.1,.1,.1,.1))
plot(te,cex=.6, font=1, type=u, edge.color = "darkblue", tip.color = rainbow(length(pops))) 
dev.off()
