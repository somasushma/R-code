tbox=c(pi/6, pi/4, pi/3, pi/2, acos(2-3*phi/2), 2*pi/3)
names(tbox)=c("pi/6", "pi/4", "pi/3", "pi/2", "acos(2-3*phi/2)", "2*pi/3")
par(mfrow=c(2,3),   mar=c(2,2,2,2))
for (j in 1:length(tbox)) {
  t=tbox[j]
  trng=array(data = NA, dim = c(3,2),dimnames = list(c("A","B","C"), c("x", "y")))
  trng["A",]=c(0,0)
  trng["B",]=c(1,0)
  trng["C",]=c((4-2*cos(t))/3*cos(t),(4-2*cos(t))/3*sin(t))
  plot(trng, pch=16, col="darkblue", axes = F, xlab="", ylab="", asp = 1, xlim = c(-.8,1.2), main = parse(text = names(tbox)[j]))
  polygon(trng,col = "lightgreen",border = "darkgreen")
  text(trng,labels = c("A", "B", "C"), pos = c(2,4,3))
  d=as.vector(dist(trng))[c(1,3,2)]
  mids=rbind((trng["A",]+ trng["B",])/2,(trng["B",]+ trng["C",])/2, (trng["C",]+ trng["A",])/2)
  text(mids,labels = round(d,3),pos = c(1,4,2))
}

pic=1
naman=paste("~/R/Figures/Figures1/arith_trigs",pic , ".png", sep  = "")
pic=pic+1
dev.copy(png, file=naman, width=6.5, height=4, res=300, units="in")
dev.off()