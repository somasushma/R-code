phi=1/2+sqrt(5)/2

e=exp(1)

Catalan=0.91596559417721901505460351493238411077414937428167213426649811962176301977625476947935651292611510624857442261919

RamSold=1.45136923488338105028396848589202744949303228364801586309300455766242559575451783565953135771108682884704075157097

t=1/3*(1+(19-3*sqrt(33))^(1/3)+(19+3*sqrt(33))^(1/3))

#division cycle---------
n=200
k=digamma(1)^2
s=sapply(1:n, function(y) sum(sapply(1:y, function(x) (-1)^(floor(x*sqrt(k)))/x)))
plot(s, type="o", pch=16, cex=1/3, xlab = "n", ylab="s", main = bquote("k="~.(as.character(fractions(k,10)))~"; m="~.(round(mean(tail(s,50)),5))))
abline(h=mean(tail(s,50)), col="blue", lty=2)


#simple cycle-----------
kbox=c(sqrt(2), phi, (3+sqrt(13))/2, sqrt(3), 2^(1/3),Re(polyroot(c(-1,-1,-1,1))[3]) , Re(polyroot(c(-1,0,-1,1))[3]), Re(polyroot(c(-1,1,0,1))[1]), log(2), -digamma(1), e, pi, pi^2/6, atan(2), atan(3),  sin(2*pi/7), Catalan, RamSold)

kbox2=c(sin(pi/17), sin(pi/8), sin(pi/7), sin(pi/5), sin(pi/4), sin(pi/3))

slist=vector(mode="list", length = length(kbox2))

names(slist)=c("sqrt(2)","phi", "beta", "sqrt(3)", "2^{1/3}", "tau", "N[c]", "H[c]", "log(2)", "gamma", "e", "pi", "pi^2/6", "atan(2)", "atan(3)", "sin(2*pi/7)",  "Catalan", "RamSold")

names(slist)=c(expression(sin(pi/17)), expression(sin(pi/8)), expression(sin(pi/7)), expression(sin(pi/5)), expression(sin(pi/4)), expression(sin(pi/3)))

for (j in 1:length(kbox2)) {
  n=1000
  k=kbox2[j]
  s=sapply(1:n, function(y) sum(sapply(1:y, function(x) (-1)^(floor(x*k)))))
  slist[[j]]=s 
}


par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
for (j in 1:length(kbox2)) {
  plot(slist[[j]], type="l", lwd=.25, xlab = "n", ylab="s", main = parse(text = paste0('k == ',  names(slist[j]))))  
}

dev.copy(pdf, file="~/R/Figures/Figures1/irrational_walk.pdf", width=11, height=8)
dev.off()

dev.copy(png, file="~/R/Figures/Figures1/irrational_walk.png", width=11, height=11*1/1.5, unit="in", res=1200)
dev.off()


#angular turn--------
for (k in 1:length(slist)) {
xbox=rep(NA,n)
ybox=rep(NA,n)
t=pi/3
x=0; y=0
l=0
for (j in 1:n) {
  x=x+cos(t*slist[[k]][j]) -> xbox[j]
  y=y+sin(t*slist[[k]][j]) -> ybox[j]
  l=l+sqrt(x^2+y^2)
}

par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
plot(xbox,ybox, type="l", lwd=1, main=parse(text = paste0('k == ',  names(slist[k]), '~"; "' ,'~l==', round(l/n))), xlab="x", ylab="y", asp=1)
}

dev.copy(pdf, file="~/R/Figures/Figures1/irrational_walk_angle2.pdf", width=11, height=8)
dev.off()

dev.copy(png, file="~/R/Figures/Figures1/irrational_walk_angle2.png", width=11, height=8, unit="in", res=1200)
dev.off()

#fractions-----------
fbox=vector(mode="list", length = length(kbox2))
for (j in 1:length(kbox2)) {
fbox[[j]]=  sapply(1:10, function(x) print(fractions(kbox2[j], x)))
f=strsplit(fbox[[j]],"/")
f=as.numeric(lapply(f, function(x) x[2]))

plot(f, type="h", lwd=3, log = "y", main = paste0("d= ", max(f[which(f<n)])/n))
}


#pi/2 curlicue like---

for (j in 1:length(kbox2)) {
  n=1000
  k=kbox2[j]
  s=sapply(1:n, function(y) sum(sapply(1:y, function(x) x*pi*2*k-x*pi*2)))
  slist[[j]]=s 
}

for (k in 1:length(slist)) {
xbox=rep(NA,n)
ybox=rep(NA,n)
x=0; y=0
for (j in 1:n) {
  x=x+cos(slist[[k]][j]) -> xbox[j]
  y=y+sin(slist[[k]][j]) -> ybox[j]
}
par(pty="m", mar=c(2.5,2.5,2.5,1), mgp=c(1.1,.3,0))
plot(xbox,ybox, type="l", lwd=1, main=parse(text = paste0('k == ',  names(slist[k]))), xlab="x", ylab="y", asp=1)
}