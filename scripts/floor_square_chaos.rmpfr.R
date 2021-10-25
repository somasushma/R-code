library(Rmpfr)

phi=sqrt(5)/2+1/2
prec=3000
k=2
n=20000
x0=-mpfr(1464,prec)/mpfr(1000,prec) #x0

#functions
flist=list()
flist[[1]]=function(x) (sqrt(floor(x))+sqrt(x-floor(x)))/sqrt(x)
flist[[2]]=function(x) (floor(x)^mpfr(2, prec)+(x-floor(x))^mpfr(2, prec))/(mpfr(2, prec)*x)
flist[[3]]=function(x) (floor(x)^mpfr(2, prec)+(x-floor(x))^mpfr(2, prec))/(mpfr(3, prec)*x)

#constants
constants=list()
constants[[1]]=c(0,0,0,0)
constants[[2]]=c(-1,  2-2*sqrt(3), 3-3*sqrt(3), -2.5)
constants[[3]]=c(-1/3,  -1/phi, -2/phi, -5/3)

xbox=rep(NA,n)
x=x0
f=flist[[k]]
const=constants[[k]]

for (j in 1:n) {
  x=f(x) 
  xbox[j]=as.numeric(x)
}

par(mar=c(2,2,3,2), mgp=c(1.1, .4, 0))
plot(xbox[10:260], type="l", main = bquote(bgroup("(",bgroup("(",group(lfloor, x, rfloor),")")^2+bgroup("(",x-group(lfloor, x, rfloor),")")^2,")")/(.(k)*x)~";"~x[0]==.(as.numeric(x0))), xlab="n", ylab=bquote(x[n]))
points(xbox[10:260], pch=16, cex=.5, col="blue")
abline(h=const, lwd=2, col="darkgreen")

naman=paste0("~/R/Figures/Figures1/floor_square01", "a", k, "_ev.png")

dev.copy(png, file=naman, width=8, height=6, res=300, units="in")
dev.off()

hist(tail(xbox,-10), breaks = 500, col="lightblue", main= bquote(bgroup("(",bgroup("(",group(lfloor, x, rfloor),")")^2+bgroup("(",x-group(lfloor, x, rfloor),")")^2,")")/(.(k)*x)~";"~x[0]==.(as.numeric(x0))), xlab =bquote(x[n]) )
abline(v=const, lwd=2, col="darkgreen")
points(x=const[c(2,3)], y=c(0,0), pch=16, col="darkgreen")
box()

naman=paste0("~/R/Figures/Figures1/floor_square01", "a", k, "_hist.png")

dev.copy(png, file=naman, width=8, height=6, res=300, units="in")
dev.off()

#------------
xbox=seq(-2,-1, length.out = 500)
ybox=rep(NA,length(xbox))
m=1
for (x in xbox) {
  n=500
  for (j in 1:n) {
    x=f2(x) 
    
  }
  ybox[m]=x
  m=m+1
}

plot(xbox, ybox, type="l")
plot(head(tail(ybox, -1),-1), type="l")