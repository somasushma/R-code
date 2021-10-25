library("numbers")
library("pracma")

#fonts
windowsFonts(f1 = windowsFont("Constantia"),
             f2 = windowsFont("MS Gothic"),
             f3 = windowsFont("Cambria"))

#9 term convergence----
n=100
f=rep(0,n)
f[1]=8
f[2]=1

for (j in 2:n) {
  f[j+1]=abs(f[j])-f[j-1]
}

#5 term convergence----
n=300
f=rep(0,n)
f[1]=1
f[2]=1
f[3]=1
for (j in 3:n) {
  f[j+1]=(1*f[j]+1)/(1*f[j-1])
}
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(f, type = "o", pch=16, cex=.5, main=paste0("f"), col="darkblue", lty=2, asp=15)

abline(h=5, col="brown")

#cubic convergence------------
n=100
f=rep(0,n)
f[1]=1
for (j in 2:n) {
  f[j]=(f[j-1]+7/27)^(1/3)
}

#kaprekar years---------
te=c(0:9)
ye=c(0:9)
for (j in c(1:3)) {
  te=as.vector(outer(te, ye,FUN = "paste0"))
}

kapre=rep(NA, length(te))
for (j in 1:length(te)) {
ye=as.numeric(unlist(strsplit(te[j],"")))
ge=sum(as.numeric(paste0(ye[1],ye[2])), as.numeric(paste0(ye[3],ye[4])))
if(ge==as.numeric(paste0(ye[2],ye[3]))) kapre[j] = T else kapre[j] = F

}

kapre=sort(as.numeric(te[which(kapre)]))

for (j in 1:20) {
 print(paste(floor(sqrt(j)+sqrt(j+1)+sqrt(j+2)), floor(sqrt(9*j+8)), collapse = " ")) 
}

#5 term iterate ------
n=9973
cnt=n-1
xbox=rep(NA, cnt)
for (m in 2:cnt) {
theta=.1
a=b=2*sin(theta)
c=2
d=n-m
e=m

xlist=vector(mode = "list", length = cnt)
xlist[[1]] =  c(a, b, c, d, e)

for (j in 2:cnt) {
if(d>e && e> 0){
  b=xlist[[j-1]][1]*xlist[[j-1]][2]-xlist[[j-1]][3]
  c=xlist[[j-1]][2]
  d=xlist[[j-1]][4]-xlist[[j-1]][5]
}  else {
  a=xlist[[j-1]][1]*xlist[[j-1]][2]-xlist[[j-1]][3]
  c=xlist[[j-1]][1]
  e=xlist[[j-1]][5]-xlist[[j-1]][4]
}
  xlist[[j]]=c(a,b,c,d,e)
  if(e==0) break
}
xbox[m]=length(which(lengths(xlist) !=0))
}

par(mar=c(2,2,2,2), mgp=c(1.1, .4, 0))
plot(x=2:cnt, y=tail(xbox, -1), type = "h", xlab = "m", ylab = "iterations", family = "f3",asp=1)
