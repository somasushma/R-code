library(numbers)
library(pracma)
n=600
f.den=vector(mode = "list", length = n)
f.den[[1]]=c(1,1)
for (j in 2:n) {
  ye=sapply(1:length(f.den[[j-1]]), function(x) c(f.den[[j-1]][x],f.den[[j-1]][x]+f.den[[j-1]][x+1]))
  ye[2,][which(ye[2,]!=j)]=NA
  ye=as.vector(ye)
  f.den[[j]]=c(na.omit(ye)) 
}

vgcd=Vectorize(gcd)
gcd.list=sapply(1:n, function(x) c(1:floor(x/2))[which(vgcd(1:floor(x/2),x)==1)])
te=sapply(1:n, function(x) x-gcd.list[[x]])
gcd.list=sapply(1:n, function(x) unique(sort(c(gcd.list[[x]],te[[x]]))))
f.num=vector(mode = "list", length = n)
for (j in 1:n) {
  te=unique(f.den[[j]])
  ye=unlist(sapply(te, function(x) which(f.den[[j]]==x)))
  f.num[[j]]=unlist(gcd.list[te])
  f.num[[j]][ye]=f.num[[j]]
}

#growth of sum  of denominators and numerators
f.den.s=unlist(lapply(f.den, sum))
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(f.den.s, type="l")
points((1:n)^3/pi*16/25, type="l", col="red")

f.num.s=unlist(lapply(f.num, sum))
points(f.num.s, type="l")
points((1:n)^3/pi*8/25, type="l", col="darkgreen")

#number of fractions
n.fracs= lengths(f.den)
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(n.fracs, type="l")
points((1:n)^2/pi^2*3, type="l", col="darkgreen")

#sum of farey fracs (= n.fracs/2)
frac.s=unlist(lapply(1:n, function(x) sum(f.num[[x]]/f.den[[x]])))
plot(frac.s, type="l")
points((1:n)^2/pi^2*3/2, type="l", col="darkgreen")

# number of n in f.den sequence
n_in_f=unlist(lapply(1:length(f.den), function(x) length(which(f.den[[x]]==x))))
plot(n_in_f, type="l")
abline(b=6/pi^2, a=0)
plot(n_in_f-6/pi^2*(1:n), type="l")

#farey burst
k=15
r.list=list(c(1,1), c(-1,1), c(-1,-1), c(1,-1))
plot(0,0, type = "n", xlim=c(-max(f.den[[k]]), max(f.den[[k]])), ylim = c(-max(f.den[[k]]), max(f.den[[k]])), asp=1)
for (j in 1:4) {
  x=f.den[[k]]*r.list[[j]][1]
  y=f.num[[k]]*r.list[[j]][2]
  points(x,y, pch=16, cex=.5, type = "l", asp=1, col="gray60")
  points(x,y, pch=16, cex=.5, asp=1, col="darkred")
  
  x=f.num[[k]]*r.list[[j]][1]
  y=f.den[[k]]*r.list[[j]][2]
  points(x,y, pch=16, cex=.5, type = "l", asp=1, col="gray60")
  points(x,y, pch=16, cex=.5, asp=1, col="darkred")
}

#farey den vs farey
k=100
plot(f.num[[k]]/f.den[[k]], f.den[[k]], pch=16, lwd=2, col="darkgreen")

#inversion of above
k=300
par(pty="m", mar=c(2,2,2,1), mgp=c(1, .3, 0))
plot(f.num[[k]]/f.den[[k]], 2*max(f.den[[k]])-f.den[[k]], pch=16, lwd=2, col="darkgreen", ylim = c(min(f.den[[k]]), 2*max(f.den[[k]])), xlab = bquote(F), ylab = bquote(F[den]), main = bquote(k==.(k)))
points(f.num[[k]]/f.den[[k]], f.den[[k]], pch=16, lwd=2, col="darkgreen")

#farey den reciprocal vs farey
k=15
plot(f.num[[k]]/f.den[[k]], 1/f.den[[k]], type = "h", lwd=2, col="darkred", asp=1)


#hf function
hf=function(x,k){
  if(x==1){ t=.5
  } else{
  te=f.num[[k]]/f.den[[k]]
  a=tail(which(te <= (x)),1)
  t=(f.den[[k]][a]+f.den[[k]][a+1])/2*(x-(f.num[[k]][a+1]+f.num[[k]][a])/(f.den[[k]][a+1]+f.den[[k]][a]))
  }
  return(t)
}

k=300
te=sapply(seq(0,1,.0001), function(x) hf(x,k))
plot(seq(0,1,.0001), te, type="l", col="darkblue", lwd=1.5, xlab="x", ylab="y", main=bquote(y==h(x,k)~","~k==.(k)))

#den vs num and rev
k=100
plot(f.num[[k]]*1i+f.den[[k]], pch=16, col="darkcyan", asp = 1, xlim=c(0,100), ylim=c(0,100), axes=F, xlab = "", ylab = "")
points(f.num[[k]]+f.den[[k]]*1i, pch=16, col="darkcyan", asp= 1)

#Farey product constant: is it related to brun2
p.frac=unlist(lapply(1:25, function(x) prod(tail(f.num[[x]],-1))/prod(tail(f.den[[x]],-1))))
pfc=sum(p.frac)

# A certain fractal transformation: mostly bad
k=50
te=(1i+f.num[[k]]^2)/(f.num[[k]]*f.den[[k]])
plot(te[-c(1,length(f.num[[k]]))], pch=16, col="darkcyan",  type="l")


#farey circle: quite bad
k=40
par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .4, 0))
plot(0,0, type = "n", xlim=c(-k,k), ylim=c(-k,k), asp=1)

for (j in 1:k) {
  points(j*cos(f.num[[j]]/f.den[[j]]*2*pi), j*sin(f.num[[j]]/f.den[[j]]*2*pi), pch=16 , col="darkcyan")
}

#hf function radial
r=seq(0,1, length.out = 360)
k=300
r=sapply(r, function(x) hf(x,k))
plot(r*e^(1i*pi/180*(1:360)), type="l")

#hf trignometric chaos
x0=1/4
k=30
te=sin(f.den[[k]]*(asin(x0)))
plot(te, type="l", asp=25)

