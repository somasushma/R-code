#cube difference-----------
library(pander)

cbrt=function(x) x^(1/3) #define cuberoot function

te=sapply(1:300000, function(x) 3*x^2+3*x+1)
ye=list()
for (j in 1:length(te)) {
  ye[[j]]=te[j]-(1:floor(te[j]^(1/3)))^3
}

cb.diff=which(unlist(lapply(ye, function(x) any(round(x^(1/3)-round(x^(1/3),0), 10)==0))))
te=ye[cb.diff]
cbrts=lapply(1:length(te), function(x) which(round(cbrt(te[[x]])-round(cbrt(te[[x]]),0), 10)==0))

pander(cbind(cb.diff, unlist(lapply(cbrts, function(x) paste(x, collapse = ", ")))), row.names = F, col.names=c("c", "ns"), justify="rr") #table of cuberoots in relationship

cbind(table(unlist(cbrts))) #numbers of appearances of each

#prepare pairs for plotting
te=list()
l=1
for(j in 1:length(cbrts)){
  if(length(cbrts[[j]])>2){
  te[[l]]=c(cbrts[[j]][1], cbrts[[j]][4])
  l=l+1
  te[[l]]=c(cbrts[[j]][2], cbrts[[j]][3])
  l=l+1
  } else {
    te[[l]]=c(cbrts[[j]][1], cbrts[[j]][2])
    l=l+1
    }
  }
te=do.call("rbind", te)

par(pty="m", mar=c(2,2,2,1), mgp=c(1.1, .3, 0))
plot(te, pch=16, col="dodgerblue4")

curve(1*x, from=0,to=5000, add = T, col="darkgreen", lty=3) #y=x line

x=1:50; y= 3*x^2+2*x+1 # every number
points(x, y, pch=16, col="red")

a=(1:50); x=c(3*a^2, 3*a^2); y=c(6*a^2 -3*a +1, 6*a^2 +3*a +1) #Japanese points
points(x, y, pch=16, col="darkmagenta") 

#ellipse 1.1
x=c(9,15,51,57)
y=c(58,64,82,82)
points(x, y, pch=16, col="deeppink1") 

#ellipse 1.2
x=c(16,22,58,64)
y=c(51,57,75,75)
points(x, y, pch=16, col="goldenrod") 

#ellipse 2.1
x=c(24,72,90,114,264,294,318,390,522,594,618,648,798,822,840,888)
y=c(547,619,643,673,823,847,865,913,979,1003,1009,1015,1015,1009,1003,979)
points(x, y, pch=16, col="deeppink1") 

#ellipse 2.2
x=c(49,97,115,139,289,319,343,415,547,619,643,673,823,847,865,913)
y=c(522,594,618,648,798,822,840,888,954,978,984,990,990,984,978,954)
points(x, y, pch=16, col="goldenrod") 

#ellipse 3.1
x=c(243,279,1719,1773,2709,2763,4203,4239)
y=c(2764,2818,4258,4294,4762,4780,4780,4762)
points(x, y, pch=16, col="deeppink1") 

#ellipse 2.2
x=c(298,334,1774,1828,2764,4294,2818,4258)
y=c(2709,2763,4203,4239,4707,4707,4725,4725)
points(x, y, pch=16, col="goldenrod") 

#tetrad set 1
x=c(37,136,69,381,201,2097)
y=c(174,141,1018,1000,3166,2824)
points(x, y, pch=16, col="gray2") 

#tetrad set 2 # associated with n-curve
x=c(16,502,46,1810)
y=c(801,729,6441,6393)
points(x, y, pch=16, col="lawngreen") 

#intercept ellipse work out
n=1:5
k0=3*n^2+1
k=6*k0^2-6*k0+1
k=54*n^4+18*n^2+1 #direct

j0=6*n^2+1
j1=3*j0^2+1
j=k^2-j1
j=3*(972*n^8+648*n^6+108*n^4-1) #direct

c=(k+1)/2
a=c+(9*n^2+1)
b=c-(9*n^2+2)

#isolate tetrads
ye=cbrts[which(lengths(cbrts)==4)]
l=1
tetrad=array(data=0, dim = c(length(ye)*2,2))
for (j in 1:length(ye)) {
  tetrad[l,]=c(ye[[j]][1],ye[[j]][4])
  l=l+1
  tetrad[l,]=c(ye[[j]][2],ye[[j]][3])
  l=l+1
}

points(tetrad, pch=16, col="orange")

#roots and ratios
pander(cbind(te, sprintf("%.3f", te[,2]/te[,1])), row.names = F, col.names=c("a", "b", "b/a"), justify="rrr") 

#rmpfr------------
library(Rmpfr)
prec=500
n=8
apr=function(x) mpfr(x, precBits = prec)

ninecubed=rep(NA, n)
for (j in 1:n) {
  ninecubed[j]=formatMpfr(apr(9)*apr(j)^3)
}

n=10000
te=rep(NA,n)
for (j in 1:n) {
  te[j]=formatMpfr(mpfr(ninecubed[4])^apr(3)+(apr(2866+j))^apr(3))
}

n=100000
cube.diff.p=rep(NA, n)

for (j in 1:n) {
  cube.diff.p[j]=formatMpfr(apr(3)*apr(j+90000)^2+apr(3)*apr(j+90000)+apr(1))
}


#hexagonal spiral--------
e=exp(1)
n=5
rots=vector(mode="list", length = n)
for(j in 1:n){
  te=c(0:(j*6-1))
  rots[[j]]=e^(te*2*pi/length(te)*1i)
  rots[[j]]=c(tail(rots[[j]],(j-1)), head(rots[[j]],length(rots[[j]])-(j-1)))
}
rots=c(0+0i, rots)
rots=unlist(rots)*c(0,seq(.3, 7, length.out = (length(unlist(rots))-1)))
par(mar=c(1,1,2,1))
plot(rots, type="n", asp=1, axes = F, xlab="", ylab="")
lines(rots, lty=2)
text(rots, labels = 1:length(rots), col = "dodgerblue3", font=2)
lines(rots[c(6,17,34, 57, 86)], col="red", lty=3)
