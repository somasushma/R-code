library("Rmpfr")
prec=1500

#constants--------
const.list=list()
const.list$be=(3+sqrt(13))/2
const.list$bep=(-3+sqrt(13))/2
const.list$sil=1+sqrt(2)
const.list$silp=-1+sqrt(2)
const.list$phi=(1+sqrt(5))/2
const.list$phip=(-1+sqrt(5))/2
const.list$Cu=2+sqrt(5)
const.list$Cup=-2+sqrt(5)

e=exp(1)
#with mpfr----------------
f.list=list()
f.list$bronze1=function(x) x/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))
f.list$bronze2=function(x) (mpfr(2,prec)*x-mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))
f.list$bronze3=function(x) (mpfr(2,prec)*x+mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(3,prec)*x-mpfr(1,prec))
f.list$silver1=function(x) x/(x^mpfr(2,prec)+mpfr(2,prec)*x-mpfr(1,prec))
f.list$silver2=function(x) (mpfr(2,prec)*x+mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(2,prec)*x-mpfr(1,prec))
f.list$golden1=function(x) x/(x^mpfr(2,prec)+mpfr(1,prec)*x-mpfr(1,prec))
f.list$golden2=function(x) x/(x^mpfr(2,prec)-mpfr(1,prec)*x-mpfr(1,prec))
f.list$golden3=function(x) (mpfr(2,prec)*x+mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(1,prec)*x-mpfr(1,prec))
f.list$copper1=function(x) x/(x^mpfr(2,prec)+mpfr(4,prec)*x-mpfr(1,prec))
f.list$copper2=function(x) (mpfr(2,prec)*x-mpfr(1,prec))/(x^mpfr(2,prec)+mpfr(4,prec)*x-mpfr(1,prec))

#regular----------------
f.s.list=list()
f.s.list$bronze1=function(x) x/(x^2+3*x-1) #1
f.s.list$bronze2=function(x) (2*x-1)/(x^2+3*x-1) #2
f.s.list$bronze3=function(x) (2*x+1)/(x^2+3*x-1) #3
f.s.list$silver1=function(x) x/(x^2+2*x-1) #4
f.s.list$silver2=function(x) (2*x+1)/(x^2+2*x-1) #5
f.s.list$golden1=function(x) x/(x^2+x-1) #6
f.s.list$golden2=function(x) x/(x^2-x-1) #7
f.s.list$golden3=function(x) (2*x+1)/(x^2+x-1) #8
f.s.list$copper1=function(x) x/(x^2+4*x-1) #9
f.s.list$copper2=function(x) (2*x-1)/(x^2+4*x-1) #10

#mpfr version----------------
w=2
f=f.list[[w]]
n=1000
xbox=rep(NA,n)
x0=unirootR(function(x) x^3+3*x^2-3*x+1, interval= mpfr(c(-3.8,-4.85), prec), tol = 1e-600, maxiter = 5000)$root #mpfr(54, prec)/mpfr(10,prec)
x=x0
for(j in 1:n) {
  x=f(x)
  xbox[j]=formatMpfr(x)
}

png(filename = "~/R/Figures/Figures1/bouncer_x1.png",width = 1500, height = 1000, res=150, pointsize = 15)
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1), oma=c(0,0,0,0))
plot(as.numeric(xbox), type="l", xlab="n", ylab=expression(x[n]), main = bquote("evolution of"~ x[0]==.(as.numeric(x0))~"under" ~ .(body(f.s.list[[w]]))))
dev.off()

#asinh plot split into 5
ye=as.numeric(xbox)
ge=ceiling(log10(abs(range(ye))))
ge=c(-10^(seq(0,ge[1],1)),0,10^(seq(0,ge[2],1)))
m=5
png(filename = "~/R/Figures/Figures1/bouncer_x2.png",width = 1500, height = 2200, res=150, pointsize = 15)
par(mfrow=c(m,1), oma=c(0,0,2,0))
for (j in 1:m) {
  a=(j-1)*n/m+1; b=j*n/m
  par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
  plot(asinh(as.numeric(xbox[a:b])), type="l", yaxt="n",xlab="n", ylab=bquote(asinh(x[n])), main = bquote(.(a)~".."~.(b)))
  axis(side = 2, at = asinh(ge), labels = ge)
  
  abline(h=asinh(c(const.list$phi, -const.list$phip, -.2,0,.2)), col=c("green","green","red","blue", "red"), lty=2, lwd=1.5)
  te=which(abs(as.numeric(xbox[a:b]))>50)
  points(x=te,y=asinh(as.numeric(xbox[a:b][te])),pch=16, cex=.85, col="red")
  points(x=te-1,y=asinh(as.numeric(xbox[a:b][te-1])), cex=.85, pch=16, col="blue")  
}
mtext(text = bquote("evolution of"~ x[0]==.(as.numeric(x0))~"under" ~ .(body(f.s.list[[w]]))),outer = T)
dev.off()

#asinh plot of particular regions----------
which(abs(ye)>100)

png(filename = "~/R/Figures/Figures1/bouncer_x3.png",width = 1500, height =1000, res=150, pointsize = 15)
a=5500; b=6100
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1), oma=c(0,0,0,0))
plot(asinh(as.numeric(xbox[a:b])), type="l", yaxt="n", xaxt="n", xlab="n", ylab=bquote(asinh(x[n])), main = bquote("evolution of"~ x[0]==.(as.numeric(x0))~"under" ~ .(body(f.s.list[[w]]))))
axis(side = 2, at = asinh(ge), labels = ge)
axis(side = 1, at = seq(a-a,b-a,100), labels = seq(a,b,100))

abline(h=asinh(c(const.list$phi,-const.list$phip, -.2,0,.2)), col=c("green","green","red","blue", "red"), lty=2, lwd=1.5)

# abline(h=asinh(c(4-2*sqrt(2), -1+1/sqrt(2))), col="darkgreen", lty=2, lwd=1.5)

te=which(abs(as.numeric(xbox[a:b]))>50)
points(x=te,y=asinh(as.numeric(xbox[a:b][te])),pch=16, cex=.85, col="red")
points(x=te-1,y=asinh(as.numeric(xbox[a:b][te-1])), cex=.85, pch=16, col="blue")
dev.off()

#histogram-------------
png(filename = "~/R/Figures/Figures1/bouncer_x4.png",width = 2000, height =1500, res=150, pointsize = 20)

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
hist(asinh(ye), breaks= 500, col = "gray", xaxt="n",xlab=bquote("asinh scale:"~ x[n]), main=bquote("frequency distribution of"~asinh(x[n])))
rug(asinh(ye), col = "dodgerblue2")
axis(side = 1, at = asinh(ge), labels = ge)
abline(v=asinh(0), lty=3)
box()

curve((1/(1+x^2))*332, from = -10, to=10, n=1000, col="blue", lwd=2, add=T) #curve fitting for above: classic cauchy
curve((1/(1+.25*abs(x)^2.3+.1*abs(x)^3.6))*332, from = -10, to=10, n = 1000, col="red", lwd=2, add=T) #curve fitting for above: best

dev.off()

curve((1/(1+.95*abs(x)^2.3))*332, from = -10, to=10, n = 1000, col="darkgreen", lwd=2, add=T) #curve fitting for above
curve((1/(1+1.3*x^2))*332, from = -10, to=10, n=1000, col="darkred", lwd=2, add=T) #curve fitting for above

#multiple points---------
x0s=c(mpfr(57, prec)/mpfr(10,prec), mpfr(1455, prec)/mpfr(100,prec), mpfr(-1245, prec)/mpfr(100,prec))

n=400
w=2
f=f.list[[w]]

png(filename = "~/R/Figures/Figures1/bouncer_xm.png",width = 1500, height = 2000, res=150, pointsize = 20)
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(3,1), oma=c(0,0,2,0))

for (j in 1:length(x0s)) {
xbox=rep(NA,n)
x0=x0s[j]
x=x0
for(j in 1:n) {
  x=f(x)
  xbox[j]=formatMpfr(x)
}

plot(as.numeric(xbox), type="l", xlab="n", ylab=expression(x[n]), main = bquote(x[0]==.(as.numeric(x0))))
}

mtext(bquote("evolution of"~ x[0]~"under" ~ .(body(f.s.list[[w]]))), outer = T)

dev.off()


#-iterations and roots to converge plot of bronze2------------
f=f.list$bronze2

n=1000
dx=mpfr(25,prec)/mpfr(1000,prec)
xmax=mpfr(15, prec)
xlist=list()
tol=6
x0s=seqMpfr(from=-xmax, to=xmax, by=dx)
for(k in 1:length(seq(-xmax,xmax,dx))) {
  x=x0s[k]
  xbox=rep(NA,n)
  for(j in 1:n) {
    x=f(x)
    y=as.numeric(x)
    if( round(y,tol) %in% round(xbox, tol)) break
    xbox[j]=y
  }
  xlist[[k]]=c(na.exclude(xbox))
}

te=as.numeric(x0s)

png(filename = "~/R/Figures/Figures1/bouncer_covergence.png",width = 2100, height = 1500, res=150, pointsize = 20)

par(mar=c(2.2,2.5,2,1), mgp=c(1.2,.4,0))
plot(x=te, y=lengths(xlist), type="h", col="#ca0020", xlab = bquote(x[0]), ylab = bquote("iterations to reach within "~10^-6 ~" of roots"), main = bquote("preferred roots for"~x[0]~ "to converge under" ~ (2*x-1)/(x^2+3*x-1))) #r1

points(x=te[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))],y=lengths(xlist)[unlist(lapply(xlist, function(x) any(abs(tail(x,10))>2)))], type="h", col="#1a9641") 

#second root vs first root for bronze2---------
r1=Re(polyroot(c(1,-3,0,1)))
r2=Re(polyroot(c(1,-2,-6,7,1)))
abline(v=r1, col="darkblue", lwd=1.5)
abline(v=r2, col="darkorange", lwd=1.5)

dev.off()

#specifics from above-----
which(lengths(xlist)==739)
k=1183

#regular
png(filename = "~/R/Figures/Figures1/rootseeker_x.png",width = 2500, height =2000, res=200, pointsize = 20)

par(mfrow=c(2,1), oma=c(0,0,2,0))

par(mar=c(2,2.5,0,1), mgp=c(1.1,.4,0))
plot(xlist[[k]], type = "l", xlab = "n", ylab = bquote(x[n]))

#asinh
ge=ceiling(log10(abs(range(xlist[[k]]))))
ge=c(-10^(seq(0,ge[1],1)),0,10^(seq(0,ge[2],1)))

par(mar=c(2,2.5,0,1), mgp=c(1.1,.4,0))
plot(asinh(xlist[[k]]), type = "l", yaxt="n", xlab = "n", ylab = bquote(asinh(x[n])))
axis(side = 2, at = asinh(ge), labels = ge)

abline(h=asinh(c(const.list$bep, -const.list$be)), col="blue")

ye=which(abs(xlist[[k]])>20)

abline(h=asinh(r1), col="darkred")

abline(h=asinh(r2), col="darkred")

points(ye, asinh(xlist[[k]][ye]), col="red", pch=16, cex=.5)
points(ye-1, asinh(xlist[[k]][ye-1]), col="blue", pch=16, cex=.5)

mtext(bquote("evolution of"~ x[0]~"="~.(te[k])~"under" ~ .(body(f.s.list[[w]]))), outer = T)

dev.off()

#r1 structure
r1.1=2*sin(pi/18)
r1.2=-2*cos(pi/9)
r1.3=2*cos(2*pi/9)

#r1 sequence
n=20
f=rep(NA,n)
f[1]=0
f[2]=1
f[3]=3
for (j in 4:n) {
  f[j]=3*f[j-1]-f[j-3]
}
sapply(1:(n-1), function(x)f[x+1]/f[x])

#r2 structure
r2.1 = (-7 + 3*sqrt(5) + sqrt(150 - 66*sqrt(5)))/4
r2.2 = (-7 - 3*sqrt(5) - sqrt(150 + 66*sqrt(5)))/4
r2.3 = (-7 + 3*sqrt(5) - sqrt(150 - 66*sqrt(5)))/4
r2.4 = (-7 - 3*sqrt(5) + sqrt(150 + 66*sqrt(5)))/4


#r2 sequence 1--------
c(1,-2,-6,7,1)
n=20
f=rep(NA,n)
f[1]=-1
f[2]=0
f[3]=1
f[4]=2
for (j in 5:n) {
  f[j]=7*f[j-1]+6*f[j-2]-2*f[j-3]-1*f[j-4]
}
sapply(1:(n-1), function(x)f[x+1]/f[x])

#r2 sequence 2--------
n=30
f=rep(NA,n)
f[1]=-1
f[2]=0
f[3]=1
f[4]=2
for (j in 5:n) {
  f[j]=7*f[j-2]+f[j-1]-sum(f[1:(j-4)])
}
sapply(1:(n-1), function(x)f[x+1]/f[x])

#copper convergence-----------

f=f.list$copper2

n=1000
dx=mpfr(1,prec)/mpfr(100,prec)
xmax=mpfr(40, prec)
xlist=list()
x0s=seqMpfr(from=-xmax, to=xmax, by=dx)
for(k in 1:length(seq(-xmax,xmax,dx))) {
  x=x0s[k]
  xbox=rep(NA,n)
  for(j in 1:n) {
    x=f(x)
    y=as.numeric(x)
    xbox[j]=y
    if( j >20 && all(tail(xbox[1:j],20) > (-3.5)) && all(tail(xbox[1:j],20)< 2)) break
  }
  xlist[[k]]=c(na.exclude(xbox))
}

te=as.numeric(x0s)

png(filename = "~/R/Figures/Figures1/bouncer_covergence2.png",width = 2100, height = 1500, res=150, pointsize = 20)

par(mar=c(2.2,2.5,2,1), mgp=c(1.2,.4,0))
plot(x=te, y=lengths(xlist), type="h", col="#ca0020", xlab = bquote(x[0]), ylab = bquote("iterations to bands"), main = bquote("Iterations to converge band under" ~ (2*x-1)/(x^2+4*x-1))) 

dev.off()

#plot specific of above----------
which(lengths(xlist)==max(lengths(xlist)))
n=300
xbox=rep(NA,n)
x0=x0s[4438]
x=x0
for(j in 1:n) {
  x=f(x)
  xbox[j]=as.numeric(x)
}

png(filename = "~/R/Figures/Figures1/band_convergence.png",width = 2500, height =2000, res=200, pointsize = 20)

par(mfrow=c(2,1), oma=c(0,0,2,0))

#regular
par(mar=c(2,2,1,1), mgp=c(1.1,.4,0))
plot(xbox, type="l", xlab="n", ylab=expression(x[n]))

#asinh
ge=ceiling(log10(abs(range(xlist[[k]]))))
ge=c(-10^(seq(0,ge[1],1)),0,10^(seq(0,ge[2],1)))

par(mar=c(2,2.5,0,1), mgp=c(1.1,.4,0))
plot(asinh(xbox), type = "l", yaxt="n", xlab = "n", ylab = bquote(asinh(x[n])))
axis(side = 2, at = asinh(ge), labels = ge)

abline(h=asinh(c(const.list$Cup, -const.list$Cu)), col="blue")

ye=which(abs(xbox)>20)

points(ye, asinh(xbox[ye]), col="red", pch=16, cex=.5)
points(ye-1, asinh(xbox[ye-1]), col="blue", pch=16, cex=.5)

mtext(bquote("evolution of"~ x[0]~"="~ .(as.numeric(x0))~" under" ~ .(body(f.s.list[[w]]))), outer = T)

dev.off()

#point plotting
n=3000
xbox=rep(NA,n)
x0=mpfr(2,prec)/(mpfr(5,prec)+sqrt(mpfr(5,prec)))
x=x0
for(j in 1:n) {
  x=f(x)
  xbox[j]=as.numeric(x)
}

png(filename = "~/R/Figures/Figures1/band_convergence_points.png",width = 2500, height =1000, res=200, pointsize = 15)

par(mar=c(2,2,2,1), mgp=c(1.1,.4,0), mfrow=c(1,1), oma=c(0,0,0,0))
plot(xbox[300:n], type="l", col="gray", xlab="n", ylab=expression(x[n]), main = bquote("evolution of"~ x[0]==.(as.numeric(x0))~"under" ~ .(body(f.s.list[[w]]))~  " n=" ~ 300 ~ ".." ~ .(n)))

points(x=which(xbox[300:n]< -2), y=xbox[300:n][which(xbox[300:n]< -2)], pch=16, cex=.5, col="darkmagenta")

points(x=which(xbox[300:n]> 1), y=xbox[300:n][which(xbox[300:n]> 1)], pch=16, cex=.5, col="deepskyblue4")

points(x=which(xbox[300:n]> 0 & xbox[300:n] < 1), y=xbox[300:n][which(xbox[300:n]> 0 & xbox[300:n] < 1)], pch=16, cex=.5, col="firebrick3")

abline(h=c(2/(5+sqrt(5)), sqrt(5)-1, 2-1/(12+sqrt(5)), -2-sqrt(5)/2, -2-1/sqrt(5), (5-sqrt(5))/2, -(12*sqrt(5)-11)/6), col="gray20", lty=2, lwd=1.5) # bounds for copper2

dev.off()

#plain for quick check---------
n=1000
f4=function(x) (2*x-1)/(x^2+3*x-1)
xbox=rep(NA,n)
x0=-3.847322101863073
x=x0
for(j in 1:n) {
  x=f4(x)
  xbox[j]=x
}
par(mar=c(2,2,2,1), mgp=c(1.1,.4,0))
plot((xbox), type="l", cex=.2)


